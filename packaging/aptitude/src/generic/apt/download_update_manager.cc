// download_update_manager.cc
//
//   Copyright (C) 2005, 2007-2009, 2011 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "download_update_manager.h"

#include "apt.h"
#include "config_signal.h"
#include "download_signal_log.h"

#include <apt-pkg/acquire-item.h>
#include <apt-pkg/cachefile.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

namespace cw = cwidget;

class my_cleaner:public pkgArchiveCleaner
{
protected:
  virtual void Erase(const char *file,
		     string pkg,
		     string ver,
		     struct stat &stat)
  {
    unlink(file);
  }
};

download_update_manager::download_update_manager()
  : log(NULL)
{
}

download_update_manager::~download_update_manager()
{
}

bool download_update_manager::prepare(OpProgress &progress,
				      pkgAcquireStatus &acqlog,
				      download_signal_log *signallog)
{
  log = signallog;

  if(apt_cache_file != NULL &&
     !(*apt_cache_file)->save_selection_list(progress))
    return false;

  // FIXME: should save_selection_list do this?
  progress.Done();

  if(src_list.ReadMainList() == false)
    {
      _error->Error(_("Couldn't read list of package sources"));
      return false;
    }

  // Abort here so we don't spew random messages below.
  if(_error->PendingError())
    return false;

  fetcher = new pkgAcquire();
  if(fetcher->Setup(&acqlog, aptcfg->FindDir("Dir::State::Lists")) == false ||
     src_list.GetIndexes(fetcher) == false)
    {
      delete fetcher;
      fetcher = NULL;
      return false;
    }

  // Run scripts
  RunScripts("APT::Update::Pre-Invoke");

  return true;
}

void download_update_manager::finish(pkgAcquire::RunResult res,
				     OpProgress *progress,
				     const sigc::slot1<void, result> &k)
{
  if(log != NULL)
    log->Complete();

  apt_close_cache();

  if(res != pkgAcquire::Continue)
    {
      k(failure);
      return;
    }

  bool failed = false;
  bool transientNetworkFailure = false;
  result rval = success;

  // We need to claim that the download failed if any source failed,
  // and invoke Finished() on any failed items.  Also, we shouldn't
  // clean the package lists if any individual item failed because it
  // makes users grumpy (see Debian bugs #201842 and #479620).
  //
  // See also apt-get.cc.
  for(pkgAcquire::ItemIterator it = fetcher->ItemsBegin();
      it != fetcher->ItemsEnd(); ++it)
    {
      if((*it)->Status == pkgAcquire::Item::StatDone)
	continue;

      (*it)->Finished();

      ::URI uri((*it)->DescURI());
      uri.User.clear();
      uri.Password.clear();
      const std::string descUri = string(uri);
      _error->Warning(_("Failed to fetch %s: %s"), descUri.c_str(), (*it)->ErrorText.c_str());

      if((*it)->Status == pkgAcquire::Item::StatTransientNetworkError)
	{
	  transientNetworkFailure = true;
	  continue;
	}

      failed = true;
      rval = failure;
    }

  // Clean old stuff out
  const std::string listsdir = aptcfg->FindDir("Dir::State::lists");
  if(!transientNetworkFailure && !failed &&
     (aptcfg->FindB("APT::Get::List-Cleanup", true) == true &&
      aptcfg->FindB("APT::List-Cleanup", true) == true) &&
     (fetcher->Clean(listsdir) == false ||
      fetcher->Clean(listsdir + "partial/") == false))
    {
      _error->Error(_("Couldn't clean out list directories"));
      rval = failure;
    }

  if(transientNetworkFailure == true)
    _error->Warning(_("Some index files failed to download. They have been ignored, or old ones used instead."));
  else if(failed == true)
    _error->Error(_("Some index files failed to download. They have been ignored, or old ones used instead."));

  if(rval != failure)
    {
      // Run the success scripts if all was fine
      if(!transientNetworkFailure && !failed)
	RunScripts("APT::Update::Post-Invoke-Success");

      // Run the other scripts
      RunScripts("APT::Update::Post-Invoke");
    }

  // Rebuild the apt caches as done in apt-get.  cachefile is scoped
  // so it dies before we possibly-reload the cache.  This will do a
  // little redundant work in visual mode, but avoids lots of
  // redundant work at the command-line.
  {
    pkgCacheFile cachefile;
    pkgCacheFile::RemoveCaches();
    if(!cachefile.BuildCaches(progress, true))
      {
	_error->Error(_("Couldn't rebuild package cache"));
	k(failure);
	return;
      }
  }

  bool need_forget_new = 
    aptcfg->FindB(PACKAGE "::Forget-New-On-Update", false);

  bool need_autoclean =
    aptcfg->FindB(PACKAGE "::AutoClean-After-Update", false);

  if(need_forget_new || need_autoclean)
    apt_load_cache(progress, true);

  if(apt_cache_file != NULL && need_forget_new)
    {
      (*apt_cache_file)->forget_new(NULL);
      post_forget_new_hook();
    }

  if(apt_cache_file != NULL && need_autoclean)
    {
      pre_autoclean_hook();

      my_cleaner cleaner;
      cleaner.Go(aptcfg->FindDir("Dir::Cache::archives"), *apt_cache_file);
      cleaner.Go(aptcfg->FindDir("Dir::Cache::archives")+"partial/",
		 *apt_cache_file);

      post_autoclean_hook();
    }

  k(rval);
  return;
}

