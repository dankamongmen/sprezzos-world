// download_install_manager.cc
//
//   Copyright (C) 2005-2011 Daniel Burrows
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

#include "download_install_manager.h"

#include "config_signal.h"
#include "download_signal_log.h"
#include "log.h"

#include <aptitude.h>

#include <apt-pkg/acquire-item.h>
#include <apt-pkg/dpkgpm.h>
#include <apt-pkg/error.h>
#include <apt-pkg/sourcelist.h>

#include <sigc++/bind.h>

#include <pthread.h>
#include <signal.h>

using namespace std;

download_install_manager::download_install_manager(bool _download_only,
                                                   bool _fix_missing,
						   const run_dpkg_in_terminal_func &_run_dpkg_in_terminal)
  : log(NULL),
    download_only(_download_only),
    fix_missing(_fix_missing),
    pm(new pkgDPkgPM(*apt_cache_file)),
    run_dpkg_in_terminal(_run_dpkg_in_terminal)
{
}

download_install_manager::~download_install_manager()
{
  delete pm;
}

bool download_install_manager::prepare(OpProgress &progress,
				       pkgAcquireStatus &acqlog,
				       download_signal_log *signallog)
{
  log = signallog;

  if(apt_cache_file == NULL)
    {
      _error->Error(_("The package cache is not available; unable to download and install packages."));
      return false;
    }

  if(!(*apt_cache_file)->save_selection_list(progress))
    return false;

  progress.Done();

  // Abort here so we don't spew random messages below.
  if(_error->PendingError())
    return false;

  fetcher = new pkgAcquire;
  if(fetcher->Setup(&acqlog, aptcfg->FindDir("Dir::Cache::archives")) == false)
    {
      delete fetcher;
      fetcher = NULL;
      return false;
    }

  if(!src_list.ReadMainList())
    {
      _error->Error(_("Couldn't read source list"));

      delete fetcher;
      fetcher = NULL;
      return false;
    }

  if(!pm->GetArchives(fetcher, &src_list, apt_package_records) ||
     _error->PendingError())
    {
      _error->Error(_("Internal error: couldn't generate list of packages to download"));

      delete fetcher;
      fetcher = NULL;
      return false;
    }

  return true;
}

download_manager::result download_install_manager::finish_pre_dpkg(pkgAcquire::RunResult res)
{
  if(res == pkgAcquire::Failed)
    return failure;

  bool failed=false;
  for(pkgAcquire::ItemIterator i = fetcher->ItemsBegin();
      i != fetcher->ItemsEnd(); ++i)
    {
      if((*i)->Status == pkgAcquire::Item::StatDone &&
	 (*i)->Complete == true)
	continue;

      if((*i)->Status == pkgAcquire::Item::StatIdle)
        {
          // Transient = true;
          continue;
        }

      failed=true;
      _error->Warning(_("Failed to fetch %s: %s"),
                      (*i)->DescURI().c_str(), (*i)->ErrorText.c_str());
    }

  if(download_only)
    {
      // TODO: Handle files on other CDROMs (StatIdle?).
      if(failed)
	{
	  _error->Error(_("Some files failed to download"));
	  return failure;
	}
      else
	{
	  return success;
	}
    }

  if(failed == true && fix_missing == false)
    {
      _error->Error(_("Unable to fetch some archives, maybe run aptitude"
                      " update or try with --fix-missing?"));
      return failure;
    }

  if(failed && !pm->FixMissing())
    {
      _error->Error(_("Unable to correct for unavailable packages"));
      return failure;
    }

  log_changes();

  // Note that someone could grab the lock before dpkg takes it;
  // without a more complicated synchronization protocol (and I don't
  // control the code at dpkg's end), them's the breaks.
  apt_cache_file->ReleaseLock();

  download_manager::result rval;

  const pkgPackageManager::OrderResult pre_fork_result =
    pm->DoInstallPreFork();

  switch(pre_fork_result)
    {
    case pkgPackageManager::Completed:  rval = success; break;
    case pkgPackageManager::Failed:     rval = failure; break;
    case pkgPackageManager::Incomplete: rval = do_again; break;
    default:
      rval = failure;
      _error->Error("Unexpected result from pkgPackageManager::DoInstallPreFork");
    }

  return rval;
}

pkgPackageManager::OrderResult download_install_manager::run_dpkg(int status_fd)
{
  sigset_t allsignals;
  sigset_t oldsignals;
  sigfillset(&allsignals);

  pthread_sigmask(SIG_UNBLOCK, &allsignals, &oldsignals);
  pkgPackageManager::OrderResult pmres = pm->DoInstallPostFork(status_fd);

  switch(pmres)
    {
    case pkgPackageManager::Failed:
      _error->DumpErrors(std::cerr, GlobalError::WARNING, false);
      cerr << _("A package failed to install.  Trying to recover:") << endl;
      if(system("DPKG_NO_TSTP=1 dpkg --configure -a") != 0) { /* ignore */ }
      break;
    case pkgPackageManager::Completed:
      break;

    case pkgPackageManager::Incomplete:
      break;
    }

  pthread_sigmask(SIG_SETMASK, &oldsignals, NULL);

  return pmres;
}

void download_install_manager::finish_post_dpkg(pkgPackageManager::OrderResult dpkg_result,
						OpProgress *progress,
						const sigc::slot1<void, result> &k)
{
  result rval;
  switch(dpkg_result)
    {
    case pkgPackageManager::Failed:
      rval = failure;
      break;
    case pkgPackageManager::Completed:
      rval = success;
      break;
    case pkgPackageManager::Incomplete:
      rval = do_again;
      break;
    }

  fetcher->Shutdown();

  if(_error->PendingError() == true)
    rval = failure;

  // Get the archives again.  This was necessary for multi-CD
  // installs, according to my comments in an old commit log in the
  // Subversion repository.
  if(rval == do_again)
    {
      if(!pm->GetArchives(fetcher, &src_list, apt_package_records))
        rval = failure;
    }

  if(!apt_cache_file->GainLock())
    // This really shouldn't happen.
    {
      _error->Error(_("Could not regain the system lock!  (Perhaps another"
                      " apt or dpkg is running?)"));
      rval = failure;
    }

  if(rval != do_again)
    {
      apt_close_cache();

      if(log != NULL)
	log->Complete();

      // We absolutely need to do this here.  Yes, it slows things
      // down, but without this we get stuff like #429388 due to
      // inconsistencies between aptitude's state file and the real
      // world.
      //
      // This implicitly updates the package state file on disk.
      if(!download_only)
	apt_load_cache(progress, true);

      if(aptcfg->FindB(PACKAGE "::Forget-New-On-Install", false)
	 && !download_only)
	{
	  if(apt_cache_file != NULL)
	    {
	      (*apt_cache_file)->forget_new(NULL);
	      (*apt_cache_file)->save_selection_list(*progress);
	      post_forget_new_hook();
	    }
	}
    }

  k(rval);
}

void download_install_manager::finish(pkgAcquire::RunResult result,
				      OpProgress *progress,
				      const sigc::slot1<void, download_manager::result> &k)
{
  const download_manager::result pre_res = finish_pre_dpkg(result);

  if(pre_res != failure && !download_only)
    {
      run_dpkg_in_terminal(sigc::mem_fun(*this, &download_install_manager::run_dpkg),
			   sigc::bind(sigc::mem_fun(*this, &download_install_manager::finish_post_dpkg),
				      progress,
				      k));
      return;
    }
  else
    {
      pkgPackageManager::OrderResult res;
      switch(pre_res)
	{
	case success:
	  res = pkgPackageManager::Completed;
	  break;
	case do_again:
	  res = pkgPackageManager::Incomplete;
	  break;
	case failure:
	  res = pkgPackageManager::Failed;
	  break;
        default:
          res = pkgPackageManager::Failed;
          _error->Error("Unexpected result from download_install_manager::finish_pre_dpkg");
	}

      finish_post_dpkg(res,
		       progress,
		       k);
      return;
    }
}
