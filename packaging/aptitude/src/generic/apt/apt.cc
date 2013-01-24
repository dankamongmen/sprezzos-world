// apt.cc
//
//  Copyright 1999-2010 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  Handles basic apt bookkeeping.

#include "apt.h"

#include <aptitude.h>
#include <loggers.h>

#include "aptitude_resolver_universe.h"
#include "config_signal.h"
#include "download_queue.h"
#include "pkg_hier.h"
#include "resolver_manager.h"
#include "rev_dep_iterator.h"
#include "tags.h"
#include "tasks.h"

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/transcode.h>

#include <generic/util/file_cache.h>
#include <generic/util/util.h>

#include <generic/util/undo.h>

#include <apt-pkg/configuration.h>
#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/init.h>
#include <apt-pkg/pkgcachegen.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/version.h>
#include <apt-pkg/aptconfiguration.h>

#include <fstream>

#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

using namespace std;
using aptitude::Loggers;

namespace cw = cwidget;

enum interesting_state {uncached = 0, uninteresting, interesting};
static interesting_state *cached_deps_interesting = NULL;

// Memoization of surrounding_or.  To save space, the low bit of each
// pointer in the following table is set to 1 when a result is cached:
static pkgCache::Dependency **cached_surrounding_or = NULL;

pkg_hier *user_pkg_hier=NULL;

string *pendingerr=NULL;
bool erroriswarning=false;


// Set to "true" if we have a version of the apt library with
// support for overriding configuration settings via RootDir.
static bool apt_knows_about_rootdir;

static Configuration *theme_config;
static Configuration *user_config;

sigc::signal0<void> cache_closed, cache_reloaded, cache_reload_failed;
sigc::signal0<void> hier_reloaded;
sigc::signal0<void> consume_errors;

static string apt_native_arch;

static void reset_interesting_dep_memoization()
{
  delete[] cached_deps_interesting;
  cached_deps_interesting = NULL;
}

static void reset_surrounding_or_memoization()
{
  delete[] cached_surrounding_or;
  cached_surrounding_or = NULL;
}

static void reload_user_pkg_hier()
{
  delete user_pkg_hier;
  user_pkg_hier=new pkg_hier;

  user_pkg_hier->input_file(PKGDATADIR "/function_groups");

  string cfgloc(get_homedir());
  if(!cfgloc.empty())
    {
      string user_hier=cfgloc+string("/.aptitude/function_pkgs");
      if(access(user_hier.c_str(), R_OK)==0)
	user_pkg_hier->input_file(user_hier);
      else
	user_pkg_hier->input_file(PKGDATADIR "/function_pkgs");
    }
}

bool get_apt_knows_about_rootdir()
{
  return apt_knows_about_rootdir;
}

void apt_preinit(const char *rootdir)
{
  logging::LoggerPtr logger(Loggers::getAptitudeAptGlobals());

  // The old name for the recommends-should-be-automatically-installed
  // setting and the new one.
  const char * const aptitudeIgnoreRecommendsImportant = PACKAGE "::Ignore-Recommends-Important";
  const char * const aptitudeRecommendsImportant = PACKAGE "::Recommends-Important";
  const char * const aptInstallRecommends = "APT::Install-Recommends";

  signal(SIGPIPE, SIG_IGN);

  // Probe apt to see if it has RootDir support.
  {
    Configuration tmp;
    config_change_pusher push1("RootDir", "/a/b/c/d", tmp);
    config_change_pusher push2("Dir::ASDF", "/x/y/z", tmp);

    std::string found_loc = tmp.FindFile("Dir::ASDF");

    std::string real_found_loc;
    for(std::string::const_iterator it = found_loc.begin();
	it != found_loc.end(); ++it)
      {
	// Drop repeated slashes.
	if(*it != '/' || real_found_loc.size() == 0 ||
	   real_found_loc[real_found_loc.size() - 1] != '/')
	  real_found_loc.push_back(*it);
      }

    apt_knows_about_rootdir =
      (real_found_loc == "/a/b/c/d/x/y/z");
  }

  theme_config=new Configuration;
  user_config=new Configuration;

  if(rootdir != NULL)
    {
      _config->Set("RootDir", rootdir);
      theme_config->Set("RootDir", rootdir);
      user_config->Set("RootDir", rootdir);
    }

  ReadConfigFile(*theme_config, PKGDATADIR "/aptitude-defaults");

  pkgInitConfig(*_config);


  ReadConfigFile(*_config, PKGDATADIR "/section-descriptions");

  // ForTranslators: Set this string to the name of a configuration
  // file in $pkgdatadir/aptitude that overrides defaults for your
  // language.  This is particularly intended for overriding entries
  // in the Aptitude::Sections::Descriptions tree.
  //
  // For instance, Sections localized for the language .ww might be
  // stored in a file named aptitude-defaults.ww, which would be
  // indicated by translating "Localized defaults|" below to
  // "aptitude-defaults.ww".  If you use this mechanism, you should
  // also add your defaults file to pkgdata_DATA in Makefile.am.
  std::string localized_config_name = P_("Localized defaults|");
  if(localized_config_name.size() > 0)
    ReadConfigFile(*_config, PKGDATADIR "/" + localized_config_name);

  pkgInitSystem(*_config, _system);

  // Allow a user-specific customization file.
  const char *HOME = getenv("HOME");

  string cfgloc;

  if(HOME != NULL && *HOME != '\0' &&
     access((string(HOME) + "/.aptitude").c_str(), R_OK | X_OK) == 0)
    cfgloc = string(HOME) + "/.aptitude/config";
  else
    {
      cfgloc = get_homedir();
      if(!cfgloc.empty())
	cfgloc += "/.aptitude/config";
    }

  if(!cfgloc.empty() && access(cfgloc.c_str(), R_OK) == 0)
    {
      ReadConfigFile(*user_config, cfgloc);
      ReadConfigFile(*_config, cfgloc);
    }

  aptcfg=new signalling_config(user_config, _config, theme_config);

  // If the user has a Recommends-Important setting and has allowed us
  // to read it by seting Ignore-Recommends-Important to false,
  // migrate it over and then set Ignore-Recommends-Important to true.
  if(!aptcfg->FindB(aptitudeIgnoreRecommendsImportant, false) &&
     aptcfg->Exists(aptitudeRecommendsImportant))
    {
      // If it was overridden to "false" and the system setting for
      // APT::Install-Recommends is "true", set the latter to "false"
      // to preserve aptitude's behavior.
      if(!aptcfg->FindB(aptitudeRecommendsImportant, true) &&
	 aptcfg->FindB(aptInstallRecommends, true))
	aptcfg->Set(aptInstallRecommends, "false");

      aptcfg->Set(aptitudeIgnoreRecommendsImportant, "true");

      apt_dumpcfg(PACKAGE);
    }

  aptcfg->connect("APT::Install-Recommends",
		  sigc::ptr_fun(&reset_interesting_dep_memoization));

  cache_closed.connect(sigc::ptr_fun(&reset_interesting_dep_memoization));

  cache_closed.connect(sigc::ptr_fun(&reset_surrounding_or_memoization));

  apt_dumpcfg(PACKAGE);

  apt_undos=new undo_list;
}

void apt_dumpcfg(const char *root)
{
  string cfgloc;

  const char *HOME = getenv("HOME");
  if(HOME != NULL && *HOME != '\0')
    {
      string tmp(HOME);
      tmp += "/.aptitude";
      if(access(tmp.c_str(), W_OK) == 0)
	cfgloc = tmp + "/config";
      else if(access(tmp.c_str(), R_OK | X_OK) == 0)
	{
	  // The squashed-root case.
	  _error->Error(_("%s is readable but not writable; unable to write configuration file."),
			tmp.c_str());
	  return;
	}
    }

  if(cfgloc.empty())
    {
      cfgloc = get_homedir();

      if(cfgloc.empty())
	return;

      cfgloc += "/.aptitude";

      if(mkdir(cfgloc.c_str(), 0700)<0 && errno != EEXIST)
	{
	  _error->Errno("mkdir", "%s", cfgloc.c_str());
	  return;
	}

      cfgloc += "/config";
    }

  ofstream f((cfgloc+".new").c_str());

  if(!f)
    {
      _error->Errno("apt_init", _("Unable to open %s for writing"), cfgloc.c_str());
      return;
    }

  // Don't write RootDir to the user's configuration file -- it causes
  // horrible confusion.
  std::string rootDir(_config->Find("RootDir", ""));
  _config->Clear("RootDir");

  aptcfg->Dump(f);

  _config->Set("RootDir", rootDir);

  f.close();

  if(rename((cfgloc+".new").c_str(), cfgloc.c_str())!=0)
    {
      _error->Errno("apt_init", _("Unable to replace %s with new configuration file"), cfgloc.c_str());
      return;
    }
}

// Revert back to the default set of options.  Is it a hack?  mHmm...
void apt_revertoptions()
{
  Configuration *old_user_config=user_config;
  Configuration *old_config=_config;

  // Preserve any existing root-dir settings in the new configuration.
  const std::string old_rootdir = _config->Find("RootDir", "");

  _config=new Configuration;

  user_config=new Configuration;

  _config->Set("RootDir", old_rootdir);
  user_config->Set("RootDir", old_rootdir);

  // ick?
  pkgInitConfig(*_config);
  pkgInitSystem(*_config, _system);

  aptcfg->setcfg(user_config, _config, theme_config);

  delete old_user_config;
  delete old_config;
}

void apt_init(OpProgress *progress_bar, bool do_initselections,
	      const char *status_fname)
{
  if(!apt_cache_file)
    apt_reload_cache(progress_bar, do_initselections, status_fname);
}

void apt_close_cache()
{
  logging::LoggerPtr logger(Loggers::getAptitudeAptGlobals());

  LOG_INFO(logger, "Closing apt cache.");

  cache_closed();

  LOG_TRACE(logger, "Done emitting cache_closed().");

  //   DANGER WILL ROBINSON!
  //
  // This must be done *** BEFORE BEFORE BEFORE *** we delete the
  // current cache file, since until the resolver manager is deleted,
  // there might actually be an active resolver thread trying to use
  // the cache!
  if(resman)
    {
      delete resman;
      resman = NULL;

      LOG_TRACE(logger, "Deleted the global dependency resolver manager.");
    }
  else
    LOG_TRACE(logger, "No global dependency resolver manager exists; none deleted.");

  aptitude::apt::reset_tasks();

  LOG_TRACE(logger, "Tasks reset.");

  if(apt_package_records)
    {
      delete apt_package_records;
      apt_package_records=NULL;
      LOG_TRACE(logger, "Deleted the apt package records.");
    }
  else
    LOG_TRACE(logger, "No global apt package records exist; none deleted.");

  if(apt_cache_file)
    {
      delete apt_cache_file;
      apt_cache_file=NULL;
      LOG_TRACE(logger, "Deleted the apt cache file.");
    }
  else
    LOG_TRACE(logger, "No global apt cache file exists; none deleted.");

  if(apt_source_list)
    {
      delete apt_source_list;
      apt_source_list=NULL;
      LOG_TRACE(logger, "Deleted the apt sources list.");
    }
  else
    LOG_TRACE(logger, "No global apt sources list exists; none deleted.");

  LOG_DEBUG(logger, "Done closing the apt cache.");
}

void apt_load_cache(OpProgress *progress_bar, bool do_initselections,
		    const char * status_fname)
{
  logging::LoggerPtr logger(Loggers::getAptitudeAptGlobals());

  if(apt_cache_file != NULL)
    {
      LOG_TRACE(logger, "Not loading apt cache: it's already loaded.");
      return;
    }

  LOG_INFO(logger, "Loading apt cache.");

  aptitudeCacheFile *new_file=new aptitudeCacheFile;

  LOG_TRACE(logger, "Reading the sources list.");

  apt_source_list=new pkgSourceList;
  apt_source_list->ReadMainList();

  bool simulate = aptcfg->FindB(PACKAGE "::Simulate", false);

  if(simulate)
    LOG_DEBUG(logger, PACKAGE "::Simulate is set; not locking the cache file.");

  // Clear the error stack so that we don't get confused by old errors.
  consume_errors();

  LOG_TRACE(logger, "Opening the apt cache.");

  bool open_failed=!new_file->Open(*progress_bar, do_initselections,
				   (getuid() == 0) && !simulate,
				   status_fname)
    || _error->PendingError();

  if(open_failed && getuid() == 0)
    {
      // Hm, we should include the errors, but there's no
      // nondestructive way to do that. :-(
      LOG_ERROR(logger, "Failed to load the apt cache; trying to open it without locking.");

      // Don't discard errors, make sure they get displayed instead.
      consume_errors();

      open_failed=!new_file->Open(*progress_bar, do_initselections,
				  false, status_fname);

      if(open_failed)
	LOG_ERROR(logger, "Unable to load the apt cache at all; giving up.");
      else
	LOG_DEBUG(logger, "Opening the apt cache with locking succeeded.");

      if(!open_failed)
	_error->Warning(_("Could not lock the cache file; this usually means that dpkg or another apt tool is already installing packages.  Opening in read-only mode; any changes you make to the states of packages will NOT be preserved!"));
    }

  if(open_failed)
    {
      delete new_file;
      LOG_DEBUG(logger, "Unable to load the apt cache; aborting and emitting cache_reload_failed().");
      cache_reload_failed();
      LOG_TRACE(logger, "Done emitting cache_reload_failed().");
      return;
    }

  apt_cache_file=new_file;

  // *If we were loading the global list of states*, dump immediate
  // changes back to it.  This reduces the chance that the user will
  // ^C and lose important changes (like the new dselect states of
  // packages).  Note, though, that we don't fail if this fails.
  if(!status_fname && apt_cache_file->is_locked())
    {
      LOG_TRACE(logger, "Trying to save the current selection list.");
      (*apt_cache_file)->save_selection_list(*progress_bar);
    }

  LOG_TRACE(logger, "Loading the apt package records.");
  apt_package_records=new pkgRecords(*apt_cache_file);

  // Um, good time to clear our undo info.
  apt_undos->clear_items();

  LOG_TRACE(logger, "Loading task information.");
  aptitude::apt::load_tasks(*progress_bar);
  LOG_TRACE(logger, "Loading tags.");
  aptitude::apt::load_tags(progress_bar);

  if(user_pkg_hier)
    {
      LOG_TRACE(logger, "Loading user-defined package hierarchy information.");
      reload_user_pkg_hier();
      hier_reloaded();
    }

  LOG_TRACE(logger, "Initializing global dependency resolver manager.");
  resman = new resolver_manager(new_file, imm::map<aptitude_resolver_package, aptitude_resolver_version>());

  LOG_TRACE(logger, "Initializing the download cache.");
  // Open the download cache.  By default, it goes in
  // ~/.aptitude/cache; it has 512Kb of in-memory cache and 10MB of
  // on-disk cache.
  const char *HOME = getenv("HOME");
  if(HOME != NULL)
    {
      std::string download_cache_file_name = string(HOME) + "/.aptitude/cache";
      const int download_cache_memory_size =
	aptcfg->FindI(PACKAGE "::UI::DownloadCache::MemorySize", 512 * 1024);
      const int download_cache_disk_size   =
	aptcfg->FindI(PACKAGE "::UI::DownloadCache::DiskSize", 10 * 1024 * 1024);
      try
	{
	  download_cache = aptitude::util::file_cache::create(download_cache_file_name,
							      download_cache_memory_size,
							      download_cache_disk_size);
	}
      catch(cwidget::util::Exception &ex)
	{
	  LOG_WARN(logger,
		   "Can't open the file cache \""
		   << download_cache_file_name
		   << "\": " << ex.errmsg());
	}
      catch(std::exception &ex)
	{
	  LOG_WARN(logger,
		   "Can't open the file cache \""
		   << download_cache_file_name
		   << "\": " << ex.what());
	}
    }

  LOG_DEBUG(logger, "Emitting cache_reloaded().");
  cache_reloaded();

  LOG_TRACE(logger, "Done emitting cache_reloaded().");
}

void apt_reload_cache(OpProgress *progress_bar, bool do_initselections,
		      const char * status_fname)
{
  apt_close_cache();
  apt_load_cache(progress_bar, do_initselections, status_fname);
}

void apt_shutdown()
{
  aptitude::shutdown_download_queue();


  apt_close_cache();

  delete aptcfg;
  aptcfg = NULL;

  delete theme_config;
  theme_config = NULL;

  delete user_config;
  user_config = NULL;

  delete apt_undos;
  apt_undos = NULL;

  delete user_pkg_hier;
  user_pkg_hier = NULL;

  delete pendingerr;
  pendingerr = NULL;


  download_cache.reset();

  cache_closed.clear();
  cache_reloaded.clear();
  cache_reload_failed.clear();
  hier_reloaded.clear();
  consume_errors.clear();
}

pkg_hier *get_user_pkg_hier()
{
  if(!user_pkg_hier)
    reload_user_pkg_hier();

  return user_pkg_hier;
}

pkg_action_state find_pkg_state(pkgCache::PkgIterator pkg,
				aptitudeDepCache &cache,
                                bool ignore_broken)
{
  aptitudeDepCache::StateCache &state = cache[pkg];
  aptitudeDepCache::aptitude_state &extstate = cache.get_ext_state(pkg);

  if(state.InstBroken() && !ignore_broken)
    return pkg_broken;
  else if(state.Delete())
    {
      if(extstate.remove_reason==aptitudeDepCache::manual)
	return pkg_remove;
      else if(extstate.remove_reason==aptitudeDepCache::unused)
	return pkg_unused_remove;
      else
	return pkg_auto_remove;
    }
  else if(state.Install())
    {
      if(!pkg.CurrentVer().end())
	{
	  if(state.iFlags&pkgDepCache::ReInstall)
	    return pkg_reinstall;
	  else if(state.Downgrade())
	    return pkg_downgrade;
	  else if(state.Upgrade())
	    return pkg_upgrade;
	  else
	    // FOO!  Should I abort here?
	    return pkg_install;
	}
      else if(state.Flags & pkgCache::Flag::Auto)
	return pkg_auto_install;
      else
	return pkg_install;
    }

  else if(state.Status==1 &&
	  state.Keep())
    {
      if(!(state.Flags & pkgDepCache::AutoKept))
	return pkg_hold;
      else
	return pkg_auto_hold;
    }

  else if(state.iFlags&pkgDepCache::ReInstall)
    return pkg_reinstall;
  // States where --configure fixes things.
  else if(pkg->CurrentState == pkgCache::State::UnPacked ||
	  pkg->CurrentState == pkgCache::State::HalfConfigured
#ifdef APT_HAS_TRIGGERS
	  || pkg->CurrentState == pkgCache::State::TriggersAwaited
	  || pkg->CurrentState == pkgCache::State::TriggersPending
#endif
	  )
    return pkg_unconfigured;

  return pkg_unchanged;
}

bool pkg_obsolete(pkgCache::PkgIterator pkg)
{
  if(pkg.CurrentVer().end())
    return false;
  else
    {
      pkgCache::VerIterator ver=pkg.VersionList();
      ver++;

      if(!ver.end())
	return false;
      else // Ok, there's only one version.  Good.
	{
	  pkgCache::VerFileIterator files=pkg.VersionList().FileList();
	  if(!files.end())
	    {
	      files++;
	      if(!files.end())
		return false; // Nope, more than one file
	    }
	}
    }

  return true;
}

// This does not assume that the dependency is the first elements of
// its OR group.
static void surrounding_or_internal(const pkgCache::DepIterator &dep,
				    pkgCache::DepIterator &start,
				    pkgCache::DepIterator &end)
{
  bool found=false;

  start=const_cast<pkgCache::DepIterator &>(dep).ParentVer().DependsList();
  end=start;

  while(!end.end() && !found)
    {
      start=end;

      while(end->CompareOp&pkgCache::Dep::Or)
	{
	  if(end==dep)
	    found=true;

	  ++end;
	}

      if(end==dep)
	found=true;

      ++end;
    }

  // If not, something is wrong with apt's cache.
  eassert(found);
}

void surrounding_or(pkgCache::DepIterator dep,
		    pkgCache::DepIterator &start,
		    pkgCache::DepIterator &end,
		    pkgCache *cache)
{
  if(cache == NULL)
    cache = *apt_cache_file;

  if(cached_surrounding_or == NULL)
    {
      cached_surrounding_or = new pkgCache::Dependency *[cache->Head().DependsCount];
      for(unsigned long i = 0; i<cache->Head().DependsCount; ++i)
	cached_surrounding_or[i] = 0;
    }

  pkgCache::Dependency *s = cached_surrounding_or[dep->ID];

  // Use the old trick of stuffing values into the low bits of a
  // pointer.
  if(((unsigned long) s & 0x1) != 0)
    {
      pkgCache::Dependency *unmunged
	= (pkgCache::Dependency *) (((unsigned long) s) & ~0x1UL);

      start = pkgCache::DepIterator(*cache, unmunged);
      end = start;

      while(end->CompareOp & pkgCache::Dep::Or)
	++end;

      ++end;
    }
  else
    {
      surrounding_or_internal(dep, start, end);

      cached_surrounding_or[dep->ID] = (pkgCache::Dependency *) ((unsigned long) ((pkgCache::Dependency *) start) | 0x1UL);
    }
}

bool package_suggested(const pkgCache::PkgIterator &pkg)
{
  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];
  pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

  for(rev_dep_iterator d(pkg); !d.end(); ++d)
    if((*d)->Type==pkgCache::Dep::Suggests)
      {
	bool satisfied=false;

	pkgCache::DepIterator start,end;

	surrounding_or(*d, start, end);

	while(start!=end)
	  {
	    if(((*apt_cache_file)[start])&pkgDepCache::DepGInstall)
	      {
		satisfied=true;
		break;
	      }

	    ++start;
	  }

	if(!satisfied)
	  {
	    // Check whether the package doing the depending is going
	    // to be installed.
	    pkgCache::PkgIterator depender=(*d).ParentPkg();
	    pkgDepCache::StateCache &depstate=(*apt_cache_file)[depender];
	    pkgCache::VerIterator depinstver=depstate.InstVerIter(*apt_cache_file);

	    if(depender.CurrentVer().end() &&
	       depstate.Install() &&
	       !depinstver.end() &&
	       !candver.end() &&
	       _system->VS->CheckDep(candver.VerStr(),
				     (*d)->CompareOp, (*d).TargetVer()))
	      {
		if((*d).ParentVer()==depinstver)
		  return true;
	      }
	  }
      }

  return false;
}

bool package_recommended(const pkgCache::PkgIterator &pkg)
{
  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];
  pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

  for(rev_dep_iterator d(pkg); !d.end(); ++d)
    if((*d)->Type==pkgCache::Dep::Recommends)
      {
	bool satisfied=false;

	pkgCache::DepIterator start,end;

	surrounding_or(*d, start, end);

	while(start!=end)
	  {
	    if(((*apt_cache_file)[start])&pkgDepCache::DepGInstall)
	      {
		satisfied=true;
		break;
	      }

	    ++start;
	  }

	if(!satisfied)
	  {
	    // Check whether the package doing the depending is going
	    // to be installed or upgraded.
	    pkgCache::PkgIterator depender=(*d).ParentPkg();
	    pkgDepCache::StateCache &depstate=(*apt_cache_file)[depender];
	    pkgCache::VerIterator depinstver=depstate.InstVerIter(*apt_cache_file);

	    if(depstate.Install() &&
	       !candver.end() &&
	       _system->VS->CheckDep(candver.VerStr(),
				     (*d)->CompareOp, (*d).TargetVer()))
	      {
		if((*d).ParentVer()==depinstver)
		  return true;
	      }
	  }
      }

  return false;
}

bool package_trusted(const pkgCache::VerIterator &ver)
{
  for(pkgCache::VerFileIterator i = ver.FileList(); !i.end(); ++i)
    {
      pkgIndexFile *index;

      if(!apt_source_list->FindIndex(i.File(), index))
	// Corresponds to the currently installed package, which is
	// always "trusted".
	return true;
      else if(index->IsTrusted())
	return true;
    }

  return false;
}

pkgCache::VerIterator install_version(const pkgCache::PkgIterator &pkg,
				      aptitudeDepCache &cache)
{
  if(pkg.VersionList().end())
    return pkgCache::VerIterator(cache);

  pkgDepCache::StateCache &state(cache[pkg]);

  if(state.Delete())
    return pkgCache::VerIterator(cache);
  else if(state.Install())
    return state.InstVerIter(cache);
  else // state.Keep()
    {
      if(pkg->CurrentState == pkgCache::State::NotInstalled ||
	 pkg->CurrentState == pkgCache::State::ConfigFiles)
	return pkgCache::VerIterator(cache);
      else
	return pkg.CurrentVer();
    }
}

pkgCache::DepIterator is_conflicted(const pkgCache::VerIterator &ver,
				    aptitudeDepCache &cache)
{
  if(ver.end())
    return pkgCache::DepIterator();

  pkgCache::PkgIterator parentPkg = ver.ParentPkg();

  // Look for forward conflicts:
  for(pkgCache::DepIterator dep = ver.DependsList(); !dep.end(); ++dep)
    if(dep->Type == pkgCache::Dep::Conflicts ||
       dep->Type == pkgCache::Dep::DpkgBreaks)
    {
      // Look for direct conflicts:
      pkgCache::VerIterator depTargetPkgInstallVer(install_version(dep.TargetPkg(),
								   cache));
	if(dep.TargetPkg() != parentPkg &&
	   !depTargetPkgInstallVer.end() &&
	   _system->VS->CheckDep(depTargetPkgInstallVer.VerStr(),
				 dep->CompareOp,
				 dep.TargetVer()))
	return dep;

      // Look for virtual conflicts:
      for(pkgCache::PrvIterator prv = dep.TargetPkg().ProvidesList();
	  !prv.end(); ++prv)
	{
	  if(prv.OwnerPkg() != parentPkg &&
	     install_version(prv.OwnerPkg(), cache) == prv.OwnerVer() &&
	     _system->VS->CheckDep(prv.ProvideVersion(),
				   dep->CompareOp,
				   dep.TargetVer()))
	    return dep;
	}
    }

  // Look for reverse conflicts:

  // Look for direct reverse conflicts:
  for(pkgCache::DepIterator dep = parentPkg.RevDependsList();
      !dep.end(); ++dep)
    {
      if(dep->Type == pkgCache::Dep::Conflicts ||
	 dep->Type == pkgCache::Dep::DpkgBreaks)
	{
	  if(dep.ParentPkg() != parentPkg &&
	     install_version(dep.ParentPkg(), cache) == dep.ParentVer() &&
	     _system->VS->CheckDep(ver.VerStr(),
				   dep->CompareOp,
				   dep.TargetVer()))
	    return dep;
	}
    }

  // Look for indirect reverse conflicts: that is, things that
  // conflict with a package that this version provides.
  for(pkgCache::PrvIterator prv = ver.ProvidesList();
      !prv.end(); ++prv)
    {
      for(pkgCache::DepIterator dep = prv.ParentPkg().RevDependsList();
	  !dep.end(); ++dep)
	{
	  if(dep->Type == pkgCache::Dep::Conflicts ||
	     dep->Type == pkgCache::Dep::DpkgBreaks)
	    {
	      if(dep.ParentPkg() != parentPkg &&
		 install_version(dep.ParentPkg(), cache) == dep.ParentVer() &&
		 _system->VS->CheckDep(prv.ProvideVersion(),
				       dep->CompareOp,
				       dep.TargetVer()))
		return dep;
	    }
	}
    }

  return pkgCache::DepIterator(cache, 0, (pkgCache::Version *)0);
}

/** \return \b true if d1 subsumes d2; that is, if one of the
 *  following holds:
 *
 *  (a) d1 and d2 target the same package and are unversioned.
 *
 *  (b) d1 and d2 are unversioned and some version of d2's target
 *      provides the target package of d1.
 *
 *  (c) d1 and d2 target the same package, d1 is unversioned, and d2
 *      is versioned.
 *
 *  (c) d1 and d2 target the same package and are versioned
 *     (with op1 ver1 and op2 ver2 being the operations and versions)
 *     and:
 *
 *       - op1 is >=, op2 is >>, >=, or =, and ver1 <= ver2; or
 *       - op1 is <=, op2 is <<, <=, or =, and ver1 >= ver2; or
 *       - op1 is >>, op2 is >>, and ver1 <= ver2; or
 *       - op1 is >>, op2 is =, and ver1 < ver2; or
 *       - op1 is <<, op2 is <<, and ver1 >= ver2; or
 *       - op1 is <<, op2 is =, and ver1 > ver2; or
 *       - op1 is =, op2 is =, and ver1 = ver2; or
 *       - op1 is !=, op2 is !=, and ver1 = ver2.
 */
static bool subsumes(const pkgCache::DepIterator &d1,
		     const pkgCache::DepIterator &d2)
{
  pkgCache::PkgIterator target1 = const_cast<pkgCache::DepIterator &>(d1).TargetPkg();
  pkgCache::PkgIterator target2 = const_cast<pkgCache::DepIterator &>(d2).TargetPkg();

  if(!d1.TargetVer())
    {
      if(target1 == target2)
	return true;

      if(d2.TargetVer())
	return false;

      for(pkgCache::PrvIterator p = target1.ProvidesList();
	  !p.end(); ++p)
	if(p.OwnerPkg() == target2)
	  return true;

      return false;
    }
  else
    {
      if(target1 != target2)
	return false;

      if(!d2.TargetVer())
	return false;

      pkgCache::Dep::DepCompareOp t1 = (pkgCache::Dep::DepCompareOp) (d1->CompareOp &~ pkgCache::Dep::Or);
      pkgCache::Dep::DepCompareOp t2 = (pkgCache::Dep::DepCompareOp) (d2->CompareOp &~ pkgCache::Dep::Or);

      int cmpresult = _system->VS->DoCmpVersion(d1.TargetVer(), d1.TargetVer()+strlen(d1.TargetVer()),
						d2.TargetVer(), d2.TargetVer()+strlen(d2.TargetVer()));

      switch(t1)
	{
	case pkgCache::Dep::LessEq:
	  return
	    (t2 == pkgCache::Dep::Less ||
	     t2 == pkgCache::Dep::LessEq ||
	     t2 == pkgCache::Dep::Equals) &&
	    cmpresult >= 0;
	case pkgCache::Dep::GreaterEq:
	  return
	    (t2 == pkgCache::Dep::Greater ||
	     t2 == pkgCache::Dep::GreaterEq ||
	     t2 == pkgCache::Dep::Equals) &&
	    cmpresult <= 0;
	case pkgCache::Dep::Less:
	  return
	    (t2 == pkgCache::Dep::Less && cmpresult >= 0) ||
	    (t2 == pkgCache::Dep::Equals && cmpresult > 0);
	case pkgCache::Dep::Greater:
	  return
	    (t2 == pkgCache::Dep::Greater && cmpresult <= 0) ||
	    (t2 == pkgCache::Dep::Equals && cmpresult < 0);
	case pkgCache::Dep::Equals:
	  return
	    (t2 == pkgCache::Dep::Equals && cmpresult == 0);
	case pkgCache::Dep::NotEquals:
	  return
	    (t2 == pkgCache::Dep::NotEquals && cmpresult == 0);

	  // These shouldn't happen:
	case pkgCache::Dep::NoOp:
	default:
	  abort();
	}
    }
}

/** \return \b true if the OR group of d1 subsumes the OR group of d2:
 *  that is, if every member of the OR group containing d2 has a
 *  subsuming element in the OR group containing d1.
 */
static bool or_group_subsumes(const pkgCache::DepIterator &d1,
			      const pkgCache::DepIterator &d2,
			      pkgCache *cache)
{
  pkgCache::DepIterator start1, end1, start2, end2;

  surrounding_or(d1, start1, end1, cache);
  surrounding_or(d2, start2, end2, cache);

  for(pkgCache::DepIterator i = start1; i != end1; ++i)
    {
      bool found = false;

      for(pkgCache::DepIterator j = start2; j != end2; ++j)
	if(subsumes(i, j))
	  {
	    found = true;
	    break;
	  }

      if(!found)
	return false;
    }

  return true;
}


// Interesting deps are:
//
//   - All critical deps
//   - All recommendations that are currently satisfied
//   - All recommendations that are unrelated under subsumption to
//     each recommendation of the current package version.
static bool internal_is_interesting_dep(const pkgCache::DepIterator &d,
					pkgDepCache *cache)
{
  pkgCache::PkgIterator parpkg = const_cast<pkgCache::DepIterator &>(d).ParentPkg();
  pkgCache::VerIterator currver = parpkg.CurrentVer();
  pkgCache::VerIterator parver = const_cast<pkgCache::DepIterator &>(d).ParentVer();

  if(!parver.Downloadable() &&
     (parver != currver || parpkg->CurrentState == pkgCache::State::ConfigFiles))
    return false;
  else if(const_cast<pkgCache::DepIterator &>(d).IsCritical())
    return true;
  else if(d->Type != pkgCache::Dep::Recommends ||
	  !aptcfg->FindB("Apt::Install-Recommends", true))
    return false;
  else
    {
      // Soft deps attached to the current version are interesting iff
      // they are currently satisfied.
      if(currver == parver)
	{
	  pkgCache::DepIterator dtmp = d;
	  while(!dtmp.end() && dtmp->CompareOp & pkgCache::Dep::Or)
	    ++dtmp;
	  if((*cache)[dtmp] & pkgDepCache::DepGNow)
	    return true;

	  return false;
	}
      else if(currver.end())
	return true;
      else
	// Check whether the current version of this package has a dep
	// that either subsumes _or is subsumed by_ this
	// recommendation.  (for mathematical "correctness" we'd only
	// check the first direction, but the goal is to not annoy the
	// user unnecessarily; losing a few new recommendations is OK)
	//
	// NB: full correctness without annoyance means actually
	// TRIMMING DOWN the target set of a recommendation by
	// subtracting elements that are in the current version's
	// recommendation list.  Needless to say, I'm ignoring this
	// for now.
	{
	  pkgCache::DepIterator d2 = currver.DependsList();

	  while(!d2.end())
	    {
	      if(d2->Type == pkgCache::Dep::Recommends &&
		 (or_group_subsumes(d2, d, &cache->GetCache()) ||
		  or_group_subsumes(d, d2, &cache->GetCache())))
		{
		  pkgCache::DepIterator dtmp = d;
		  while(!dtmp.end() && dtmp->CompareOp & pkgCache::Dep::Or)
		    ++dtmp;
		  if((*cache)[dtmp] & pkgDepCache::DepGNow)
		    return true;

		  return false;
		}

	      while(!d2.end() && ((d2->CompareOp & pkgCache::Dep::Or) != 0))
		++d2;

	      if(!d2.end())
		++d2;
	    }

	  return true;
	}
    }
}

bool is_interesting_dep(const pkgCache::DepIterator &d,
			pkgDepCache *cache)
{
  if(cached_deps_interesting == NULL)
    {
      cached_deps_interesting = new interesting_state[cache->Head().DependsCount];
      for(unsigned long i = 0; i<cache->Head().DependsCount; ++i)
	cached_deps_interesting[i] = uncached;
    }

  switch(cached_deps_interesting[d->ID])
    {
    case uncached:
      {
	pkgCache::DepIterator start, end;
	surrounding_or(d, start, end, &cache->GetCache());

	bool rval = internal_is_interesting_dep(start, cache);

	cached_deps_interesting[d->ID] = rval ? interesting : uninteresting;
	return rval;
      }
    case interesting:
      return true;
    case uninteresting:
      return false;
    default:
      abort();
    }
}

std::wstring get_short_description(const pkgCache::VerIterator &ver,
				   pkgRecords *records)
{
  if(ver.end() || ver.FileList().end() || records == NULL)
    return std::wstring();

#ifndef HAVE_DDTP
  pkgCache::VerFileIterator vf = ver.FileList();

  if(vf.end())
    return std::wstring();
  else
    return cw::util::transcode(records->Lookup(vf).ShortDesc());
#else
  pkgCache::DescIterator d = ver.TranslatedDescription();

  if(d.end())
    return std::wstring();

  pkgCache::DescFileIterator df = d.FileList();

  if(df.end())
    return std::wstring();
  else
    // apt "helpfully" cw::util::transcodes the description for us, instead of
    // providing direct access to it.  So I need to assume that the
    // description is encoded in the current locale.
    return cwidget::util::transcode(records->Lookup(df).ShortDesc());
#endif
}

std::wstring get_long_description(const pkgCache::VerIterator &ver,
				  pkgRecords *records)
{
  if(ver.end() || ver.FileList().end() || records == NULL)
    return std::wstring();

#ifndef HAVE_DDTP
  pkgCache::VerFileIterator vf = ver.FileList();

  if(vf.end())
    return std::wstring();
  else
    return cw::util::transcode(records->Lookup(vf).LongDesc());
#else
  pkgCache::DescIterator d = ver.TranslatedDescription();

  if(d.end())
    return std::wstring();

  pkgCache::DescFileIterator df = d.FileList();

  if(df.end())
    return std::wstring();
  else
    return cwidget::util::transcode(records->Lookup(df).LongDesc());
#endif
}

const char *multiarch_type(unsigned char type)
{
  switch(type)
    {
    case pkgCache::Version::Foreign:
    case pkgCache::Version::AllForeign:
      return _("foreign");
    case pkgCache::Version::Same:
      return _("same");
    case pkgCache::Version::Allowed:
    case pkgCache::Version::AllAllowed:
      return _("allowed");
    default:
      return "";
    }
}

int get_arch_order(const char *a)
{
  static const std::vector<std::string> archs =
    APT::Configuration::getArchitectures();

  if(strcmp(a, "all") == 0)
    return -1;

  const std::vector<std::string>::const_iterator it =
    std::find(archs.begin(), archs.end(), a);
  return it - archs.begin();
}

int get_deptype_order(const pkgCache::Dep::DepType t)
{
  switch(t)
    {
    case pkgCache::Dep::PreDepends: return 7;
    case pkgCache::Dep::Depends:    return 6;
    case pkgCache::Dep::Recommends: return 5;
    case pkgCache::Dep::Conflicts:  return 4;
    case pkgCache::Dep::DpkgBreaks: return 3;
    case pkgCache::Dep::Suggests:   return 2;
    case pkgCache::Dep::Replaces:   return 1;
    case pkgCache::Dep::Obsoletes:  return 0;
    default: return -1;
    }
}

namespace aptitude
{
  namespace apt
  {
    bool is_full_replacement(const pkgCache::DepIterator &dep)
    {
      if(dep.end())
	return false;

      if(dep->Type != pkgCache::Dep::Replaces)
	return false;

      if((dep->CompareOp & ~pkgCache::Dep::Or) != pkgCache::Dep::NoOp)
	return false;

      pkgCache::PkgIterator target = const_cast<pkgCache::DepIterator &>(dep).TargetPkg();

      // Check whether the parent of the dep provides this target and
      // conflicts with it.

      bool found_provides = false;

      for(pkgCache::PrvIterator prv = const_cast<pkgCache::DepIterator &>(dep).ParentVer().ProvidesList();
	  !found_provides && !prv.end(); ++prv)
	{
	  if(prv.ParentPkg() == target)
	    found_provides = true;
	}

      if(!found_provides)
	return false;

      bool found_conflicts = false;

      for(pkgCache::DepIterator possible_conflict = const_cast<pkgCache::DepIterator &>(dep).ParentVer().DependsList();
	  !found_conflicts && !possible_conflict.end(); ++possible_conflict)
	{
	  if(possible_conflict->Type == pkgCache::Dep::Conflicts &&
	     (possible_conflict->CompareOp & ~pkgCache::Dep::Or) == pkgCache::Dep::NoOp &&
	     possible_conflict.TargetPkg() == target)
	    found_conflicts = true;
	}

      if(!found_conflicts)
	return false;


      return true;
    }

    const std::vector<std::string> get_top_sections(const bool cached)
    {
      static std::vector<std::string> top_sections;
      const char *defaults[] =
        {N_("main"),N_("contrib"),N_("non-free"),N_("non-US")};

      if(top_sections.empty() == false)
        {
          if(cached == true)
            return top_sections;
          else
            top_sections.clear();
        }

      top_sections = aptcfg->FindVector(PACKAGE "::Sections::Top-Sections");
      if(top_sections.empty() == true)
        top_sections.assign(defaults, defaults + sizeof(defaults)/sizeof(*defaults));

      return top_sections;
    }

    bool is_native_arch(const pkgCache::VerIterator &ver)
    {
      if(apt_native_arch.empty())
	apt_native_arch = aptcfg->Find("APT::Architecture");
      const char *arch = ver.Arch();
      return apt_native_arch == arch || strcmp(arch, "all") == 0;
    }
  }
}
