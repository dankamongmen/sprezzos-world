// cmdline_do_action.cc
//
//  Copyright (C) 2004, 2010 Daniel Burrows
//  Copyright (C) 2012 Daniel Hartwig
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


// Local includes:
#include "cmdline_do_action.h"

#include "cmdline_action.h"
#include "cmdline_common.h"
#include "cmdline_prompt.h"
#include "cmdline_resolver.h"
#include "cmdline_show_broken.h"
#include "cmdline_simulate.h"
#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>

#include <aptitude.h>


// System includes:
#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/progress.h>

#include <stdio.h>

using namespace std;

using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using boost::shared_ptr;

namespace
{
  void run_dpkg_directly(sigc::slot1<pkgPackageManager::OrderResult, int> f,
			 sigc::slot1<void, pkgPackageManager::OrderResult> k)
  {
    k(f(aptcfg->FindI("APT::Status-Fd", -1)));
  }
}

/** \brief Used to track whether an upgrade is being performed, and if
 *  so, what sort of upgrade is being performed.
 */
enum upgrade_mode_tp
  {
    /** \brief Indicates that no upgrade is being performed. */
    no_upgrade,
    /** \brief Indicates that a full-upgrade is being performed: all
     *  packages will be marked for upgrade by default, and the
     *  resolver will consider "unsafe" actions.
     */
    full_upgrade,
    /** \brief Indicates that a safe-upgrade is being performed: all
     *  packages will be marked for upgrade by default, and the
     *  resolver will not consider "unsafe" actions; automatic
     *  installation of dependencies is also disabled.
     */
    safe_upgrade
  };

// TODO: add an option to obey sticky states even if it wasn't
//      explicitly requested.
//
// TODO: perhaps when trying to find a list of possible candidates for
// installation, we should use a formatted display?
int cmdline_do_action(int argc, char *argv[],
		      const char *status_fname, bool simulate,
		      bool assume_yes, bool download_only, bool fix_broken,
		      bool showvers, bool showdeps,
		      bool showsize, bool showwhy,
		      bool visual_preview, bool always_prompt,
		      resolver_mode_tp resolver_mode, bool safe_resolver_show_actions,
		      bool no_new_installs, bool no_new_upgrades,
		      const std::vector<aptitude::cmdline::tag_application> &user_tags,
		      bool arch_only,
		      bool queue_only, int verbose)
{
  shared_ptr<terminal_io> term = create_terminal();

  consume_errors();

  cmdline_pkgaction_type default_action=cmdline_install;

  upgrade_mode_tp upgrade_mode = no_upgrade;

  // Parse the action.  This sets the upgrade mode and (if it was not
  // overridden by the user) the resolver mode.
  if(!strcasecmp(argv[0], "install"))
    default_action=cmdline_install;
  else if(!strcasecmp(argv[0], "reinstall"))
    default_action=cmdline_reinstall;
  else if(!strcasecmp(argv[0], "full-upgrade") ||
	  !strcasecmp(argv[0], "dist-upgrade"))
    {
      default_action = cmdline_upgrade;
      if(argc == 1)
	upgrade_mode = full_upgrade;
      if(resolver_mode == resolver_mode_default)
	resolver_mode = resolver_mode_full;
    }
  else if(!strcasecmp(argv[0], "safe-upgrade") ||
	  !strcasecmp(argv[0], "upgrade"))
    {
      default_action = cmdline_upgrade;
      // If safe-upgrade is the only argument, we treat this as a full
      // upgrade; otherwise we treat it as an install-type action that
      // applies only to the packages listed on the command-line.
      if(argc == 1)
	upgrade_mode = safe_upgrade;
      if(resolver_mode == resolver_mode_default)
	resolver_mode = resolver_mode_safe;
    }
  else if(!strcasecmp(argv[0], "remove"))
    default_action=cmdline_remove;
  else if(!strcasecmp(argv[0], "purge"))
    default_action=cmdline_purge;
  else if(!strcasecmp(argv[0], "hold"))
    default_action=cmdline_hold;
  else if(!strcasecmp(argv[0], "keep") || !strcasecmp(argv[0], "keep-all"))
    default_action=cmdline_keep;
  else if(!strcasecmp(argv[0], "unhold"))
    default_action=cmdline_unhold;
  else if(!strcasecmp(argv[0], "markauto"))
    default_action=cmdline_markauto;
  else if(!strcasecmp(argv[0], "unmarkauto"))
    default_action=cmdline_unmarkauto;
  else if(!strcasecmp(argv[0], "forbid-version"))
    default_action=cmdline_forbid_version;
  else if(!strcasecmp(argv[0], "build-depends") ||
	  !strcasecmp(argv[0], "build-dep"))
    default_action = cmdline_build_depends;
  else
    {
      // Should never happen.
      _error->Error(_("Invalid operation %s"), argv[0]);
      return 100;
    }

  if(resolver_mode == resolver_mode_default)
    resolver_mode = resolver_mode_full;

  shared_ptr<OpProgress> progress = make_text_progress(false, term, term, term);

  aptcfg->SetNoUser(PACKAGE "::Auto-Upgrade", "false");

  // If there are no arguments and the default action is "install", act
  // on stickies.
  //
  // This way, "aptitude install" will just perform any pending
  // installations.
  apt_init(progress.get(),
           (argc==1 && default_action==cmdline_install &&
            upgrade_mode == no_upgrade), status_fname);

  if(_error->PendingError())
    return 100;

  // In case we aren't root.
  if(!simulate)
    apt_cache_file->GainLock();
  else
    apt_cache_file->ReleaseLock();

  if(_error->PendingError())
    return 100;

  pkgPolicy policy(&(*apt_cache_file)->GetCache());
  ReadPinFile(policy);
  ReadPinDir(policy);

  pkgset to_upgrade, to_install, to_hold, to_remove, to_purge;

  if(upgrade_mode == full_upgrade || upgrade_mode == safe_upgrade)
    {
      bool ignore_removed = (upgrade_mode == safe_upgrade);

      (*apt_cache_file)->get_upgradable(ignore_removed, to_install);

      bool use_autoinst = (resolver_mode != resolver_mode_safe);
      (*apt_cache_file)->mark_all_upgradable(use_autoinst, ignore_removed, NULL);
    }
  /*else if(argc==1 && default_action==cmdline_install)
    {
      // FIXME: Build to_install to avoid a big printout
      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
	{

	}
	}*/

  // TODO: look for filenames and call dpkg directly if that's the case.

  {
    aptitudeDepCache::action_group group(*apt_cache_file, NULL);

  // If keep-all is the argument, we expect no patterns and keep all
  // packages back.
  if(!strcasecmp(argv[0], "keep-all"))
    {
      if(argc != 1)
	{
          _error->Error(_("Unexpected pattern argument following \"keep-all\""));
          return 100;
	}

      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	  !i.end(); ++i)
	(*apt_cache_file)->mark_keep(i, false, false, NULL);
    }
  else
    {
      typedef std::pair<cmdline_pkgaction_type, std::string> action_pair;
      std::vector<action_pair> actions;
      for(int i=1; i<argc; ++i)
	{
	  cmdline_pkgaction_type action = default_action;
	  std::string target = argv[i];
	  int tmp = strlen(argv[i]) - 1;

	  // HACK: disable interpreting of escapes if it's an existing
	  //      package name.
	  if((*apt_cache_file)->FindPkg(argv[i]).end())
	    switch(argv[i][tmp])
	      {
	      case '-':
		action = cmdline_remove;
		target = std::string(argv[i], 0, tmp);
		break;
	      case '=':
		action = cmdline_hold;
		target = std::string(argv[i], 0, tmp);
		break;
	      case '+':
		action = cmdline_install;
		target = std::string(argv[i], 0, tmp);
		break;
	      case '_':
		action = cmdline_purge;
		target = std::string(argv[i], 0, tmp);

		break;
	      case ':':
		action = cmdline_keep;
		target = std::string(argv[i], 0, tmp);
		break;
	      case 'D':
		// "&BD" for installing build depends.
		if(tmp >= 2 &&
		   argv[i][tmp - 1] == 'B' &&
		   argv[i][tmp - 2] == '&')
		  {
		    action = cmdline_build_depends;
		    target = std::string(argv[i], 0, strlen(argv[i]) - 3);
		  }
		break;
	      case 'm':
	      case 'M':
		if(tmp > 0 && argv[i][tmp - 1] == '&')
		  {
		    if(argv[i][tmp] == 'm')
		      action = cmdline_unmarkauto;
		    else
		      action = cmdline_markauto;
		    target = std::string(argv[i], 0, tmp - 1);
		  }
		else if(tmp>0 && argv[i][tmp-1] == '+' && argv[i][tmp] == 'M')
		  {
		    action = cmdline_installauto;
		    target = std::string(argv[i], 0, tmp - 1);
		  }
	      }

	  actions.push_back(action_pair(action, target));
	}

      // If we have auto-install turned on, do a second run over all
      // the packages being installed to blindly resolve their deps.
      // Note that we skip auto-install if the safe resolver is turned
      // on, on the grounds that auto-install will remove packages for
      // Conflicts too.
      const bool do_autoinstall = (resolver_mode != resolver_mode_safe) && aptcfg->FindB(PACKAGE "::Auto-Install", true);
      const int num_passes = do_autoinstall ? 2 : 1;
      std::set<pkgCache::PkgIterator> seen_virtual_packages;
      for(int pass = 0; pass < num_passes; ++pass)
	{
	  // Clear these to avoid undesirable interactions between the
	  // first and second passes.
	  to_install.clear();
	  to_hold.clear();
	  to_remove.clear();
	  to_purge.clear();


	  for(std::vector<action_pair>::const_iterator it = actions.begin();
	      it != actions.end(); ++it)
	    {
	      cmdline_applyaction(it->second, seen_virtual_packages, it->first,
				  to_install, to_hold, to_remove, to_purge,
				  verbose, policy, arch_only, pass > 0,
                                  term);
	    }
	}
    }
  }

  if(_error->PendingError() == true)
    return 100;

  if(resolver_mode == resolver_mode_safe)
    {
      if(!aptitude::cmdline::safe_resolve_deps(verbose,
                                               no_new_installs,
                                               no_new_upgrades,
                                               safe_resolver_show_actions,
                                               term))
	{
          _error->Error(_("Unable to safely resolve dependencies, try"
                          " running with --full-resolver"));
          return 100;
	}
    }

  if(visual_preview)
    {
      ui_preview();
      return 0;
    }
  else if(simulate)
    return cmdline_simulate(upgrade_mode != no_upgrade,
			    to_install, to_hold, to_remove, to_purge,
			    showvers, showdeps,
			    showsize, showwhy,
			    always_prompt, verbose, assume_yes,
			    !fix_broken,
			    policy, arch_only,
                            term);
  else if(queue_only)
    {
      aptitude::cmdline::apply_user_tags(user_tags);

      if(!(*apt_cache_file)->save_selection_list(*progress))
        return 100;
      else
	return 0;
    }
  else
    {
      if(!cmdline_do_prompt(upgrade_mode != no_upgrade,
			    to_install, to_hold, to_remove, to_purge,
			    showvers, showdeps, showsize, showwhy,
			    always_prompt, verbose, assume_yes,
			    !fix_broken,
			    policy, arch_only, term))
	{
	  printf(_("Abort.\n"));
	  return 1;
	}

      aptitude::cmdline::apply_user_tags(user_tags);

      download_install_manager m(download_only,
                                 aptcfg->FindB(PACKAGE "::CmdLine::Fix-Missing", false),
				 sigc::ptr_fun(&run_dpkg_directly));

      // FIXME: Temporary work-around for bug #677175 in apt.
      const size_t prev_err_stack_count = _error->StackCount();

      int rval =
	(cmdline_do_download(&m, verbose, term, term, term, term)
         == download_manager::success ? 0 : 100);

      while(_error->StackCount() > prev_err_stack_count)
        _error->MergeWithStack();

      if(_error->PendingError())
	rval = 100;

      return rval;
    }
}
