// cmdline_action.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_action.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/tasks.h>

#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/version.h>

#include <stdio.h>
#include <stdlib.h>

#include <cwidget/generic/util/transcode.h>

namespace cw = cwidget;

using aptitude::cmdline::terminal_metrics;
using boost::shared_ptr;

namespace
{
  bool cmdline_do_build_depends(const string &pkg,
				std::set<pkgCache::PkgIterator> &seen_virtual_packages,
				cmdline_version_source version_source,
				const string &version_source_string,
				bool arch_only,
				pkgPolicy &policy,
				pkgset &to_install, pkgset &to_hold,
				pkgset &to_remove, pkgset &to_purge,
				int verbose,
				bool allow_auto,
                                const shared_ptr<terminal_metrics> &term_metrics)
  {
    aptitude::cmdline::source_package sourcepkg =
      aptitude::cmdline::find_source_package(pkg,
					     version_source,
					     version_source_string);

    if(!sourcepkg.valid())
      {
	printf(_("Unable to find the source package for \"%s\".\n"),
	       pkg.c_str());
	return false;
      }

    if(apt_cache_file == NULL)
      {
	// Should never happen.
	printf("Sanity-check failed: apt_cache_file should not be NULL here.\n");
	return false;
      }

    bool rval = true;
    typedef pkgSrcRecords::Parser::BuildDepRec BuildDepRec;
    typedef std::vector<BuildDepRec> BuildDepList;
    for(BuildDepList::const_iterator it = sourcepkg.get_build_deps().begin();
	it != sourcepkg.get_build_deps().end(); ++it)
      {
	bool is_conflict = (it->Type == pkgSrcRecords::Parser::BuildConflictIndep ||
			    it->Type == pkgSrcRecords::Parser::BuildConflict);

	if(arch_only && (it->Type == pkgSrcRecords::Parser::BuildDependIndep ||
			 it->Type == pkgSrcRecords::Parser::BuildConflictIndep))
	  continue;

	BuildDepList::const_iterator or_group_start = it;

	// Find the bounds of the OR group.
	while((it->Op & pkgCache::Dep::Or) != 0)
	  ++it;

	bool ok = true;
	if(!is_conflict)
	  {
	    // Walk over the OR group and see if it's satisfied.
	    //
	    // NB: if two build-deps require incompatible versions
	    // of the same package, we'll just get confused.
	    bool satisfied = false;
	    for(BuildDepList::const_iterator it2 = or_group_start;
		!satisfied && it2 <= it; ++it2)
	      {
		pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(it2->Package);
		if(!pkg.end())
		  {
		    pkgCache::VerIterator ver = pkg.CurrentVer();
		    if(!ver.end() &&
		       _system->VS->CheckDep(ver.VerStr(),
					     it2->Op,
					     it2->Version.c_str()))
		       satisfied = true;
		  }
	      }

	    if(!satisfied)
	      {
		for(BuildDepList::const_iterator it2 = or_group_start;
		    !satisfied && it2 <= it; ++it2)
		  {
		    pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(it2->Package);
		    if(!pkg.end())
		      {
			int best_priority = -1;
			pkgCache::VerIterator best_ver;
			for(pkgCache::VerIterator ver = pkg.VersionList();
			    !ver.end(); ++ver)
			  {
			    if(!_system->VS->CheckDep(ver.VerStr(), it->Op, it->Version.c_str()))
			      continue;

			    for(pkgCache::VerFileIterator vf = ver.FileList();
				!vf.end(); ++vf)
			      {
				const int priority = policy.GetPriority(vf.File());
				if(best_ver.end() || best_priority < priority)
				  {
				    best_ver = ver;
				    best_priority = priority;
				  }
			      }
			  }

			if(!best_ver.end())
			  {
			    cmdline_applyaction(cmdline_install,
						pkg,
						seen_virtual_packages,
						to_install, to_hold,
						to_remove, to_purge,
						verbose,
						cmdline_version_version,
						best_ver.VerStr(),
						policy,
						arch_only,
						allow_auto,
                                                term_metrics);
			    satisfied = true;
			  }
		      }
		  }

		if(!satisfied)
		  ok = false;
	      }
	  }
	else // if(!is_conflict); now we're satisfying a conflict.
	  {
	    for(BuildDepList::const_iterator it2 = or_group_start;
		it2 <= it; ++it2)
	      {
		pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(it2->Package);
		if(!pkg.end())
		  {
		    pkgCache::VerIterator ver = pkg.CurrentVer();
		    if(!ver.end() &&
		       _system->VS->CheckDep(ver.VerStr(),
					     it2->Op,
					     it2->Version.c_str()))
		      {
			cmdline_applyaction(cmdline_remove,
					    pkg,
					    seen_virtual_packages,
					    to_install, to_hold,
					    to_remove, to_purge,
					    verbose,
					    cmdline_version_cand,
					    std::string(),
					    policy,
					    arch_only,
					    allow_auto,
                                            term_metrics);
		      }
		  }
	      }
	  }

	if(!ok)
	  {
	    rval = false;

	    std::string build_dep_description = pkgSrcRecords::Parser::BuildDepType(it->Type);;
	    build_dep_description += ": ";
	    for(BuildDepList::const_iterator it2 = or_group_start;
		it2 <= it; ++it2)
	      {
		if(it2 != or_group_start)
		  build_dep_description += " | ";
		build_dep_description += it2->Package;
		if((it2->Op & ~pkgCache::Dep::Or) != pkgCache::Dep::NoOp)
		  {
		    build_dep_description += " (";
		    build_dep_description += pkgCache::CompType(it2->Type);
		    build_dep_description += " ";
		    build_dep_description += it2->Version;
		    build_dep_description += ")";
		  }
	      }
	    printf(_("Unable to satisfy the build-depends: %s.\n"),
		   build_dep_description.c_str());
	  }
      }

    return rval;
  }
}

bool cmdline_applyaction(cmdline_pkgaction_type action,
			 pkgCache::PkgIterator pkg,
			 pkgset &seen_virtual_packages,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 cmdline_version_source source,
			 const string &sourcestr,
			 pkgPolicy &policy,
			 bool arch_only,
			 bool allow_auto,
                         const shared_ptr<terminal_metrics> &term_metrics)
{
  // Handle virtual packages.
  if(!pkg.ProvidesList().end() && pkg.VersionList().end())
    {
      const bool seen_in_first_pass =
        seen_virtual_packages.find(pkg) != seen_virtual_packages.end();

      if(!seen_in_first_pass)
        {
          switch(action)
            {
            case cmdline_install:
            case cmdline_installauto:
              for(pkgCache::PrvIterator prv=pkg.ProvidesList();
                  prv.end() == false;
                  ++prv)
                {
                  if(prv.OwnerPkg().CurrentVer() == prv.OwnerVer())
                    {
                      if(verbose > 0)
                        printf(_("Note: \"%s\", providing the virtual package\n"
                                 "      \"%s\", is already installed.\n"),
                               prv.OwnerPkg().FullName(true).c_str(),
                               pkg.FullName(true).c_str());
                      return true;
                    }
                  else if((*apt_cache_file)[prv.OwnerPkg()].InstVerIter(*apt_cache_file)==prv.OwnerVer())
                    {
                      if(verbose > 0)
                        printf(_("Note: \"%s\", providing the virtual package\n"
                                 "      \"%s\", is already going to be installed.\n"),
			       prv.OwnerPkg().FullName(true).c_str(),
                               pkg.FullName(true).c_str());
                      return true;
                    }
                }
              break;
            default:
              break;
            }
        }

      seen_virtual_packages.insert(pkg);

      // See if there's only one possible package to install.
      pkgvector cands;

      for(pkgCache::PrvIterator prv=pkg.ProvidesList();
          !prv.end(); ++prv)
        {
          if((*apt_cache_file)[prv.OwnerPkg()].CandidateVerIter(*apt_cache_file)==prv.OwnerVer())
            cands.push_back(prv.OwnerPkg());
        }

      if(cands.size()==0)
        {
          if(!seen_in_first_pass)
            printf(_("\"%s\" exists in the package database, but it is not a\n"
                     "real package and no package provides it.\n"),
                   pkg.FullName(true).c_str());
          return false;
        }
      else if(cands.size()>1)
        {
          if(!seen_in_first_pass)
            {
              printf(_("\"%s\" is a virtual package provided by:\n"),
                     pkg.FullName(true).c_str());
              cmdline_show_pkglist(cands, term_metrics);
              printf(_("You must choose one to install.\n"));
              printf("\n");
              _error->Error(_("Package '%s' has no installation candidate"),
                            pkg.FullName(true).c_str());
            }
          return false;
        }
      else if(cands.size()==1)
        {
          if(!seen_in_first_pass)
            {
              printf(_("Note: selecting \"%s\" instead of the\n"
                       "      virtual package \"%s\"\n"),
                     cands[0].FullName(true).c_str(), pkg.Name());
            }
          pkg = cands[0];
        }
    }

  pkgCache::VerIterator ver=pkg.CurrentVer();
  if(action==cmdline_install || action == cmdline_installauto)
    {
      ver=cmdline_find_ver(pkg, source, sourcestr);
      if(ver.end() == true)
        return false;
    }

  pkgDepCache::StateCache &pkg_state((*apt_cache_file)[pkg]);

  switch(action)
    {
    case cmdline_installauto:
    case cmdline_install:
      if(pkg.CurrentVer()!=ver || pkg->CurrentState!=pkgCache::State::Installed)
	to_install.insert(pkg);
      else if(pkg_state.Keep() && verbose>0)
	printf(_("%s is already installed at the requested version (%s)\n"),
	       pkg.FullName(true).c_str(),
	       ver.VerStr());
      break;
    case cmdline_upgrade:
      if(pkg_state.Status == 1)
	to_install.insert(pkg);
      else if(pkg_state.Status > 1)
	{
	  if(verbose > 0)
	    printf(_("%s is not currently installed, so it will not be upgraded.\n"),
                   pkg.FullName(true).c_str());
	}
      else if(verbose > 0)
	printf(_("%s is already installed at the latest version, so it will not be upgraded.\n"),
               pkg.FullName(true).c_str());
      break;

    case cmdline_reinstall:
      if(pkg.CurrentVer().end())
	printf(_("%s is not currently installed, so it will not be reinstalled.\n"),
               pkg.FullName(true).c_str());

      break;
    case cmdline_remove:
      if(!pkg.CurrentVer().end())
	to_remove.insert(pkg);
      else if(pkg_state.Keep() && verbose>0)
	printf(_("Package %s is not installed, so it will not be removed\n"),
               pkg.FullName(true).c_str());
      break;
    case cmdline_purge:
      if(!pkg.CurrentVer().end() || pkg->CurrentState!=pkgCache::State::ConfigFiles)
	to_purge.insert(pkg);
      else if(pkg_state.Keep() && verbose>0)
	printf(_("Package %s is not installed, so it will not be removed\n"),
               pkg.FullName(true).c_str());
      break;
    case cmdline_hold:
      to_hold.insert(pkg);
      break;
    case cmdline_keep:
      to_install.erase(pkg);
      to_remove.erase(pkg);
      to_purge.erase(pkg);
      to_hold.erase(pkg);
      break;
    case cmdline_unhold:
      to_hold.erase(pkg);
      break;
    case cmdline_forbid_version:
      if(source==cmdline_version_cand)
	{
	  if(pkg.CurrentVer().end())
	    printf(_("Package %s is not installed, cannot forbid an upgrade\n"),
                   pkg.FullName(true).c_str());
	  else if(!pkg_state.Upgradable())
	    printf(_("Package %s is not upgradable, cannot forbid an upgrade\n"),
                   pkg.FullName(true).c_str());
	}
    default:
      break;
    }

  switch(action)
    {
    case cmdline_installauto:
    case cmdline_install:
    case cmdline_reinstall:
      if(action==cmdline_reinstall && pkg.CurrentVer().end())
	break;

      (*apt_cache_file)->set_candidate_version(ver, NULL);
      (*apt_cache_file)->mark_install(pkg, allow_auto && aptcfg->FindB(PACKAGE "::Auto-Install", true),
				      action == cmdline_reinstall, NULL);
      if(action == cmdline_installauto)
	(*apt_cache_file)->mark_auto_installed(pkg, true, NULL);
      break;
    case cmdline_upgrade:
      if(pkg_state.Status == 1)
	(*apt_cache_file)->mark_install(pkg, allow_auto && aptcfg->FindB(PACKAGE "::Auto-Install", true),
					false, NULL);
      break;
    case cmdline_remove:
      (*apt_cache_file)->mark_delete(pkg, false, false, NULL);
      break;
    case cmdline_purge:
      (*apt_cache_file)->mark_delete(pkg, true, false, NULL);
      break;
    case cmdline_hold:
      (*apt_cache_file)->mark_keep(pkg, false, true, NULL);
      break;
    case cmdline_keep:
      (*apt_cache_file)->mark_keep(pkg, false, false, NULL);
      break;
    case cmdline_unhold:
      if(pkg_state.Keep())
	(*apt_cache_file)->mark_keep(pkg, false, false, NULL);
      break;
    case cmdline_markauto:
      (*apt_cache_file)->mark_auto_installed(pkg, true, NULL);
      break;
    case cmdline_unmarkauto:
      (*apt_cache_file)->mark_auto_installed(pkg, false, NULL);
      break;
    case cmdline_forbid_version:
      if(source!=cmdline_version_cand)
	(*apt_cache_file)->forbid_upgrade(pkg, sourcestr, NULL);
      else
	{
	  pkgCache::VerIterator curver=pkg.CurrentVer();
	  pkgCache::VerIterator candver = pkg_state.CandidateVerIter(*apt_cache_file);
	  if(!curver.end() && !candver.end() && curver!=candver)
	    (*apt_cache_file)->forbid_upgrade(pkg, candver.VerStr(), NULL);
	}
      break;
    case cmdline_build_depends:
      return cmdline_do_build_depends(pkg.Name(),
				      seen_virtual_packages,
				      source,
				      sourcestr,
				      arch_only,
				      policy,
				      to_install, to_hold,
				      to_remove, to_purge,
				      verbose,
				      allow_auto,
                                      term_metrics);
    default:
      fprintf(stderr, "Internal error: impossible pkgaction type\n");
      abort();
    }

  return true;
}

bool cmdline_applyaction(string s,
			 std::set<pkgCache::PkgIterator> &seen_virtual_packages,
			 cmdline_pkgaction_type action,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 pkgPolicy &policy, bool arch_only,
			 bool allow_auto,
                         const shared_ptr<terminal_metrics> &term_metrics)
{
  using namespace aptitude::matching;

  bool rval=true;

  cmdline_version_source source=cmdline_version_cand;

  string sourcestr, package;

  if(!cmdline_parse_source(s, source, package, sourcestr))
    return false;

  // This is harmless for other commands, but it won't make sense.
  if(source == cmdline_version_version &&
     action != cmdline_install &&
     action != cmdline_forbid_version &&
     action != cmdline_installauto &&
     action != cmdline_build_depends)
    return _error->Error(_("You can only specify a package version with an"
                           " 'install' command or a 'forbid-version' command"));

  if(source == cmdline_version_archive &&
     action != cmdline_install &&
     action != cmdline_installauto &&
     action != cmdline_build_depends)
    return _error->Error(_("You can only specify a package archive with an"
                           " 'install' command"));

  pkgset pkgset;
  if(aptitude::cmdline::pkgset_from_string(&pkgset, package,
                                           GlobalError::ERROR,
                                           GlobalError::WARNING) == false)
    {
      // pkgset_from_string generates a warning for patterns with no
      // matches.
      if(is_pattern(package) == true)
        return false;

      // Assume the user asked for a source package.
      if(action == cmdline_build_depends)
        return cmdline_do_build_depends(package,
                                        seen_virtual_packages,
                                        source,
                                        sourcestr,
                                        arch_only,
                                        policy,
                                        to_install, to_hold,
                                        to_remove, to_purge,
                                        verbose,
                                        allow_auto,
                                        term_metrics);

      // Maybe they misspelled the package name?
      pkgvector possible;

      for(pkgCache::PkgIterator j=(*apt_cache_file)->PkgBegin();
          !j.end(); ++j)
        {
          if(!(j.VersionList().end() && j.ProvidesList().end()) &&
             strstr(j.Name(), package.c_str()) != NULL)
            possible.push_back(j);
        }

      if(!possible.empty())
        {
          // Don't overwhelm the user.
          if(possible.size()>40)
            printf(_("Couldn't find package \"%s\", and more than 40\n"
                     "packages contain \"%s\" in their name.\n"),
                   package.c_str(), package.c_str());
          else
            {
              printf(_("Couldn't find package \"%s\".  However, the following\n"
                       "packages contain \"%s\" in their name:\n"),
                     package.c_str(), package.c_str());
              cmdline_show_pkglist(possible, term_metrics);
            }
        }
      else
        {
          for(pkgCache::PkgIterator j=(*apt_cache_file)->PkgBegin();
              !j.end(); ++j)
            {
              for(pkgCache::VerIterator v = j.VersionList();
                  !v.end(); ++v)
                {
                  std::wstring desc = get_long_description(v, apt_package_records);
                  if(desc.find(cw::util::transcode(package)) != desc.npos)
                    {
                      possible.push_back(j);
                      break;
                    }
                }
            }

          if(possible.empty())
            printf(_("Couldn't find any package whose name or description matched \"%s\"\n"),
                   package.c_str());
          else if(possible.size()>40)
            printf(_("Couldn't find any package matching \"%s\", and more than 40\n"
                     "packages contain \"%s\" in their description.\n"),
                   package.c_str(), package.c_str());
          else
            {
              printf(_("Couldn't find any package matching \"%s\".  However, the following\n"
                       "packages contain \"%s\" in their description:\n"),
                     package.c_str(), package.c_str());
              cmdline_show_pkglist(possible, term_metrics);
            }
        }

      return false;
    }

  for(pkgset::const_iterator it = pkgset.begin();
      it != pkgset.end();
      ++it)
    rval &= cmdline_applyaction(action, *it,
                                seen_virtual_packages,
                                to_install, to_hold, to_remove, to_purge,
                                verbose, source,
                                sourcestr, policy, arch_only, allow_auto,
                                term_metrics);

  return rval;
}

/** \return \b false iff the beginning of s doesn't look like an
 *  action specifier.
 */
static bool parse_action_str(const string &s,
			     string::size_type &loc,
			     cmdline_pkgaction_type &action)
{
  if(loc<s.size())
    {
      switch(s[loc])
	{
	case '_':
	  action=cmdline_purge;
	  ++loc;
	  break;
	case '-':
	  action=cmdline_remove;
	  ++loc;
	  break;
	case '=':
	  action=cmdline_hold;
	  ++loc;
	  break;
	case '+':
	  action=cmdline_install;
	  ++loc;
	  if(loc<s.size() && s[loc]=='M')
	    {
	      action=cmdline_installauto;
	      ++loc;
	    }
	  break;
	case ':':
	  action=cmdline_keep;
	  ++loc;
	  break;
	case '&':
	  ++loc;
	  if(loc<s.size())
	    switch(s[loc])
	      {
	      case 'B':
		// &BD to install build-deps.
		++loc;
		if(loc < s.size() && s[loc] == 'D')
		  {
		    action = cmdline_build_depends;
		    ++loc;
		  }
		else
		  return false;

		break;
	      case 'M':
		action=cmdline_markauto;
		++loc;
		break;
	      case 'm':
		action=cmdline_unmarkauto;
		++loc;
		break;
	      default:
		return false;
	      }
	  break;
	default:
	  return false;
	}
      return true;
    }
  else
    return false;
}

void cmdline_parse_action(string s,
			  std::set<pkgCache::PkgIterator> &seen_virtual_packages,
			  pkgset &to_install, pkgset &to_hold,
			  pkgset &to_remove, pkgset &to_purge,
			  int verbose,
			  pkgPolicy &policy, bool arch_only,
			  bool allow_auto,
                          const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics)
{
  string::size_type loc=0;

  while(loc<s.size() && isspace(s[loc]))
    ++loc;

  if(loc==s.size())
    return;

  cmdline_pkgaction_type action;

  if(!parse_action_str(s, loc, action))
    {
      printf(_("Bad action character '%c'\n"), s[0]);
      return;
    }

  while(loc<s.size())
    {
      while(loc<s.size() && isspace(s[loc]))
	++loc;

      if(loc<s.size())
	{
	  if(!parse_action_str(s, loc, action))
	    {
	      string pkgname;
	      while(loc<s.size() && !isspace(s[loc]))
		pkgname+=s[loc++];

	      if(!cmdline_applyaction(pkgname,
				      seen_virtual_packages,
				      action,
				      to_install, to_hold,
				      to_remove, to_purge,
				      verbose, policy,
				      arch_only, allow_auto,
                                      term_metrics))
		return;
	    }
	}
    }
}
