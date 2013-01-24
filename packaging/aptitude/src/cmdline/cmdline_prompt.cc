// cmdline_prompt.cc
//
// Copyright (C) 2010-2011 Daniel Burrows
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
//
//   Handles the preview and prompt that's displayed from the command-line.


// Local includes:
#include "cmdline_prompt.h"

#include "cmdline_action.h"
#include "cmdline_changelog.h"
#include "cmdline_resolver.h"
#include "cmdline_show.h"
#include "cmdline_show_broken.h"
#include "cmdline_util.h"
#include "cmdline_why.h"

#include <ui.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/infer_reason.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/util/util.h>

// System includes:
#include <apt-pkg/algorithms.h>
#include <apt-pkg/dpkgpm.h>
#include <apt-pkg/error.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/strutl.h>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>

#include <algorithm>

using namespace std;
namespace cw = cwidget;

using aptitude::cmdline::terminal_metrics;
using aptitude::why::make_cmdline_why_callbacks;
using aptitude::why::why_callbacks;
using boost::shared_ptr;

struct fetchinfo
{
  unsigned long long FetchBytes, FetchPBytes, DebBytes;

  fetchinfo()
    : FetchBytes(0), FetchPBytes(0), DebBytes(0)
  {
  }
};

static bool get_fetchinfo(fetchinfo &f)
{
  download_signal_log m;
  pkgAcquire fetcher;
  fetcher.Setup(&m);
  pkgSourceList l;
  if(!l.ReadMainList())
    return _error->Error(_("Couldn't read list of sources"));

  pkgDPkgPM pm(*apt_cache_file);
  pm.GetArchives(&fetcher, &l, apt_package_records);

  f.FetchBytes=fetcher.FetchNeeded();
  f.FetchPBytes=fetcher.PartialPresent();
  f.DebBytes=fetcher.TotalNeeded();

  return true;
}

static string reason_string_list(set<reason> &reasons)
{
  set<reason>::iterator prev=reasons.end();
  string s;

  bool first=true;
  for(set<reason>::iterator why=reasons.begin();
      why!=reasons.end(); prev=why++)
    {
      // Filter duplicates.
      if(prev!=reasons.end() &&
	 prev->pkg==why->pkg && prev->dep->Type==why->dep->Type)
	continue;

      if(!first)
	s+=", ";
      else
	{
	  s+=" (";
	  first=false;
	}

      string dep_type = const_cast<pkgCache::DepIterator &>(why->dep).DepType();
      s += cw::util::transcode(cw::util::transcode(dep_type).substr(0, 1));
      s+=": ";
      s+=why->pkg.FullName(true);
    }
  if(!first)
    s+=")";

  return s;
}

namespace
{
  // Sort action vectors by the name of the package in the first
  // action.
  struct compare_first_action
  {
    pkg_name_lt base;
  public:
    typedef aptitude::why::action action;
    bool operator()(const std::vector<action> &reason1,
		    const std::vector<action> &reason2)
    {
      if(reason1.empty())
	return !reason2.empty();
      else if(reason2.empty())
	return false;
      else
	return base(reason1.front().get_dep().ParentPkg(),
                    reason2.front().get_dep().ParentPkg());
    }
  };

  std::string roots_string(const pkgCache::PkgIterator &pkg,
			   int verbose,
                           const shared_ptr<terminal_metrics> &term_metrics)
  {
    using namespace aptitude::matching;
    using cw::util::ref_ptr;

    using namespace aptitude::why;
    pkgDepCache::StateCache &state((*apt_cache_file)[pkg]);

    // Don't show anything for packages that are kept back or are
    // manually installed.
    if(state.Keep() ||
       (state.Install() && ((state.Flags & pkgCache::Flag::Auto) == 0)))
      return "";

    target t(state.Install() ? target::Install(pkg) : target::Remove(pkg));
    std::vector<ref_ptr<pattern> > leaves;
    leaves.push_back(parse("?not(?automatic)"));

    std::vector<std::vector<action> > reasons;

    std::vector<search_params> params;
    params.push_back(search_params(search_params::InstallNotCurrent,
				   search_params::DependsOnly,
				   false));
    params.push_back(search_params(search_params::InstallNotCurrent,
				   search_params::DependsOnly,
				   true));
    params.push_back(search_params(search_params::InstallNotCurrent,
				   search_params::Recommends,
				   false));
    params.push_back(search_params(search_params::InstallNotCurrent,
				   search_params::Recommends,
				   true));

    const boost::shared_ptr<why_callbacks> callbacks =
      make_cmdline_why_callbacks(verbose, term_metrics);
    for(std::vector<search_params>::const_iterator it = params.begin();
	it != params.end(); ++it)
      {
	if(find_justification(t,
			      leaves,
			      *it,
			      true,
                              callbacks,
			      reasons))
	  break;
      }

    if(reasons.size() == 0)
      return "";

    std::vector<std::string> reason_strings;
    roots_string_mode mode;
    if(verbose > 1)
      mode = show_chain_with_versions;
    else if(verbose > 0)
      mode = show_chain;
    else
      mode = show_requiring_packages;
    summarize_reasons(reasons, mode, reason_strings);

    std::string rval = "(";
    if(mode == show_requiring_packages)
      rval += "for ";
    bool first = true;
    for(std::vector<std::string>::const_iterator it = reason_strings.begin();
	it != reason_strings.end(); ++it)
      {
	if(first)
	  first = false;
	else
	  rval += ", ";

	rval += *it;
      }
    rval += ")";

    return rval;
  }
}

/** Prints a description of a list of packages, with annotations
 *  reflecting how or why they will be changed.
 *
 *  Tries to infer the dependencies that caused a package to be installed,
 *  removed, or held.
 *
 *  \param items the set of items to examine
 *  \param verbose controls various aspects of how verbose the list is.
 *  \param showvers if \b true, display version numbers as appropriate
 *  \param showdeps if \b true, display the packages that depend on
 *                  automatically installed packages.
 *  \param showsize if \b true, display the change in each package's size
 *  \param showpurge if \b true, display flags indicating which packages
 *                   are being purged.
 *  \param showwhy  if \b true, infer and display the set of manually
 *                  installed packages that depend on each automatically
 *                  installed package.
 */
static void cmdline_show_instinfo(pkgvector &items,
				  int verbose,
				  bool showvers,
				  bool showdeps,
				  bool showsize,
				  bool showpurge,
				  bool showwhy,
                                  const shared_ptr<terminal_metrics> &term_metrics)
{
  sort(items.begin(), items.end(), pkg_name_lt());
  strvector output;

  for(pkgvector::iterator i=items.begin(); i!=items.end(); ++i)
    {
      std::string tags;
      string s=i->FullName(true);

      pkgDepCache::StateCache &state=(*apt_cache_file)[*i];
      //aptitudeDepCache::aptitude_state &extstate=(*apt_cache_file)->get_ext_state(*i);
      pkgCache::VerIterator instver=state.InstVerIter(*apt_cache_file);

      if(showpurge)
	{
	  if(state.Delete() && state.iFlags&pkgDepCache::Purge)
	    tags.push_back('p');
	}

      if(state.InstBroken())
        tags.push_back('b');

      switch(find_pkg_state(*i, *apt_cache_file, true))
	{
	case pkg_auto_remove:
	case pkg_auto_install:
	case pkg_auto_hold:
	  tags.push_back('a');
	  break;
	case pkg_unused_remove:
	  tags.push_back('u');
	  break;
        default:
          break;
	}

      if(!tags.empty())
	{
	  std::sort(tags.begin(), tags.end());
	  s.push_back('{');
	  s += tags;
	  s.push_back('}');
	}

      // Display version numbers.
      if(showvers)
	{
	  pkgCache::VerIterator cur = i->CurrentVer();
	  pkgCache::VerIterator inst = state.InstVerIter(*apt_cache_file);

	  // Display x -> y for upgraded, held, and downgraded packages.
	  if( (state.Status==1 || state.Downgrade()) &&
	      state.Install() &&
	      i->CurrentVer() != inst)
	    {
	      s+=" [";
	      if(cur.end())
		s += "??";
	      else
		s += cur.VerStr();
	      s+=" -> ";
	      if(inst.end())
		s += "??";
	      else
		s += inst.VerStr();
	      s+="]";
	    }
	  else if(state.Install())
	    {
	      s+=" [";
	      if(inst.end())
		s += "??";
	      else
		s += inst.VerStr();
	      s+="]";
	    }
	  else if(state.Delete())
	    {
	      s += " [";
	      if(cur.end())
		{
		  if((*i)->CurrentState == pkgCache::State::ConfigFiles)
		    s += _("Config files");
		  else
		    s += "??";
		}
	      else
		s += cur.VerStr();
	      s += "]";
	    }
	}

      // Show the change in size between the versions.
      if(showsize)
	{
	  int dsize=(instver.end()?0:instver->InstalledSize)
	    -(i->CurrentVer().end()?0:i->CurrentVer()->InstalledSize);

	  if(dsize>0)
	    s+=" <+"+SizeToStr(dsize)+"B>";
	  else if(dsize<0)
	    s+=" <-"+SizeToStr(dsize)+"B>";
	}

      if(showdeps)
	{
	  set<reason> reasons;
	  infer_reason(*i, reasons);
	  s+=reason_string_list(reasons);
	}

      if(showwhy)
	{
	  std::string whystring(roots_string(*i, verbose, term_metrics));
	  if(!whystring.empty())
	    {
	      s += " ";
	      s += whystring;
	    }
	}

      if(showvers || showsize || showdeps || showwhy)
	s += ' ';

      output.push_back(s);
    }

  cmdline_show_stringlist(output, term_metrics);
}

// Note that not all of these are actually used, but I'm preserving
// the tags in case there are requests to reinstate them.
static const char *cmdline_action_descriptions[num_pkg_action_states]={
  N_("The following packages are BROKEN:"),
  N_("The following packages are unused and will be REMOVED:"),
  N_("The following packages have been automatically kept back:"),
  N_("The following NEW packages will be automatically installed:"),
  N_("The following packages will be automatically REMOVED:"),
  N_("The following packages will be DOWNGRADED:"),
  N_("The following packages have been kept back:"),
  N_("The following packages will be REINSTALLED:"),
  N_("The following NEW packages will be installed:"),
  N_("The following packages will be REMOVED:"),
  N_("The following packages will be upgraded:"),
  N_("The following partially installed packages will be configured:")
};

// Probably something like cin.getline() would work, but I don't trust that
// for interactive use.
string prompt_string(const string &prompt)
{
  printf("%s", prompt.c_str());
  fflush(stdout);

  string rval;
  char buf[1024];
  cin.getline(buf, 1023);
  rval+=buf;

  while(!cin && !cin.eof())
    {
      cin.getline(buf, 1023);
      rval+=buf;
    }

  if(!cin)
    throw StdinEOFException();

  return rval;
}

/** Checks for broken/deleted essential packages and displays a big
 *  fat warning message about them.  Returns false if the user doesn't
 *  want to continue.
 */
static bool prompt_essential(const shared_ptr<terminal_metrics> &term_metrics)
{
  pkgvector todelete, whatsbroken;
  bool ok=true;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      if( (pkg->Flags & pkgCache::Flag::Essential) ||
	  (pkg->Flags & pkgCache::Flag::Important))
	{
	  // Eek!
	  if((*apt_cache_file)[pkg].Delete())
	    todelete.push_back(pkg);

	  if((*apt_cache_file)[pkg].InstBroken())
	    whatsbroken.push_back(pkg);
	}
    }

  if(!todelete.empty())
    {
      ok=false;

      printf(_("The following ESSENTIAL packages will be REMOVED!\n"));
      cmdline_show_pkglist(todelete, term_metrics);
      printf("\n");
    }

  if(!whatsbroken.empty())
    {
      ok=false;

      printf(_("The following ESSENTIAL packages will be BROKEN by this action:\n"));

      for(pkgvector::iterator i=whatsbroken.begin(); i!=whatsbroken.end(); ++i)
	show_broken_deps(*i);

      printf("\n");
    }

  if(!ok)
    {
      printf(_("WARNING: Performing this action will probably cause your"
               " system to break!\n"
               "         Do NOT continue unless you know EXACTLY what you"
               " are doing!\n"));

      string untranslated_prompt = N_("I am aware that this is a very bad idea");
      string prompt = _(untranslated_prompt.c_str());
      char buf[1024];

      printf(_("To continue, type the phrase \"%s\":\n"), prompt.c_str());
      cin.getline(buf, 1023);
      bool rval = (prompt == buf || untranslated_prompt == buf);

      while(!cin && !cin.eof())
	cin.getline(buf, 1023);

      if(!cin)
	throw StdinEOFException();

      return rval;
    }

  return true;
}

/** Checks for trust violations and displays a big fat warning if any exist.
 *
 *  \return true if everything is OK or the user overrode the warning.
 */
static bool prompt_trust(const shared_ptr<terminal_metrics> &term_metrics)
{
  pkgvector untrusted;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

      if(state.Install())
	{
	  pkgCache::VerIterator curr=pkg.CurrentVer();
	  pkgCache::VerIterator cand=state.InstVerIter(*apt_cache_file);

	  if((curr.end() || package_trusted(curr)) &&
	     !package_trusted(cand))
	    untrusted.push_back(pkg);
	}
    }

  if(!untrusted.empty())
    {
      printf(_("WARNING: untrusted versions of the following packages will be installed!\n\n"
	       "Untrusted packages could compromise your system's security.\n"
	       "You should only proceed with the installation if you are certain that\n"
	       "this is what you want to do.\n\n"));

      cmdline_show_pkglist(untrusted, term_metrics);

      printf("\n");


      if(aptcfg->FindB(PACKAGE "::CmdLine::Ignore-Trust-Violations", false))
	{
	  printf(_("*** WARNING ***   Ignoring these trust violations because\n"
		   "                  %s::CmdLine::Ignore-Trust-Violations is 'true'!\n"),
		 PACKAGE);
	  return true;
	}

      if(aptcfg->FindB("Apt::Get::AllowUnauthenticated", false))
	{
	  printf("%s",
		 _("*** WARNING ***   Ignoring these trust violations because\n"
		   "                  Apt::Get::AllowUnauthenticated is 'true'!\n"));
	  return true;
	}


      // ForTranslators: This string is a confirmation message, which
      // users (especially CJK users) should be able to input without
      // input methods.  Please include nothing but ASCII characters.
      // The text preceding the pipe character (|) will be ignored and
      // can be removed from your translation.
      const string okstr    = P_("Go ahead and ignore the warning|Yes");
      // ForTranslators: This string is a confirmation message, which
      // users (especially CJK users) should be able to input without
      // input methods.  Please include nothing but ASCII characters.
      // The text preceding the pipe character (|) will be ignored and
      // can be removed from your translation.
      const string abortstr = P_("Abort instead of overriding the warning|No");

      // These strings are used to compare in a translation-invariant
      // way, so that "yes" and "no" are always valid inputs; if the
      // user can't enter the translated string for some reason,
      // he/she can always enter the fallback strings.
      const string fallback_okstr = "Yes";
      const string fallback_abortstr = "No";

      while(1)
	{
	  printf(_("Do you want to ignore this warning and proceed anyway?\n"));
	  printf(_("To continue, enter \"%s\"; to abort, enter \"%s\": "), okstr.c_str(), abortstr.c_str());
	  char buf[1024];
	  cin.getline(buf, 1023);
	  buf[1023]='\0';

	  if(cin.eof())
	    throw StdinEOFException();


	  const bool is_ok =             strncasecmp(okstr.c_str(), buf, okstr.size()) == 0;
	  const bool is_fallback_ok =    strncasecmp(fallback_okstr.c_str(), buf, fallback_okstr.size()) == 0;
	  const bool is_abort =          strncasecmp(abortstr.c_str(), buf, abortstr.size()) == 0;
	  const bool is_fallback_abort = strncasecmp(fallback_abortstr.c_str(), buf, fallback_abortstr.size()) == 0;

	  const bool rval = is_ok || (is_fallback_ok && !is_abort);

	  if(!is_ok && !is_abort && !is_fallback_ok && !is_fallback_abort)
	    printf(_("Unrecognized input.  Enter either \"%s\" or \"%s\".\n"), okstr.c_str(), abortstr.c_str());
	  else
	    return rval;
	}
    }

  return true;
}

/** Displays a preview of the stuff to be done -- like apt-get, it collects
 *  all the "stuff to install" in one place.
 *
 *  The arguments and return value are for when this is used for a targeted
 *  install/remove; it can figure out whether any packages not requested by the
 *  user are being installed/removed (eg, because of sticky states) and
 *  tell the caller to pause for confirmation.
 */
bool cmdline_show_preview(bool as_upgrade, pkgset &to_install,
			  pkgset &to_hold, pkgset &to_remove,
			  bool showvers, bool showdeps,
			  bool showsize, bool showwhy,
			  int verbose,
                          const shared_ptr<terminal_metrics> &term_metrics)
{
  const int quiet = aptcfg->FindI("Quiet", 0);

  pkgvector lists[num_pkg_action_states];
  pkgvector recommended, suggested;
  pkgvector extra_install, extra_remove;
  unsigned long Upgrade=0, Downgrade=0, Install=0, ReInstall=0;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      if((*apt_cache_file)[pkg].NewInstall())
	++Install;
      else if((*apt_cache_file)[pkg].Upgrade())
	++Upgrade;
      else if((*apt_cache_file)[pkg].Downgrade())
	++Downgrade;
      else if(!(*apt_cache_file)[pkg].Delete() &&
	      ((*apt_cache_file)[pkg].iFlags & pkgDepCache::ReInstall))
	++ReInstall;

      pkg_action_state state=find_pkg_state(pkg, *apt_cache_file, true);

      switch(state)
	{
	case pkg_auto_install:
	case pkg_install:
	case pkg_upgrade:
	  if(to_install.find(pkg)==to_install.end())
	    extra_install.push_back(pkg);
	  break;
	case pkg_auto_remove:
	case pkg_unused_remove:
	case pkg_remove:
	  if(to_remove.find(pkg)==to_remove.end())
	    extra_remove.push_back(pkg);
	  break;
	case pkg_unchanged:
	  if(pkg.CurrentVer().end())
	    {
	      if(package_recommended(pkg))
		recommended.push_back(pkg);
	      else if(package_suggested(pkg))
		suggested.push_back(pkg);
	    }
	default:
	  break;
	}

      switch(state)
	{
	case pkg_auto_install:
	  lists[pkg_install].push_back(pkg);
	  break;
	case pkg_unused_remove:
	case pkg_auto_remove:
	  lists[pkg_remove].push_back(pkg);
	  break;
	case pkg_auto_hold:
	  if(as_upgrade && to_install.find(pkg) != to_install.end())
	    lists[pkg_hold].push_back(pkg);
	  break;
	case pkg_hold:
	  if(to_install.find(pkg) != to_install.end())
	    lists[pkg_hold].push_back(pkg);
	  break;
	case pkg_unchanged:
	  break;
	default:
	  lists[state].push_back(pkg);
	}
    }

  for(int i=0; i<num_pkg_action_states; ++i)
    {
      if(!lists[i].empty())
	{
	  printf("%s\n", _(cmdline_action_descriptions[i]));
	  cmdline_show_instinfo(lists[i],
				verbose,
				showvers, showdeps, showsize,
				i == pkg_remove ||
				i == pkg_auto_remove ||
				i == pkg_unused_remove,
				showwhy,
                                term_metrics);
	}
    }

  if(quiet == 0 && !recommended.empty())
    {
      printf(_("The following packages are RECOMMENDED but will NOT be installed:\n"));
      cmdline_show_instinfo(recommended, verbose, showvers, showdeps, showsize, false, showwhy, term_metrics);
    }

  if(verbose>0 && !suggested.empty())
    {
      printf(_("The following packages are SUGGESTED but will NOT be installed:\n"));
      cmdline_show_instinfo(suggested, verbose, showvers, showdeps, showsize, false, showwhy, term_metrics);
    }

  if((*apt_cache_file)->DelCount() == 0 &&
     (*apt_cache_file)->InstCount() == 0)
    printf(_("No packages will be installed, upgraded, or removed.\n"));

  printf(_("%lu packages upgraded, %lu newly installed, "),
	 Upgrade, Install);

  if(ReInstall!=0)
    printf(_("%lu reinstalled, "), ReInstall);
  if(Downgrade!=0)
    printf(_("%lu downgraded, "), Downgrade);

  printf(_("%lu to remove and %lu not upgraded.\n"),
	 (*apt_cache_file)->DelCount(),(*apt_cache_file)->KeepCount());

  fetchinfo f;
  if(get_fetchinfo(f))
    {
      if(f.DebBytes!=f.FetchBytes)
	printf(_("Need to get %sB/%sB of archives. "),
	       SizeToStr(f.FetchBytes).c_str(), SizeToStr(f.DebBytes).c_str());
      else
	printf(_("Need to get %sB of archives. "),
	       SizeToStr(f.DebBytes).c_str());
    }
  // else
  //   _error->DumpErrors();

  if((*apt_cache_file)->UsrSize() >=0)
    printf(_("After unpacking %sB will be used.\n"),
	   SizeToStr((*apt_cache_file)->UsrSize()).c_str());
  else
    printf(_("After unpacking %sB will be freed.\n"),
	   SizeToStr(-(*apt_cache_file)->UsrSize()).c_str());

  // If I return directly below, g++ complains about control reaching the
  // end of a non-void function!
  bool rval;

  rval=((as_upgrade && !lists[pkg_upgrade].empty()) ||
	!(extra_install.empty() && extra_remove.empty()));

  return rval;
}

static void cmdline_parse_show(string response,
			       int verbose,
                               const shared_ptr<terminal_metrics> &term_metrics)
{
  // assume response[0]=='i'
  std::vector<std::string> packages;
  splitws(response, packages, 1, response.size());

  if(packages.empty())
    printf(_("No packages to show -- enter the package names on the line after 'i'.\n"));
  else
    for(std::vector<std::string>::const_iterator it = packages.begin();
	it != packages.end(); ++it)
      do_cmdline_show(*it, verbose, term_metrics);

  prompt_string(_("Press Return to continue."));
}

// Erm.  Merge w/ above?
static void cmdline_parse_changelog(string response, const shared_ptr<terminal_metrics> &term_metrics)
{
  vector<string> packages;
  // assume response[0]=='c'
  splitws(response, packages, 1, response.size());

  if(packages.empty())
    printf(_("No packages found -- enter the package names on the line after 'c'.\n"));
  else
    do_cmdline_changelog(packages, term_metrics);

  prompt_string(_("Press Return to continue."));
}

static void cmdline_parse_why(string response,
                              const shared_ptr<terminal_metrics> &term_metrics)
{
  vector<string> arguments;
  // assume response[0]=='w'
  splitws(response, arguments, 1, response.size());

  if(arguments.empty())
    printf(_("No packages found -- enter zero or more roots of the search followed by the package to justify.\n"));
  else
    {
      bool success;
      string root = arguments.back();
      arguments.pop_back();
      const shared_ptr<why_callbacks> callbacks =
        make_cmdline_why_callbacks(0, term_metrics);
      std::auto_ptr<cw::fragment> frag(do_why(arguments, root,
					      aptitude::why::no_summary,
					      false, false,
                                              callbacks,
                                              success));
      if(frag.get() != NULL)
        {
          const unsigned int screen_width = term_metrics->get_screen_width();
          std::cout << frag->layout(screen_width, screen_width, cwidget::style());
        }
      _error->DumpErrors();
    }
}

static inline cw::fragment *flowindentbox(int i1, int irest, cw::fragment *f)
{
  return indentbox(i1, irest, flowbox(f));
}

static void prompt_help(ostream &out,
                        bool show_resolver_key,
                        const shared_ptr<terminal_metrics> &term_metrics)
{
  std::vector<cw::fragment *> fragments;

  fragments.push_back(cw::fragf(_("y: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("continue with the installation")))));

  fragments.push_back(cw::fragf(_("n: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("abort and quit")))));

  fragments.push_back(cw::fragf(_("i: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("show information about one or more packages; the package names should follow the 'i'")))));

  fragments.push_back(cw::fragf(_("c: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("show the Debian changelogs of one or more packages; the package names should follow the 'c'")))));

  fragments.push_back(cw::fragf(_("d: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("toggle the display of dependency information")))));

  fragments.push_back(cw::fragf(_("s: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("toggle the display of changes in package sizes")))));

  fragments.push_back(cw::fragf(_("v: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("toggle the display of version numbers")))));

  fragments.push_back(cw::fragf(_("w: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("try to find a reason for installing a single package, or explain why installing one package should lead to installing another package.")))));

  if(show_resolver_key)
    fragments.push_back(cw::fragf(_("r: %F"),
				  flowindentbox(0, 3,
						cw::text_fragment(_("run the automatic dependency resolver to fix the broken dependencies.")))));

  fragments.push_back(cw::fragf(_("e: %F"),
				flowindentbox(0, 3,
					      cw::fragf(_("enter the full visual interface")))));

  fragments.push_back(cw::fragf("\n"));
  fragments.push_back(cwidget::flowbox(cw::fragf(_("You may also specify modification to the actions which will be taken.  To do so, type an action character followed by one or more package names (or patterns).  The action will be applied to all the packages that you list.  The following actions are available:"))));
  fragments.push_back(cw::fragf("\n"));

  // FIXME: copied from
  // cmdline_resolver.cc, maybe this
  // should be placed in a common file?
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'+' to install packages"))));

  fragments.push_back(flowindentbox(0, 5,
				    cw::fragf(_("'+M' to install packages and immediately flag them as automatically installed"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'-' to remove packages"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'_' to purge packages"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'=' to place packages on hold"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("':' to keep packages in their current state without placing them on hold"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'&M' to mark packages as automatically installed"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'&m' to mark packages as manually installed"))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'&BD' to install the build-dependencies of a package."))));

  fragments.push_back(cw::fragf("\n"));
  fragments.push_back(cw::flowbox(cw::fragf(_("In the list of actions to be performed, some packages will be followed by one or more characters enclosed in braces; for instance: \"aptitude{u}\".  These characters provide extra information about the package's state, and can include any combination of the following:"))));
  fragments.push_back(cw::fragf("\n"));

  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'a': the package was automatically installed or removed."))));
  fragments.push_back(flowindentbox(0, 4,
                                    cw::fragf(_("'b': some of the package's dependencies are violated by the proposed changes."))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'p': the package will be purged in addition to being removed."))));
  fragments.push_back(flowindentbox(0, 4,
				    cw::fragf(_("'u': the package is being removed because it is unused."))));

  cw::fragment *f = indentbox(2, 2, cw::sequence_fragment(fragments));

  const unsigned int screen_width = term_metrics->get_screen_width();
  out << _("Commands:") << endl;
  out << f->layout(screen_width, screen_width, cwidget::style());
  delete f;
}

bool cmdline_do_prompt(bool as_upgrade,
		       pkgset &to_install,
		       pkgset &to_hold,
		       pkgset &to_remove,
		       pkgset &to_purge,
		       bool showvers,
		       bool showdeps,
		       bool showsize,
		       bool showwhy,
		       bool always_prompt,
		       int verbose,
		       bool assume_yes,
		       bool force_no_change,
		       pkgPolicy &policy,
		       bool arch_only,
                       const shared_ptr<terminal_metrics> &term_metrics)
{
  bool exit=false;
  bool rval=true;
  bool first=true;
  // If true, we will automatically use the internal resolver.  If
  // false, the internal resolver has failed at least once and so we
  // should not use it.
  //
  // The idea here is that the resolver stays disabled until the
  // dependencies are resolved (unless explicitly re-enabled), then
  // becomes available for future breakage.
  bool use_internal_resolver = true;

  while(!exit)
    {
      bool have_broken = false;
      // If we're only doing what the user asked and it's OK to go
      // ahead, we can break out immediately.
      if(!cmdline_show_preview(true, to_install, to_hold, to_remove,
			       showvers, showdeps, showsize, showwhy, verbose,
                               term_metrics) &&
	 first &&
	 !always_prompt &&
	 (*apt_cache_file)->BrokenCount()==0)
	exit=true;
      else if((*apt_cache_file)->BrokenCount() > 0)
	{
	  if(use_internal_resolver)
	    {
	      switch(cmdline_resolve_deps(to_install,
					  to_hold,
					  to_remove,
					  to_purge,
					  assume_yes,
					  force_no_change,
					  verbose,
					  policy,
					  arch_only,
                                          term_metrics))
		{
		case aptitude::cmdline::resolver_success:
		  break;
		case aptitude::cmdline::resolver_incomplete:
		  have_broken = true;
		  use_internal_resolver = false;
		  break;
		case aptitude::cmdline::resolver_user_exit:
		  exit = true;
		  rval = false;
		  break;
		}

	      if(!exit)
		{
		  // Re-display the preview so the user can see any
		  // changes the resolver made.
		  cmdline_show_preview(true, to_install, to_hold, to_remove,
				       showvers, showdeps, showsize, showwhy,
				       verbose, term_metrics);

		  if((*apt_cache_file)->DelCount() == 0 &&
		     (*apt_cache_file)->InstCount() == 0)
		    exit = true;
		}

	      if(first && assume_yes)
		{
		  // If we're supposed to assume "yes", then we actually
		  // say to abort if there are still broken packages, and
		  // say to continue otherwise.
		  rval = !have_broken;
		  exit = true;
		}
	    }
	  else
	    // The internal resolver is disabled, fall back to manual
	    // resolution.
	    have_broken = true;
	}
      else if(first && assume_yes)
	exit=true;

      // Re-enable the resolver, if it was disabled for some reason
      // already (e.g., if the dependencies have been fixed manually).
      if(!have_broken)
	use_internal_resolver = true;

      if(!exit)
	{
	  bool valid_response=false;

	  if(have_broken)
	    {
	      if(first)
		{
		  const std::string msg = _("aptitude failed to find a solution to these dependencies.  You can solve them yourself by hand or type 'n' to quit.");
                  const unsigned int screen_width = term_metrics->get_screen_width();
		  cw::fragment *f = cw::text_fragment(msg);
		  cout << f->layout(screen_width,
				    screen_width,
				    cwidget::style());
		  delete f;

		  show_broken();
		}
	    }

	  while(!valid_response)
	    {
	      valid_response=true;
	      fflush(stdout);

	      string prompt =
		!have_broken
		    ? _("Do you want to continue? [Y/n/?] ")
		    : _("Resolve these dependencies by hand? [N/+/-/_/:/?] ");

	      string response=prompt_string(prompt);
	      string::size_type loc=0;

	      while(loc<response.size() && isspace(response[loc]))
		++loc;

	      // If the user just pushes Enter, default to accepting
	      // the current state if there aren't broken packages; if
	      // there are broken packages, abort.
	      if(loc==response.size())
		{
		  response = !have_broken ? 'y' : 'n';
		  loc=0;
		}

	      const std::string unknown_key_message =
		_("Invalid response.  Please enter a valid command or '?' for help.\n");
	      switch(toupper(response[loc]))
		{
		case 'Y':
		  if(have_broken)
		    {
		      cw::fragment *f = flowbox(cw::text_fragment(_("Enter a package management command (such as '+ package' to install a package), 'R' to attempt automatic dependency resolution or 'N' to abort.")));
                      const unsigned int screen_width = term_metrics->get_screen_width();
		      cout << f->layout(screen_width,
					screen_width,
					cwidget::style());
		      delete f;

		      valid_response = false;
		    }
		  else
		    {
		      rval=true;
		      exit=true;
		    }
		  break;
		case 'N':
		  rval=false;
		  exit=true;
		  break;
		case 'R':
		  if(!have_broken)
		    {
		      // Pretend we don't understand.
		      printf("%s", unknown_key_message.c_str());
		      valid_response=false;
		    }
		  else
		    use_internal_resolver = true;
		  break;
		case 'D':
		  showdeps=!showdeps;
		  if(showdeps)
		    printf(_("\nDependency information will be shown.\n\n"));
		  else
		    printf(_("\nDependency information will not be shown.\n\n"));
		  break;
		case 'V':
		  showvers=!showvers;

		  if(showvers)
		    printf(_("\nVersions will be shown.\n\n"));
		  else
		    printf(_("\nVersions will not be shown.\n\n"));
		  break;
		case 'S':
		  showsize=!showsize;
		  if(showsize)
		    printf(_("\nSize changes will be shown.\n\n"));
		  else
		    printf(_("\nSize changes will not be shown.\n\n"));
		  break;
		case 'I':
		  cmdline_parse_show(response, verbose, term_metrics);
		  break;
		case 'C':
		  cmdline_parse_changelog(response, term_metrics);
		  break;
		case 'W': // should be 'Y' but that's for "yes"
		  cmdline_parse_why(response, term_metrics);
		  break;
		case '+':
		case '-':
		case '=':
		case '_':
		case ':':
		case '&':
		  {
		    // Don't play 'which packages have we seen?' games
		    // now. (should I do two passes like at the
		    // command-line?)
		    std::set<pkgCache::PkgIterator> seen_virtual_packages;
		    cmdline_parse_action(response, seen_virtual_packages,
					 to_install, to_hold,
					 to_remove, to_purge, verbose,
					 policy, arch_only, true,
                                         term_metrics);
		  }
		  break;
		case 'E':
		  ui_preview();
		case '?':
		  valid_response=false;
		  prompt_help(cout, have_broken, term_metrics);
		  break;
		default:
		  printf("%s", unknown_key_message.c_str());
		  valid_response=false;
		  break;
		}
	    }
	}

      // Note: only show the prompt if we're planning to continue.
      if(rval && (!prompt_essential(term_metrics) || !prompt_trust(term_metrics)))
	{
	  rval=false;
	  exit=true;
	}

      first=false;
    }

  return rval;
}
