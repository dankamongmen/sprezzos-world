// reason_fragment.cc
//
//  Copyright 2004 Daniel Burrows

#include "reason_fragment.h"

#include "aptitude.h"
#include "pkg_item.h"
#include "pkg_ver_item.h"
#include "ui.h"

#include <generic/apt/apt.h>
#include <generic/apt/infer_reason.h>

#include <cwidget/config/colors.h>
#include <cwidget/fragment.h>

#include <set>

using namespace std;

namespace cw = cwidget;

/** Returns a cw::fragment describing a dependency (as in "depends
 *  on" or "suggests" rather than "Depends" or "Suggests")
 */
cw::fragment *depname_frag(pkgCache::DepIterator dep)
{
  switch(dep->Type)
    {
    case pkgCache::Dep::Depends: return cw::text_fragment(_("depends on"),
							  cw::style_attrs_on(A_BOLD));
    case pkgCache::Dep::PreDepends: return cw::text_fragment(_("pre-depends on"),
							     cw::style_attrs_on(A_BOLD));
    case pkgCache::Dep::Suggests: return cw::text_fragment(_("suggests"));
    case pkgCache::Dep::Recommends: return cw::text_fragment(_("recommends"),
							     cw::style_attrs_on(A_BOLD));
    case pkgCache::Dep::Conflicts: return cw::text_fragment(_("conflicts with"),
							    cw::style_attrs_on(A_BOLD));
    case pkgCache::Dep::DpkgBreaks: return cw::text_fragment(_("breaks"),
							     cw::style_attrs_on(A_BOLD));
    case pkgCache::Dep::Replaces: return cw::text_fragment(_("replaces"));
    case pkgCache::Dep::Obsoletes: return cw::text_fragment(_("obsoletes"));
    case pkgCache::Dep::Enhances: return cw::text_fragment(_("enhances"));
    }

  // Untranslated (internal error that will only happen if things go
  // entirely wonky, and I want to be able to understand it if it
  // appears)
  return cw::text_fragment("has an invalid dependency type!", cw::get_style("Error"));
}

/** Generate a cw::fragment describing the packages providing a given package.
 *
 *  \param dep the dependency that the provides are related to;
 *             will be used to throw away provides that don't
 *             provide the right version of the package.
 *  \param pkg the package whose providers will be described.
 *  \param ignpkg a package which should not be shown as a provider.
 *
 *  \param installed a filter: if \b true, only providees which are
 *  installed, or which will be installed, are shown; otherwise, all
 *  providees are shown.
 */
cw::fragment *prvfrag(pkgCache::DepIterator dep,
		      pkgCache::PkgIterator pkg,
		      pkgCache::PkgIterator ignpkg,
		      bool installed)
{
  // All packages providing the given package name are listed.
  //
  // For each package, we check whether both its current and
  // candidate versions (if any) provide the name; if so, the
  // package name is listed, colorized for the package.  If only
  // one provides the name, its version is listed, colorized for
  // the version.

  vector<cw::fragment*> fragments;

  set<pkgCache::VerIterator, ver_ptr_lt> providing_versions;
  set<pkgCache::PkgIterator, pkg_name_lt> providing_packages;

  pkgCache::VerIterator candver=(*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);	

  for(pkgCache::PrvIterator P=pkg.ProvidesList();
      !P.end(); ++P)
    {
      if(P.OwnerPkg() != ignpkg &&
	 _system->VS->CheckDep(P.ProvideVersion(),
			       dep->CompareOp,
			       dep.TargetVer()))
	{
	  providing_versions.insert(P.OwnerVer());
	  providing_packages.insert(P.OwnerPkg());
	}
    }

  for(set<pkgCache::PkgIterator>::const_iterator P=providing_packages.begin();
      P!=providing_packages.end(); ++P)
    {
      bool provided_curr=false, provided_cand=false;
      pkgCache::VerIterator currver=P->CurrentVer();
      pkgCache::VerIterator instver=(*apt_cache_file)[*P].InstVerIter(*apt_cache_file);	

      if(installed)
	{
	  if(instver.end() && currver.end())
	    continue;
	}
#if 0
      else
	{
	  if(!(!currver.end() && instver.end()))
	    continue;
	}
#endif

      if(!currver.end() &&
	 providing_versions.find(currver)!=providing_versions.end())
	provided_curr=true;
      if(!candver.end() &&
	 providing_versions.find(candver)!=providing_versions.end())
	provided_cand=true;

      if((currver.end() || provided_cand) &&
	 (currver.end() || provided_curr) &&
	 (provided_cand || provided_curr))
	fragments.push_back(cw::text_fragment(P->FullName(true),
					      pkg_item::pkg_style(*P, false)));
      else if(provided_cand || provided_curr)
	{
	  pkgCache::VerIterator &pv=provided_cand?candver:currver;

	  fragments.push_back(cw::style_fragment(cw::fragf("%s %s",
							   P->FullName(true).c_str(),
							   pv.VerStr()),
						 pkg_ver_item::ver_style(pv, false)));
	}
      else
	// Bail and print EVERYTHING IN SIGHT...not very efficiently, either.
	{
	  for(set<pkgCache::VerIterator>::const_iterator i=providing_versions.begin();
	      i!=providing_versions.end(); ++i)
	    if(i->ParentPkg()==*P)
	      fragments.push_back(cw::style_fragment(cw::fragf("%s %s",
							       P->FullName(true).c_str(),
							       i->VerStr()),
						     pkg_ver_item::ver_style(*i, false)));
	}
    }

  if(fragments.size()==0)
    return cw::fragf("");
  else
    return cw::fragf(_(" (provided by %F)"),
		     cw::join_fragments(fragments, L", "));
}

/** Generate a cw::fragment describing the given dependency iterator. */
cw::fragment *dep_singlefrag(pkgCache::PkgIterator pkg,
			     pkgCache::DepIterator dep)
{
  cw::fragment *verfrag;

  pkgCache::VerIterator instver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

  if(!dep.TargetVer())
    verfrag=cw::text_fragment("");
  else
    {
      // Figure out the state of the versioned dep.
      //
      // Display it as "uninstalled" if it is not satisfied and won't
      // be; "installed" if it is satisfied and will be; "removed" if
      // it is satisfied but won't be, and "installing" if it is not
      // satisfied and will be.

      cw::style verstyle;

      bool matches_now=!pkg.CurrentVer().end() &&
	_system->VS->CheckDep(pkg.CurrentVer().VerStr(),
			      dep->CompareOp,
			      dep.TargetVer());
      bool matches_inst=!instver.end() &&
	_system->VS->CheckDep(instver.VerStr(),
			      dep->CompareOp,
			      dep.TargetVer());

      if(matches_now)
	{
	  if(matches_inst)
	    verstyle=cw::style_attrs_on(A_BOLD);
	  else
	    verstyle=cw::get_style("PkgToRemove");
	}
      else
	{
	  if(matches_inst)
	    verstyle=cw::get_style("PkgToInstall");
	}

      verfrag=cw::fragf(" (%s %F)",
			dep.CompType(),
			cw::text_fragment(dep.TargetVer(), verstyle));
    }

  // Display a note if the package that is depended upon is not in
  // main and is not the package being displayed.
  string sec=dep.TargetPkg().Section()?dep.TargetPkg().Section():"";
  if(sec.find('/')==sec.npos || dep.TargetPkg()==pkg)
    sec="";
  else
    sec=string(sec, 0, sec.find('/'));

  bool available=false;

  for(pkgCache::VerIterator i=dep.TargetPkg().VersionList(); !i.end(); i++)
    if(_system->VS->CheckDep(i.VerStr(), dep->CompareOp, dep.TargetVer()))
      available=true;

  for(pkgCache::PrvIterator i=dep.TargetPkg().ProvidesList(); !i.end(); i++)
    if(_system->VS->CheckDep(i.ProvideVersion(), dep->CompareOp, dep.TargetVer()))
      available=true;

  return cw::fragf("%F%s%F%F%s",
		   cw::text_fragment(dep.TargetPkg().FullName(true),
				     pkg_item::pkg_style(dep.TargetPkg(), false)),
		   sec.empty() || sec=="main"?"":(" ["+sec+']').c_str(),
		   verfrag,
		   prvfrag(dep,
			   dep.TargetPkg(),
			   dep.ParentPkg(),
			   is_conflict(dep->Type)),
		   available?"":(string(" [")+_("UNAVAILABLE")+"]").c_str());
}

/** Generate a cw::fragment describing the OR group that contains the
 *  given dependency, assuming that we are examining pkg.  Assumes
 *  that duplicate OR dependencies are already dealt with in some way.
 */
cw::fragment *dep_or_frag(pkgCache::PkgIterator pkg,
			  pkgCache::DepIterator dep)
{
  vector<cw::fragment*> fragments;

  pkgCache::DepIterator or_begin, or_end;

  surrounding_or(dep, or_begin, or_end);

  for(pkgCache::DepIterator D=or_begin; D!=or_end; ++D)
    if(D->CompareOp&pkgCache::Dep::Or)
      fragments.push_back(cw::fragf("%F | ",
				    dep_singlefrag(pkg, D)));
    else
      fragments.push_back(dep_singlefrag(pkg, D));

  // Display a note if the source of the dependency is not in main and
  // is not the package being displayed.
  string sec=dep.ParentVer().Section()?dep.ParentVer().Section():"";
  if(sec.find('/')==sec.npos || dep.ParentPkg()==pkg)
    sec="";
  else
    sec=string(sec, 0, sec.find('/'));

  return cw::fragf(_("%F%s %F %F"),
		   cw::text_fragment(dep.ParentPkg().FullName(true),
				     pkg_item::pkg_style(dep.ParentPkg(), false)),
		   sec.empty() || sec=="main"?"":(" ["+sec+']').c_str(),
		   depname_frag(dep),
		   cw::sequence_fragment(fragments));
}

typedef pair<pkgCache::DepIterator, pkgCache::DepIterator> deppair;

/** Return a cw::fragment describing the reasons in the given vector. */
cw::fragment *reasonsfrag(pkgCache::PkgIterator pkg, set<reason> &reasons)
{
  vector<cw::fragment*> fragments;

  // Used to exclude dependencies from the same OR that show up twice.
  // If this is too expensive, switch to a set.
  vector<deppair> seen_ors;

  for(set<reason>::const_iterator i=reasons.begin(); i!=reasons.end(); ++i)
    {
      pkgCache::DepIterator depbegin, depend;
      surrounding_or(i->dep, depbegin, depend);

      bool seen=false;
      for(vector<deppair>::const_iterator j=seen_ors.begin();
	  j!=seen_ors.end(); ++j)
	if(j->first==depbegin && j->second==depend)
	  seen=true;

      if(!seen)
	{
	  seen_ors.push_back(deppair(depbegin, depend));

	  cw::fragment *itemtext=dep_or_frag(pkg, i->dep);

	  fragments.push_back(cw::sequence_fragment(cw::text_fragment("  * ",
								      cw::get_style("Bullet")),
						    indentbox(0, 4, flowbox(itemtext)),
						    NULL));
	}
    }

  return cw::sequence_fragment(fragments);
}

/** Return a cw::fragment describing the lack of a package. */
cw::fragment *nopackage()
{
  return wrapbox(cw::text_fragment(_("If you select a package, an explanation of its current state will appear in this space.")));
}

cw::fragment *reason_fragment(const pkgCache::PkgIterator &pkg)
{
  bool dummy;

  return reason_fragment(pkg, dummy);
}

cw::fragment *reason_fragment(const pkgCache::PkgIterator &pkg, bool &breakage)
{
  breakage=false;

  if(pkg.end())
    return nopackage();

  set<reason> reasons;

  infer_reason(pkg, reasons);

  vector<cw::fragment *> fragments;
  pkg_action_state actionstate = find_pkg_state(pkg, *apt_cache_file);

  aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
  aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
  pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
  pkgCache::VerIterator instver=state.InstVerIter(*apt_cache_file);	

  // TODO: get non-lame text...some of these are just placeholders and
  // should be reworded before release.
  switch(actionstate)
    {
    case pkg_unused_remove:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b was installed automatically;  it is being removed because all of the packages which depend upon it are being removed:"),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_auto_remove:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be automatically removed because of dependency errors:"),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_auto_install:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be automatically installed to satisfy the following dependencies:"),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_auto_hold:
      {
	if(candver.end() || candver==pkg.CurrentVer())
	  fragments.push_back(wrapbox(cw::fragf(_("%B%s%b cannot be upgraded now, but if it could be, it would still be held at version %B%s%b."),
						pkg.FullName(true).c_str(), pkg.CurrentVer().VerStr())));
	else
	  fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will not be upgraded to version %B%s%b, to avoid breaking the following dependencies:"),
						pkg.FullName(true).c_str(),
						candver.VerStr())));
	break;
      }
    case pkg_unchanged:
      if(!pkg.CurrentVer().end())
	{
	  if((*apt_cache_file)->is_held(pkg))
	    fragments.push_back(wrapbox(cw::fragf(_("%B%s%b cannot be upgraded now, but if it could be, it would still be held at version %B%s%b."),
						  pkg.FullName(true).c_str(), pkg.CurrentVer().VerStr())));
	  else
	    fragments.push_back(wrapbox(cw::fragf(_("%B%s%b is currently installed."),
						  pkg.FullName(true).c_str())));
	  break;
	}
      else
	{
	  fragments.push_back(wrapbox(cw::fragf(_("%B%s%b is not currently installed."),
						pkg.FullName(true).c_str())));

	  break;
	}
    case pkg_broken:
      breakage=true;

      fragments.push_back(wrapbox(cw::fragf(_("Some dependencies of %B%s%b are not satisfied:"),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_downgrade:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be downgraded."),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_hold:
      {
	if(estate.selection_state != pkgCache::State::Hold &&
	   !candver.end() && candver.VerStr() == estate.forbidver)
	  fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will not be upgraded to the forbidden version %B%s%b."),
						pkg.FullName(true).c_str(),
						candver.VerStr())));
	else
	  fragments.push_back(wrapbox(cw::fragf(_("%B%s%b could be upgraded to version %B%s%b, but it is being held at version %B%s%b."),
						pkg.FullName(true).c_str(),
						candver.VerStr(),
						pkg.CurrentVer().VerStr())));
      }
      break;
    case pkg_reinstall:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be re-installed."),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_install:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be installed."),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_remove:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be removed."),
					    pkg.FullName(true).c_str())));
      break;
    case pkg_upgrade:
      {
	fragments.push_back(wrapbox(cw::fragf(_("%B%s%b will be upgraded from version %B%s%b to version %B%s%b."),
					      pkg.FullName(true).c_str(),
					      pkg.CurrentVer().VerStr(),
					      candver.VerStr(), A_BOLD)));
      }
      break;
    case pkg_unconfigured:
      fragments.push_back(wrapbox(cw::fragf(_("%B%s%b is only partly installed; its installation will be completed."), pkg.FullName(true).c_str())));
      break;
    default:
      // Another non-translatable internal error.
      fragments.push_back(wrapbox(cw::fragf("Internal error: Unknown package state for %s!",
					    pkg.FullName(true).c_str())));
    }


  if(!reasons.empty())
    fragments.push_back(cw::sequence_fragment(cw::newline_fragment(),
					      cw::newline_fragment(),
					      NULL));

  fragments.push_back(reasonsfrag(pkg, reasons));

  reasons.clear();

  infer_reverse_breakage(pkg, reasons);

  if(!reasons.empty())
    {
      breakage=true;

      fragments.push_back(cw::sequence_fragment(cw::newline_fragment(),
						cw::newline_fragment(),
						NULL));

      // It will end up un-installed.
      if(instver.end())
	{
	  if(state.Delete())
	    fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on %B%s%b and will be broken by its removal:"),
						  pkg.FullName(true).c_str())));
	  else
	    fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on %B%s%b and are broken:"),
						  pkg.FullName(true).c_str())));
	}
      // It will end up installed.
      else
	{
	  if(pkg.CurrentVer().end())
	    fragments.push_back(wrapbox(cw::fragf(_("The following packages conflict with %B%s%b and will be broken by its installation:"),
						  pkg.FullName(true).c_str())));
	  else
	    // up/downgrade; could be either Depends or Conflicts/Breaks
	    {
	      bool has_depends=false;
	      bool has_conflicts=false;
	      bool now_broken=false;
	      bool inst_broken=false;

	      for(set<reason>::const_iterator i=reasons.begin();
		  i!=reasons.end(); ++i)
		{
		  if(is_conflict(i->dep->Type))
		    has_conflicts=true;
		  else
		    has_depends=true;

		  // um, these are not entirely accurate :-(
		  //
		  // They show whether the whole dependency is ok, not
		  // whether this particular part is ok. (think ORed deps)
		  if(!((*apt_cache_file)[i->dep]&pkgDepCache::DepGNow))
		    now_broken=true;
		  if(!((*apt_cache_file)[i->dep]&pkgDepCache::DepGInstall))
		    inst_broken=true;
		}

	      if(state.Keep() || (now_broken && inst_broken))
		{
		  if(has_conflicts && has_depends)
		    {
		      if(state.Keep())
			fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on a version of %B%s%b other than the currently installed version of %B%s%b, or conflict with the currently installed version:"),
							      pkg.FullName(true).c_str(),
							      pkg.CurrentVer().VerStr())));
		      else
			fragments.push_back(wrapbox(cw::fragf(_("The following packages conflict with %B%s%b, or depend on a version of it which is not going to be installed."),
							      pkg.FullName(true).c_str())));
		    }
		  else if(has_conflicts)
		    fragments.push_back(wrapbox(cw::fragf(_("The following packages conflict with %B%s%b:"),
							  pkg.FullName(true).c_str())));
		  else if(has_depends)
		    {
		      if(state.Keep())
			fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on a version of %B%s%b other than the currently installed version of %B%s%b:"),
							      pkg.FullName(true).c_str(),
							      pkg.CurrentVer().VerStr())));
		      else
			fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on a version of %B%s%b which is not going to be installed."),
							      pkg.FullName(true).c_str())));
		    }
		}
	      else
		{
		  const char *actionname=(actionstate==pkg_upgrade)?_("upgraded"):_("downgraded");

		  if(has_conflicts && has_depends)
		    // I hope this is ok for the translators :-/ --
		    // factoring out upgraded/downgraded in its two senses
		    // would be a royal pain even if gettext supported it.
		    fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on the currently installed version of %B%s%b (%B%s%b), or conflict with the version it will be %s to (%B%s%b), and will be broken if it is %s."),
							  pkg.FullName(true).c_str(),
							  pkg.CurrentVer().VerStr(),
							  actionname,
							  instver.VerStr(),
							  actionname)));
		  else if(has_conflicts)
		    fragments.push_back(wrapbox(cw::fragf(_("The following packages conflict with version %B%s%b of %B%s%b, and will be broken if it is %s."),
							  instver.VerStr(),
							  pkg.FullName(true).c_str(),
							  actionname)));
		  else if(has_depends)
		    fragments.push_back(wrapbox(cw::fragf(_("The following packages depend on version %B%s%b of %B%s%b, and will be broken if it is %s."),
							  pkg.CurrentVer().VerStr(),
							  pkg.FullName(true).c_str(),
							  actionname)));
		}
	    }
	}

      fragments.push_back(cw::sequence_fragment(cw::newline_fragment(),
						cw::newline_fragment(),
						NULL));

      fragments.push_back(reasonsfrag(pkg, reasons));
    }

  return cw::sequence_fragment(fragments);
}
