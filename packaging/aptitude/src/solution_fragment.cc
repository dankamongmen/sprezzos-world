// solution_fragment.cc
//
//
//   Copyright (C) 2005, 2007, 2009-2010 Daniel Burrows
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

// g++ generates a spurious warning in a call to std::sort below;
// downgrade it to a non-error.
#pragma GCC diagnostic warning "-Wuninitialized"

#include "solution_fragment.h"

#include <aptitude.h>

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/solution.h>

#include <generic/util/util.h>

#include <boost/lexical_cast.hpp>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>

#include <algorithm>
#include <functional>
#include <vector>

typedef generic_solution<aptitude_universe> aptitude_solution;
typedef generic_choice<aptitude_universe> choice;
typedef generic_choice_set<aptitude_universe> choice_set;

using namespace std;
namespace cw = cwidget;

string archives_text(const pkgCache::VerIterator &ver,
                     bool suppress_now,
                     const std::string &inter_archive_string)
{
  string rval;

  bool is_first = true;

  // Uniquify and sort the archives.
  std::set<std::string> archives;

  for(pkgCache::VerFileIterator vf=ver.FileList(); !vf.end(); ++vf)
    {
      if(vf.File().Archive())
        archives.insert(vf.File().Archive());
      else
        archives.insert(_("<NULL>"));
    }

  if(suppress_now)
    archives.erase("now");

  for(std::set<std::string>::const_iterator it = archives.begin();
      it != archives.end(); ++it)
    {
      if(is_first)
	is_first = false;
      else
	rval += inter_archive_string;

      rval += *it;
    }

  return rval;
}

string dep_targets(const pkgCache::DepIterator &start)
{
  string rval;

  bool is_first = true;

  eassert(!start.end());

  for(pkgCache::DepIterator d = start; !d.end(); ++d)
    {
      if(is_first)
	is_first = false;
      else
	rval += " | ";

      rval += d.TargetPkg().FullName(true).c_str();

      if(d.TargetVer())
	{
	  rval += " (";
	  rval += d.CompType();
	  rval += " ";
	  rval += d.TargetVer();
	  rval += ")";
	}

      if((d->CompareOp & pkgCache::Dep::Or) == 0)
	break;
    }

  return rval;
}

wstring dep_text(const pkgCache::DepIterator &d)
{
  const string fullname = const_cast<pkgCache::DepIterator &>(d).ParentPkg().FullName(true);
  const char *name = fullname.c_str();

  string targets = dep_targets(d);

  switch(d->Type)
    {
    case pkgCache::Dep::Depends:
      return swsprintf(W_("%s depends upon %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::PreDepends:
      return swsprintf(W_("%s pre-depends upon %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Suggests:
      return swsprintf(W_("%s suggests %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Recommends:
      return swsprintf(W_("%s recommends %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Conflicts:
      return swsprintf(W_("%s conflicts with %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::DpkgBreaks:
      return swsprintf(W_("%s breaks %s").c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Replaces:
      return swsprintf(W_("%s replaces %s").c_str(),
				 name, targets.c_str());
    case pkgCache::Dep::Obsoletes:
      return swsprintf(W_("%s obsoletes %s").c_str(),
				 name, targets.c_str());
    case pkgCache::Dep::Enhances:
      return swsprintf(W_("%s enhances %s").c_str(),
				 name, targets.c_str());
    default:
      abort();
    }
}

wstring conflict_text(const pkgCache::DepIterator &conflict,
		      const pkgCache::PrvIterator &prv)
{
  if(prv.end() || !is_conflict(conflict->Type))
    return dep_text(conflict);

  return swsprintf(W_("%s conflicts with %s [provided by %s %s]").c_str(),
		   const_cast<pkgCache::DepIterator &>(conflict).ParentPkg().FullName(true).c_str(),
		   const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().FullName(true).c_str(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().FullName(true).c_str(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerVer().VerStr());
}

cw::fragment *choice_fragment(const choice &c)
{
  switch(c.get_type())
    {
    case choice::install_version:
      if(c.get_ver().get_ver().end())
	return cw::fragf(_("Removing %s"), c.get_ver().get_pkg().FullName(true).c_str());
      else
	return cw::fragf(_("Installing %s %s (%s)"),
			 c.get_ver().get_pkg().FullName(true).c_str(),
                         c.get_ver().get_ver().VerStr(),
			 archives_text(c.get_ver().get_ver()).c_str());

    case choice::break_soft_dep:
      return cw::fragf(_("Leave %ls unresolved."), dep_text(c.get_dep().get_dep()).c_str());

    default:
      return cw::fragf("Unhandled choice type %d.", c.get_type());
    }
}

cw::fragment *choice_state_fragment(const choice &c)
{
  std::string flag;

  switch(c.get_type())
    {
    case choice::install_version:
      if(resman->is_rejected(c.get_ver()))
	flag = "R";
      else if(resman->is_mandatory(c.get_ver()))
	flag = "A";
      break;

    case choice::break_soft_dep:
      if(resman->is_hardened(c.get_dep()))
	flag = "R";
      else if(resman->is_approved_broken(c.get_dep()))
	flag = "A";
      break;
    }

  if(flag.empty())
    return NULL;
  else
    return cw::text_fragment(flag);
}

namespace
{
  // Used to centralize the logic to generate an ID map and the
  // columns for the ID and state flags if requested (otherwise it
  // does nothing).
  class ids_column_generator
  {
    int next_id;
    std::vector<cw::fragment *> id_fragments, flag_fragments;
    std::map<std::string, choice> *ids_out;

    void append_flag(const choice &c)
    {
      flag_fragments.push_back(choice_state_fragment(c));
    }

  public:
    ids_column_generator(int first_id, std::map<std::string, choice> *_ids_out)
      : next_id(first_id),
	ids_out(_ids_out)
    {
    }

    ~ids_column_generator()
    {
      for(std::vector<cw::fragment *>::const_iterator it =
	    id_fragments.begin();
	  it != id_fragments.end(); ++it)
	delete *it;

      for(std::vector<cw::fragment *>::const_iterator it =
	    flag_fragments.begin();
	  it != flag_fragments.end(); ++it)
	delete *it;
    }

    /** \brief If ID generation is enabled, create an ID for the given
     *         choice, adding it to both the output map and the in-order
     *         list of IDs.
     */
    void append_id(const choice &c)
    {
      if(ids_out != NULL)
	{
	  std::string key = boost::lexical_cast<std::string>(next_id);
	  id_fragments.push_back(cw::fragf("%s)", key.c_str()));
	  (*ids_out)[key] = c;

	  append_flag(c);

	  ++next_id;
	}
    }

    /** \brief Append a newline to the IDs column, normally to match a
     *         newline in the matching fragments column.
     */
    void append_newline()
    {
      if(ids_out != NULL)
	{
	  id_fragments.push_back(cw::newline_fragment());
	  flag_fragments.push_back(cw::newline_fragment());
	}
    }

    /** \brief Append a fragment for each row in the ID listing
     *  column to the given vector.
     *
     *  After this function completes, its caller is responsible for
     *  deleting the fragments.
     */
    void release_ids_column(std::vector<cw::fragment *> &fragments)
    {
      for(std::vector<cw::fragment *>::const_iterator it = id_fragments.begin();
	  it != id_fragments.end(); ++it)
	fragments.push_back(*it);

      id_fragments.clear();
    }

    void release_flags_column(std::vector<cw::fragment *> &fragments)
    {
      for(std::vector<cw::fragment *>::const_iterator it = flag_fragments.begin();
	  it != flag_fragments.end(); ++it)
	fragments.push_back(*it);

      flag_fragments.clear();
    }
  };

  class pkg_name_pair_lt
  {
    pkg_name_lt base;
  public:
    bool operator()(const std::pair<pkgCache::PkgIterator, choice> &p1,
		    const std::pair<pkgCache::PkgIterator, choice> &p2) const
    {
      return base(p1.first, p2.first);
    }
  };

  class ver_name_pair_lt
  {
    ver_name_lt base;
  public:
    bool operator()(const std::pair<pkgCache::VerIterator, choice> &p1,
		    const std::pair<pkgCache::VerIterator, choice> &p2) const
    {
      return base(p1.first, p2.first);
    }
  };
}

cw::fragment *solution_fragment_with_ids(const aptitude_solution &sol,
					 std::map<std::string, choice> *ids)
{
  // Bin packages according to what will happen to them.
  vector<std::pair<pkgCache::PkgIterator, choice> > remove_packages;
  vector<std::pair<pkgCache::PkgIterator, choice> > keep_packages;
  vector<std::pair<pkgCache::VerIterator, choice> > install_packages;
  vector<std::pair<pkgCache::VerIterator, choice> > downgrade_packages;
  vector<std::pair<pkgCache::VerIterator, choice> > upgrade_packages;
  vector<std::pair<pkgCache::DepIterator, choice> > unresolved;

  for(choice_set::const_iterator i = sol.get_choices().begin();
      i != sol.get_choices().end(); ++i)
    {
      switch(i->get_type())
	{
	case choice::install_version:
	  {
	    pkgCache::PkgIterator pkg = i->get_ver().get_pkg();
	    pkgCache::VerIterator curver = pkg.CurrentVer();
	    pkgCache::VerIterator newver = i->get_ver().get_ver();

	    if(curver.end())
	      {
		if(newver.end())
		  keep_packages.push_back(std::make_pair(pkg, *i));
		else
		  install_packages.push_back(std::make_pair(newver, *i));
	      }
	    else if(newver.end())
	      remove_packages.push_back(std::make_pair(pkg, *i));
	    else if(newver == curver)
	      keep_packages.push_back(std::make_pair(pkg, *i));
	    else
	      {
		int cmp=_system->VS->CmpVersion(curver.VerStr(),
						newver.VerStr());

		// The versions shouldn't be equal -- otherwise
		// something is majorly wrong.
		// eassert(cmp!=0);
		//
		// The above is not true: consider, eg, the case of a
		// locally compiled package and a standard package.

		/** \todo indicate "sidegrades" separately? */
		if(cmp<=0)
		  upgrade_packages.push_back(std::make_pair(newver, *i));
		else if(cmp>0)
		  downgrade_packages.push_back(std::make_pair(newver, *i));
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  unresolved.push_back(std::make_pair(i->get_dep().get_dep(), *i));
	  break;
	}
    }

  sort(remove_packages.begin(), remove_packages.end(),
       pkg_name_pair_lt());
  sort(keep_packages.begin(), keep_packages.end(),
       pkg_name_pair_lt());
  sort(install_packages.begin(), install_packages.end(),
       ver_name_pair_lt());
  sort(downgrade_packages.begin(), downgrade_packages.end(),
       ver_name_pair_lt());
  sort(upgrade_packages.begin(), upgrade_packages.end(),
       ver_name_pair_lt());
  // \todo Sort the unresolved list in some readable fashion.

  vector<cw::fragment *> fragments;
  ids_column_generator ids_column(1, ids);

  if(!remove_packages.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf(_("%BRemove%b the following packages:%n")));
      for(std::vector<std::pair<pkgCache::PkgIterator, choice> >::const_iterator i=remove_packages.begin();
	  i!=remove_packages.end(); ++i)
	{
	  ids_column.append_id(i->second);
	  fragments.push_back(cw::fragf("  %s%n", i->first.FullName(true).c_str()));
	}

      ids_column.append_newline();
      fragments.push_back(cw::newline_fragment());
    }

  if(!install_packages.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf(_("%BInstall%b the following packages:%n")));
      for(std::vector<std::pair<pkgCache::VerIterator, choice> >::const_iterator i=install_packages.begin();
	  i!=install_packages.end(); ++i)
	{
	  ids_column.append_id(i->second);
	  fragments.push_back(cw::fragf("  %s [%s (%s)]%n",
					i->first.ParentPkg().FullName(true).c_str(),
					i->first.VerStr(),
					archives_text(i->first).c_str()));
	}

      ids_column.append_newline();
      fragments.push_back(cw::newline_fragment());
    }

  if(!keep_packages.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf(_("%BKeep%b the following packages at their current version:%n")));
      for(std::vector<std::pair<pkgCache::PkgIterator, choice> >::const_iterator i=keep_packages.begin();
	  i!=keep_packages.end(); ++i)
	{
	  ids_column.append_id(i->second);

	  if(i->first.CurrentVer().end())
	    fragments.push_back(cw::fragf("  %s [%s]%n",
					  i->first.FullName(true).c_str(),
					  _("Not Installed")));
	  else
	    fragments.push_back(cw::fragf("  %s [%s (%s)]%n",
					  i->first.FullName(true).c_str(),
					  i->first.CurrentVer().VerStr(),
					  archives_text(i->first.CurrentVer()).c_str()));
	}

      ids_column.append_newline();
      fragments.push_back(cw::newline_fragment());
    }

  if(!upgrade_packages.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf(_("%BUpgrade%b the following packages:%n")));
      for(std::vector<std::pair<pkgCache::VerIterator, choice> >::const_iterator i=upgrade_packages.begin();
	  i!=upgrade_packages.end(); ++i)
	{
	  ids_column.append_id(i->second);
	  fragments.push_back(cw::fragf("  %s [%s (%s) -> %s (%s)]%n",
					i->first.ParentPkg().FullName(true).c_str(),
					i->first.ParentPkg().CurrentVer().VerStr(),
					archives_text(i->first.ParentPkg().CurrentVer()).c_str(),
					i->first.VerStr(),
					archives_text(i->first).c_str()));
	}

      ids_column.append_newline();
      fragments.push_back(cw::newline_fragment());
    }

  if(!downgrade_packages.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf(_("%BDowngrade%b the following packages:%n")));
      for(std::vector<std::pair<pkgCache::VerIterator, choice> >::const_iterator i=downgrade_packages.begin();
	  i!=downgrade_packages.end(); ++i)
	{
	  ids_column.append_id(i->second);

	  fragments.push_back(cw::fragf("  %s [%s (%s) -> %s (%s)]%n",
					i->first.ParentPkg().FullName(true).c_str(),
					i->first.ParentPkg().CurrentVer().VerStr(),
					archives_text(i->first.ParentPkg().CurrentVer()).c_str(),
					i->first.VerStr(),
					archives_text(i->first).c_str()));
	}

      ids_column.append_newline();
      fragments.push_back(cw::newline_fragment());
    }

  if(!unresolved.empty())
    {
      ids_column.append_newline();
      fragments.push_back(cw::fragf("%s%n", _("Leave the following dependencies unresolved:")));

      for(std::vector<std::pair<pkgCache::DepIterator, choice> >::const_iterator i = unresolved.begin();
	  i != unresolved.end(); ++i)
	{
	  ids_column.append_id(i->second);
	  fragments.push_back(cw::fragf("  %ls%n", dep_text(i->first).c_str()));
	}
    }

  ids_column.append_newline();

  if(ids == NULL)
    return flowbox(cw::sequence_fragment(fragments));
  else
    {
      std::vector<cw::fragment_column_entry> columns;

      std::vector<cw::fragment *> ids_column_fragments, flags_column_fragments;
      ids_column.release_ids_column(ids_column_fragments);
      ids_column.release_flags_column(flags_column_fragments);

      columns.push_back(cw::fragment_column_entry(false, true,
						  1, cw::fragment_column_entry::top,
						  ids_column_fragments));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  NULL));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  flags_column_fragments));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  NULL));
      columns.push_back(cw::fragment_column_entry(false, true,
						  1, cw::fragment_column_entry::top,
						  fragments));

      return cw::fragment_columns(columns);
    }
}

cw::fragment *solution_fragment_with_ids(const aptitude_solution &sol,
					 std::map<std::string, choice> &ids)

{
  return solution_fragment_with_ids(sol, &ids);
}

cw::fragment *solution_fragment(const aptitude_solution &sol)
{
  return solution_fragment_with_ids(sol, NULL);
}
