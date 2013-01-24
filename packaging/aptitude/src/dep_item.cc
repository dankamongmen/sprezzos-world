// dep_item.cc
//
//  Copyright 1999-2005, 2008 Daniel Burrows
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
// Implementation of 'dependency items'.
//
//  For now we use the current version..

#include "aptitude.h"

#include "dep_item.h"
#include "pkg_sortpolicy.h"
#include "pkg_subtree.h"
#include "pkg_ver_item.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <apt-pkg/version.h>

using namespace std;
namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

using aptitude::apt::is_foreign_arch;

class pkg_depitem:public pkg_subtree
{
  pkgCache::DepIterator firstdep;

  // calculated at initialization: true iff at least one package
  // fulfilling the dependency is available (not necessarily installable)
  bool available;

  bool is_broken();
  // Returns true if the dependency should be displayed as a broken dependency
public:
  pkg_depitem(pkgCache::DepIterator &_first, pkg_signal *_sig);

  void paint(cw::tree *win, int y, bool hierarchical,
	     const cw::style &st);

  cw::style get_normal_style();

  void select(undo_group *undo);
  // A special case -- installs the 'best' target package (using
  // SmartTargetPkg() )
};

pkg_depitem::pkg_depitem(pkgCache::DepIterator &first, pkg_signal *sig):
  pkg_subtree(L""),firstdep(first),available(false)
{
  clear_num_packages();

  pkgCache::DepIterator start,end;
  first.GlobOr(start,end);

  string currlabel="";
  bool firstiter=true;
  bool is_or=(start->CompareOp&pkgCache::Dep::Or);

  do
    {
      if(!firstiter)
	currlabel+=" | ";

      firstiter=false;

      currlabel+=start.TargetPkg().Name();
      if(start.TargetVer())
	{
	  currlabel+=" (";
	  currlabel+=start.CompType();
	  currlabel+=" ";
	  currlabel+=start.TargetVer();
	  currlabel+=")";
	}

      for(pkgCache::VerIterator i=start.TargetPkg().VersionList(); !i.end(); i++)
	if(_system->VS->CheckDep(i.VerStr(), start->CompareOp, start.TargetVer()))
	  {
	    available=true;
            const bool show_pkg_name = is_or || is_foreign_arch(i);
            add_child(new pkg_ver_item(i, sig, show_pkg_name));
	    inc_num_packages();
	  }

      for(pkgCache::PrvIterator i=start.TargetPkg().ProvidesList(); !i.end(); i++)
	if(_system->VS->CheckDep(i.ProvideVersion(), start->CompareOp, start.TargetVer()))
	  {
	    available=true;
	    add_child(new pkg_ver_item(i.OwnerVer(), sig, true));
	    inc_num_packages();
	  }

      if(start==end)
	break;
      // Bleach.  Anyone who can offer a cleaner way to do this gets a virtual cookie :)
      start++;
    } while(1);
  pkg_subtree::set_label(cw::util::transcode(currlabel, "ASCII"));
}

bool pkg_depitem::is_broken()
{
  pkgCache::DepIterator dep=firstdep;

  // Ok, here's the story: we need to check the DepGInstall dependency flag
  // for the whole `or' group.  HOWEVER, this is only set for the LAST member
  // of an `or' group.  SO, we need to find the last item in the group in order
  // to do this.
  //
  // Alles klar? :)
  while(dep->CompareOp & pkgCache::Dep::Or)
    ++dep;

  // Showing "Replaces" dependencies as broken is weird.
  return ((firstdep.IsCritical() ||
	   firstdep->Type==pkgCache::Dep::Recommends ||
	   firstdep->Type==pkgCache::Dep::Suggests) &&
	  !((*apt_cache_file)[dep]&pkgDepCache::DepGInstall));
}

void pkg_depitem::paint(cw::tree *win, int y, bool hierarchical,
			const cw::style &st)
{
  if(is_broken())
    {
      // If we can quickly prove that this is not satisfiable, tell
      // the user that.  (specifically, if there are no available packages
      // fulfilling the dependency)

      wstring broken_str = available ? W_("UNSATISFIED") : W_("UNAVAILABLE");

      cw::subtree<pkg_tree_node>::paint(win, y, hierarchical,
					get_name()+L" ("+broken_str+L")");
    }
  else
    pkg_subtree::paint(win, y, hierarchical, st);
}

cw::style pkg_depitem::get_normal_style()
{
  if(is_broken())
    return cw::get_style("DepBroken");
  else
    return pkg_subtree::get_normal_style();
}

void pkg_depitem::select(undo_group *undo)
{
  if(firstdep->Type!=pkgCache::Dep::Conflicts &&
     firstdep->Type!=pkgCache::Dep::DpkgBreaks &&
     firstdep->Type!=pkgCache::Dep::Replaces)
    {
      bool last_or=true;
      for(pkgCache::DepIterator dep=firstdep;
	  !dep.end() && last_or;
	  dep++)
	{
	  last_or&=dep->CompareOp&pkgCache::Dep::Or;
	  if((*apt_cache_file)[dep]&pkgDepCache::DepInstall)
	    return;
	}
      pkgCache::PkgIterator pkg=firstdep.SmartTargetPkg();
      // (note: no OR handling, since the first one is the preferred choice..)
      if(!pkg.end())
	(*apt_cache_file)->mark_install(pkg, aptcfg->FindB(PACKAGE "::Auto-Install", true), false, undo);
    }
}

class pkg_grouppolicy_dep:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_dep(pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    pkg_item_with_generic_subtree *newtree=new pkg_item_with_generic_subtree(pkg,
									     get_sig());

    root->add_child(newtree);
    // The count for the containing tree is only increased by 1, not
    // by the number of dependencies of this tree.
    root->inc_num_packages();

    setup_package_deps<pkg_item_with_generic_subtree>(pkg,
						      pkg.CurrentVer(),
						      newtree,
						      get_sig());
  }

  virtual ~pkg_grouppolicy_dep() {}
};

pkg_grouppolicy *pkg_grouppolicy_dep_factory::instantiate(pkg_signal *_sig,
							  desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_dep(_sig, _desc_sig);
}

cw::treeitem *pkg_dep_screen::setup_new_root(const pkgCache::PkgIterator &pkg,
					     const pkgCache::VerIterator &ver)
{
  pkg_item_with_generic_subtree *newtree=new pkg_item_with_generic_subtree(pkg, get_sig(), true);

  if(!reverse)
    setup_package_deps<pkg_item_with_generic_subtree>(pkg, ver, newtree, get_sig(), false);
  else
    {
      setup_package_deps<pkg_item_with_generic_subtree>(pkg, ver, newtree, get_sig(), true);
    }

  return newtree;
}

pkg_dep_screen::pkg_dep_screen(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver,
			       bool _reverse)
  :apt_info_tree(pkg.FullName(true), ver.end()?"":ver.VerStr()), reverse(_reverse)
{
  set_root(setup_new_root(pkg, ver), true);
}

typedef std::map<string, pkg_subtree *> tree_map;

// Used to ensure that pkg_subtrees containing reverse deps
// get a proper count; reverse deps are also created using
// pkg_item_with_subtree objects, which can't accumulate counts.
template<typename parent_type>
struct set_num_packages_parent_if_pkg_subtree
{
  void operator()(pkg_subtree *child, parent_type *parent) const
  {
  }
};

template<>
struct set_num_packages_parent_if_pkg_subtree<pkg_subtree>
{
  void operator()(pkg_subtree *child, pkg_subtree *parent) const
  {
    child->set_num_packages_parent(parent);
  }
};

template<class tree_type>
void setup_package_deps(const pkgCache::PkgIterator &pkg,
			const pkgCache::VerIterator &ver,
			tree_type *tree,
			pkg_signal *sig,
			bool reverse)
{
  tree_map subtrees;

  if(ver.end() && !reverse)
    return;

  pkgCache::DepIterator D=reverse?pkg.RevDependsList():ver.DependsList();
  while(!D.end())
    {
      if(reverse)
	{
	  if((ver.end() && (D->CompareOp&0x0F)!=pkgCache::Dep::NoOp) ||
	     (!ver.end() &&
	      !_system->VS->CheckDep(ver.VerStr(), D->CompareOp, D.TargetVer())))

	    {
	      D++;
	      continue;
	    }
	}

      pkg_subtree *subtree;

      tree_map::iterator found=subtrees.find(D.DepType());
      if(found==subtrees.end())
	{
	  subtree = new pkg_subtree(cw::util::transcode(D.DepType()),
				    true);
	  subtrees[D.DepType()]=subtree;
	  tree->add_child(subtree);

	  // For reverse dependency items, the subtrees contain the
	  // package versions that depend on the selected package.  So
	  // it makes sense to collect their count at the top level.
	  if(reverse)
	    set_num_packages_parent_if_pkg_subtree<tree_type>()(subtree, tree);
	}
      else
	subtree=found->second;

      if(!reverse)
	{
	  subtree->add_child(new pkg_depitem(D, sig));
	  subtree->inc_num_packages();
	}
      else
	{
	  subtree->add_child(new pkg_ver_item(D.ParentVer(), sig, true));
	  subtree->inc_num_packages();
	  D++;
	}
    }

  if(reverse && !ver.end())
    // I'm starting to think the reverse-depends stuff should be split into
    // another function..this is getting too nasty
    for(pkgCache::PrvIterator i=ver.ProvidesList(); !i.end(); i++)
      {
	for(pkgCache::DepIterator D=i.ParentPkg().RevDependsList(); !D.end(); D++)
	  {
	    if(_system->VS->CheckDep(i.ProvideVersion(), D->CompareOp, D.TargetVer()))
	      {
		pkg_subtree *subtree;
		tree_map::iterator found=subtrees.find(D.DepType());
		if(found==subtrees.end())
		  {
		    subtree = new pkg_subtree(cw::util::transcode(D.DepType()),
					      true);
		    subtrees[D.DepType()]=subtree;
		    tree->add_child(subtree);
		  }
		else
		  subtree=found->second;

		subtree->add_child(new pkg_ver_item(D.ParentVer(), sig, true));
		subtree->inc_num_packages();
	      }
	  }
      }

  pkg_sortpolicy *policy = pkg_sortpolicy_name(pkg_sortpolicy_ver(NULL, false), false);
  pkg_sortpolicy_wrapper sorter(policy);
  for(tree_map::const_iterator i = subtrees.begin();
      i != subtrees.end(); ++i)
    i->second->sort(sorter);
  delete policy;
}

template void setup_package_deps<pkg_subtree>(const pkgCache::PkgIterator &pkg,
					      const pkgCache::VerIterator &ver,
					      pkg_subtree *tree,
					      pkg_signal *sig,
					      bool reverse);
