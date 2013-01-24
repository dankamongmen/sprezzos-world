// pkg_grouppolicy.cc
//
//  Copyright 1999-2005, 2007-2010 Daniel Burrows
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
//  Implementation of stuff that needs implementing.

#include "aptitude.h"

#include "pkg_grouppolicy.h"
#include "pkg_item.h"
#include "pkg_subtree.h"

#include <cwidget/generic/util/transcode.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/pkg_hier.h>
#include <generic/apt/tags.h>
#include <generic/apt/tasks.h>

#include <generic/util/util.h>

#include <apt-pkg/configuration.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <sigc++/functors/mem_fun.h>
#include <sigc++/trackable.h>

#include <map>
#include <set>

using namespace std;

namespace cw = cwidget;

namespace matching = aptitude::matching;
using cw::util::ref_ptr;

pkg_grouppolicy_factory::~pkg_grouppolicy_factory()
{
}

// This special tree munges its tag to allow an integer to be prepended to it.
// Ok, it's a dreadful hack.  I admit it.
class pkg_subtree_with_order:public pkg_subtree
{
  wstring my_tag;
public:
  pkg_subtree_with_order(wstring name, wstring description,
			 sigc::signal1<void, std::wstring> *_info_signal,
			 unsigned char order, bool expand=false)
    :pkg_subtree(name, description, _info_signal, expand)
  {
    my_tag+=order;
    my_tag+=pkg_subtree::tag();
  }

  pkg_subtree_with_order(wstring name, unsigned char order, bool expand=false)
    :pkg_subtree(name, expand)
  {
    my_tag+=order;
    my_tag+=pkg_subtree::tag();
  }

  virtual const wchar_t *tag()
  {
    return my_tag.c_str();
  }
};

// The following class is a special policy which is used to terminate a
// policy chain:
class pkg_grouppolicy_end:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_end(pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  virtual void add_package(const pkgCache::PkgIterator &i, pkg_subtree *root)
    {
      root->add_child(new pkg_item(i, get_sig()));
      root->inc_num_packages();
    }
};

pkg_grouppolicy *pkg_grouppolicy_end_factory::instantiate(pkg_signal *_sig,
							  desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_end(_sig, _desc_sig);
}

/*****************************************************************************/

// Groups packages so that the latest version of the package is shown, sorted
// by section.

class pkg_grouppolicy_section:public pkg_grouppolicy
{
  typedef std::map<string, pair<pkg_grouppolicy *, pkg_subtree *> > section_map;
  section_map sections;

  pkg_grouppolicy_factory *chain;

  // As in the factory
  pkg_grouppolicy_section_factory::split_mode_type split_mode;
  bool passthrough;

  // The descriptions are in the cw::style used by package descriptions.
  static std::map<string, wstring> section_descriptions;
  // The available top sections (i.e. main, contrib, non-free) in order.
  static std::vector<string> top_sections;
  static void init_section_data();

public:
  pkg_grouppolicy_section(pkg_grouppolicy_section_factory::split_mode_type _split_mode,
			  bool _passthrough,
			  pkg_grouppolicy_factory *_chain,
			  pkg_signal *_sig,
			  desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain),
     split_mode(_split_mode), passthrough(_passthrough)
  {
    init_section_data();
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  virtual ~pkg_grouppolicy_section()
    {
      for(section_map::iterator i=sections.begin(); i!=sections.end(); i++)
        delete i->second.first;
    }
};

pkg_grouppolicy *pkg_grouppolicy_section_factory::instantiate(pkg_signal *_sig,
							      desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_section(split_mode, passthrough, chain,
				     _sig, _desc_sig);
}

std::map<string, wstring> pkg_grouppolicy_section::section_descriptions;
std::vector<string> pkg_grouppolicy_section::top_sections;

namespace
{
  std::string unquote_section_description(const std::string &desc)
  {
    std::string rval;
    // Replace "\\n" with a newline and "''" with a double-quote.
    // The latter replacement is necessary because apt doesn't know
    // about backslash-escaped double quotes (Bug #260446).
    std::string::size_type pos = 0;

    while(pos != desc.size())
      {
	switch(desc[pos])
	  {
	  case '\'':
	    if(pos + 1 < desc.size() &&
	       desc[pos + 1] == '\'')
	      {
		rval += '"';
		pos += 2;
	      }
	    else
	      {
		rval += '\'';
		++pos;
	      }
	    break;
	  case '\\':
	    if(pos + 1 < desc.size())
	      {
		switch(desc[pos + 1])
		  {
		  case 'n':
		    rval += '\n';
		    break;
		  case 't':
		    rval += '\t';
		    break;
		  default:
		    rval += desc[pos + 1];
		    break;
		  }

		pos += 2;
	      }
	    else
	      {
		rval += '\\';
		++pos;
	      }
	    break;
	  default:
	    rval += desc[pos];
	    ++pos;
	    break;
	  }
      }
    return rval;
  }
}

void pkg_grouppolicy_section::init_section_data()
{
  static bool already_done=false;

  // TODO: this is wrong -- we need to reset already_done when
  // anything in the Sections hierarchy changes!
  if(already_done)
    return;

  // Read in the top-level section descriptions.
  const Configuration::Item * const Root = aptcfg->Tree(PACKAGE "::Sections::Descriptions");
  if(Root != NULL)
    {
      for(const Configuration::Item *Curr = Root->Child;
	  Curr != NULL; Curr = Curr->Next)
	{
	  const std::string desc = Curr->Value;
	  const std::string final_desc = unquote_section_description(desc);;

	  section_descriptions[Curr->Tag] = cw::util::transcode(final_desc, "UTF-8");
	}
    }

  // Read in the list of Top-Sections; do not use a cached value.
  top_sections = aptitude::apt::get_top_sections(false);

  already_done = true;
}

void pkg_grouppolicy_section::add_package(const pkgCache::PkgIterator &pkg,
					  pkg_subtree *root)
{
  // This flag tracks whether we're in a branch of the logic in which
  // the passthrough option is obeyed.  That basically means that the
  // package is virtual, has no section, or is a task package.
  bool may_passthrough = false;

  string section;
  if(pkg.VersionList().end())
    {
      section=_("virtual");
      may_passthrough = true;
    }
  else if(!pkg.VersionList().Section() ||
	  (*pkg.VersionList().Section()) == '\0')
    {
      section=_("Unknown");
      may_passthrough = true;
    }
  else if(strcmp(pkg.VersionList().Section(), "tasks") == 0)
    {
      section=_("Tasks");
      may_passthrough = true;
    }
  else
    {
      section=pkg.VersionList().Section();

      // Find the first section divider ('/'); if the split mode is
      // supposed to include only the part of the section preceding it
      // or only the part following it, modify the section
      // accordingly.
      string::size_type first_split = section.find('/');
      if(split_mode == pkg_grouppolicy_section_factory::split_topdir)
	section = (first_split != section.npos
		   ? section.substr(0,first_split)
		   : top_sections[0]);
      else if((split_mode == pkg_grouppolicy_section_factory::split_subdir ||
	       split_mode == pkg_grouppolicy_section_factory::split_subdirs) &&
	      first_split != section.npos)
	section = section.substr(first_split + 1);
    }

  // If passthrough is enabled and this package is in a section that
  // can pass through, place it directly in the top-level tree.
  if(passthrough && may_passthrough)
    {
      if(sections.find(section) == sections.end())
	sections[section].first = chain->instantiate(get_sig(), get_desc_sig());
      sections[section].first->add_package(pkg, root);
    }
  // Find the subtree in which this package is to be placed and add it
  // to that tree.
  else
    {
      pkg_subtree *current_root = root;
      // The fully qualified path of the tree in which the current
      // section is to be placed.  "" for the root tree, "games" for
      // "games" in the root tree, "non-free/games/arcade" for
      // "arcade" in the tree for "games" in the tree for "non-free"
      // in the root tree, etc.
      string section_id;
      // The portion of the section that hasn't been instantiated in
      // the tree yet.
      string sections_remaining = section;
      // Sentinel value, set to "true" when we've processed the whole
      // string.
      bool done = false;
      do
	{
	  if(split_mode == pkg_grouppolicy_section_factory::split_subdirs)
	    {
	      // If we're splitting into subdirectories, we need to
	      // strip one path element from the string and
	      // instantiate it.
	      string::size_type next_split = sections_remaining.find('/', 1);
	      section_id.append(sections_remaining.substr(0, next_split));
	      section = (sections_remaining.at(0) == '/'
			 ? sections_remaining.substr(1, next_split)
			 : sections_remaining.substr(0, next_split));
	      if(next_split == sections_remaining.npos)
		done = true;
	      else
		sections_remaining = sections_remaining.substr(next_split);
	    }
	  else
	    {
	      // In this case we've precomputed the full path above,
	      // and the section will be placed in the root node.
	      section_id = section;
	      done = true;
	    }

	  // If the subtree into which this package should be placed
	  // doesn't exist, create it.
	  section_map::iterator found = sections.find(section_id);
	  if(found == sections.end())
	    {
	      string section_tail = section;
	      pkg_subtree *newtree = 0;

	      // Look up the description of the section based on the
	      // last component of its name, and create a new subtree
	      // for it.
	      string::size_type last_split = section.rfind('/');
	      if(last_split != section.npos)
		section_tail = section.substr(last_split+1);

	      // mafm: bug#181997, added complexity to allow that top-sections
	      // appear "in natural order" (main, contrib, non-free) instead of
	      // sorted alphabetically (contrib, main, non-free)
	      bool use_order = false;
	      int order = -1;

	      // get the order of the top-section (the lower the number,
	      // the higher the priority)
	      for(size_t i = 0; i < top_sections.size(); ++i)
	      	{
	      	  if(section == top_sections[i])
	      	    {
	      	      order = i;
	      	      use_order = true;
	      	      break;
	      	    }
	      	}

	      // decide which [short] descriptions to use
	      wstring shortdesc;
	      wstring desc;
	      if(section_descriptions.find(section_tail) != section_descriptions.end())
		{
		  desc = section_descriptions[section_tail];
		  if(desc.find(L'\n') != desc.npos)
		    shortdesc = cw::util::transcode(section) + L" - " + wstring(desc, 0, desc.find('\n'));
		  else
		    {
		      shortdesc = cw::util::transcode(section) + desc;
		      desc = L"";
		    }
		}
	      else
	        {
		  shortdesc = cw::util::transcode(section);
		  desc = L"";
	        }

	      // do create tree with desired descriptions, ordered or not
	      if(use_order)
		newtree = new pkg_subtree_with_order(shortdesc,
						     desc,
						     get_desc_sig(),
						     order);
	      else
		newtree = new pkg_subtree(shortdesc,
					  desc,
					  get_desc_sig());

	      // Generate a new sub-grouping-policy, and insert it
	      // into the map with the new tree.
	      //
	      // Note that the new grouping policy is only used for
	      // packages that are in this section; packages in
	      // sub-sections go directly to the policy of the
	      // sub-section.
	      sections[section_id].first = chain->instantiate(get_sig(), get_desc_sig());
	      sections[section_id].second = newtree;

	      current_root->add_child(newtree);
	      newtree->set_num_packages_parent(current_root);
	    }
	  current_root=sections[section_id].second;
	} while(!done);

      sections[section_id].first->add_package(pkg, sections[section_id].second);
    }
}

/*****************************************************************************/

class pkg_grouppolicy_status:public pkg_grouppolicy
{
  static const int numgroups=7;
  pkg_grouppolicy_factory *chain;

  enum states {security_upgradable, upgradable, newpkg, installed, not_installed, obsolete_pkg, virtual_pkg};

  static const char * const state_titles[numgroups];
  // FIXME: need better titles :)

  pair<pkg_grouppolicy *, pkg_subtree *> children[numgroups];
public:
  pkg_grouppolicy_status(pkg_grouppolicy_factory *_chain,
			 pkg_signal *_sig,
			 desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
    {
      for(int i=0; i<( (int) (sizeof(children)/sizeof(children[0]))); i++)
	{
	  children[i].first=NULL;
	  children[i].second=NULL;
	}
    }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  virtual ~pkg_grouppolicy_status()
    {
      for(int i=0; i<numgroups; i++)
	delete children[i].first;
    }
};

const char * const pkg_grouppolicy_status::state_titles[numgroups] =
{
  N_("Security Updates\n Security updates for these packages are available from security.debian.org."),
  N_("Upgradable Packages\n A newer version of these packages is available."),
  N_("New Packages\n These packages have been added to Debian since the last time you cleared the list of \"new\" packages (choose \"Forget new packages\" from the Actions menu to empty this list)."),
  N_("Installed Packages\n These packages are currently installed on your computer."),
  N_("Not Installed Packages\n These packages are not installed on your computer."),
  N_("Obsolete and Locally Created Packages\n These packages are currently installed on your computer, but they are not available from any apt source.  They may be obsolete and removed from the archive, or you may have built a private version of them yourself."),
  N_("Virtual Packages\n These packages do not exist; they are names other packages use to require or provide some functionality.")
};

pkg_grouppolicy *pkg_grouppolicy_status_factory::instantiate(pkg_signal *_sig,
							     desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_status(chain, _sig, _desc_sig);
}

// Stolen from apt-watch:

/** Tests whether a particular version is security-related.
 *
 *  \return \b true iff the given package version comes from security.d.o
 */
static bool version_is_security(const pkgCache::VerIterator &ver)
{
  for(pkgCache::VerFileIterator F=ver.FileList(); !F.end(); ++F)
    if(string(F.File().Site())=="security.debian.org")
      return true;

  return false;
}

void pkg_grouppolicy_status::add_package(const pkgCache::PkgIterator &pkg,
					 pkg_subtree *root)
{
  states section;
  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

  if(pkg.VersionList().end())
    section=virtual_pkg;
  else
    {
      if(pkg_obsolete(pkg))
	section=obsolete_pkg;
      else if(state.Status != 2 && state.Upgradable())
	{
	  pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
	  if(version_is_security(candver))
	    section=security_upgradable;
	  else
	    section=upgradable;
	}
      else if((*apt_cache_file)->get_ext_state(pkg).new_package)
	section=newpkg;
      else if(state.Status==2)
	section=not_installed;
      else
	section=installed;
    }

  if(!children[section].second)
    {
      wstring desc = W_(state_titles[section]);
      wstring shortdesc(desc, 0, desc.find('\n'));

      pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc, desc,
						      get_desc_sig(), section);
      children[section].first=chain->instantiate(get_sig(), get_desc_sig());
      children[section].second=newtree;
      root->add_child(newtree);
      newtree->set_num_packages_parent(root);
    }

  children[section].first->add_package(pkg, children[section].second);
}

/*****************************************************************************/
class pkg_grouppolicy_filter:public pkg_grouppolicy
{
  ref_ptr<matching::pattern> filter;
  ref_ptr<matching::search_cache> search_info;

  pkg_grouppolicy *chain;
public:
  pkg_grouppolicy_filter(pkg_grouppolicy_factory *_chain,
			 const ref_ptr<matching::pattern> &_filter,
			 pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     filter(_filter),
     search_info(matching::search_cache::create()),
     chain(_chain->instantiate(_sig, _desc_sig))
  {
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    if(matching::get_match(filter, pkg, search_info, *apt_cache_file, *apt_package_records).valid())
      chain->add_package(pkg, root);
  }

  virtual ~pkg_grouppolicy_filter() {delete chain;}
};

pkg_grouppolicy *pkg_grouppolicy_filter_factory::instantiate(pkg_signal *_sig,
							     desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_filter(chain, filter, _sig, _desc_sig);
}

pkg_grouppolicy_filter_factory::~pkg_grouppolicy_filter_factory()
{
  delete chain;
}

/*****************************************************************************/

class pkg_grouppolicy_mode:public pkg_grouppolicy
{
  const static char * const child_names[];
  pair<pkg_grouppolicy *, pkg_subtree *> children[num_pkg_action_states];
  pair<pkg_grouppolicy *, pkg_subtree *> suggested_child;
  pair<pkg_grouppolicy *, pkg_subtree *> recommended_child;
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_mode(pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
  {
    for(int i=0; i<num_pkg_action_states; i++)
      {
	children[i].first=NULL;
	children[i].second=NULL;
      }
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    int group = find_pkg_state(pkg, *apt_cache_file);
    if(group!=pkg_unchanged)
      {
	if(!children[group].second)
	  {
	    wstring desc = W_(child_names[group]);
	    wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    group,
							    true);
	    root->add_child(newtree);
	    newtree->set_num_packages_parent(root);
	    children[group].first=chain->instantiate(get_sig(),
						     get_desc_sig());
	    children[group].second=newtree;
	  }

	children[group].first->add_package(pkg, children[group].second);
      }
    else if(pkg.CurrentVer().end() && package_recommended(pkg))
      {
	if(!recommended_child.second)
	  {
	    const wstring desc=W_("Packages which are recommended by other packages\n These packages are not strictly required, but they may be necessary to provide full functionality in some other programs that you are installing or upgrading.");
	    const wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    num_pkg_action_states,
							    true);
	    root->add_child(newtree);
	    newtree->set_num_packages_parent(root);
	    recommended_child.first=chain->instantiate(get_sig(),
						       get_desc_sig());
	    recommended_child.second=newtree;
	  }

	recommended_child.first->add_package(pkg, recommended_child.second);
      }
    else if(pkg.CurrentVer().end() && package_suggested(pkg))
      {
	if(!suggested_child.second)
	  {
	    const wstring desc=W_("Packages which are suggested by other packages\n These packages are not required in order to make your system function properly, but they may provide enhanced functionality for some programs that you are currently installing.");
	    const wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    num_pkg_action_states+1,
							    false);
	    root->add_child(newtree);
	    newtree->set_num_packages_parent(root);
	    suggested_child.first=chain->instantiate(get_sig(),
						     get_desc_sig());
	    suggested_child.second=newtree;
	  }

	suggested_child.first->add_package(pkg, suggested_child.second);
      }
  }

  virtual ~pkg_grouppolicy_mode()
  {
    for(int i=0; i<num_pkg_action_states; i++)
      delete children[i].first;
  }
};

const char * const pkg_grouppolicy_mode::child_names[num_pkg_action_states]=
{
  N_("Packages with unsatisfied dependencies\n The dependency requirements of these packages will be unmet after the install is complete.\n .\n The presence of this tree probably indicates that something is broken, either on your system or in the Debian archive."),
  N_("Packages being removed because they are no longer used\n These packages are being deleted because they were automatically installed to fulfill dependencies, and the planned action will result in no installed package declaring an 'important' dependency on them.\n"),
  N_("Packages being automatically held in their current state\n These packages could be upgraded, but they have been kept in their current state to avoid breaking dependencies."),
  N_("Packages being automatically installed to satisfy dependencies\n These packages are being installed because they are required by another package you have chosen for installation."),
  N_("Packages being deleted due to unsatisfied dependencies\n These packages are being deleted because one or more of their dependencies is no longer available, or because another package conflicts with them."),
  N_("Packages to be downgraded\n An older version of these packages than is currently installed will be installed."),
  N_("Packages being held back\n These packages could be upgraded, but you have asked for them to be held at their current version."),
  N_("Packages to be reinstalled\n These packages will be reinstalled."),
  N_("Packages to be installed\n These packages have been manually selected for installation on your computer."),
  N_("Packages to be removed\n These packages have been manually selected for removal."),
  N_("Packages to be upgraded\n These packages will be upgraded to a newer version."),
  N_("Packages that are partially installed\n These packages are not fully installed and configured; an attempt will be made to complete their installation."),
};

pkg_grouppolicy *pkg_grouppolicy_mode_factory::instantiate(pkg_signal *_sig,
							   desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_mode(chain, _sig, _desc_sig);
}

/*****************************************************************************/

class pkg_grouppolicy_firstchar:public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  typedef map<string, pair<pkg_grouppolicy *, pkg_subtree *> > childmap;
  // Store the child group policies and their associated subtrees.
  childmap children;
public:
  pkg_grouppolicy_firstchar(pkg_grouppolicy_factory *_chain,
			    pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
  {
  }

  ~pkg_grouppolicy_firstchar()
  {
    for(childmap::iterator i=children.begin(); i!=children.end(); i++)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    eassert(pkg.Name());

    string treename;
    if(strncmp(pkg.Name(), "lib", 3) == 0)
      treename = string(pkg.Name(), 4);
    else
      treename = pkg.Name()[0];

    childmap::iterator found=children.find(treename);
    if(found!=children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	pkg_subtree *newtree=new pkg_subtree(cw::util::transcode(treename),
					     L"", get_desc_sig());
	pkg_grouppolicy *newchild=chain->instantiate(get_sig(),
						     get_desc_sig());
	children[treename].first=newchild;
	children[treename].second=newtree;
	root->add_child(newtree);
	newtree->set_num_packages_parent(root);

	newchild->add_package(pkg, newtree);
      }
  }
};

pkg_grouppolicy *pkg_grouppolicy_firstchar_factory::instantiate(pkg_signal *sig,
								desc_signal *desc_sig)
{
  return new pkg_grouppolicy_firstchar(chain, sig, desc_sig);
}

/*****************************************************************************/

// Groups packages by priority
class pkg_grouppolicy_priority:public pkg_grouppolicy
{
  // a map may be overkill, but I figure better safe than sorry..
  // who knows, maybe someone will change the number of priorities someday..
  typedef map<unsigned char,
	      pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
  pkg_grouppolicy_factory *chain;

  pkg_grouppolicy *spillover;
public:
  pkg_grouppolicy_priority(pkg_grouppolicy_factory *_chain,
			   pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     chain(_chain),
     spillover(_chain->instantiate(get_sig(), get_desc_sig()))
  {
  }

  ~pkg_grouppolicy_priority()
  {
    for(childmap::iterator i=children.begin(); i!=children.end(); i++)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    unsigned char priority;
    const char *pstr;

    if(!pkg.VersionList().end())
      {
	if(!pkg.CurrentVer().end())
	  priority=pkg.CurrentVer()->Priority;
	else
	  priority=pkg.VersionList()->Priority;

	pstr=(*apt_cache_file)->GetCache().Priority(priority);
      }
    else
      {
	pstr=NULL;
	priority=0;
      }

    // Some packages don't have a reasonable priority
    if(!pstr)
      {
	pstr=_("unknown");
	priority=0;
      }

    childmap::iterator found=children.find(priority);

    if(found!=children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	char buf[512];
	snprintf(buf, 512, _("Priority %s"), pstr);
	int order;
	switch(priority)
	  {
	  case pkgCache::State::Required : order=1; break;
	  case pkgCache::State::Important: order=2; break;
	  case pkgCache::State::Standard : order=3; break;
	  case pkgCache::State::Optional : order=4; break;
	  case pkgCache::State::Extra    : order=5; break;
	  default                        : order=6; break;
	  };

	pkg_subtree *newtree=new pkg_subtree_with_order(cw::util::transcode(buf),
							L"",
							get_desc_sig(),
							order);
	pkg_grouppolicy *newchild=chain->instantiate(get_sig(),
						     get_desc_sig());
	children[priority].first=newchild;
	children[priority].second=newtree;
	root->add_child(newtree);
	newtree->set_num_packages_parent(root);

	newchild->add_package(pkg, newtree);
      }
  }  
};

pkg_grouppolicy *pkg_grouppolicy_priority_factory::instantiate(pkg_signal *sig,
							       desc_signal *desc_sig)
{
  return new pkg_grouppolicy_priority(chain, sig, desc_sig);
}

/*****************************************************************************/

// This class generates a pkg_subtree from a hierarchy
class pkg_hier_grouper:public pkg_hier::hierarchy_realizer, public sigc::trackable
{
  pkg_subtree *uncategorized;
  pkg_grouppolicy *uncategorized_policy;

  pkg_grouppolicy_factory *chain;

  pkg_signal *sig;
  desc_signal *desc_sig;

  pkg_subtree *root;

  typedef pair<pkg_grouppolicy *, pkg_subtree *> nodedata;

  // This tracks the grouping policies we have created so that we can
  // destroy them.
  vector<nodedata *> data_out_there;

  void handle_reload()
  {
    for(vector<nodedata *>::iterator i=data_out_there.begin();
     	i!=data_out_there.end();
     	++i)
      {
     	delete (*i)->first;
     	delete *i;
      }
    data_out_there.clear();

    uncategorized=NULL;
    uncategorized_policy=NULL;

    root=NULL;

    reset_groupinfo();
  }
public:
  pkg_hier_grouper(pkg_hier *_hier,
		   pkg_grouppolicy_factory *_chain,
		   pkg_signal *_sig,
		   desc_signal *_desc_sig)
    :hierarchy_realizer(_hier), uncategorized(NULL),
     uncategorized_policy(NULL), chain(_chain), sig(_sig), desc_sig(_desc_sig),
     root(NULL)
  {
    hier_reloaded.connect(sigc::mem_fun(*this, &pkg_hier_grouper::handle_reload));
  }

  ~pkg_hier_grouper()
  {
    delete uncategorized_policy;
    for(vector<nodedata *>::iterator i=data_out_there.begin();
     	i!=data_out_there.end();
     	++i)
      {
     	delete (*i)->first;
     	delete *i;
      }
  }

  // HACK: the root has to be set each time
  void set_root(pkg_subtree *_root)
  {
    root=_root;
  }

  void realize_item(pkg_hier::item *item, void *parent_data)
  {
    pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(item->name);
    nodedata *data=(nodedata *) parent_data;

    if(data)
      data->first->add_package(pkg, data->second);
    else
      {
	if(!uncategorized)
	  {
	    uncategorized=new pkg_subtree(W_("UNCATEGORIZED"), L"", desc_sig);
	    uncategorized_policy=chain->instantiate(sig, desc_sig);
	    root->add_child(uncategorized);
	    uncategorized->set_num_packages_parent(root);
	  }

	uncategorized_policy->add_package(pkg, uncategorized);
      }
  }

  void *realize_group(pkg_hier::group *group, void *parent_data)
  {
    nodedata *data=(nodedata *) parent_data;

    pkg_subtree *newtree=new pkg_subtree(cw::util::transcode(group->description),
					 L"", desc_sig);
    pkg_grouppolicy *newpolicy=chain->instantiate(sig, desc_sig);

    nodedata *rval=new nodedata(newpolicy, newtree);
    data_out_there.push_back(rval);

    if(data)
      {
	data->second->add_child(newtree);
	newtree->set_num_packages_parent(data->second);
      }
    else
      {
	root->add_child(newtree);
	newtree->set_num_packages_parent(root);
      }

    return rval;
  }
};

class pkg_grouppolicy_hier:public pkg_grouppolicy
{
  pkg_hier *hier;
  pkg_hier_grouper grouper;

public:
  pkg_grouppolicy_hier(pkg_hier *_hier, pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), hier(_hier),
     grouper(_hier, _chain, _sig, _desc_sig)
  {
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    // HACK:
    grouper.set_root(root);

    if(!hier->realize_item_up(pkg.Name(), &grouper))
      {
	pkg_hier::item tmp;
	tmp.name=pkg.Name();

	// MORE HACK
	grouper.realize_item(&tmp, NULL);
      }
  }
};

pkg_grouppolicy *pkg_grouppolicy_hier_factory::instantiate(pkg_signal *sig,
							   desc_signal *desc_sig)
{
  return new pkg_grouppolicy_hier(hier, chain, sig, desc_sig);
}

pkg_grouppolicy_hier_factory::~pkg_grouppolicy_hier_factory()
{
  if(del_hier)
    delete hier;
  delete chain;
}

/****************************************************************************/

//  This class generates a "hierarchy" by grouping packages by Task, and
// optionally throwing away non-task packages.

class pkg_grouppolicy_task:public pkg_grouppolicy
{
  /** The subtrees are indexed by Task. */
  typedef std::map<string, pkg_subtree *> subtree_map;

  /** A list of [task,description] pairs. */
  typedef vector<pair<string, wstring> > desclist;

  // HACK: put all our stuff under this.
  pkg_subtree *tasks_subtree;

  subtree_map task_children, section_children;

  pkg_grouppolicy *chain;

  static desclist task_section_descriptions;
  // Lifted from tasksel.  There are so few of them that it's not
  // worth using a std::map, IMNSHO.

  static void init_section_descriptions();
public:
  pkg_grouppolicy_task(pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), tasks_subtree(NULL),
     chain(_chain?_chain->instantiate(_sig, _desc_sig):NULL)
  {
    init_section_descriptions();
  }

  ~pkg_grouppolicy_task() {delete chain;}

  void add_package(const pkgCache::PkgIterator &pkg,
		   pkg_subtree *root);
};

pkg_grouppolicy_task::desclist pkg_grouppolicy_task::task_section_descriptions;

void pkg_grouppolicy_task::init_section_descriptions()
{
  static bool already_done=false;

  if(already_done)
    return;

  task_section_descriptions.push_back(pair<string,wstring>("user", W_("End-user")));
  task_section_descriptions.push_back(pair<string,wstring>("server", W_("Servers")));
  task_section_descriptions.push_back(pair<string,wstring>("devel", W_("Development")));
  task_section_descriptions.push_back(pair<string,wstring>("l10n", W_("Localization")));
  task_section_descriptions.push_back(pair<string,wstring>("hware", W_("Hardware Support")));
  task_section_descriptions.push_back(pair<string,wstring>("misc", W_("Miscellaneous")));
  task_section_descriptions.push_back(pair<string,wstring>("unknown", W_("Unrecognized tasks")));
}

/** Uses the fact that 0<=Relevance<=10 to encode Relevance as a character. */
class task_subtree:public pkg_subtree
{
  char relevance[2];
  void set_relevance(int _relevance)
  {
    relevance[0]='z'-_relevance;
    relevance[1]=0;
  }

public:
  task_subtree(const std::wstring &_name, const std::wstring &_description,
	       desc_signal *_info_signal,
	       int _relevance)
    :pkg_subtree(_name, _description, _info_signal)
  {
    set_relevance(_relevance);
  }

  task_subtree(const aptitude::apt::task &_task, desc_signal *_info_signal)
    : pkg_subtree(_task.shortdesc, _task.longdesc, _info_signal)
  {
    set_relevance(_task.relevance);
  }

  const char *tag() const {return relevance;}
};

void pkg_grouppolicy_task::add_package(const pkgCache::PkgIterator &pkg,
				       pkg_subtree *root)
{
  using aptitude::apt::task_list;

  set<string> *tasks = aptitude::apt::get_tasks(pkg);

  eassert(tasks);

  chain->add_package(pkg, root);

  for(set<string>::iterator i = tasks->begin(); i != tasks->end(); ++i)
    {
      subtree_map::iterator found=task_children.find(*i);

      if(found==task_children.end())
	{
	  string section = "unknown";
          aptitude::apt::task *task = aptitude::apt::find_task(*i);
          bool taskfound = (task != NULL);
	  pkg_subtree *newtree, *sectiontree;

          if(taskfound == true)
            {
	      if(task->keys_present() == false)
		continue; // skip to the next task of this package.
              section = task->section;
            }

	  if(!tasks_subtree)
	    {
	      tasks_subtree=new pkg_subtree(W_("Tasks"),
					    W_("\n Tasks are groups of packages which provide an easy way to select a predefined set of packages for a particular purpose."), get_desc_sig());
	      root->add_child(tasks_subtree);
	      tasks_subtree->set_num_packages_parent(root);
	    }

	  subtree_map::iterator sectionfound=section_children.find(section);
	  if(sectionfound==section_children.end())
	    {
	      wstring sectiondesc=cw::util::transcode(section, "ASCII");

	      for(desclist::iterator j=task_section_descriptions.begin();
		  j!=task_section_descriptions.end();
		  ++j)
		if(j->first==section)
		  {
		    sectiondesc=j->second;
		    break;
		  }

	      sectiontree = new pkg_subtree(sectiondesc, L"", get_desc_sig());
	      section_children[section]=sectiontree;
	      tasks_subtree->add_child(sectiontree);
	      sectiontree->set_num_packages_parent(tasks_subtree);
	    }
	  else
	    sectiontree=sectionfound->second;

	  if(taskfound == true)
	    newtree=new task_subtree(*task, get_desc_sig());
	  else
	    newtree=new task_subtree(cw::util::transcode(*i), L"",
				     get_desc_sig(), 5);

	  task_children[*i]=newtree;

	  sectiontree->add_child(newtree);
	  newtree->set_num_packages_parent(sectiontree);

	  newtree->add_child(new pkg_item(pkg, get_sig()));
	  newtree->inc_num_packages();
	}
      else
	{
	  found->second->add_child(new pkg_item(pkg, get_sig()));
	  found->second->inc_num_packages();
	}
    }
}

pkg_grouppolicy *pkg_grouppolicy_task_factory::instantiate(pkg_signal *sig,
							   desc_signal *desc_sig)
{
  return new pkg_grouppolicy_task(chain, sig, desc_sig);
}

pkg_grouppolicy_task_factory::~pkg_grouppolicy_task_factory()
{
  delete chain;
}


class pkg_grouppolicy_patterns : public pkg_grouppolicy
{
public:
  typedef pkg_grouppolicy_patterns_factory::match_entry match_entry;

  struct subtree_pair
  {
    pkg_grouppolicy *policy;
    pkg_subtree *tree;

    subtree_pair():policy(NULL), tree(NULL)
    {
    }

    subtree_pair(pkg_grouppolicy *_policy, pkg_subtree *_tree)
      :policy(_policy), tree(_tree)
    {
    }
  };
private:
  pkg_grouppolicy_factory *chain;
  pkg_grouppolicy *passthrough_policy;
  ref_ptr<matching::search_cache> search_info;
  const vector<match_entry> &subgroups;
  typedef map<wstring, subtree_pair> subtree_map;
  subtree_map subtrees;

  wstring substitute(const wstring &s,
		     const ref_ptr<matching::structural_match> &res)
  {
    wstring rval;

    wstring::const_iterator i = s.begin();
    while(i != s.end())
      {
	while(i != s.end() && *i != L'\\')
	  {
	    rval += *i;
	    ++i;
	  }

	if(i != s.end())
	  {
	    ++i;
	    if(i == s.end())
	      rval += L'\\';
	    else if(*i == L'\\')
	      {
		rval += L'\\';
		++i;
	      }
	    else if(iswdigit(*i))
	      {
		wstring tocvt;

		while(i != s.end() && iswdigit(*i))
		  {
		    tocvt += *i;
		    ++i;
		  }

		wchar_t *endptr = NULL;
		unsigned long val = wcstoul(tocvt.c_str(), &endptr, 0);

		if(endptr && (*endptr) != L'\0')
		  {
		    wchar_t buf[512];

		    swprintf(buf, 512, W_("Bad number in format string: %ls").c_str(),
			     tocvt.c_str());

		    return buf;
		  }

		if(val < 1)
		  {
		    wchar_t buf[512];
		    swprintf(buf, 512, W_("Match indices must be 1 or greater, not \"%s\"").c_str(),
			     tocvt.c_str());
		    return buf;
		  }

		--val;

		if(val >= res->get_num_groups())
		  {
		    string group_values;
		    for(unsigned int i = 0; i < res->get_num_groups(); ++i)
		      {
			if(i > 0)
			  group_values += ",";
			group_values += res->get_group(i);
		      }

		    wchar_t buf[1024];
		    swprintf(buf, 1024, W_("Match index %ls is too large; available groups are (%s)").c_str(),
			     tocvt.c_str(), group_values.c_str());

		    return buf;
		  }

		rval += cw::util::transcode(res->get_group(val));
	      }
	  }
      }

    return rval;
  }
public:
  pkg_grouppolicy_patterns(pkg_grouppolicy_factory *_chain,
			   pkg_signal *_sig, desc_signal *_desc_sig,
			   const vector<match_entry> &_subgroups)
        :pkg_grouppolicy(_sig, _desc_sig),
	 chain(_chain), passthrough_policy(NULL),
	 search_info(matching::search_cache::create()),
	 subgroups(_subgroups)
  {
  }

  ~pkg_grouppolicy_patterns()
  {
    for(subtree_map::const_iterator i = subtrees.begin();
	i != subtrees.end(); ++i)
      delete i->second.policy;
    delete passthrough_policy;
  }

  void add_package(const pkgCache::PkgIterator &pkg,
		   pkg_subtree *root)
  {
    for(vector<match_entry>::const_iterator i = subgroups.begin();
	i != subgroups.end(); ++i)
	{
	  ref_ptr<matching::structural_match> res(matching::get_match(i->pattern, pkg, search_info, *apt_cache_file, *apt_package_records));
	  if(res.valid())
	    {
	      pkg_grouppolicy_factory * const local_chain =
		i->chain != NULL ? i->chain : chain;

	      if(i->passthrough)
		{
		  if(passthrough_policy == NULL)
		    passthrough_policy = local_chain->instantiate(get_sig(),
								  get_desc_sig());
		  passthrough_policy->add_package(pkg, root);
		  break;
		}

	      wstring title = substitute(i->tree_name, res);

	      subtree_map::const_iterator found =
		subtrees.find(title);

	      if(found != subtrees.end())
		found->second.policy->add_package(pkg, found->second.tree);
	      else
		{
		  pkg_subtree *tree = new pkg_subtree(title, L"", get_desc_sig());
		  pkg_grouppolicy *policy = local_chain->instantiate(get_sig(),
								     get_desc_sig());
		  root->add_child(tree);
		  tree->set_num_packages_parent(root);

		  subtrees[title]=subtree_pair(policy, tree);

		  policy->add_package(pkg, tree);
		}

	      break;
	    }
	}
  }
};

pkg_grouppolicy *pkg_grouppolicy_patterns_factory :: instantiate(pkg_signal *sig,
								 desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_patterns(chain, sig, _desc_sig, subgroups);
}

pkg_grouppolicy_patterns_factory :: ~pkg_grouppolicy_patterns_factory()
{
  for(std::vector<match_entry>::const_iterator i = subgroups.begin();
      i != subgroups.end(); ++i)
    {
      delete i->chain;
    }
  delete chain;
}





class pkg_grouppolicy_tag : public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  string match_facet;

  typedef std::map<string, pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
public:
  pkg_grouppolicy_tag(const string &_match_facet, pkg_grouppolicy_factory *_chain,
		      pkg_signal *sig, desc_signal *desc_sig)
    :pkg_grouppolicy(sig, desc_sig), chain(_chain), match_facet(_match_facet)
  {
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg,
			   pkg_subtree *root)
  {
    using aptitude::apt::get_facet_name;
    using aptitude::apt::get_tag_long_description;
    using aptitude::apt::get_tag_name;
    using aptitude::apt::get_tag_short_description;
    using aptitude::apt::get_tags;
    using aptitude::apt::tag;

    const set<tag> tags(get_tags(pkg));

    if(tags.empty() == true)
      return;

    for(set<tag>::const_iterator ti = tags.begin();
	ti != tags.end(); ++ti)
      {
        const std::string thisfacet = get_facet_name(*ti);

	// Don't create items for tags that aren't in our facet.
	if(thisfacet != match_facet)
	  return;

	// TODO: split up by sub-facet or whatever the debtags guys
	// call it?
	std::string tagname = get_tag_name(*ti);

	childmap::const_iterator found =
	  children.find(tagname);

	pkg_subtree *subtree = NULL;
	pkg_grouppolicy *subpolicy = NULL;

	if(found == children.end())
	  {
	    const std::string desc = get_tag_long_description(*ti);
	    const std::string shortdesc = get_tag_short_description(*ti);

	    subtree = new pkg_subtree(swsprintf(L"%s - %s",
						tagname.c_str(),
						shortdesc.c_str()),
				      cw::util::transcode(desc),
				      get_desc_sig());
	    root->add_child(subtree);
	    subtree->set_num_packages_parent(root);
	    subpolicy = chain->instantiate(get_sig(),
					   get_desc_sig());

	    children[tagname] = pair<pkg_grouppolicy *, pkg_subtree *>(subpolicy, subtree);
	  }
	else
	  {
	    subpolicy = found->second.first;
	    subtree = found->second.second;
	  }

	subpolicy->add_package(pkg, subtree);
      }
  }

  ~pkg_grouppolicy_tag()
  {
    // Delete the subpolicies.
    for(childmap::const_iterator i = children.begin();
	i != children.end(); ++i)
      delete i->second.first;
  }
};

pkg_grouppolicy *pkg_grouppolicy_tag_factory::instantiate(pkg_signal *pkg_sig,
							  desc_signal *desc_sig)
{
  return new pkg_grouppolicy_tag(facet, chain, pkg_sig, desc_sig);
}

pkg_grouppolicy_tag_factory::~pkg_grouppolicy_tag_factory()
{
  delete chain;
}





class pkg_grouppolicy_facet_tag : public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  typedef std::map<string, pair<pkg_grouppolicy *, pkg_subtree *> > tagmap;

  typedef std::map<string, pair<tagmap *, pkg_subtree *> > facetmap;

  pkg_subtree *untagged_tree;
  pkg_grouppolicy *untagged_policy;

  facetmap children;
public:
  pkg_grouppolicy_facet_tag(pkg_grouppolicy_factory *_chain,
			    pkg_signal *pkg_sig,
			    desc_signal *desc_sig)
    :pkg_grouppolicy(pkg_sig, desc_sig),
     chain(_chain),
     untagged_tree(NULL),
     untagged_policy(NULL)
  {
  }


  virtual void add_package(const pkgCache::PkgIterator &pkg,
			   pkg_subtree *root)
  {
    using aptitude::apt::get_tags;
    using aptitude::apt::tag;

    const set<tag> tags(get_tags(pkg));

    // Put all untagged, non-virtual packages into a separate list.
    if(tags.empty() == true && !pkg.VersionList().end())
      {
	if(untagged_tree == NULL)
	  {
	    eassert(untagged_policy == NULL);

	    untagged_tree = new pkg_subtree(W_("TAGLESS PACKAGES"),
					    W_("\n These packages have not yet been classified in debtags, or the debtags database is not present (installing debtags may correct this problem)."),
					    get_desc_sig());
	    root->add_child(untagged_tree);
	    untagged_tree->set_num_packages_parent(root);

	    untagged_policy = chain->instantiate(get_sig(), get_desc_sig());
	  }

	untagged_policy->add_package(pkg, untagged_tree);
      }

    if(tags.empty() == true)
      return;

    for(set<tag>::const_iterator ti = tags.begin();
	ti != tags.end(); ++ti)
      {
        using aptitude::apt::get_facet_long_description;
        using aptitude::apt::get_facet_name;
        using aptitude::apt::get_facet_short_description;
        using aptitude::apt::get_tag_long_description;
        using aptitude::apt::get_tag_name;
        using aptitude::apt::get_tag_short_description;

	std::string thisfacet(get_facet_name(*ti));
	std::string thistag(get_tag_name(*ti));

	facetmap::const_iterator facetfound =
	  children.find(thisfacet);

	tagmap *tagchildren = NULL;
	pkg_subtree *tagtree = NULL;

	if(facetfound == children.end())
	  {
	    string desc(get_facet_long_description(*ti));
	    string shortdesc(get_facet_short_description(*ti));

	    if(!shortdesc.empty())
	      tagtree = new pkg_subtree(swsprintf(L"%s - %s",
						  thisfacet.c_str(),
						  shortdesc.c_str()),
					cw::util::transcode(desc),
					get_desc_sig());
	    else
	      tagtree = new pkg_subtree(cw::util::transcode(thisfacet),
					cw::util::transcode(desc),
					get_desc_sig());
	    root->add_child(tagtree);
	    tagtree->set_num_packages_parent(root);
	    tagchildren = new tagmap;

	    children[thisfacet] = pair<tagmap *, pkg_subtree *>(tagchildren, tagtree);
	  }
	else
	  {
	    tagchildren = facetfound->second.first;
	    tagtree = facetfound->second.second;
	  }

	tagmap::const_iterator tagfound =
	  tagchildren->find(thistag);

	pkg_grouppolicy *subpolicy = NULL;
	pkg_subtree *subtree = NULL;

	if(tagfound == tagchildren->end())
	  {
	    string desc(get_tag_long_description(*ti));
	    string shortdesc(get_tag_short_description(*ti));

	    if(!shortdesc.empty())
	      subtree = new pkg_subtree(swsprintf(L"%s - %s",
						  thistag.c_str(),
						  shortdesc.c_str()),
					cw::util::transcode(desc),
					get_desc_sig());
	    else
	      subtree = new pkg_subtree(cw::util::transcode(thistag),
					cw::util::transcode(desc),
					get_desc_sig());

	    tagtree->add_child(subtree);
	    subtree->set_num_packages_parent(tagtree);
	    subpolicy = chain->instantiate(get_sig(), get_desc_sig());

	    (*tagchildren)[thistag] = pair<pkg_grouppolicy *, pkg_subtree *>(subpolicy, subtree);
	  }
	else
	  {
	    subpolicy = tagfound->second.first;
	    subtree = tagfound->second.second;
	  }

	subpolicy->add_package(pkg, subtree);
      }
  }

  ~pkg_grouppolicy_facet_tag()
  {
    for(facetmap::const_iterator i = children.begin();
	i != children.end(); ++i)
      {
	for(tagmap::const_iterator j = i->second.first->begin();
	    j != i->second.first->end(); ++j)
	  delete j->second.first;

	delete i->second.first;
      }
  }
};

pkg_grouppolicy *
pkg_grouppolicy_facet_tag_factory::instantiate(pkg_signal *sig,
					       desc_signal *desc_sig)
{
  return new pkg_grouppolicy_facet_tag(chain, sig, desc_sig);
}

pkg_grouppolicy_facet_tag_factory::~pkg_grouppolicy_facet_tag_factory()
{
  delete chain;
}


/*****************************************************************************/

// Groups packages by source package
class pkg_grouppolicy_source:public pkg_grouppolicy
{
  typedef map<string,
	      pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
  pkg_grouppolicy_factory *chain;

  pkg_grouppolicy *spillover;
public:
  pkg_grouppolicy_source(pkg_grouppolicy_factory *_chain,
			 pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     chain(_chain),
     spillover(_chain->instantiate(get_sig(), get_desc_sig()))
  {
  }

  ~pkg_grouppolicy_source()
  {
    for(childmap::iterator i = children.begin(); i != children.end(); ++i)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    if(pkg.VersionList().end() || pkg.VersionList().FileList().end())
      return;
    std::string source_package_name =
      apt_package_records->Lookup(pkg.VersionList().FileList()).SourcePkg();
    if(source_package_name.length() == 0)
      source_package_name = pkg.Name();

    childmap::iterator found = children.find(source_package_name);

    if(found != children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	pkg_subtree *newtree = new pkg_subtree(cw::util::transcode(source_package_name),
                                               L"",
                                               get_desc_sig());
	pkg_grouppolicy *newchild = chain->instantiate(get_sig(),
                                                       get_desc_sig());
	children[source_package_name].first = newchild;
	children[source_package_name].second = newtree;
	root->add_child(newtree);
	newtree->set_num_packages_parent(root);

	newchild->add_package(pkg, newtree);
      }
  }
};

pkg_grouppolicy *pkg_grouppolicy_source_factory::instantiate(pkg_signal *sig,
							     desc_signal *desc_sig)
{
  return new pkg_grouppolicy_source(chain, sig, desc_sig);
}

/*****************************************************************************/

// Groups packages by architecture
class pkg_grouppolicy_arch:public pkg_grouppolicy
{
  typedef map<string,
	      pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
  pkg_grouppolicy_factory *chain;

  pkg_grouppolicy *spillover;

public:
  pkg_grouppolicy_arch(pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     chain(_chain),
     spillover(_chain->instantiate(get_sig(), get_desc_sig()))
  {
  }

  ~pkg_grouppolicy_arch()
  {
    for(childmap::iterator i=children.begin(); i!=children.end(); i++)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    string arch;

    if(!pkg.VersionList().end())
      {
	arch=pkg.VersionList().Arch();
      }
    else
      {
	arch=_("virtual");
      }

    childmap::iterator found=children.find(arch);

    if(found!=children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	pkg_subtree *newtree=new pkg_subtree(cw::util::transcode(arch),
					     L"",
					     get_desc_sig());
	pkg_grouppolicy *newchild=chain->instantiate(get_sig(),
						     get_desc_sig());
	children[arch].first=newchild;
	children[arch].second=newtree;
	root->add_child(newtree);
	newtree->set_num_packages_parent(root);

	newchild->add_package(pkg, newtree);
      }
  }  
};

pkg_grouppolicy *pkg_grouppolicy_arch_factory::instantiate(pkg_signal *sig,
							   desc_signal *desc_sig)
{
  return new pkg_grouppolicy_arch(chain, sig, desc_sig);
}
