// pkg_grouppolicy.h       -*-c++-*-
//
//  Copyright 1999-2002, 2005, 2007-2008 Daniel Burrows
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

#ifndef PKG_GROUPPOLICY_H
#define PKG_GROUPPOLICY_H

#include <apt-pkg/pkgcache.h>
#include <sigc++/signal.h>

#include <vector>

#include <generic/apt/matching/pattern.h>

/** \brief Provides a flexible way to group and filter packages
 *
 * 
 *  Ok, this requires (a little) explanation -- basically, the idea is that
 *  we need a flexible way to group (and filter) packages.  These classes
 *  provide that.  A grouping policy knows what tree it should add things to,
 *  and can add subtrees or packages at its own discretion.  A policy
 *  *factory* is used as a way to chain policies together -- rather than
 *  directly adding packages to trees they create, most packages will
 *  instantiate (using a factory) a policy for each subtree.
 *  
 *  In other words: say we want to group by top-level section, then by
 *  subsection.  We create a pkg_groupsectionpolicy_factory with the
 *  proper arguments to group by top-level section, with its 'chain' argument
 *  set to another pkg_groupsectionpolicy_factory (which, however, groups
 *  by subsection) with its chain argument set to a pkg_grouppolicy_end_factory.
 *  
 *  Note that the policy classes are not themselves visible to the user,
 *  simply to avoid unnecessary recompiles.  The factories are the only way
 *  to get a policy class.
 *  
 *  The factories could probably be functions/function pointers as well,
 *  but classes are more flexible in this instance.
 *  
 *  Because of the way factories and policies generally work you should never
 *  destroy a factory before destroying the new objects created by it.
 *  
 *  An abstract base class for objects containing the logic to sort packages
 *  into groups
 * 
 *  \file pkg_grouppolicy.h
 */

// Used to set the visible status if a package is available.
typedef sigc::signal2<void,
		      const pkgCache::PkgIterator &,
		      const pkgCache::VerIterator &> pkg_signal;

// Used to set the visible description for other items.
typedef sigc::signal1<void, std::wstring> desc_signal;

class pkg_subtree;

class pkg_grouppolicy
{
  pkg_signal *sig;
  desc_signal *desc_sig;
protected:
  pkg_signal *get_sig() {return sig;}
  desc_signal *get_desc_sig() {return desc_sig;}
public:
  pkg_grouppolicy(pkg_signal *_sig, desc_signal *_desc_sig)
    :sig(_sig), desc_sig(_desc_sig) {}

  virtual ~pkg_grouppolicy() {}

  virtual void add_package(const pkgCache::PkgIterator &i,
			   pkg_subtree *root)=0;
};

// Where policy comes from.
class pkg_grouppolicy_factory
{
public:
  virtual pkg_grouppolicy *instantiate(pkg_signal *_sig,
				       desc_signal *_desc_sig)=0;

  virtual ~pkg_grouppolicy_factory();
};

// ==========================================================================
//                           BEGIN SPECIALIZED FACTORIES
// NOTE: This code may move to its own header/source files someday.

// 'end' policies are a way to terminate policy chains.  Rather than form
// subtrees, they simply deposit their packages into the given root.
class pkg_grouppolicy_end_factory:public pkg_grouppolicy_factory
{
  public:
  pkg_grouppolicy_end_factory() {}
  virtual pkg_grouppolicy *instantiate(pkg_signal *_sig,
				       desc_signal *_desc_sig);
};

// The following policy groups packages by section.
class pkg_grouppolicy_section_factory:public pkg_grouppolicy_factory
{
public:
  /** \brief Which part of the section to use: */
  enum split_mode_type
    {
      /** \brief Use the whole section, uninterpreted.
       *
       *  This gives you names like "games" and "non-free/editors".
       */
      split_none = 0,

      /** \brief Use the part of the section before the first slash,
       *  or "main" if there is no slash ("/").
       *
       *  This gives you names like "main", "non-free", etc.
       */
      split_topdir = 1,

      /** \brief Use the part of the section after the first slash,
       *  or the whole section if there is no slash.
       */
      split_subdir = 2,

      /** \brief Remove any text preceding a slash, then split the
       *  remainder at slashes and build a multi-level hierarchy of
       *  sections.
       *
       *  If you have packages in the sections "games/arcade" and
       *  "games/strategy", this will produce a tree named "games",
       *  with trees named "arcade" and "strategy" beneath it.
       */
      split_subdirs = 3
    };

private:
  pkg_grouppolicy_factory *chain;

  split_mode_type split_mode;

  // If this is true, virtual packages, packages in unknown sections, and
  // task packages will be 'passed through' to the next policy without having
  // a new level of tree structure created.
  bool passthrough;
public:
  pkg_grouppolicy_section_factory(split_mode_type _split_mode,
				  bool _passthrough,
				  pkg_grouppolicy_factory *_chain)
    :chain(_chain), split_mode(_split_mode), passthrough(_passthrough) {}

  virtual pkg_grouppolicy *instantiate(pkg_signal *_sig,
				       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_section_factory()
  {delete chain;}
};

// The following policy groups packages by status.
class pkg_grouppolicy_status_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_status_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  virtual pkg_grouppolicy *instantiate(pkg_signal *_sig,
				       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_status_factory()
  {delete chain;}
};

/** Filter out packages that don't match the given pattern. */
class pkg_grouppolicy_filter_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;

  cwidget::util::ref_ptr<aptitude::matching::pattern> filter;
public:
  pkg_grouppolicy_filter_factory(const cwidget::util::ref_ptr<aptitude::matching::pattern> &_filter,
				 pkg_grouppolicy_factory *_chain)
    :chain(_chain), filter(_filter) {}

  virtual pkg_grouppolicy *instantiate(pkg_signal *_sig,
				       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_filter_factory();
};

// Filters out missing packages -- packages with no available versions and
// nothing providing them.
bool pkg_missing_filter(const pkgCache::PkgIterator &pkg);

class pkg_grouppolicy_mode_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_mode_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  pkg_grouppolicy *instantiate(pkg_signal *_sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_mode_factory()
  {delete chain;}
};

//   Evidently some people like this.  No, I don't know why.  But other programs
// have it and it's trivial to add as an option, so..
//
//   Creates a separate subtree for each first character (so you get one for
// packages beginning in "a", one for "b", etc.
class pkg_grouppolicy_firstchar_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_firstchar_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  pkg_grouppolicy *instantiate(pkg_signal *_sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_firstchar_factory()
  {delete chain;}
};

// Groups by priority
class pkg_grouppolicy_priority_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_priority_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  pkg_grouppolicy *instantiate(pkg_signal *_sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_priority_factory()
  {delete chain;}
};

class pkg_hier;

// Groups by a 'hierarchy' as described in generic/README.hier
class pkg_grouppolicy_hier_factory:public pkg_grouppolicy_factory
{
  // This is deleted with the factory iff del_hier==true.
  pkg_hier *hier;

  pkg_grouppolicy_factory *chain;

  // If true, delete our hierarchy object when we are destroyed.
  bool del_hier;
public:
  pkg_grouppolicy_hier_factory(pkg_hier *_hier,
			       bool _del_hier,
			       pkg_grouppolicy_factory *_chain)
    :hier(_hier), chain(_chain), del_hier(_del_hier)
  {
  }

  pkg_grouppolicy *instantiate(pkg_signal *sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_hier_factory();
};

// Groups packages by task; if they are not in any task, they may be
// discarded (based on the value of "discard")
class pkg_grouppolicy_task_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_task_factory(pkg_grouppolicy_factory *_chain)
    :chain(_chain)
  {
  }

  pkg_grouppolicy *instantiate(pkg_signal *sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_task_factory();
};

/** \brief Groups packages using the given list of patterns/tree names.
 *
 *  Match results can be substituted into tree names using \N
 *  notation.  Each branch of the tree can have its own policy chain;
 *  if none is specified, the default chain will be used.
 */
class pkg_grouppolicy_patterns_factory:public pkg_grouppolicy_factory
{
public:
  struct match_entry
  {
    cwidget::util::ref_ptr<aptitude::matching::pattern> pattern;
    /** \brief A pointer to the specialized sub-factory for
     *  this entry, or NULL to use the default chain.
     */
    pkg_grouppolicy_factory *chain;
    std::wstring tree_name;
    bool passthrough;

    match_entry(const cwidget::util::ref_ptr<aptitude::matching::pattern> &_pattern,
		pkg_grouppolicy_factory *_chain,
		const std::wstring &_tree_name,
		bool _passthrough)
      : pattern(_pattern),
	chain(_chain),
        tree_name(_tree_name),
	passthrough(_passthrough)
    {
    }
  };

private:
  pkg_grouppolicy_factory *chain;

  std::vector<match_entry> subgroups;
public:

  pkg_grouppolicy_patterns_factory(const std::vector<match_entry> &_subgroups,
				   pkg_grouppolicy_factory *_chain)
    :chain(_chain), subgroups(_subgroups)
  {
  }

  pkg_grouppolicy *instantiate(pkg_signal *sig,
			       desc_signal *_desc_sig);

  ~pkg_grouppolicy_patterns_factory();
};

/** Groups packages by their tags within a single facet. */
class pkg_grouppolicy_tag_factory : public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;

  std::string facet;
public:
  pkg_grouppolicy_tag_factory(const std::string &_facet,
			      pkg_grouppolicy_factory *_chain)
    :chain(_chain), facet(_facet)
  {
  }

  pkg_grouppolicy *instantiate(pkg_signal *sig,
			       desc_signal *desc_sig);

  ~pkg_grouppolicy_tag_factory();
};

/** Groups packages by their facets and their tags. */
class pkg_grouppolicy_facet_tag_factory : public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_facet_tag_factory(pkg_grouppolicy_factory *_chain)
    :chain(_chain)
  {
  }

  pkg_grouppolicy *instantiate(pkg_signal *sig,
			       desc_signal *desc_sig);

  ~pkg_grouppolicy_facet_tag_factory();
};

// Groups by source package
class pkg_grouppolicy_source_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_source_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  pkg_grouppolicy *instantiate(pkg_signal *_sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_source_factory()
  {delete chain;}
};

// Groups by architecture
class pkg_grouppolicy_arch_factory:public pkg_grouppolicy_factory
{
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_arch_factory(pkg_grouppolicy_factory *_chain):chain(_chain) {}

  pkg_grouppolicy *instantiate(pkg_signal *_sig,
			       desc_signal *_desc_sig);

  virtual ~pkg_grouppolicy_arch_factory()
  {delete chain;}
};

#endif
