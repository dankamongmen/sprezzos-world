// apt.h  -*-c++-*-
//
//  Copyright 1999-2002, 2004-2005, 2007-2010 Daniel Burrows
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


#ifndef APT_H
#define APT_H

#include "aptcache.h"

#include <string.h>

#include <boost/shared_ptr.hpp>

#include <utility>
#include <vector>

/** \brief Central repository for apt structures
 *
 * 
 *  Argh.  It seems that it's pretty much necessary to have a central
 *  repository for apt structures -- the cache in particular.  This is it.
 *  No class wrapper because, well, the cache is already a class and you can't
 *  open more than one at once, so there :)
 * 
 *  \file apt.h
 */

namespace aptitude
{
  namespace util
  {
    class file_cache;
  }
}
class OpProgress;
class pkgRecords;
class pkgSourceList;
class pkg_hier;
class signalling_config;
class undo_list;
class resolver_manager;

// Global state variables for the apt stuff:
extern signalling_config *aptcfg;
extern aptitudeCacheFile *apt_cache_file;
extern resolver_manager *resman;
extern pkgSourceList *apt_source_list;
extern pkgRecords *apt_package_records;

pkg_hier *get_user_pkg_hier();
// Currently, package hierarchies are (by default) loaded using
// DATADIR/aptitude/hier_groups and ~/.aptitude/pkgclass.  This is to
// facilitate the editing of hierarchies.  user_pkg_hier contains the
// information which is loaded in this way.  It is accessible only
// through an accessor method in order to implement lazy loading (so
// people who don't use the browser don't take the hit of having to
// load in the (BIG) hierarchy info file)

void apt_preinit(const char *rootdir);
// Performs initialization of stuff that has to happen before apt_init is
// called (eg, pkgInitialize and setting up the undo structure)

void apt_init(OpProgress *progess_bar,
	      bool do_initselections, const char * status_fname=NULL);
//  It actually doesn't do what you expect!  This routine is a NOP if
// it has already been called; the rationale is that it allows any apt-using
// class to call this in its constructor (thus guaranteeing that the cache
// file is in a sane state)  To force a reload, call reload_cache().
//
//  One more note: pkgInitialize is not called in this routine, as some info
// from that (eg, config data) may be needed before apt_init itself is called.
// This routine loads in the cache, source lists, etc.


/** Close the cache file and destroy the associated data structures. */
void apt_close_cache();

/** \brief Close and destroy all global data structures.
 *
 *  In addition to closing the apt cache, this closes the download
 *  cache and the download queue.
 */
void apt_shutdown();

/** \return \b true if the version of the apt library against
 *  which we are running supports the RootDir convention.
 *
 *  This returns \b false if apt_preinit() has not yet been invoked.
 */
bool get_apt_knows_about_rootdir();

/** If the cache is closed, open it; otherwise do nothing.
 *
 *  \param progress_bar a progress bar with which to display the
 *                      status of loading the cache.
 *  \param do_initselections if \b true, the selection status
 *                           of packages will be set from aptitude's
 *                           sticky database.
 *  \param status_fname if not \b NULL, a filename to use in lieu
 *                      of /var/lib/aptitude/pkgstates.
 */
void apt_load_cache(OpProgress *progress_bar,
		    bool do_initselections, const char *status_fname = NULL);

void apt_reload_cache(OpProgress *progress_bar,
		      bool do_initselections, const char * status_fname=NULL);
//  Forces the cache to be reloaded.
//
//  NOTE: at the moment, the interface won't work too well while this
// routine runs :)  I have Plans [ insert evil laughter here ] to do this in
// a background thread, but I need to know more about various bits of apt
// first.

extern sigc::signal0<void> cache_closed;
// Announces when the cache is (or is about to be) closed.
// This means that anyone using it should immediately drop references to it.
// Generally followed by cache_reloaded; always followed by cache_reloaded
// or cache_reload_failed.
extern sigc::signal0<void> hier_reloaded;
// Announces that user_pkg_hier has been reloaded.  (it's necessary to
// do this BEFORE cache_reloaded, as some things try to use the old
// user_pkg_hier in cache_reloaded if we don't)
extern sigc::signal0<void> cache_reloaded;
// Announces that the cache has been reloaded
extern sigc::signal0<void> cache_reload_failed;
// Announces that reloading the cache failed.

/** Called when the UI should display and remove all pending errors
 *  from the queue.  It is incumbent on the UI to remove errors on its
 *  own after calling APT routines -- this just indicates that some
 *  errors might need to be removed while a routine is executing.
 */
extern sigc::signal0<void> consume_errors;

/** \brief Used to cache downloaded data, to avoid multiple
 *  downloads of items such as changelogs and screenshots.
 */
extern boost::shared_ptr<aptitude::util::file_cache> download_cache;

void apt_dumpcfg(const char *root);
// Dumps a subtree of the configuration to ~/.aptitude/config

void apt_revertoptions();
// Reloads the standard options from apt.conf (ignoring the user's
// configuration file)

void forget_new();
// Discard all in-memory information about what packages are newly seen.  This
// actually involves generating new information, but you can think of it as
// if it discarded info. :)

extern undo_list *apt_undos;
// There's a global undo stack for apt actions, to keep things sane..

/** \brief Represents a package's logical state.
 *
 *  find_pkg_state computes this value.
 */
enum pkg_action_state
  {
    /** \brief No action is being performed on the package. */
    pkg_unchanged=-1,
    /** \brief The package has broken dependencies. */
    pkg_broken,
    /** \brief The package is unused and will be removed. */
    pkg_unused_remove,
    /** \brief The package was automatically held on the system. */
    pkg_auto_hold,
    /** \brief The package is being installed to fulfill dependencies. */
    pkg_auto_install,
    /** \brief The package is being removed to fulfill dependencies. */
    pkg_auto_remove,
    /** \brief The package is being downgraded. */
    pkg_downgrade,
    /** \brief The package is held back. */
    pkg_hold,
    /** \brief The package is being reinstalled. */
    pkg_reinstall,
    /** \brief The package is being installed. */
    pkg_install,
    /** \brief The package is being removed. */
    pkg_remove,
    /** \brief The package is being upgraded. */
    pkg_upgrade,
    /** \brief The package is installed but not configured. */
    pkg_unconfigured
  };
/** \brief The number of package action states, not counting
 *  pkg_unchanged.
 */
const int num_pkg_action_states=12;
/** \brief Compute the action state of a package.
 *
 *  \param pkg   The package whose state is to be computed.
 *  \param cache The open package cache associated with pkg.
 *  \param ignore_broken  Never return pkg_broken; instead
 *                        return whatever status the package
 *                        would have if it wasn't broken.
 */
pkg_action_state find_pkg_state(pkgCache::PkgIterator pkg,
				aptitudeDepCache &cache,
                                bool ignore_broken = false);
// A utility routine to return a useful notion of a package's "action-state"
// and an enum associated with it

bool pkg_obsolete(pkgCache::PkgIterator pkg);
// Returns true if the package is "obsolete".

/** Finds the OR group enclosing the given (forward or reverse) dependency.
 *
 *  The range [start,end) is the OR group when this terminates.
 *  \param cache the cache file in which to operate, or \b NULL to use
 *               apt_cache_file.
 */
void surrounding_or(pkgCache::DepIterator dep,
		    pkgCache::DepIterator &start,
		    pkgCache::DepIterator &end,
		    pkgCache *cache = NULL);


/** \return a short description string corresponding to the given
 *  version.
 */
std::wstring get_short_description(const pkgCache::VerIterator &ver,
				   pkgRecords *records);

/** \return a long description string corresponding to the given
 *  version.
 */
std::wstring get_long_description(const pkgCache::VerIterator &ver,
				  pkgRecords *records);

/** \return true if pkg is suggested by another package which will be
 *  installed.
 */
bool package_suggested(const pkgCache::PkgIterator &pkg);

/** \return true if pkg is recommended by another package which will
 *  be installed or upgraded.
 */
bool package_recommended(const pkgCache::PkgIterator &pkg);

/** \return true if the given package version is available solely from
 * a "trusted" source.
 */
bool package_trusted(const pkgCache::VerIterator &ver);

/** \return the package version that is to be installed for the given
 *  package, or an invalid iterator if the package will be removed or
 *  will not be installed.
 *
 *  Handles various corner cases in the apt database.
 */
pkgCache::VerIterator install_version(const pkgCache::PkgIterator &pkg,
				      aptitudeDepCache &cache);

/** \return the first conflict between the given version and a
 * to-be-installed package version, if any.
 *
 *  Breaks are treated as Conflicts for the purposes of this routine.
 *  End iterators are never conflicted.
 */
pkgCache::DepIterator is_conflicted(const pkgCache::VerIterator &ver,
				    aptitudeDepCache &cache);

/** A pair (veriterator,verfile) -- used for building a list of
 *  versions sorted by file location.
 */
typedef std::pair<pkgCache::VerIterator, pkgCache::VerFileIterator> loc_pair;

/** Used to compare two version files based on their location. */
struct location_compare
{
  bool operator()(loc_pair a, loc_pair b) const
  {
    if(a.second->File == b.second->File)
      return a.second->Offset<b.second->Offset;
    else
      return a.second->File<b.second->File;
  }
};

/** \return \b true if the given dependency is "interesting":
 *          specifically, if it's either critical or a Recommends
 *          that's "new" or currently satisfied.
 *
 *  \param d the dependency to test
 *  \param cache the cache in which to check d (used to find out whether
 *         d is currently satisfied).
 */
bool is_interesting_dep(const pkgCache::DepIterator &d,
			pkgDepCache *cache);

/** \return an int representing an architecture's position within the
 *          system's prefered order.  Smaller values indicate greater
 *          preference.
 *
 *  The order is defined by APT::Architectures with "all" considered
 *  to be of preference `-1`.
 *
 *  \param a the architecture of interest.
 */
int get_arch_order(const char *a);

/** Sort architectures by order of preference. */
struct arch_lt
{
public:
  bool operator()(const char *a1,
                  const char *a2) const
  {
    if(a1 == a2)
      return false;
    return get_arch_order(a1) < get_arch_order(a2);
  }
};

/** Sort packages by their full name (incl. architecture). */
struct pkg_name_lt
{
  struct arch_lt alt;
public:
  bool operator()(const pkgCache::PkgIterator &p1,
		  const pkgCache::PkgIterator &p2) const
  {
    const int order = strcmp(p1.Name(), p2.Name());
    if(order == 0)
      return alt(p1.Arch(), p2.Arch());
    return order < 0;
  }
};

/** Compare two packages by memory location (useful for inserting into
 *  maps when the particular order is uninteresting).
 *
 *  It is safe to use < and not std::less here because the pointers
 *  share an array in the package cache.
 */
struct pkg_ptr_lt
{
public:
  bool operator()(const pkgCache::PkgIterator &p1,
                  const pkgCache::PkgIterator &p2) const
  {
    return &*p1 < &*p2;
  }
};

/** Sort versions by package name. */
struct ver_name_lt
{
  pkg_name_lt plt;
public:
  bool operator()(const pkgCache::VerIterator &v1,
		  const pkgCache::VerIterator &v2) const
  {
    return plt(v1.ParentPkg(), v2.ParentPkg());
  }
};

/** Compare two versions by memory location (useful for inserting into
 *  maps when the particular order is uninteresting).
 *
 *  It is safe to use < and not std::less here because the pointers
 *  share an array in the package cache.
 */
struct ver_ptr_lt
{
public:
  bool operator()(const pkgCache::VerIterator &v1,
		  const pkgCache::VerIterator &v2) const
  {
    return &*v1 < &*v2;
  }
};

/** Sort dependency types in the order:
 *    Obsoletes, Replaces, Suggests, Breaks, Conflicts, Recommends,
 *    Depends, PreDepends.
 */
int get_deptype_order(const pkgCache::Dep::DepType t);

struct deptype_lt
{
public:
  bool operator()(const pkgCache::Dep::DepType t1,
                  const pkgCache::Dep::DepType t2) const
  {
    return get_deptype_order(t1) < get_deptype_order(t2);
  }
};

/** \return \b true if the given dependency type is Conflicts or
 *  Breaks.
 *
 *  We need this overload because the Type field of the dependency
 *  structure is an unsigned char, not a DepType.
 */
inline bool is_conflict(unsigned char type)
{
  return
    type == pkgCache::Dep::Conflicts ||
    type == pkgCache::Dep::DpkgBreaks;
}

/** \return \b true if the given dependency type is Conflicts or
 *  Breaks.
 */
inline bool is_conflict(pkgCache::Dep::DepType type)
{
  return is_conflict(static_cast<unsigned char>(type));
}

// TODO: This belongs upstream.
/** \return a string describing the multi-arch type.
 */
const char *multiarch_type(unsigned char type);

namespace aptitude
{
  namespace apt
  {
    /** \return \b true if the given dependency is a Replaces dependency
     *  and participates in a conflicts/provides/replaces relationship.
     *
     *  Note that replaces and conflicts relationships without a
     *  version aren't considered; should they be?
     */
    bool is_full_replacement(const pkgCache::DepIterator &dep);

    /** \return an ordered vector of the Top Sections
     *
     *  From the configuration item Aptitude::Sections::Top-Sections
     *  or a builtin list of defaults.
     */
    const std::vector<std::string> get_top_sections(const bool cached=true);

    /** \return \b true if the given package is for the native
     *  architecture.
     */
    bool is_native_arch(const pkgCache::VerIterator &ver);

    inline bool is_foreign_arch(const pkgCache::VerIterator &ver)
    {
      return !is_native_arch(ver) && (strcmp(ver.Arch(), "all") != 0);
    }
  }
}

#endif
