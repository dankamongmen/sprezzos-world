// aptcache.h  -*-c++-*-
//
//  Copyright 1999-2005, 2007-2009, 2011 Daniel Burrows
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

#ifndef APTCACHE_H
#define APTCACHE_H

#include <config.h>

#include <cwidget/generic/util/bool_accumulate.h>

#include <apt-pkg/depcache.h>
#include <apt-pkg/pkgrecords.h>

#include <sigc++/signal.h>
#include <sigc++/trackable.h>

#include <iostream>
#include <map>
#include <set>
#include <vector>

/** \brief Replacements for the Apt cache and dependency cache file classes
 * 
 * 
 *  Replacements for the Apt cache and dependency cache file classes.  Those
 *  are lacking in a couple of crucial ways (eg: persistent state), so I
 *  have to come up with my own set of routines.. :(
 *  
 *  In order to track the appropriate package information, we must keep our
 *  own file (I think..apt doesn't have hooks for adding stuff..) containing
 *  that information, properly lock/unlock it, etc.
 * 
 * \file aptcache.h
 */

class undoable;
class undo_group;
class pkgProblemResolver;
class aptitude_universe;
template<typename PackageUniverse> class generic_solution;

class aptitudeDepCache:public pkgDepCache, public sigc::trackable
{
  typedef int user_tag_reference;

public:
  /** This is a general enum that's used for several purposes by the
   *  extended state.  Not every value is valid in every case.
   */
  enum changed_reason {manual, user_auto, libapt, from_resolver, unused};

  /** \brief An opaque type used to store references into the
   *  user-tags list.
   *
   *  We only store one copy of each tag string, to save space.
   */
  class user_tag
  {
    friend class aptitudeDepCache;
    user_tag_reference tag_num;
    explicit user_tag(const user_tag_reference &_tag_num) : tag_num(_tag_num)
    {
    }

  public:
    bool operator==(const user_tag &other) const
    {
      return tag_num == other.tag_num;
    }

    bool operator<(const user_tag &other) const
    {
      return tag_num < other.tag_num;
    }
  };

  /** This structure augments the basic depCache state structure to
   *  support special aptitude features.
   */
  struct aptitude_state
  {
    /** The current action stored on the package. */
    pkgCache::State::PkgSelectedState selection_state;

    /** Stores the version, if any, that the user explicitly selected.
     */
    std::string candver;

    /** Stores the version, if any, which the user has forbidden
     *  aptitude to choose as an upgrade target.  (handles a situation
     *  unstable users are familiar with, where package X version V is
     *  broken, so you want to hold at V-1 until V+1 is available..)
     *
     *  If this string is empty, no "forbid" qualifier is in place.
     */
    std::string forbidver;

    /** \brief Stores the tags attached to this package by the user. */
    std::set<user_tag> user_tags;

    /** If the package is going to be removed, this gives the reason
     *  for the removal.
     */
    changed_reason remove_reason:3;

    /** True if the package is a "new" package.  This is set on every
     *  package the first time it is seen, and remains set until
     *  forget_new_packages() is called.
     *
     *  If this field is missing in the state file, it defaults to \b
     *  false.
     */
    bool new_package:1;

    /** \brief True if an aptitude flag from the last run indicates
     *  that this package should be set to automatically installed.
     *
     *  This flag is used to migrate old settings and to flag queued
     *  installations as automatic.
     *
     *  \warning This value is stored here to get it from the code
     *  that interprets pkgstates to the code that sets up package
     *  states; user code should not assume that it is meaningful.
     */
    bool previously_auto_package:1;

    /** If the package is in state Install and is upgradable, this
     *  field determines whether or not it should be upgraded.  This
     *  field is not updated as the package is modified; it is used
     *  when the package states are initially loaded, then ignored.
     *
     *  This solves the problem of "part of my upgrade failed and
     *  aptitude forgot I wanted to upgrade anything!".  I am
     *  concerned about its behavior if the package cache is updated
     *  between upgrade runs, and I have considered indicating which
     *  version each package should be upgraded to...but I haven't yet.
     *
     *  If this field is missing in the state file, it defaults to \b
     *  false.
     */
    bool upgrade:1;

    /** If \b true, the package in question will be reinstalled.
     *
     *  This member is not saved to the state file.
     */
    bool reinstall:1;

    /** If \b true, the package in question is tagged for an action.
     *
     *  This member is not saved to the state file.
     */
    bool tagged:1;

    /** If \b true, the package in question is flagged for future reference. */
    bool flagged:1;
  };

  /** \brief Represents a group of aptitude actions. */
  class action_group
  {
    /** The parent group.  This is a member and not a parent class so
     *  that I can force its cleanup routines to run before mine.
     */
    pkgDepCache::ActionGroup *parent_group;

    aptitudeDepCache &cache;

    undo_group *group;

    action_group(const action_group &other);
  public:
    /** \brief Create a new action group.
     *
     *  \param cache  The package cache on which to act.
     *  \param group  The undo group to add changes to, or NULL to not remember
     *                changes.
     */
    action_group(aptitudeDepCache &cache, undo_group *group = NULL);

    ~action_group();
  };

  /** This flag is \b true iff the persistent state has changed (ie, we
   *  need to save the cache).
   */
  bool dirty;

  /** This flag is \b true if the cache is in 'read-only' mode.
   */
  bool read_only;

  // Some internal classes for undo information
  class apt_undoer;
  class forget_undoer;
  friend class apt_undoer;
  friend class forget_undoer;

  class candver_undoer;
  friend class candver_undoer;

  /** This class "remembers" the current cache state, and is used to
   *  perform post facto analysis of the decisions libapt makes.
   *  (thus enabling the "undo" function)
   */
  class apt_state_snapshot
  {
    StateCache *PkgState;
    unsigned char *DepState;
    aptitude_state *AptitudeState;
    signed long long iUsrSize;
    unsigned long long iDownloadSize;
    unsigned long iInstCount;
    unsigned long iDelCount;
    unsigned long iKeepCount;
    unsigned long iBrokenCount;
    unsigned long iBadCount;

  private:
    apt_state_snapshot():PkgState(NULL), DepState(NULL), AptitudeState(NULL) {}

  public:
    ~apt_state_snapshot()
    {
      delete[] PkgState;
      delete[] DepState;
      delete[] AptitudeState;
    }

    friend class aptitudeDepCache;
  };

  const std::string &deref_user_tag(const user_tag &tag) const
  {
    return user_tags[tag.tag_num];
  }
private:
  void parse_user_tags(std::set<user_tag> &tags,
		       const char *&start, const char *end,
		       const std::string &package_name);

  aptitude_state *package_states;
  // To speed the program up and save memory, I only store one copy of
  // each distinct tag, and keep a reference to this list.  The list
  // is not managed especially intelligently: if you repeatedly add
  // and remove never-before-seen tags to a package, it will grow
  // without bound.  I don't consider this a problem, because users
  // are unlikely to add "very many" (say, more than a few hundred)
  // tags in a single session.  If it does become a problem, tags can
  // be reference-counted, at the expense of maintaining an explicit
  // free list.
  std::vector<std::string> user_tags;
  // Stores the reference corresponding to each string.
  std::map<std::string, user_tag_reference> user_tags_index;
  // Read a set of user tags from the given string region
  // and write the tags into the index and into the given
  // set of tags.
  void parse_usertags(std::set<user_tag_reference> &tags,
		      const char *&start, const char *end);

  int lock;
  // The lock on the extra-info file.
  int group_level;
  // The current 'group level' -- how many times start_action_group has been
  // called without a matching end_action_group.

  /** The number of "new" packages. */
  int new_package_count;

  apt_state_snapshot backup_state;
  // Stores what the cache was like just before an action was performed

  pkgRecords *records;

  /** Call whenever the cache state is modified; discards the
   *  state of the active resolver.
   *
   *  \param undo the undo group with which this discarding should be
   *  associated.
   */
  void discard_resolver(undo_group *undo);

  /** Call whenever a new resolver should be instantiated. */
  void create_resolver();

  undoable *state_restorer(PkgIterator pkg, StateCache &state, aptitude_state &ext_state);
  // Returns an 'undoable' object which will restore the given package to the
  // given state via {Mark,Set}* routines

  void duplicate_cache(apt_state_snapshot *target);
  // This makes the **ASSUMPTION** that if the target's tables aren't
  // NULL, they're properly sized..

  void cleanup_after_change(undo_group *undo,
			    std::set<pkgCache::PkgIterator> *changed_packages,
			    bool alter_stickies=true);
  // Finds anything that magically changed and creates an undo item for it..
  // If alter_stickies is false, sticky states will be left alone.  (hack :( )

  void MarkFromDselect(const PkgIterator &Pkg);
  // Marks the package based on its current status and its dselect state,
  // adjusting its selected state as appropriate.

  // The following methods just perform the core part of the named
  // action, without creating a new action group or running
  // mark&sweep.

  /**
   * Use this instead of pkgDepCache::MarkInstall; it ensures that the
   * package's auto flag is set properly.
   */
  void internal_mark_install(const PkgIterator &Pkg, bool AutoInst, bool ReInstall);
  void internal_mark_delete(const PkgIterator &Pkg, bool Purge, bool unused_delete);
  void internal_mark_keep(const PkgIterator &Pkg, bool Automatic, bool SetHold);

  /** Handle changing package states to take into account the garbage
   *  collector's output.  Uses the core pkgDepCache methods.
   */
  void sweep();
  void begin_action_group();
  void end_action_group(undo_group *undo);
public:
  /** Create a new depcache from the given cache and policy.  By
   *  default, the depcache is readonly if and only if it is not
   *  locked.
   */
  aptitudeDepCache(pkgCache *cache, Policy *Plcy=0);

  bool Init(OpProgress *Prog, bool WithLock,
	    bool do_initselections, const char * status_fname=NULL);

  bool is_locked() {return lock!=-1;}

  bool is_dirty() {return dirty;}

  pkgRecords &get_records() { return *records; }

  // If do_initselections is "false", the "sticky states" will not be used
  // to initialize packages.  (important for the command-line mode)
  bool build_selection_list(OpProgress &Prog, bool WithLock,
			    bool do_initselections,
			    const char * status_fname=NULL);

  void forget_new(undoable **undoer);
  // Clears all information about which packages are 'new'.  Overwrites undoer
  // if it is not NULL

  /** Sets the "new" flag for the given package; use this instead of
   *  directly manipulating the cache so the count of new packages is
   *  updated. (gross?)
   */
  void set_new_flag(const pkgCache::PkgIterator &pkg, bool is_new);

  /** Gets the number of new packages. */
  int get_new_package_count() const {return new_package_count;}

  inline aptitude_state &get_ext_state(const PkgIterator &Pkg)
  {return package_states[Pkg->ID];}

  bool save_selection_list(OpProgress &prog, const char *status_fname=NULL);
  // If the list isn't locked (or an fd isn't provided), is a NOP.

  void mark_install(const PkgIterator &Pkg, bool AutoInst, bool ReInstall, undo_group *undo);
  void mark_delete(const PkgIterator &Pkg, bool Purge, bool unused_delete, undo_group *undo);

  /** \brief Keep the given package at its current version.
   *
   *  \param Pkg The package to keep.
   *
   *  \param Automatic If \b true, then this is an automatically
   *  triggered action.
   *
   *  \param SetHold If \b true, then a sticky hold will be set on
   *  this package.
   *
   *  \param undo The undo group into which actions should be placed.
   */
  void mark_keep(const PkgIterator &Pkg, bool Automatic, bool SetHold, undo_group *undo);

  void set_candidate_version(const VerIterator &TargetVer, undo_group *undo);
  // These just wrap the equivalent depCache functions for the UI's benefit;
  // they mark what the user wants done with each package.
  //  If an undo group is passed in, items are added to it for each of the
  // actions taken.

  /** Forbids an upgrade to the given version.  If the package is currently
   *  marked for upgrade to that version, it is kept back.
   */
  void forbid_upgrade(const pkgCache::PkgIterator &pkg,
		      std::string verstr, undo_group *undo);

  /** Marks all upgradable and non-held packages for upgrade.
   *
   *  \param with_autoinst if \b true, the dependencies of packages
   *  begin upgraded will automatically be installed.
   *
   *  \param ignore_removed if \b false, all upgradable packages that
   *  are not held back will be upgraded; otherwise, packages that are
   *  going to be removed will be ignored.
   *
   *  \param undo an undo group with which the actions taken by this
   *  routine will be registered, or \b NULL.
   */
  void mark_all_upgradable(bool with_autoinst, bool ignore_removed,
			   undo_group *undo);

  /** \brief Retrieve the set of packages that mark_all_upgradable
   *  would attempt to upgrade.
   *
   *  \param ignore_removed if \b false, all upgradable packages that
   *  are not held back will be upgraded; otherwise, packages that are
   *  going to be removed will be ignored.
   *
   *  \param upgradable A location in which to place the set of
   *  currently upgradable packages.
   */
  void get_upgradable(bool ignore_removed, std::set<pkgCache::PkgIterator> &upgradable);

  void mark_single_install(const PkgIterator &pkg, undo_group *undo);
  // Marks this package to be install, and all other packages to be kept.
  //  The "keep" on the other packages, however, is NOT sticky and will NOT
  // be saved in the extended state file! (this is a bit of a hack..)
  // (this could be fairly easily emulated; it's a convenience routine)

  void mark_auto_installed(const PkgIterator &pkg,
			   bool set_auto,
			   undo_group *undo);

  /** \brief Attach a tag to a package.
   *
   *  The tag will be added to the user_tags member of the package's
   *  extended state.
   */
  void attach_user_tag(const PkgIterator &pkg, const std::string &tag,
		       undo_group *undo);

  /** \brief Remove a tag from a package.
   *
   *  The tag will be removed from the user_tags member of the
   *  package's extended state.  If it isn't already present, nothing
   *  will happen.
   */
  void detach_user_tag(const PkgIterator &pkg, const std::string &tag,
		       undo_group *undo);

  // Marks the given package as having been autoinstalled (so it will be
  // removed automatically) or having been installed manually.

  /** Retrieve the read-only flag. */
  bool get_read_only() const { return read_only; }

  /** Set the read-only flag.  If the cache is read-only, then any
   *  attempt to modify a package's state will instead call the
   *  read_only_alert() signal, which can either allow the
   *  modification to continue or cancel it.
   */
  void set_read_only(bool new_read_only);

  /** Apply the given solution as a resolver result; any actions
   *  that it requests will be marked as having been performed to
   *  fulfill dependencies.
   *
   *  \param solution   the solution that is to be applied.  A local
   *                    copy will be made (thus incrementing the
   *                    reference count on the underlying
   *                    representation) so this routine is safe if one
   *                    of the signals it emits destroys the solution
   *                    object.
   *
   *  \param undo       the undo group to which any undo actions
   *                    generated by applying the solution should
   *                    be added.
   */
  void apply_solution(const generic_solution<aptitude_universe> &solution,
		      undo_group *undo);

  /** \return \b true if automatic aptitude upgrades should ignore this
   *  package.
   */
  bool is_held(const PkgIterator &pkg);

  bool all_upgrade(bool with_autoinst, undo_group *undo);
  // Wrapper for pkgAllUpgrade (the engine of "apt-get upgrade")

  bool try_fix_broken(undo_group *undo);
  // Attempts to fix any broken packages, dumping the changes the problem
  // fixer creates into the given undo group.

  bool try_fix_broken(pkgProblemResolver &fixer, undo_group *undo);
  // Just runs the resolver given and catches automatic changes.
  // (this lets callers customize the information given to the resolver)

  const apt_state_snapshot *snapshot_apt_state();
  // Returns the current state of the *APT* cache (no information about
  // Aptitude states is included); this is meant to be used to implement undo,
  // detection of "wtf happened??" after the problem resolver runs, etc.
  void restore_apt_state(const apt_state_snapshot *snapshot);
  // Restores the *APT* cache to the given state.

  /** This signal is emitted *before* any package's install state is
   *  changed.  It may be emitted more than once per state change; if
   *  no states actually change, it might not be emitted at all.
   */
  sigc::signal0<void> pre_package_state_changed;

  /** This signal is emitted when any package's install state is
   *  changed.
   */
  sigc::signal0<void> package_state_changed;

  /** \brief This signal is emitted when any changes to a package's state
   *  that might need to trigger a redraw of that package take place.
   */
  sigc::signal1<void, const std::set<pkgCache::PkgIterator> *> package_states_changed;

  // Emitted when a package's categorization is potentially changed.
  // (in particular, when package "new" states are forgotten)
  sigc::signal0<void> package_category_changed;

  /** This signal is emitted when the user attempts to modify the
   *  cache while it is in read-only mode.  If any callback returns \b
   *  false, the modification is cancelled.
   */
  sigc::signal0<bool, cwidget::util::accumulate_and> read_only_permission;

  /** This signal is emitted when a read-only operation fails.  It is
   *  emitted exactly once for every action group.
   */
  sigc::signal0<void> read_only_fail;

  /** \name GC control methods */
  // @{

  InRootSetFunc *GetRootSetFunc();
  bool MarkFollowsRecommends();
  bool MarkFollowsSuggests();

  // @}

  bool IsInstallOk(const pkgCache::PkgIterator &pkg,
		   bool AutoInst,
		   unsigned long Depth,
		   bool FromUser);

  bool IsDeleteOk(const pkgCache::PkgIterator &pkg,
		  bool Purge,
		  unsigned long Depth,
		  bool FromUser);

  virtual ~aptitudeDepCache();
};

class pkgPolicy;

class aptitudeCacheFile
// Hack around problems in libapt.  Most of the code associated with this
// class was copied directly from libapt and reindented..
{
  MMap *Map;

  pkgCache *Cache;
  aptitudeDepCache *DCache;

  bool have_system_lock;
  // hm, used to make it look like the old stuff?
public:

  pkgPolicy *Policy;

  // We look pretty much exactly like a pointer to a dep cache
  inline operator pkgCache &() {return *Cache;};
  inline operator pkgCache *() {return Cache;};
  inline operator aptitudeDepCache &() {return *DCache;};
  inline operator aptitudeDepCache *() {return DCache;};
  inline aptitudeDepCache *operator ->() {return DCache;};
  inline aptitudeDepCache &operator *() {return *DCache;};
  inline aptitudeDepCache::StateCache &operator [](pkgCache::PkgIterator const &I) {return (*DCache)[I];};
  inline unsigned char &operator [](pkgCache::DepIterator const &I) {return (*DCache)[I];};

  bool Open(OpProgress &Progress, bool do_initselections, bool WithLock=true,
	    const char * status_fname=NULL);
  bool is_locked() {return have_system_lock;} // EWW (also not quite right)

  void ReleaseLock();
  bool GainLock();

  aptitudeCacheFile();
  ~aptitudeCacheFile();
};

#endif
