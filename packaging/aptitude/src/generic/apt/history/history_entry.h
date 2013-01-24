// history_entry.h         -*-c++-*-
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

#ifndef HISTORY_ENTRY_H
#define HISTORY_ENTRY_H

/** \file history_entry.h */

#include <apt-pkg/pkgcache.h> // For enumerations.

#include <generic/apt/aptcache.h>

#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/util/ref_ptr.h>

namespace aptitude
{
  /** \brief Interfaces dedicated to storing the history of the user's actions. */
  namespace history
  {
    // \todo String values here should maybe be interned.  Suggest
    // creating imm::interned_string_pool and imm::interned_string
    // over in util/ and using them to reduce the amount of copying if
    // this becomes a problem.  In fact, a generic interning framework
    // might be in order; it would be useful to be able to intern
    // dependency objects, for instance.

    /** \brief An exception related to the history code. */
    class HistoryException : public cwidget::util::Exception
    {
      std::string msg;

    public:
      /** \brief Create a history exception.
       *
       *  \param _msg  The error message associated with this exception.
       */
      HistoryException(const std::string &_msg);

      /** \brief Retrieve the error message associated with this exception. */
      std::string errmsg() const;
    };

    /** \brief A dependency as known to the history system.
     *
     *  Dependencies cannot be stored as apt DepIterators, because the
     *  dependency that historically caused an action may no longer
     *  exist when the action is examined later.  Instead, we use
     *  package names and version strings to identify dependencies.
     *
     *  Dependencies are serialized as "foo Depends {bar (1.5), ...}".
     *
     *  \todo Reduce the amount of implicit list-copying that
     *  dependencies do (maybe make them refcounted and use swap() to
     *  initialize their internal list of targets?).
     */
    class dep
    {
    public:
      /** \brief One target of the dependency.
       *
       *  As with dpkg dependencies, this includes a name, a
       *  comparison operator and a version string.
       */
      class target
      {
	std::string name;
	std::string version;
	/** We use the same trick as apt to avoid nested lists: the OR
	 *  bit is set on each target but the last in an OR group.
	 */
	pkgCache::Dep::DepCompareOp op;

      public:
	/** \brief Create a new dependency target.
	 *
	 *  \param _name     The name of the target package.
	 *  \param _op       The version comparison operator, including
	 *                   the OR flag.
	 *  \param _version  The string being compared against, if any.
	 */
	target(const std::string &_name,
	       pkgCache::Dep::DepCompareOp _op,
	       const std::string &_version)
	  : name(_name),
	    version(_version),
	    op(_op)
	{
	}

	/** \brief Get the name of the target package. */
	const std::string &get_name() const { return name; }
	/** \brief Get the comparison operation that restricts this
	 *  dependency, if any.
	 *
	 *  If there is no comparison operation, will return
	 *  pkgCache::Dep::NoOp.
	 */
	pkgCache::Dep::DepCompareOp get_compare_op() const { return static_cast<pkgCache::Dep::DepCompareOp>(op & ~pkgCache::Dep::Or); }

	/** \brief Check whether this is the last target in the
	 *  current OR group.
	 *
	 *  \return \b true if this is the last target in its OR group,
	 *  or \b false if the next target is in the same OR group.
	 */
	bool get_is_end_of_or_group() const { return (op & pkgCache::Dep::Or) == 0; }
	
	/** \brief Get the version string used in the comparison (if
	 *  there is no comparison, this returns the empty string).
	 */
	const std::string &get_version() const { return version; }
      };

    private:
      pkgCache::Dep::DepType type;
      std::string source;
      std::vector<target> targets;

    public:
      /** \brief Create a new dependency.
       *
       *  \param _source  The package that declared this dependency.
       *  \param _type    The type of this dependency.
       *  \param _targets The targets of this dependency.
       */
      dep(const std::string &_source, pkgCache::Dep::DepType _type,
	  const std::vector<target> &_targets)
	: type(_type), source(_source), targets(_targets)
      {
      }

      /** \brief Get the type of this dependency (Depends, Conflicts, etc). */
      pkgCache::Dep::DepType get_type() const { return type; }
      /** \brief Get the package that declared this dependency. */
      const std::string &get_source() const { return source; }
      /** \brief Get the targets of this dependency. */
      const std::vector<target> &get_targets() const { return targets; }
    };

    /** \brief An action performed by the resolver.
     *
     *  From the point of view of effect, there are really only two
     *  types of action: "change the target version of package P from
     *  V1 to V2" and "leave the dependency D unresolved".  I include
     *  three branches for the first type of action because this
     *  mirrors the format used to serialize actions in the log, and
     *  because it might be useful to retrospectively know the
     *  difference between installing V and holding a package at
     *  version V, or between removing P and cancelling its
     *  installation.
     */
    class resolver_action
    {
    public:
      /** \brief What the resolver did. */
      enum type
	{
	  /** \brief The resolver installed a package. */
	  install,
	  /** \brief The resolver kept a package at its current version. */
	  keep,
	  /** \brief The resolver removed a package. */
	  remove,
	  /** \brief The resolver left a Recommends unresolved. */
	  unresolved
	};

    private:
      type tp;
      // The package affected by this action.
      std::string package;
      std::string target_version;
      std::string current_version;
      dep dependency;

      /** \brief Create a resolver action.
       *
       *  \param _tp       The type of the action to create.
       *  \param _package  The package the action relates to.
       *  \param _target_version   The package version that is now to be
       *                           installed, or an empty string if none.
       *  \param _current_version  The package version that was to be
       *                           installed, or an empty string if none.
       *  \param _dependency       The dependency related to this action.
       */
      resolver_action(type _tp,
		      const std::string &_package,
		      const std::string &_target_version,
		      const std::string &_current_version,
		      const dep &_dependency)
	: tp(_tp),
	  package(_package),
	  target_version(_target_version),
	  current_version(_current_version),
	  dependency(_dependency)
      {
      }

    public:
      /** \brief Create an "install" resolver action.
       *
       *  \param package         The package being installed.
       *  \param current_version The package version that is currently
       *                         to be installed, or an empty string if
       *                         the package is to be not-installed.
       *  \param target_version  The package version that will
       *                         be installed.
       *  \param dependency      The dependency that this action
       *                         resolves.
       */
      static resolver_action make_install(const std::string &package,
					  const std::string &current_version,
					  const std::string &target_version,
					  const dep &dependency)
      {
	return resolver_action(install, package, current_version, target_version, dependency);
      }

      /** \brief Create a "keep" resolver action.
       *
       *  \param package         The package being kept at its current version.
       *  \param current_version The package version that was to be
       *                         installed, or an empty string if the
       *                         package was to be removed.
       *  \param target_version  The version that this package is being
       *                         kept at, or the empty string if it
       *                         isn't currently installed.
       *  \param dependency      The dependency that this action
       *                         resolves.
       *
       *  Note that in this case, the "current" version is actually
       *  not the currently installed version; it's the version that
       *  was *to be* installed (or an empty string if the package is
       *  being removed).  This consistency with "install" means that
       *  we can always "roll back" a resolver action by targeting the
       *  "current" version of the package.
       */
      static resolver_action make_keep(const std::string &package,
				       const std::string &current_version,
				       const std::string &target_version,
				       const dep &dependency)
      {
	return resolver_action(keep, package, current_version, target_version, dependency);
      }

      /** \brief Create a "remove" resolver action.
       *
       *  \param package         The package being removed.
       *  \param current_version The package version that was to be
       *                         installed, or an empty string if the
       *                         package was to be removed.
       *  \param target_version  The version that this package is being
       *                         kept at, or the empty string if it
       *                         isn't currently installed.
       *  \param dependency      The dependency that this action
       *                         resolves.
       */
      static resolver_action make_remove(const std::string &package,
					 const std::string &current_version,
					 const dep &dependency)
      {
	return resolver_action(remove, package, current_version, std::string(), dependency);
      }

      /** \brief Create an "unresolved" resolver action.
       *
       *  \param dependency  The dependency that was left unresolved.
       *
       *  The created action indicates that applying a dependency
       *  solution didn't solve a particular dependency.
       */
      static resolver_action make_unresolved(const dep &dependency)
      {
	return resolver_action(unresolved, std::string(), std::string(), std::string(), dependency);
      }

      /** \brief Get the type of this action. */
      type get_type() const { return tp; }

      /** \brief Get the "target" version of this action.
       *
       * This is the version of the package that will be installed
       * after the action is performed.  For "keep", this is empty if
       * the package is not currently installed; for "remove" and
       * "unresolved", this is always empty.
       */
      const std::string &get_target_version() const { return target_version; }

      /** \brief Get the "current" version of this action.
       *
       * This is the version of the package that was to be installed
       * before the action was performed.  For "install" and "keep",
       * this is empty if the package isn't currently installed.  For
       * "unresolved", this is always empty.
       */
      const std::string &get_current_version() const { return current_version; }

      /** \brief Get the dependency related to this action.
       *
       *  For "install", "keep", and "remove", this is the dependency
       *  that the action resolved; for "unresolved", this is the
       *  dependency that was left unresolved by this dependency
       *  solution.
       */
      const dep &get_dependency() const { return dependency; }
    };

    /** \brief One action that was performed.
     *
     *  For actions that should be reversible, we err on the side of
     *  providing too much information about the previous state, to
     *  make it simpler to ensure that we are able to roll them back.
     *
     *  History entries use some common concepts:
     *
     *   - aptitude state: the current extended aptitude state of a
     *     package.  This consists of several components: the
     *     selection state, the chosen candidate version (if any), the
     *     forbidden version (if any), the reason the package was
     *     marked for removal if it's being removed, whether the
     *     package is flagged for upgrade, and whether the package is
     *     being reinstalled.
     *
     *     User tags are deliberately omitted: they are conceptually
     *     different from the state information, and repeating them
     *     all the time would take up a lot of space if they were used
     *     much.
     *
     *     aptitude states are serialized like this:
     *
     *       selection-state =CANDVER !FORBIDVER
     *
     *     where selection-state is "install", "upgrade", "reinstall",
     *     "delete", "purge", "keep", or "hold", CANDVER is the
     *     selected candidate version number (missing if none) and
     *     FORBIDVER is the forbidden version number (missing if
     *     none).  If the selection-state is "purge" or "delete", the
     *     next word will be either "manual", "libapt",
     *     "from-resolver", or "unused".  Note that we could infer the
     *     *new* remove reason from the type of change being made, but
     *     not the old one.
     *
     *   - Dpkg selected state: the "future state" recorded by
     *     dselect.  One of "unknown", "install", "hold", "deinstall",
     *     and "purge".  We detect changes here by recording the last
     *     seen state in /var/lib/aptitude/pkgstates.
     *
     *   - Dpkg current state: the current status of this package in
     *     the dpkg database.  One of "notinstalled", "unpacked",
     *     "halfconfigured", "halfinstalled", "configfiles",
     *     "installed", "triggersawaited", and "triggerspending".  All
     *     states except "notinstalled" have an attached version.  We
     *     detect changes by recording the last seen state in
     *     /var/lib/aptitude/pkgstates.
     *
     *  \todo Changes to the "auto" flag between aptitude runs won't
     *  be picked up with this system because we don't know the
     *  last-seen value.  Perhaps that should be stored in the
     *  extended states file too?
     */
    class entry
    {
    public:
      /** \name Enumerations */

      // @{

      /** \brief Represents the type of a history entry. */
      enum type
	{
	  /** \brief A package's aptitude state was changed.
	   *
	   *  change <package> <reason> <oldaptitudestate> -> <newaptitudestate>
	   *
	   *  Valid <reason>s are:
	   *
	   *   - dep <dep>
	   *   - dpkg-sync
	   *   - select
	   *   - unknown
	   *   - unused {set-of-requiring-packages}
	   *
	   *  \todo what about selected upgrades?  Should we note what
	   *  version the user is trying to upgrade to/from?
	   */
	  change,

	  /** \brief A package's actual status was changed by dpkg.
	   *
	   *  dpkg-change <package> <oldselectedstate> <oldcurrentstate> -> <selectedstate> <currentstate>
	   *
	   *  These are generated on startup, when the cache is
	   *  initially loaded.
	   */
	  dpkg_change,

	  /** \brief One or more history entries, grouped together
	   * because they were performed as a single action.
	   *
	   *  group:
	   *  # action1
	   *  ...
	   *
	   *  Actions within a group are prefixed by a pound sign
	   *  ("#").  Actions within two groups are prefixed by two
	   *  pound signs.  ("##") The purpose of the pound signs is
	   *  to add a little robustness against history corruption:
	   *  if some lines are lost, we can detect when a group ends
	   *  by checking the number of pound signs at the front of
	   *  each line; if a group header is lost, we can guess that
	   *  a group is happening without knowing the exact type of
	   *  the group.
	   */
	  group,

	  /** \brief The "new" flag of the package was changed.
	   *
	   *  new-changed <package> <old_new> -> <new_new>
	   *
	   *  old_ and new_new are "Y" or "N".
	   */
	  new_changed,

	  /** \brief aptitude is about to run dpkg.
	   *
	   *  plan-dpkg <package> <oldselectedstate> <oldcurrentstate> -> <planselectedstate> <plancurrentstate>
	   */
	  plan_dpkg,


	  /** \brief One or more actions were redone.
	   *
	   *  The actions stored here are the actions that were
	   *  performed as part of the redo process.
	   *
	   *  redo:
	   *  # action1
	   *  ...
	   */
	  redo,

	  /** \brief A resolver solution was applied.
	   *
	   *  resolver-apply:
	   *  # install <package> ([<version> ->] <version>) for <dep>
	   *  # keep <package> ([<version> ->] <version) for <dep>
	   *  # remove <package> (<version>) for <dep>
	   *  # unresolved <dep>
	   *
	   *  The solution is serialized with all information
	   *  preserved, including the dependency attached to each
	   *  action.  Actions are written in the order they were
	   *  inserted into the solution.
	   *
	   *  \todo Still need to figure out whether this is enough
	   *  information -- probably the aptitude-state-change needs
	   *  to be included too.  Do I try to associate changes with
	   *  deps?  I think so.  What about just including the
	   *  resolver information in the normal change message,
	   *  instead of having a special "resolver-apply" entry?  But
	   *  then I couldn't group together changes from the same
	   *  solution, not so great.  (unless I used "group", but
	   *  that doesn't provide the semantic information that it's
	   *  a resolver application)
	   */
	  resolver_apply,

	  /** \brief One or more actions were rolled back.
	   *
	   *  Rollbacks are distinct from the undo/redo system; only
	   *  the most recent action can be undone, while *any* action
	   *  can be rolled back (but the state might not be what it
	   *  was prior to that action).  Rollbacks *can* be undone /
	   *  redone.
	   *
	   *  rollback:
	   *  # action1
	   *  ...
	   *
	   *  \todo A way of uniquely identifying the target of the
	   *  rollback could be useful.  Actions have timestamps;
	   *  adding a (threadsafe yada yada) sequence number to that
	   *  would let us say *which* action was rolled back.
	   */
	  rollback,

	  /** \brief One or more actions were undone.
	   *
	   *  The actions stored here are the actions that were
	   *  performed as part of the "undo" process.
	   *
	   *  undo:
	   *  # action1
	   *  ...
	   */
	  undo,

	  /** \brief The package's user tags were changed.
	   *
	   *  user-tags {old-tags} -> {new-tags}
	   */
	  user_tags
	};

      /** \brief Represents the reason a change was made. */
      enum change_reason_enum
	{
	  change_dep,
	  change_dpkg_sync,
	  change_select,
	  change_unknown,
	  change_unused
	};

      /** \brief Represents the aptitude selection state of a package.
       *
       *  A separate enum is used here in case the legal values
       *  change, and also so we can distinguish between "keep" and
       *  "hold".
       */
      enum selection_state_enum
	{
	  select_delete,
	  select_hold,
	  select_install,
	  select_keep,
	  select_purge,
	  select_reinstall,
	  select_upgrade,
	};

      /** \brief Represents why a package was removed.
       *
       *  As before, a separate enum is used for future-proofing:
       *  someday the set of recognized values in aptcache.h might
       *  change and we'll still want to be able to parse old logs.
       */
      enum remove_reason_enum
	{
	  remove_from_resolver,
	  remove_libapt,
	  remove_manual,
	  remove_unused,
	};

      /** \brief Represents the dpkg selected state. */
      enum dpkg_selection_state_enum
	{
	  dpkg_select_deinstall,
	  dpkg_select_hold,
	  dpkg_select_install,
	  dpkg_select_purge,
	  dpkg_select_unknown,
	};

      /** \brief Represents the dpkg current state. */
      enum dpkg_current_state_enum
	{
	  dpkg_current_notinstalled,
	  dpkg_current_unpacked,
	  dpkg_current_halfconfigured,
	  dpkg_current_halfinstalled,
	  dpkg_current_configfiles,
	  dpkg_current_installed,
	  dpkg_current_triggersawaited,
	  dpkg_current_triggerspending,
	};

      // @}

      /** \brief Auxiliary structures. */

      // @{

      class aptitude_state
      {
	selection_state_enum selection_state;
	std::string candver; // Empty if none.
	std::string forbidver; // Empty if none.
	remove_reason_enum remove_reason;

      public:
	aptitude_state(selection_state_enum _selection_state,
		       const std::string &_candver,
		       const std::string &_forbidver,
		       remove_reason_enum _remove_reason)
	  : selection_state(_selection_state),
	    candver(_candver),
	    forbidver(_forbidver),
	    remove_reason(_remove_reason)
	{
	}

	selection_state_enum get_selection_state() const { return selection_state; }
	const std::string &get_candver() const { return candver; }
	const std::string &get_forbidver() const { return forbidver; }
	remove_reason_enum get_remove_reason() const { return remove_reason; }
      };

      // @}

    private:
      type tp;

      std::string package;

      // Used to store the reason for a change.
      change_reason_enum change_reason;

      // Stores the previous aptitude state.
      aptitude_state former_aptitude_state;

      // Stores the new aptitude state.
      aptitude_state next_aptitude_state;

      dpkg_selection_state_enum old_dpkg_selection_state, new_dpkg_selection_state;

      dpkg_current_state_enum old_dpkg_current_state, new_dpkg_current_state;

      bool old_new_flag : 1, new_new_flag : 1;

      std::vector<resolver_action> resolver_actions;

      std::set<aptitudeDepCache::user_tag> old_user_tags, new_user_tags;

      std::vector<cwidget::util::ref_ptr<entry> > sub_entries;

      /** \name change constructor and accessors. */

      // @{

      /** \brief Create a change */
      
    };
  }
}

#endif
