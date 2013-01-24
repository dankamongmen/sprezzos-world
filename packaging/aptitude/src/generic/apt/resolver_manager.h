// resolver_manager.h                                -*-c++-*-
//
//   Copyright (C) 2005, 2007-2009 Daniel Burrows
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
//


#ifndef RESOLVER_MANAGER_H
#define RESOLVER_MANAGER_H

#include <cwidget/generic/threads/threads.h>

#include <cwidget/generic/util/exception.h>

#include <apt-pkg/pkgcache.h>

#include <boost/shared_ptr.hpp>

#include <sigc++/signal.h>
#include <sigc++/trackable.h>

#include <queue>
#include <set>
#include <vector>

#include <generic/util/immset.h>
#include <generic/util/post_thunk.h>

/** \brief A higher-level resolver interface
 *
 * 
 *  A higher-level resolver interface. This code is responsible for
 *  maintaining a list of previously observed solutions, for passing
 *  certain actions on to the underlying resolver (protecting users
 *  from having to actually import the whole resolver definition), for
 *  managing the resolver in the face of cache reloads and resets, and
 *  for managing threaded access to the resolver.
 * 
 *  \file resolver_manager.h
 */

class aptitude_universe;
class aptitude_resolver_package;
class aptitude_resolver_version;
class aptitude_resolver_dep;
class aptitudeCacheFile;
template<typename PackageUniverse> class generic_solution;
template<typename PackageUniverse> class generic_problem_resolver;
class aptitude_resolver;
class undo_group;
class undo_list;

/** Manages a resolver for a single cache object.  When broken
 *  packages arise, a new resolver is created; whenever the state of a
 *  package changes, the resolver is deleted and reset.  While a
 *  resolver is active, users of this class can "select" a particular
 *  solution, then "generate" it.
 *
 *  Solutions can be generated in a background thread, but of course
 *  only one background thread may be running at a time.  The
 *  solutions returned should only be accessed from one thread at a
 *  time unless you clone() them.
 *
 *  Note: of course it would also be possible to simply query the
 *  manager for the Nth solution; however, using a selection pointer
 *  makes it easy for different UI modules to share information about
 *  the currently selected solution.
 */
class resolver_manager : public sigc::trackable
{
public:
  /** This class represents the continuation of get_solution() in a
   *  background thread.  See get_background_solution() for details.
   */
  class background_continuation
  {
  public:
    virtual ~background_continuation();

    /** Invoked when a solution has been successfully generated. */
    virtual void success(const generic_solution<aptitude_universe> &) = 0;
    /** Invoked when all solutions have been exhausted (corresponds to
     *  the NoMoreSolutions exception).
     */
    virtual void no_more_solutions() = 0;

    /** Invoked when time has expired. (corresponds to NoMoreTime) */
    virtual void no_more_time() = 0;

    /** Invoked when the solver was interrupted. (corresponds to
     *  InterruptedException)
     */
    virtual void interrupted() = 0;

    /** Invoked when a fatal exception was thrown. */
    virtual void aborted(const std::string &errmsg) = 0;
  };

  /** A snapshot of the state of the resolver. */
  struct state
  {
    /** The currently selected solution. */
    int selected_solution;

    /** The number of already-generated solutions. */
    int generated_solutions;

    /** If \b true, then there are no more solutions to generate. */
    bool solutions_exhausted;

    /** If \b true, then the resolver is not \b null (i.e., it exists;
     *  i.e., there are broken packages).
     */
    bool resolver_exists;

    /** If \b true, the background thread has jobs. */
    bool background_thread_active;

    /** If \b true, then background_thread_active is \b false and
     *  the background thread was killed by an uncaught exception.
     */
    bool background_thread_aborted;

    /** If background_thread_aborted is \b true, holds the error
     *  message that killed the background thread.
     */
    std::string background_thread_abort_msg;

    /** The size of the resolver's open queue. */
    size_t open_size;

    /** The size of the resolver's closed queue. */
    size_t closed_size;

    /** The size of the resolver's deferred queue. */
    size_t deferred_size;

    /** The number of conflicts discovered by the resolver. */
    size_t conflicts_size;
  };

private:
  /** \brief Remembers a single user interaction with the resolver.
   *
   *  This is used to save a debug trace.
   */
  class resolver_interaction;

  /** Information about a single request posted to the background
   *  thread.
   */
  struct job_request
  {
    /** The solution number to be calculated. */
    int sol_num;

    /** The number of steps to allow for this calculation. */
    int max_steps;

    /** The continuation of this computation. */
    boost::shared_ptr<background_continuation> k;

    /** \brief The trampoline function used to invoke the
     *  continuation's methods.
     */
    post_thunk_f post_thunk;

    job_request(int _sol_num, int _max_steps,
		const boost::shared_ptr<background_continuation> &_k,
		post_thunk_f _post_thunk)
      : sol_num(_sol_num), max_steps(_max_steps), k(_k),
	post_thunk(_post_thunk)
    {
    }
  };

  /** Sort job requests by their solution number and step count. */
  struct job_request_compare
  {
    bool operator()(const job_request &jr1, const job_request &jr2) const
    {
      return jr1.sol_num < jr2.sol_num ||
	(jr1.sol_num == jr2.sol_num && jr1.max_steps < jr2.max_steps);
    }
  };

  /** The cache file on which this manager operates. */
  aptitudeCacheFile *cache_file;

  /** The active resolver, or \b NULL if none is active. */
  aptitude_resolver *resolver;

  /** An undo list for resolver-specific items.  This is cleared
   *  whenever the resolver is discarded.
   */
  undo_list *undos;

  /** \brief The operations performed by the user since the last
   *  solution was generated.
   *
   *  Used to generate the actions-since-the-last-solution when a new
   *  solution is generated.  This should always be accessed with the
   *  background thread suspended.
   */
  std::vector<resolver_interaction> actions_since_last_solution;

  /** \brief The number of ticks for which the resolver has been
   *  run since the last solution.
   *
   *  Like actions_since_last_solution, should be accessed with the
   *  background thread suspended.
   */
  int ticks_since_last_solution;

  /** \brief Stores the information needed to reproduce a solution. */
  class solution_information
  {
    const std::vector<resolver_interaction> *interactions;
    int ticks;
    const generic_solution<aptitude_universe> *solution;
    bool is_keep_all_solution;

  public:
    solution_information(const std::vector<resolver_interaction> *_interactions,
			 int _ticks,
			 const generic_solution<aptitude_universe> *_solution,
			 bool _is_keep_all_solution)
      : interactions(_interactions), ticks(_ticks), solution(_solution),
	is_keep_all_solution(_is_keep_all_solution)
    {
    }

    ~solution_information();

    /** \brief Get the modifications the user made to the resolver
     *  before this solution was produced.
     */
    const std::vector<resolver_interaction> *get_interactions() const
    {
      return interactions;
    }

    /** \brief Get an upper bound on the number of resolver steps
     *  needed to produce this solution.
     */
    int get_ticks() const { return ticks; }

    /** \brief Get the solution that was produced by the resolver. */
    const generic_solution<aptitude_universe> *get_solution() const
    {
      return solution;
    }

    /** \return \b true if this solution is the "revert-all" solution,
     *  the one that keeps all packages at their current version.
    */
    bool get_is_keep_all_solution() const
    {
      return is_keep_all_solution;
    }
  };

  /** The solutions generated by this manager since the last change to
   *  the cache, and the interaction that took place before each
   *  solution was generated.
   */
  std::vector<const solution_information *> solutions;

  /** True if the solution search was aborted due to an uncaught
   *  exception.
   */
  bool solution_search_aborted;

  /** If solution_search_aborted is true, stored the error message
   *  attached to the exception in question.
   */
  std::string solution_search_abort_msg;

  /** A lock for solutions, solution_search_aborted, and
   *  solution_search_abort_msg; used to allow the background thread
   *  to immediately post results without taking the big class lock
   *  (since that might be taken by stop_background_resolver()).
   */
  mutable cwidget::threads::mutex solutions_mutex;

  /** The index of the currently selected solution. */
  unsigned int selected_solution;

  /** The pending job requests for the background thread.
   */
  std::priority_queue<job_request, std::vector<job_request>,
		      job_request_compare> pending_jobs;

  /** If \b true, the background thread should abort its execution. */
  bool background_thread_killed;

  /** If \b true, the background thread is currently running. */
  bool background_thread_running;

  /** If \b true, the resolver is \b NULL.  (this is used rather than
   *  checking the variable directly in order to make it painfully
   *  clear what the proper locking protocol is)
   */
  bool resolver_null;

  /** If set to a non-empty string, an excerpt of the cache
   *  corresponding to the packages touched by the resolver will be
   *  written to this directory whenever the resolver finishes
   *  running.
   *
   *  This is in the scope of background_control_mutex.
   */
  std::string resolver_trace_dir;

  /** If set to a non-empty string, an excerpt of the cache
   *  corresponding to the packages touched by the resolver will be
   *  archived and written to this file whenever the resolver finishes
   *  running.
   *
   *  This is in the scope of background_control_mutex.
   */
  std::string resolver_trace_file;

  /** The number of times the background thread has been suspended; it
   *  will only be allowed to run if this value is 0.
   */
  int background_thread_suspend_count;

  /** If \b true, the background thread is currently running in the
   *  resolver; this indicates that foreground threads trying to
   *  suspend the background thread should wait on
   *  background_in_resolver_cond until this becomes \b false.
   */
  bool background_thread_in_resolver;

  /** \brief The initial set of installations; used when setting up
   *  the resolver.
   */
  imm::map<aptitude_resolver_package, aptitude_resolver_version> initial_installations;

  /** A lock around pending_jobs, background_thread_killed,
   *  background_thread_suspend_count, background_thread_in_resolver,
   *  resolver_null, and resolver_trace_dir.
   */
  cwidget::threads::mutex background_control_mutex;

  /** A condition signalled for pending_jobs,
   *  background_thread_killed, background_thread_suspend_count,
   *  resolver_null, and resolver_trace_dir.
   */
  cwidget::threads::condition background_control_cond;

  /** A condition signalled for background_thread_in_resolver.
   */
  cwidget::threads::condition background_resolver_cond;

  /** The thread in which a background resolver is running, or \b NULL
   *  if none is.
   */
  cwidget::threads::thread *resolver_thread;

  /** This lock is used to serialize all accesses to this object,
   *  except background_get_solution().
   */
  mutable cwidget::threads::mutex mutex;

  void discard_resolver();
  void create_resolver();

  /** A class that bootstraps the routine below. */
  class background_thread_bootstrap;
  friend class background_thread_bootstrap;

  /** A class that stops the background thread when it's created, and
   *  restarts it when it's destroyed.  If background_resolver_active
   *  is set to \b false in the meantime, the resolver won't be
   *  restarted.
   */
  class background_suspender;
  friend class background_suspender;

  /** \brief Write a test control file, which can be used to replicate
   * test output.
   *
   *  \param visited_packages   the packages visited by the problem resolver.
   *
   *  \param solution_number the setup and parameters used to compute
   *  all solutions up to and including this solution index will be
   *  dumped to the file.
   */
  void write_test_control_file(const std::string &outDir,
			       const std::set<aptitude_resolver_package> &visited_packages,
			       int solution_number);

  /** \brief Dump the visited packages to a file if necessary.
   *
   *  Must be called with background_control_mutex held.
   *
   *  \param visited_packages  the packages touched by the
   *                           problem resolver.
   *  \param solution_number   the index of the last solution to
   *                           dump a test for.
   */
  void dump_visited_packages(const std::set<aptitude_resolver_package> &visited_packages,
			     int solution_number);

  /** Low-level code to get a solution; it does not take the global
   *  lock, does not stop a background thread, and must run in the
   *  background.  It is called by background_thread_execution.
   */
  const generic_solution<aptitude_universe> *
  do_get_solution(int max_steps, unsigned int solution_number,
		  std::set<aptitude_resolver_package> &visited_packages);

  /** The actual background thread. */
  void background_thread_execution();

  /** Start a background thread if none exists. */
  void start_background_thread();

  /** Destroy the background thread completely and reset its control
   *  parameters.  Waits until the thread has terminated to return.
   *
   *  If no thread exists, do nothing.
   *
   *  \warning This routine must only be invoked by the
   *  resolver_manager destructor; the resolver thread should survive
   *  until the resolver manager is destroyed.
   */
  void kill_background_thread();

  /** Increments the suspend count of the background thread, and (if
   *  necessary) interrupts a running resolution and waits for the
   *  thread to leave the resolver.
   */
  void suspend_background_thread();

  /** Decrements the suspend count of the background thread, and (if
   *  necessary) unsuspends it.
   */
  void unsuspend_background_thread();

  /** Create a resolver if necessary. */
  void maybe_create_resolver();

  /** Collects common code for the resolver manipulations such as
   *  reject_version, unreject_version, etc: locks this class,
   *  suspends the resolver, runs the manipulation and adds any undo
   *  that is generated to the undo list, and finally executes
   *  state_changed().
   */
  template<typename T>
  void resolver_manipulation(const T &t,
			     void (generic_problem_resolver<aptitude_universe>::*action)(const T &, undo_group *),
			     const resolver_interaction &act);
public:
  /** Create a new resolver manager for the given cache file. */
  resolver_manager(aptitudeCacheFile *cache_file,
		   const imm::map<aptitude_resolver_package, aptitude_resolver_version> &_initial_installations);

  virtual ~resolver_manager();

  /** \brief Discard all past interactions and return the resolver to
   *  its initial state.
   */
  void reset_resolver();

  /** If \b true, then a resolver has been created, indicating that
   *  problems may exist in the cache.
   */
  bool resolver_exists() const;


  /** Requires that resolver_exists() is \b true.
   *
   *  \param activate if \b true, enable debugging to cout.  Any
   *  change to the state of any package will reset this to the
   *  default (off).  \todo allow any ostream.
   */
  void set_debug(bool activate);

  /** \brief Set the directory to which future resolver runs will
   *  dump a cache containing only references to packages that
   *  were touched by the resolver.
   *
   *  \param path the directory to which the subset should be
   *  written, or an empty string to not write anything.
   */
  void set_resolver_trace_dir(const std::string &path);

  /** The number of solutions generated. */
  unsigned int generated_solution_count() const;

  /** Get the selection location, which will be in the range
   *  [0,generated_solution_count()).  Note that this is meaningless
   *  if generated_solution_count==0.
   */
  unsigned int get_selected_solution() const {return selected_solution;}

  /** Requires that resolver_exists() is \b true.  Return the solution
   *  in the given position, generating it if it is past the end of
   *  the list; will continue a search even if it ran out of time
   *  previously.
   *
   *  If solution_num refers to an already-generated solution, this
   *  routine returns immediately (without suspending the thread).
   *
   *  \throw NoMoreSolutions if the list of solutions is exhausted
   *  \throw NoMoreTime if time is exhausted while searching for
   *                    the solution (time here is counted separately
   *                    at each step).
   *  \throw ResolverManagerThreadClashException if a new solution
   *         would be generated and a background thread exists.
   *  \throw Exception if the background thread aborted with an exception.
   */
  const generic_solution<aptitude_universe> &get_solution(unsigned int solution_num,
							  int max_steps);

  /** As get_solution, but returns whether the solution in question
   *  is equal to the resolver's "keep-all" solution.
   */
  bool get_is_keep_all_solution(unsigned int solution_num, int max_steps);

  /** As get_solution, but run in a background thread if necessary.
   *
   *  \param solution_num the solution to retrieve
   *
   *  \param max_steps the number of steps to allow the computation
   *
   *  \param k a background_continuation object; when the background
   *  computation is finished, a method corresponding to its result
   *  will be invoked on continuation in the background thread.  It is
   *  safe for this method to manipulate the resolver (for instance,
   *  to enqueue a new computation).
   *
   *  \param post_thunk A callback that is invoked in the background
   *  thread; it should arrange to safely invoke its argument.  This
   *  means either invoking it in the main thread or guaranteeing that
   *  no other thread modifies the resolver while the thunk executes,
   *  or that the thunk does not modify the resolver.  The second
   *  option includes taking actions that trigger a resolver
   *  modification, such as closing the cache, and is only used by the
   *  command line (which blocks the main thread while the resolver
   *  runs).
   *
   *  \throw ResolverManagerThreadClashException if a background
   *         resolver already exists.
   */
  void get_solution_background(unsigned int solution_num,
			       int max_steps,
			       const boost::shared_ptr<background_continuation> &k,
			       post_thunk_f post_thunk);

  /** If \b true, all solutions have been generated.  This is equivalent
   *  to the solutions_exhausted member of the state snapshot.
   */
  bool solution_generation_complete() /*const*/;

  /** If \b true, the solution pointer is set to the first
   *  solution.
   */
  bool solutions_at_start() const;

  /** If \b true, the background thread is working on a job. */
  bool background_thread_active();

  /** If \b true, the background thread was killed by a fatal
   *  exception.
   */
  bool background_thread_aborted();

  /** If background_thread_aborted is \b true, returns the error
   *  message associated with the exception that killed the background
   *  thread; otherwise returns an empty string.
   */
  std::string background_thread_abort_msg();

  /** Get a snapshot of the current resolver state; contains the
   *  values that would be returned by get_selected_solution(),
   *  generated_solution_count(), solutions_exhausted(),
   *  background_thread_active(), background_thread_aborted(),
   *  and background_thread_abort_msg(); however, this snapshot
   *  is taken atomically.
   */
  state state_snapshot();



  /** \brief Reject all versions that will break holds or install
   *  forbidden versions.
   *
   *  This is performed by default when the resolver is created in
   *  aptitude_resolver::aptitude_resolver, but the same rejects can
   *  be added explicitly using this routine.
   */
  void reject_break_holds();

  /** Requires that resolver_exists() is \b true.  Temporarily rejects
   *  any solutions generated by the currently active installer that
   *  involve installing the given version; the rejection will be
   *  discarded when the resolver is.
   */
  void reject_version(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists() is \b true.  Cancels a
   *  rejection created via resolver_reject_version().
   */
  void unreject_version(const aptitude_resolver_version &ver);

  /** Requires the resolver_exists() is \b true.  Returns \b true if
   *  the given version is currently rejected.
   */
  bool is_rejected(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists() is \b true.  Like
   *  resolver_reject_version, but rejects any solution that does \b
   *  not install the given version.
   */
  void mandate_version(const aptitude_resolver_version &ver);

  /** Cancels a resolver_mandate_version call. */
  void unmandate_version(const aptitude_resolver_version &ver);

  /** \return \b true if the given version is mandatory. */
  bool is_mandatory(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists is \b true.  Force the resolver to
   *  treat the given soft dependency as a hard dependency; as above,
   *  you can always cancel this instruction later.
   */
  void harden_dep(const aptitude_resolver_dep &dep);

  /** Cancels a resolver_harden_dep call. */
  void unharden_dep(const aptitude_resolver_dep &dep);

  /** \return \b true if the given dep is hardened. */
  bool is_hardened(const aptitude_resolver_dep &dep);

  /** Require the resolver to leave the given soft dependency broken
   *  whenever possible.
   */
  void approve_broken_dep(const aptitude_resolver_dep &dep);

  /** Cancel an approval set up with approve_broken(). */
  void unapprove_broken_dep(const aptitude_resolver_dep &dep);

  /** \return \b true if the given dependency is in the
   *  approved-broken set.
   */
  bool is_approved_broken(const aptitude_resolver_dep &dep);



  /** \return \b true if undo items exist in this resolver manager. */
  bool has_undo_items();

  /** If this resolver has any undo items, invoke the "topmost" one.
   *
   *  \return \b true if an undo item was invoked.
   */
  bool undo();


  /** Set the selection pointer to a particular solution. */
  void select_solution(unsigned int solnum);

  /** Discards information about the last error encountered
   *  in a search.
   *
   *  aptitude uses this when the user asks to retrieve the "next"
   *  solution, to allow them to try to search past errors.
   */
  void discard_error_information();

  /** Move the selection pointer to the next solution, without
   *  generating it.
   */
  void select_next_solution();

  /** Move the selection pointer to the previous solution, without
   *  generating it.
   *
   *  \throws NoMoreSolutions if solutions_at_start()
   */
  void select_previous_solution();

  /** \brief Tells the background thread to start calculating
   *  the next solution, if appropriate.
   *
   *  The thread will be started if all of the following are true:
   *
   *    1. There are broken packages.
   *    2. The selected solution is the next solution to generate.
   *    3. The solution set is not exhausted.
   *    4. The background thread is not already active.
   *    5. The background thread didn't abort with an error.
   *
   *  \param k           The continuation of the dependency resolver.
   *                     It will be invoked in the background thread
   *                     when a solution is found.
   *
   *  \param post_thunk A callback that is invoked in the background
   *  thread; it should arrange to safely invoke its argument.  This
   *  means either invoking it in the main thread or guaranteeing that
   *  no other thread modifies the resolver while the thunk executes,
   *  or that the thunk does not modify the resolver.  The second
   *  option includes taking actions that trigger a resolver
   *  modification, such as closing the cache, and is only used by the
   *  command line (which blocks the main thread while the resolver
   *  runs).
   */
  void maybe_start_solution_calculation(const boost::shared_ptr<background_continuation> &k,
					post_thunk_f post_thunk);

  /** Tweak the resolver score of a particular package/version.  This
   *  requires that resolver_exists() and that the resolver is "fresh"
   *  (i.e., that next_solution() and current_solution() have never
   *  been called)
   *
   *  \param pkg the package to adjust
   *  \param ver the version to adjust
   *  \param score an adjustment to be added to the score of pkg:ver
   */
  void tweak_score(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   int score);

  /** If a resolver exists, write its state (including scores, etc)
   *  to the given stream.
   */
  void dump(std::ostream &out);

  /** This signal is emitted when the selected solution changes, when
   *  the user takes an action that might change the number of
   *  available solutions (such as un-rejecting a package), and when a
   *  new resolver is created.
   *
   *  Note that this is NOT signalled when a new solution is added to
   *  the solution list by the background thread.  You are free to
   *  manually emit the signal, but of course be aware of threading
   *  considerations if you do so.
   */
  sigc::signal0<void> state_changed;

  /** \brief This signal is emitted when the mandated/rejected state
   *  of any resolver version is changed via the resolver_manager.
   */
  sigc::signal<void, aptitude_resolver_version> version_accept_reject_changed;
  /** \brief This signal is emitted when the mandated/rjected state of
   *  any break-dependency action is changed via the resolver manager.
   */
  sigc::signal<void, aptitude_resolver_dep> break_dep_accept_reject_changed;

  /** \brief Safe resolver logic. */
  // @{

private:
  /** \brief Set up the resolver for "safe" dependency resolution;
   *         used by safe_resolve_deps().
   *
   * Essentialy this means (1) forbid it from removing any package,
   * and (2) only install the default candidate version of a package
   * or the current version.  Invoking this routine will throw away
   * any dependency resolution that has already been performed.
   *
   *  \param no_new_installs   If \b true, the resolver will also not
   *                           be allowed to install any new packages;
   *                           only upgrades will be permitted.
   *  \param no_new_upgrades   If \b true, the resolver will not be
   *                           allowed to upgrade any packages to resolve
   *                           dependencies (so it can only cancel
   *                           upgrades).
   */
  void setup_safe_resolver(bool no_new_installs, bool no_new_upgrades);

  // Needs to be a member class so that it can access mutexes and so
  // on (necessary so that locking works properly; I consider this OK
  // because it's really just part of the resolver manager code, so
  // close cooperation on locking is acceptable).
  class safe_resolver_continuation;

public:
  /** \brief Resolve dependencies by installing default versions and
   *  cancelling upgrades / installs.
   *
   *  \param no_new_installs   If \b true, the resolver will also not
   *                           be allowed to install any new packages;
   *                           only upgrades will be permitted.
   *  \param no_new_upgrades   If \b true, the resolver will not be
   *                           allowed to upgrade any packages to resolve
   *                           dependencies (so it can only cancel
   *                           upgrades).
   *  \param k                 A continuation to invoke when the resolver
   *                           is finished.
   *
   *  \param post_thunk A callback that is invoked in the background
   *  thread; it should arrange to safely invoke its argument.  This
   *  means either invoking it in the main thread or guaranteeing that
   *  no other thread modifies the resolver while the thunk executes,
   *  or that the thunk does not modify the resolver.  The second
   *  option includes taking actions that trigger a resolver
   *  modification, such as closing the cache, and is only used by the
   *  command line (which blocks the main thread while the resolver
   *  runs).
   *
   *  This routine will wipe out the state of the resolver and start
   *  from scratch; no other code should adjust the resolver's state
   *  until the safe resolution completes.
   */
  void safe_resolve_deps_background(bool no_new_installs, bool no_new_upgrades,
				    const boost::shared_ptr<background_continuation> &k,
				    post_thunk_f post_thunk);

  // @}
};

#endif
