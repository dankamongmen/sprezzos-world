// resolver_manager.cc
//
//   Copyright (C) 2005, 2007-2010 Daniel Burrows
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

#include "resolver_manager.h"

#include <aptitude.h>
#include "apt.h"
#include "aptitude_resolver.h"
#include "aptitude_resolver_cost_settings.h"
#include "aptitude_resolver_cost_syntax.h"
#include "aptitude_resolver_universe.h"
#include "config_signal.h"
#include "dump_packages.h"

#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <loggers.h>

#include <generic/problemresolver/problemresolver.h>
#include <generic/util/temp.h>
#include <generic/util/undo.h>

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <sigc++/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <fstream>

#include <sys/types.h>
#include <sys/wait.h>

using aptitude::Loggers;

const int defaultStepLimit = 500000;

class resolver_manager::resolver_interaction
{
public:
  /** \brief The type tag of a resolver interaction. */
  enum tag
    {
      reject_version,
      unreject_version,
      mandate_version,
      unmandate_version,
      harden_dep,
      unharden_dep,
      approve_broken_dep,
      unapprove_broken_dep,
      undo
    };

private:
  tag type;

  aptitude_resolver_version version;
  aptitude_resolver_dep dep;

  resolver_interaction(tag _type,
		       const aptitude_resolver_version &_version,
		       const aptitude_resolver_dep &_dep)
    : type(_type), version(_version), dep(_dep)
  {
  }

public:
  static resolver_interaction RejectVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(reject_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction UnRejectVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(unreject_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction MandateVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(mandate_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction UnMandateVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(unmandate_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction HardenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(harden_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction UnHardenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(unharden_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction ApproveBrokenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(approve_broken_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction UnApproveBrokenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(unapprove_broken_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction Undo()
  {
    return resolver_interaction(undo,
				aptitude_resolver_version(),
				aptitude_resolver_dep());
  }

  tag get_type() const { return type; }
  const aptitude_resolver_version &get_version() const
  {
    eassert(!version.get_pkg().end());
    return version;
  }
  const aptitude_resolver_dep &get_dep() const
  {
    eassert(!dep.get_dep().end());
    return dep;
  }
};

resolver_manager::solution_information::~solution_information()
{
  delete interactions;
  delete solution;
}

// NB: we need a recursive mutex because some routines can be called
// either by other routines of the class (already have a mutex lock)
// or by the user (don't have a mutex lock); I could sidestep this
// with some clever magic, but there's no point unless it turns out to
// be a bottleneck.
resolver_manager::resolver_manager(aptitudeCacheFile *_cache_file,
				   const imm::map<aptitude_resolver_package, aptitude_resolver_version> &_initial_installations)
  :cache_file(_cache_file),
   resolver(NULL),
   undos(new undo_list),
   ticks_since_last_solution(0),
   solution_search_aborted(false),
   selected_solution(0),
   background_thread_killed(false),
   background_thread_running(false),
   resolver_null(true),
   background_thread_suspend_count(0),
   background_thread_in_resolver(false),
   initial_installations(_initial_installations),
   resolver_thread(NULL),
   mutex(cwidget::threads::mutex::attr(PTHREAD_MUTEX_RECURSIVE))
{
  (*cache_file)->pre_package_state_changed.connect(sigc::mem_fun(this, &resolver_manager::discard_resolver));
  (*cache_file)->package_state_changed.connect(sigc::mem_fun(this, &resolver_manager::maybe_create_resolver));

  aptcfg->connect("Apt::Install-Recommends",
		  sigc::mem_fun(this,
				&resolver_manager::discard_resolver));

  start_background_thread();

  maybe_create_resolver();
}

resolver_manager::~resolver_manager()
{
  eassert(background_thread_running);

  discard_resolver();

  kill_background_thread();

  for(std::vector<const solution_information *>::const_iterator it =
	solutions.begin(); it != solutions.end(); ++it)
    {
      delete *it;
    }

  delete undos;
}

void resolver_manager::reset_resolver()
{
  discard_resolver();
  maybe_create_resolver();
}

resolver_manager::background_continuation::~background_continuation()
{
}

class resolver_manager::background_suspender
{
  resolver_manager &m;

  bool suspended;
public:
  background_suspender(resolver_manager &_m)
    :m(_m), suspended(true)
  {
    m.suspend_background_thread();
  }

  void unsuspend()
  {
    if(suspended)
      {
	m.unsuspend_background_thread();
	suspended = false;
      }
  }

  ~background_suspender()
  {
    if(suspended)
      m.unsuspend_background_thread();
  }
};

/** A class that assigns a value to an object when it is destroyed.
 */
template<typename T>
class set_when_destroyed
{
  T &target;
  T val;

public:
  /** Create a set_when_destroyed.
   *
   *  \param _target The object to be set.
   *  \param _val The value to assign to _target.
   */
  set_when_destroyed(T &_target, const T &_val)
    : target(_target), val(_val)
  {
  }

  /** Assign val to target. */
  ~set_when_destroyed()
  {
    target = val;
  }
};

void resolver_manager::write_test_control_file(const std::string &outDir,
					       const std::set<aptitude_resolver_package> &visited_packages,
					       int solution_number)
{
  static const char * const strBadChars = "\\\"";

  std::string control_filename = outDir + "/APTITUDE.TRACE";
  std::ofstream control_file(control_filename.c_str());
  if(!control_file)
    return; // Should we output an error here?

  // The state files are written out in make_truncated_state_copy.
  // Here we just indicate the actual installs / removals that are
  // queued up.

  control_file << "initial {" << std::endl;
  for(std::set<aptitude_resolver_package>::const_iterator
	it = visited_packages.begin();
      it != visited_packages.end();
      ++it)
    {
      const aptitude_resolver_package &p(*it);
      if(visited_packages.find(p) == visited_packages.end())
	continue;

      pkg_action_state action = find_pkg_state(p.get_pkg(), **cache_file);
      std::string actionstr;
      switch(action)
	{
	case pkg_unused_remove:
	case pkg_auto_remove:
	case pkg_remove:
	  actionstr = "remove";
	  break;

	case pkg_auto_install:
	case pkg_downgrade:
	case pkg_install:
	case pkg_upgrade:
	  actionstr = std::string("install ") +
	    (*cache_file)[p.get_pkg()].InstVerIter(*cache_file).VerStr();
	  break;

	default:
	  break;
	}

      if(!actionstr.empty())
	control_file << "  " << actionstr << " \"" << QuoteString(p.get_name(), strBadChars) << "\"" << std::endl;;
    }

  control_file << "}" << std::endl;
  control_file << std::endl;

  {
    cwidget::threads::mutex::lock sol_l(solutions_mutex);

    for(std::vector<const solution_information *>::const_iterator infIt =
	  solutions.begin();
	infIt != solutions.end() && infIt - solutions.begin() <= solution_number;
	++infIt)
      {
	control_file << "test {" << std::endl;

	const solution_information &inf = **infIt;
	const std::vector<resolver_interaction> &acts =
	  *inf.get_interactions();
	const generic_solution<aptitude_universe> &sol = *inf.get_solution();

	for(std::vector<resolver_interaction>::const_iterator actIt =
	      acts.begin(); actIt != acts.end(); ++actIt)
	  {
	    switch(actIt->get_type())
	      {
	      case resolver_interaction::reject_version:
		control_file << "reject " << actIt->get_version() << std::endl;
		break;
	      case resolver_interaction::unreject_version:
		control_file << "unreject " << actIt->get_version() << std::endl;
		break;
	      case resolver_interaction::mandate_version:
		control_file << "mandate " << actIt->get_version() << std::endl;
		break;
	      case resolver_interaction::unmandate_version:
		control_file << "unmandate " << actIt->get_version() << std::endl;
		break;
	      case resolver_interaction::harden_dep:
		control_file << "harden " << actIt->get_dep() << std::endl;
		break;
	      case resolver_interaction::unharden_dep:
		control_file << "unharden " << actIt->get_dep() << std::endl;
		break;
	      case resolver_interaction::approve_broken_dep:
		control_file << "approve_broken " << actIt->get_dep() << std::endl;
		break;
	      case resolver_interaction::unapprove_broken_dep:
		control_file << "unapprove_broken " << actIt->get_dep() << std::endl;
		break;
	      case resolver_interaction::undo:
		control_file << "undo" << std::endl;
		break;
	      }
	  }

	control_file << "  expect " << inf.get_ticks() << " {" << std::endl;

	typedef generic_choice_set<aptitude_universe> choice_set;
	typedef generic_choice<aptitude_universe> choice;
	typedef aptitude_resolver_package package;
	typedef aptitude_resolver_version version;
	typedef aptitude_resolver_dep dep;
	const choice_set &choices = sol.get_choices();
	for(choice_set::const_iterator solChoiceIt =
	      choices.begin(); solChoiceIt != choices.end(); ++solChoiceIt)
	  {
	    switch(solChoiceIt->get_type())
	      {
	      case choice::install_version:
		control_file << "    install \""
			     << QuoteString(solChoiceIt->get_ver().get_package().get_name(), strBadChars)
			     << "\" \""
			     << QuoteString(solChoiceIt->get_ver().get_name(), strBadChars)
			     << "\""
			     << std::endl;
		break;

	      case choice::break_soft_dep:
		{
		  dep d(solChoiceIt->get_dep());
		  version source(d.get_source());

		  control_file << "    break \""
			       << QuoteString(source.get_package().get_name(), strBadChars)
			       << "\" \""
			       << QuoteString(source.get_name(), strBadChars)
			       << "\" -> {";

		  bool first = true;
		  for(dep::solver_iterator sIt = d.solvers_begin();
		      !sIt.end(); ++sIt)
		    {
		      if(first)
			first = false;
		      else
			control_file << ", ";

		      version solver(*sIt);
		      control_file << "\""
				   << QuoteString(solver.get_package().get_name(), strBadChars)
				   << "\" \""
				   << QuoteString(solver.get_name(), strBadChars)
				   << "\"";
		    }

		  control_file << "}" << std::endl;
		}

		break;

	      default:
		// ... recover sanely.
		control_file << "ERROR" << std::endl;
	      }
	  }
	control_file << "  }" << std::endl;

	control_file << "}" << std::endl;
      }
  }
}

void resolver_manager::dump_visited_packages(const std::set<aptitude_resolver_package> &visited_packages,
					     int solution_number)
{
  std::string trace_dir;
  temp::dir temp_trace_dir;
  if(!resolver_trace_dir.empty())
    trace_dir = resolver_trace_dir;
  else if(!resolver_trace_file.empty())
    {
      temp_trace_dir = temp::dir("aptitude-trace-dump");
      trace_dir = temp_trace_dir.get_name();
    }
  else
    return;

  std::set<pkgCache::PkgIterator> packages;
  for(std::set<aptitude_resolver_package>::const_iterator it = visited_packages.begin();
      it != visited_packages.end(); ++it)
    {
      packages.insert((*it).get_pkg());
    }

  aptitude::apt::make_truncated_state_copy(trace_dir, packages);
  write_test_control_file(trace_dir, visited_packages, solution_number);

  if(!resolver_trace_file.empty())
    {
      int pid = fork();
      if(pid == -1)
	_error->Errno("fork", "Unable to run \"tar\" to create the trace file.");
      else if(pid != 0)
	{
	  int status;
	  while(waitpid(pid, &status, 0) != pid ||
		(!WIFEXITED(status) &&
		 !WIFSIGNALED(status)))
	    ;

	  if(WIFSIGNALED(status))
	    _error->Error(_("Unable to create the output file: child killed by signal %d."), WTERMSIG(status));
	  else if(WEXITSTATUS(status) != 0)
	    _error->Error(_("Unable to create the output file: child exited with status %d."), WEXITSTATUS(status));

	  // Whee, we win!
	}
      else
	{
	  // Should I close open fds here?

	  // Should I allow tar to be found on the path?
	  execl("/bin/tar", "/bin/tar", "czf",
		resolver_trace_file.c_str(),
		"-C",
		trace_dir.c_str(),
		".",
		NULL);

	  // TODO: communicate to the parent that we couldn't find
	  // tar?
	  exit(1);
	}
    }
}

namespace
{
  // Trampoline used to invoke continuations directly in the
  // background thread (where it's OK to do so).
  void inline_continuation_trampoline(const sigc::slot<void> &f)
  {
    f();
  }
}

// This assumes that background_resolver_active is empty when it
// starts (see restart_background_resolver)
//
// FIXME: max_steps should be changed when the configuration is (not a
// visible bug at the moment since you can't change that option
// interactively)
void resolver_manager::background_thread_execution()
{
  logging::LoggerPtr logger = Loggers::getAptitudeResolverThread();
  LOG_TRACE(logger, "Resolver thread: starting.");
  std::set<aptitude_resolver_package> visited_packages;

  cwidget::threads::mutex::lock l(background_control_mutex);
  set_when_destroyed<bool> cancel_set_running(background_thread_running, false);

  while(1)
    {
      while((background_thread_suspend_count > 0 || resolver_null || pending_jobs.empty()) &&
	    !background_thread_killed)
	{
	  if(logger->isEnabledFor(logging::TRACE_LEVEL))
	    {
	      std::vector<std::string> why_suspended;
	      if(background_thread_suspend_count > 0)
		why_suspended.push_back("waiting to be unsuspended");
	      if(resolver_null)
		why_suspended.push_back("waiting for a resolver to be created");
	      if(pending_jobs.empty())
		why_suspended.push_back("waiting for a job");

	      if(!why_suspended.empty())
		{
		  std::string msg;
		  for(std::vector<std::string>::const_iterator it = why_suspended.begin();
		      it != why_suspended.end(); ++it)
		    {
		      if(!msg.empty())
			msg += ", ";
		      msg += *it;
		    }

		  LOG_TRACE(logger, "Resolver thread: " << msg << ".");
		}
	    }

	  background_control_cond.wait(l);
	}

      if(background_thread_killed)
	{
	  LOG_TRACE(logger, "Resolver thread: exiting (killed).");
	  break;
	}

      job_request job = pending_jobs.top();
      pending_jobs.pop();

      LOG_DEBUG(logger,
		"Resolver thread: got a new job { solution number = "
		<< job.sol_num << ", max steps = " << job.max_steps
		<< ", continuation = " << job.k << " }");

      background_thread_in_resolver = true;
      background_resolver_cond.wake_all();
      l.release();

      try
	{
	  const aptitude_resolver::solution *sol =
	    do_get_solution(job.max_steps,
			    job.sol_num,
			    visited_packages);

	  LOG_DEBUG(logger,
		    "Resolver thread: got a solution: " << *sol);

	  // Set the state variable BEFORE exiting the resolver; this
	  // is done so that if there are no more jobs, the foreground
	  // thread sees that we're out of the resolver when it
	  // examines the solution.
	  l.acquire();
	  dump_visited_packages(visited_packages,
				job.sol_num);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  // A slot that invokes job.k->success(*sol):
	  sigc::slot<void> success_slot =
	    sigc::bind(sigc::mem_fun(*job.k,
				     &background_continuation::success),
		       *sol);
	  // Wrap a keepalive slot around that so job.k lives.
	  job.post_thunk(make_keepalive_slot(success_slot, job.k));
	}
      catch(InterruptedException)
	{
	  // Put it back into the pot.
	  l.acquire();

	  LOG_DEBUG(logger,
		    "Resolver thread: interrupted, pushing a job back on the queue "
		    << "{ solution number = " << job.sol_num
		    << ", max_steps = " << job.max_steps
		    << ", continuation = " << job.k << "}");

	  dump_visited_packages(visited_packages,
				job.sol_num);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  pending_jobs.push(job);

	  l.release();
	}
      catch(NoMoreSolutions)
	{
	  l.acquire();

	  LOG_DEBUG(logger,
		    "Resolver thread: out of solutions.");

	  dump_visited_packages(visited_packages,
				job.sol_num);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  sigc::slot<void> no_more_solutions_slot =
	    sigc::mem_fun(*job.k,
			  &background_continuation::no_more_solutions);
	  job.post_thunk(make_keepalive_slot(no_more_solutions_slot, job.k));
	}
      catch(NoMoreTime)
	{
	  l.acquire();

	  LOG_DEBUG(logger,
		    "Resolver thread: out of time.");

	  dump_visited_packages(visited_packages,
				job.sol_num);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  sigc::slot<void> no_more_time_slot =
	    sigc::mem_fun(*job.k,
			  &background_continuation::no_more_time);
	  job.post_thunk(make_keepalive_slot(no_more_time_slot, job.k));
	}
      catch(cwidget::util::Exception &e)
	{
	  LOG_ERROR(logger,
		    "Resolver thread: caught a fatal error from the resolver: "
		    << e.errmsg());

	  dump_visited_packages(visited_packages,
				job.sol_num);

	  sigc::slot<void> aborted_slot =
	    sigc::bind(sigc::mem_fun(*job.k,
				     &background_continuation::aborted),
		       e.errmsg());
	  job.post_thunk(make_keepalive_slot(aborted_slot, job.k));
	}

      l.acquire();

      background_thread_in_resolver = false;
      background_resolver_cond.wake_all();
    }
}

// Need this because sigc slots aren't threadsafe :-(
struct resolver_manager::background_thread_bootstrap
{
  resolver_manager &m;
public:
  background_thread_bootstrap(resolver_manager &_m)
    :m(_m)
  {
  }

  void operator()()
  {
    m.background_thread_execution();
  }
};

void resolver_manager::start_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread == NULL)
    {
      background_thread_running = true;
      resolver_thread = new cwidget::threads::thread(background_thread_bootstrap(*this));
    }
}

void resolver_manager::kill_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread != NULL)
    {
      cwidget::threads::mutex::lock control_lock(background_control_mutex);

      if(resolver != NULL)
	resolver->cancel_solver();
      background_thread_killed = true;
      background_control_cond.wake_all();

      control_lock.release();

      resolver_thread->join();
      delete resolver_thread;
      resolver_thread = NULL;


      // Reset the associated data structures.
      control_lock.acquire();
      pending_jobs = std::priority_queue<job_request, std::vector<job_request>, job_request_compare>();
      background_thread_killed = false;
      background_thread_suspend_count = 0;
      background_thread_in_resolver = false;
      solution_search_aborted = false;
      solution_search_abort_msg.clear();
    }
}

void resolver_manager::suspend_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  // May occur due to background_suspend objects existing while
  // kill_background_thread runs.
  if(resolver_thread == NULL)
    return;

  cwidget::threads::mutex::lock control_lock(background_control_mutex);

  if(resolver != NULL)
    resolver->cancel_solver();

  ++background_thread_suspend_count;
  background_control_cond.wake_all();

  while(background_thread_in_resolver)
    background_resolver_cond.wait(control_lock);

  if(resolver != NULL)
    resolver->uncancel_solver();
}

void resolver_manager::unsuspend_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread == NULL)
    return;

  cwidget::threads::mutex::lock control_lock(background_control_mutex);

  eassert(background_thread_suspend_count > 0);
  --background_thread_suspend_count;
  background_control_cond.wake_all();
}

void resolver_manager::maybe_create_resolver()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver == NULL && ((*cache_file)->BrokenCount() > 0 || !initial_installations.empty()))
    {
      {
	cwidget::threads::mutex::lock l(background_control_mutex);
	resolver_trace_dir = aptcfg->Find(PACKAGE "::ProblemResolver::Trace-Directory", "");
	resolver_trace_file = aptcfg->Find(PACKAGE "::ProblemResolver::Trace-File", "");
      }
      create_resolver();

      // If there are initial installations, we don't know whether
      // there are broken dependencies until we actually create the
      // resolver.  If there aren't broken dependencies, don't create
      // a resolver object.
      if(resolver->get_initial_broken().empty())
	discard_resolver();
    }

  // Always signal a state change: we are signalling for the whole
  // discard/create pair, and even if we didn't create a new resolver
  // we have to inform the listeners that the old one went away
  // (maybe).
  l.release();
  state_changed();
}

void resolver_manager::discard_resolver()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver == NULL)
    return;

  background_suspender bs(*this);

  undos->clear_items();

  delete resolver;

  {
    cwidget::threads::mutex::lock l2(solutions_mutex);
    actions_since_last_solution.clear();
    ticks_since_last_solution = 0;

    for(std::vector<const solution_information *>::const_iterator it =
	  solutions.begin(); it != solutions.end(); ++it)
      delete *it;

    solutions.clear();
    solution_search_aborted = false;
    solution_search_abort_msg.clear();
    selected_solution = 0;
  }

  resolver = NULL;

  {
    cwidget::threads::mutex::lock l2(background_control_mutex);
    resolver_null = true;
    pending_jobs = std::priority_queue<job_request, std::vector<job_request>, job_request_compare>();
    background_control_cond.wake_all();
  }
}

void resolver_manager::create_resolver()
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver == NULL);

  // \todo We should parse these once on startup to avoid duplicate
  // error messages, support modifying the list dynamically (for the
  // sake of GUI users), etc.
  std::vector<aptitude_resolver::hint> hints;

  const Configuration::Item * const root =
    aptcfg->Tree(PACKAGE "::ProblemResolver::Hints");

  if(root != NULL)
    {
      for(const Configuration::Item *itm = root->Child;
	  itm != NULL; itm = itm -> Next)
	{
	  aptitude_resolver::hint hint;
	  if(aptitude_resolver::hint::parse(itm->Value, hint))
	    hints.push_back(hint);
	}
    }

  // NOTE: the performance of the resolver is highly sensitive to
  // these settings; choosing bad ones can result in hitting
  // exponential cases in practical situations.  In general,
  // penalizing actions means that the resolver will be more likely to
  // blow up trying to avoid them, with the danger increasing as the
  // penalty does.  Thus, aside from broken deps (which are penalized
  // to guide us towards a solution), I only penalize removals (which
  // are usually either unnecessary or easy to prove necessary) and
  // leaving soft dependencies (recommendations) unfixed.  The
  // relative penalties of these are also important; for instance,
  // penalizing unfixed soft deps more than removals means that the
  // resolver will actually remove packages rather than leaving their
  // Recommends: field unsatisfied!


  std::string cost_configuration = aptcfg->Find(PACKAGE "::ProblemResolver::SolutionCost",
                                                "safety,priority");

  boost::shared_ptr<std::vector<cost_component_structure> > cost_components;
  try
    {
      cost_components = parse_cost_settings(cost_configuration);
    }
  catch(ResolverCostParseException &ex)
    {
      LOG_ERROR(Loggers::getAptitudeResolver(),
                boost::format(_("Failed to parse the cost settings string: %s")) % ex.what());

      _error->Error(_("Failed to parse the cost settings string: %s"),
                    ex.what());

      // Fall back to a default cost settings list containing the
      // "safety" and "priority" components.
      cost_components = boost::make_shared<std::vector<cost_component_structure> >();

      std::vector<cost_component_structure::entry> level0;
      level0.push_back(cost_component_structure::entry("safety", 1));
      cost_components->push_back(cost_component_structure(cost_component_structure::combine_none, level0));

      std::vector<cost_component_structure::entry> level1;
      level1.push_back(cost_component_structure::entry("priority", 1));
      cost_components->push_back(cost_component_structure(cost_component_structure::combine_none, level1));
    }


  aptitude_resolver_cost_settings cost_settings(cost_components);


  cost ignored_recommends_cost;
  {
    aptitude_resolver_cost_settings::component ignored_recommends_component =
      cost_settings.get_or_create_component("ignored-recommends", aptitude_resolver_cost_settings::additive);
    ignored_recommends_cost = cost_settings.add_to_cost(ignored_recommends_component, 1);
  }

  resolver=new aptitude_resolver(aptcfg->FindI(PACKAGE "::ProblemResolver::StepScore", 70),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::BrokenScore", -100),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::UnfixedSoftScore", -200),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::Infinity", 1000000),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::ResolutionScore", 50),
                                 ignored_recommends_cost,
				 aptcfg->FindI(PACKAGE "::ProblemResolver::FutureHorizon", 50),
                                 cost_settings,
				 initial_installations,
				 (*cache_file),
				 cache_file->Policy);

  // Set auto flags for initial installations as if the installs were
  // done by the user.  i.e., if the package is currently installed,
  // we use the current value of the Auto flag; otherwise we treat it
  // as manual.
  std::map<aptitude_resolver_package, bool> manual_flags;
  for(imm::map<aptitude_resolver_package, aptitude_resolver_version>::const_iterator it =
	initial_installations.begin(); it != initial_installations.end(); ++it)
    {
      pkgCache::PkgIterator pkg(it->first.get_pkg());
      aptitude_resolver_package resolver_pkg(pkg, *cache_file);

      if(pkg->CurrentState != pkgCache::State::NotInstalled &&
	 pkg->CurrentState != pkgCache::State::ConfigFiles)
	{
	  manual_flags[resolver_pkg] =
	    ((*cache_file)[pkg].Flags & pkgCache::Flag::Auto) == 0;

	  LOG_DEBUG(Loggers::getAptitudeResolverInitialManualFlags(),
		    "Set the manual flag for the currently installed package "
		    << resolver_pkg << " to "
		    << (manual_flags[resolver_pkg] ? "true" : "false")
		    << " to reflect its current auto flag.");
	}
      else
	{
	  manual_flags[resolver_pkg] = true;
	  LOG_DEBUG(Loggers::getAptitudeResolverInitialManualFlags(),
		    "Set the manual flag for the non-installed package "
		    << resolver_pkg << " to "
		    << (manual_flags[resolver_pkg] ? "true" : "false"));
	}
    }

  resolver->add_action_scores(aptcfg->FindI(PACKAGE "::ProblemResolver::PreserveManualScore", 60),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::PreserveAutoScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::RemoveScore", -300),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::KeepScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::InstallScore", -20),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::UpgradeScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::NonDefaultScore", -40),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::EssentialRemoveScore", -100000),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::FullReplacementScore", 500),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::UndoFullReplacementScore", -500),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::BreakHoldScore", -300),
			      aptcfg->FindB(PACKAGE "::ProblemResolver::Allow-Break-Holds", false),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::DefaultResolutionScore", 400),
			      manual_flags,
			      hints);

  resolver->add_priority_scores(aptcfg->FindI(PACKAGE "::ProblemResolver::ImportantScore", 5),
				aptcfg->FindI(PACKAGE "::ProblemResolver::RequiredScore", 4),
				aptcfg->FindI(PACKAGE "::ProblemResolver::StandardScore", 3),
				aptcfg->FindI(PACKAGE "::ProblemResolver::OptionalScore", 1),
				aptcfg->FindI(PACKAGE "::ProblemResolver::ExtraScore", -1));

  {
    cwidget::threads::mutex::lock l2(background_control_mutex);
    resolver_null = false;
    background_control_cond.wake_all();
  }
}

void resolver_manager::set_resolver_trace_dir(const std::string &path)
{
  cwidget::threads::mutex::lock l(background_control_mutex);
  resolver_trace_dir = path;
}

void resolver_manager::set_debug(bool activate)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  eassert(resolver_exists());

  resolver->set_debug(activate);
}

bool resolver_manager::resolver_exists() const
{
  cwidget::threads::mutex::lock l(mutex);

  return resolver != NULL;
}

unsigned int resolver_manager::generated_solution_count() const
{
  cwidget::threads::mutex::lock l(mutex);
  cwidget::threads::mutex::lock l2(solutions_mutex);

  return solutions.size();
}

bool resolver_manager::solution_generation_complete() // const
{
  return state_snapshot().solutions_exhausted;
}

bool resolver_manager::solutions_at_start() const
{
  cwidget::threads::mutex::lock l(mutex);

  if(!resolver_exists())
    return true;
  else
    return selected_solution == 0;
}

bool resolver_manager::background_thread_active()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock ctl_l(background_control_mutex);

  return !pending_jobs.empty() || background_thread_in_resolver;
}

bool resolver_manager::background_thread_aborted()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  return solution_search_aborted;
}

std::string resolver_manager::background_thread_abort_msg()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  return solution_search_aborted ? solution_search_abort_msg : "";
}

resolver_manager::state resolver_manager::state_snapshot()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock ctl_l(background_control_mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  state rval;

  rval.selected_solution           = selected_solution;
  rval.generated_solutions         = solutions.size();
  rval.resolver_exists             = (resolver != NULL);
  rval.background_thread_active    = !solution_search_aborted &&
                                        (!pending_jobs.empty() ||
				         background_thread_in_resolver);
  rval.background_thread_aborted   = solution_search_aborted;
  rval.background_thread_abort_msg = solution_search_abort_msg;

  if(resolver != NULL)
    {
      aptitude_resolver::queue_counts c = resolver->get_counts();

      rval.open_size      = c.open;
      rval.closed_size    = c.closed;
      rval.deferred_size  = c.deferred;
      rval.conflicts_size = c.conflicts;
      rval.solutions_exhausted = c.finished;
    }
  else
    {
      rval.open_size      = 0;
      rval.closed_size    = 0;
      rval.deferred_size  = 0;
      rval.conflicts_size = 0;

      rval.solutions_exhausted = false;
    }

  return rval;
}

const aptitude_resolver::solution *
resolver_manager::do_get_solution(int max_steps, unsigned int solution_num,
				  std::set<aptitude_resolver_package> &visited_packages)
{
  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solution_num < solutions.size())
    return solutions[solution_num]->get_solution();

  while(solution_num >= solutions.size())
    {
      sol_l.release();

      try
	{
	  generic_solution<aptitude_universe> sol = resolver->find_next_solution(max_steps, &visited_packages);

	  sol_l.acquire();

	  bool is_keep_all_solution =
	    (sol.get_choices() == resolver->get_keep_all_solution());

	  solutions.push_back(new solution_information(new std::vector<resolver_interaction>(actions_since_last_solution),
						       ticks_since_last_solution + max_steps,
						       new aptitude_resolver::solution(sol.clone()),
						       is_keep_all_solution));
	  actions_since_last_solution.clear();
	  sol_l.release();
	}
      catch(const InterruptedException &e)
	{
	  ticks_since_last_solution += e.get_steps();
	  throw e;
	}
      catch(NoMoreTime)
	{
	  ticks_since_last_solution += max_steps;
	  throw NoMoreTime();
	}
      catch(NoMoreSolutions)
	{
	  throw NoMoreSolutions();
	}
      catch(cwidget::util::Exception &e)
	{
	  sol_l.acquire();
	  solution_search_aborted = true;
	  solution_search_abort_msg = e.errmsg();
	  throw;
	}
    }

  return solutions[solution_num]->get_solution();
}

/** A continuation that works by either placing \b true in the Boolean
 *  variable corresponding to the thrown exception, or updating the
 *  given solution, then signalling the given condition.  If the
 *  search terminated with an exception, we set the output solution to
 *  NULL and set abort_msg to the exception's description.
 */
class solution_return_continuation : public resolver_manager::background_continuation
{
  const generic_solution<aptitude_universe> * &sol;
  std::string &abort_msg;
  bool &oot;
  bool &oos;
  cwidget::threads::mutex &m;
  cwidget::threads::condition &c;
public:
  solution_return_continuation(const generic_solution<aptitude_universe> * &_sol,
			       std::string &_abort_msg,
			       bool &_oot,
			       bool &_oos,
			       cwidget::threads::mutex &_m,
			       cwidget::threads::condition &_c)
    :sol(_sol), abort_msg(_abort_msg), oot(_oot), oos(_oos), m(_m), c(_c)
  {
  }

  void success(const generic_solution<aptitude_universe> &result)
  {
    cwidget::threads::mutex::lock l(m);

    sol = &result;
    c.wake_all();
  }

  void no_more_time()
  {
    cwidget::threads::mutex::lock l(m);

    oot = true;
    c.wake_all();
  }

  void no_more_solutions()
  {
    cwidget::threads::mutex::lock l(m);
    oos = true;
    c.wake_all();
  }

  void interrupted()
  {
    // Should never happen, since we hold the big lock.
    abort();
  }

  void aborted(const std::string &errmsg)
  {
    cwidget::threads::mutex::lock l(m);

    sol = NULL;
    abort_msg = errmsg;
    c.wake_all();
  }
};

/** \brief Sadly, we can't easily save the real exception and reuse it
 *  :(.
 */
class SolutionSearchAbortException : public cwidget::util::Exception
{
  std::string msg;

public:
  SolutionSearchAbortException(const std::string &_msg)
  {
    msg = _msg;
  }

  std::string errmsg() const
  {
    return msg;
  }
};

const aptitude_resolver::solution &resolver_manager::get_solution(unsigned int solution_num,
								  int max_steps)
{
  cwidget::threads::mutex::lock l(mutex);

  eassert(resolver);

  {
    cwidget::threads::mutex::lock l2(solutions_mutex);
    if(solution_num < solutions.size())
      return *solutions[solution_num]->get_solution();
  }


  const generic_solution<aptitude_universe> *sol = NULL;
  std::string abort_msg;
  bool oot = false;
  bool oos = false;
  cwidget::threads::mutex m;
  cwidget::threads::condition c;

  {
    boost::shared_ptr<background_continuation> k;
    k.reset(new solution_return_continuation(sol, abort_msg,
					     oot, oos, m, c));
    get_solution_background(solution_num, max_steps, k,
			    &inline_continuation_trampoline);
  }
  l.release();

  cwidget::threads::mutex::lock cond_l(m);

  while(!sol && !oot && !oos)
    c.wait(cond_l);

  if(oot)
    throw NoMoreTime();
  else if(oos)
    throw NoMoreSolutions();
  else if(sol == NULL)
    throw SolutionSearchAbortException(abort_msg);

  return *sol;
}

bool resolver_manager::get_is_keep_all_solution(unsigned int solution_num,
						int max_steps)
{
  cwidget::threads::mutex::lock l(mutex);

  get_solution(solution_num, max_steps);

  {
    cwidget::threads::mutex::lock l2(solutions_mutex);
    eassert(solution_num < solutions.size());

    return solutions[solution_num]->get_is_keep_all_solution();;
  }
}

void resolver_manager::get_solution_background(unsigned int solution_num,
					       int max_steps,
					       const boost::shared_ptr<background_continuation> &k,
					       post_thunk_f post_thunk)
{
  cwidget::threads::mutex::lock l(mutex);

  // It's necessary to stop the background thread because we might be
  // decreasing the maximum number of steps to search.
  background_suspender bs(*this);

  eassert(resolver_exists());

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solution_num < solutions.size())
    {
      const generic_solution<aptitude_universe> *sol = solutions[solution_num]->get_solution();
      sol_l.release();

      k->success(*sol);
      return;
    }
  sol_l.release();


  cwidget::threads::mutex::lock control_lock(background_control_mutex);
  pending_jobs.push(job_request(solution_num, max_steps, k, post_thunk));
  background_control_cond.wake_all();
}

class blocking_continuation : public resolver_manager::background_continuation
{
  /** The real continuation */
  boost::shared_ptr<resolver_manager::background_continuation> k;

  /** The solution for which we are searching. */
  unsigned int solution_num;

  /** The channel through which the result should be announced. */
  cwidget::threads::box<bool> &result_box;

  /** The number of steps to try searching for a solution after this if
   *  time runs out.
   */
  int remaining_steps;

  /** The manager associated with this continuation. */
  resolver_manager &m;

  /** \brief How to invoke thunks safely. */
  post_thunk_f post_thunk;

public:
  blocking_continuation(const boost::shared_ptr<background_continuation> &_k,
			unsigned int _solution_num,
			cwidget::threads::box<bool> &_result_box,
			int _remaining_steps,
			resolver_manager &_m,
			post_thunk_f _post_thunk)
    : k(_k), solution_num(_solution_num), result_box(_result_box),
      remaining_steps(_remaining_steps), m(_m),
      post_thunk(_post_thunk)
  {
  }

  void success(const generic_solution<aptitude_universe> &sol)
  {
    sigc::slot<void> success_slot =
      sigc::bind(sigc::mem_fun(*k,
			       &background_continuation::success),
		 sol);

    post_thunk(make_keepalive_slot(success_slot, k));
    result_box.put(true);
  }

  void no_more_solutions()
  {
    sigc::slot<void> no_more_solutions_slot =
      sigc::mem_fun(*k, &background_continuation::no_more_solutions);

    post_thunk(make_keepalive_slot(no_more_solutions_slot, k));
    result_box.put(true);
  }

  void no_more_time()
  {
    m.get_solution_background(solution_num, remaining_steps, k, post_thunk);
    k.reset();
    result_box.put(false);
  }

  void interrupted()
  {
    // Give up and run in the background.
    m.get_solution_background(solution_num, remaining_steps, k, post_thunk);
    k.reset();
    result_box.put(false);
  }

  void aborted(const std::string &errmsg)
  {
    sigc::slot<void> aborted_slot =
      sigc::bind(sigc::mem_fun(*k, &background_continuation::aborted),
		 errmsg);

    post_thunk(make_keepalive_slot(aborted_slot, k));
    result_box.put(true);
  }
};

template<typename T>
void resolver_manager::resolver_manipulation(const T &t,
					     void (generic_problem_resolver<aptitude_universe>::*action)(const T &, undo_group *),
					     const resolver_interaction &act)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  undo_group *undo = new undo_group;
  (resolver->*action)(t, undo);
  if(undo->empty())
    delete undo;
  else
    undos->add_item(undo);

  actions_since_last_solution.push_back(act);

  l.release();
  bs.unsuspend();
  state_changed();
}

void resolver_manager::reject_break_holds()
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  std::auto_ptr<undo_group> undo(new undo_group);

  for(aptitude_universe::package_iterator pi = resolver->get_universe().packages_begin();
      !pi.end(); ++pi)
    {
      const aptitude_universe::package p = *pi;

      for(aptitude_universe::package::version_iterator vi = p.versions_begin(); !vi.end(); ++vi)
	{
	  const aptitude_universe::version v = *vi;
	  if(resolver->is_break_hold(v))
	    {
	      actions_since_last_solution.push_back(resolver_interaction::RejectVersion(v));
	      reject_version(v);
	    }
	}
    }

  if(!undo->empty())
    undos->add_item(undo.release());

  l.release();
  bs.unsuspend();
  state_changed();
}

void resolver_manager::reject_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::reject_version,
			resolver_interaction::RejectVersion(ver));
  version_accept_reject_changed(ver);
}

void resolver_manager::unreject_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::unreject_version,
			resolver_interaction::UnRejectVersion(ver));
  version_accept_reject_changed(ver);
}

bool resolver_manager::is_rejected(const aptitude_resolver_version &ver)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_rejected(ver);
}

void resolver_manager::mandate_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::mandate_version,
			resolver_interaction::MandateVersion(ver));
  version_accept_reject_changed(ver);
}

void resolver_manager::unmandate_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::unmandate_version,
			resolver_interaction::UnMandateVersion(ver));
  version_accept_reject_changed(ver);
}

bool resolver_manager::is_mandatory(const aptitude_resolver_version &ver)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_mandatory(ver);
}

void resolver_manager::harden_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::harden,
			resolver_interaction::HardenDep(dep));
  break_dep_accept_reject_changed(dep);
}

void resolver_manager::unharden_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::unharden,
			resolver_interaction::UnHardenDep(dep));
  break_dep_accept_reject_changed(dep);
}

bool resolver_manager::is_hardened(const aptitude_resolver_dep &dep)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_hardened(dep);
}

void resolver_manager::approve_broken_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::approve_break,
			resolver_interaction::ApproveBrokenDep(dep));
  break_dep_accept_reject_changed(dep);
}

void resolver_manager::unapprove_broken_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::unapprove_break,
			resolver_interaction::UnApproveBrokenDep(dep));
  break_dep_accept_reject_changed(dep);
}

bool resolver_manager::is_approved_broken(const aptitude_resolver_dep &dep)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver != NULL);

  return resolver->is_approved_broken(dep);
}

bool resolver_manager::has_undo_items()
{
  cwidget::threads::mutex::lock l(mutex);

  return undos->size() > 0;
}

bool resolver_manager::undo()
{
  cwidget::threads::mutex::lock l(mutex);

  if(undos->size() > 0)
    {
      background_suspender bs(*this);

      undos->undo();

      actions_since_last_solution.push_back(resolver_interaction::Undo());

      bs.unsuspend();
      l.release();

      state_changed();

      return true;
    }
  else
    return false;
}

void resolver_manager::select_solution(unsigned int solnum)
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solnum >= 0 && solnum <= solutions.size())
    selected_solution = solnum;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::discard_error_information()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  solution_search_aborted = false;
  solution_search_abort_msg.clear();

  sol_l.release();
  l.release();

  state_changed();
}

void resolver_manager::select_next_solution()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(selected_solution < solutions.size())
    ++selected_solution;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::select_previous_solution()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(selected_solution > 0)
    --selected_solution;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::tweak_score(const pkgCache::PkgIterator &pkg,
				   const pkgCache::VerIterator &ver,
				   int score)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  eassert(resolver_exists());
  eassert(resolver->fresh());

  aptitude_resolver_version res_ver;
  if(ver.end())
    res_ver = aptitude_resolver_version::make_removal(pkg, *cache_file);
  else
    res_ver = aptitude_resolver_version::make_install(ver, *cache_file);

  resolver->add_version_score(res_ver, score);
}

void resolver_manager::dump(ostream &out)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  if(!resolver_exists())
    return;

  // First, dump the universe.
  dump_universe(resolver->get_universe(), out);

  // Now dump the scores as a test instruction.
  out << "TEST " << resolver->get_step_score() << " "
      << resolver->get_broken_score() << " "
      << resolver->get_unresolved_soft_dep_score() << " "
      << resolver->get_infinity() << " "
      << resolver->get_full_solution_score() << " ";

  resolver->dump_scores(out);

  out << "EXPECT ( " << aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", defaultStepLimit) << " ANY )" << std::endl;
}

void resolver_manager::maybe_start_solution_calculation(const boost::shared_ptr<background_continuation> &k,
							post_thunk_f post_thunk)
{
  state st = state_snapshot();

  if(st.resolver_exists &&
     st.selected_solution == st.generated_solutions &&
     !st.solutions_exhausted &&
     !st.background_thread_active &&
     !st.background_thread_aborted)
    {
      const int selected = st.selected_solution;
      // TODO: duplication of information!  These config values should
      // be moved into a central function that everyone else can call.
      const int limit = aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", defaultStepLimit);

      if(limit > 0)
	get_solution_background(selected, limit, k, post_thunk);
    }
}

// Safe resolver logic:

class resolver_manager::safe_resolver_continuation : public resolver_manager::background_continuation
{
  boost::shared_ptr<background_continuation> real_continuation;
  resolver_manager *manager;
  // Used to update the statistics in the manager.
  generic_solution<aptitude_universe> last_sol;
  post_thunk_f post_thunk;

  safe_resolver_continuation(const boost::shared_ptr<background_continuation> &_real_continuation,
			     resolver_manager *_manager,
			     const generic_solution<aptitude_universe> &_last_sol,
			     post_thunk_f _post_thunk)
    : real_continuation(_real_continuation),
      manager(_manager),
      last_sol(_last_sol),
      post_thunk(_post_thunk)
  {
  }

public:
  safe_resolver_continuation(const boost::shared_ptr<background_continuation> &_real_continuation,
			     resolver_manager *_manager,
			     post_thunk_f _post_thunk)
    : real_continuation(_real_continuation),
      manager(_manager),
      post_thunk(_post_thunk)
  {
  }

  void success(const generic_solution<aptitude_universe> &sol)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolver());

    LOG_TRACE(logger, "safe_resolve_deps: got intermediate solution: " << sol);

    typedef generic_choice<aptitude_universe> choice;
    typedef generic_choice_set<aptitude_universe> choice_set;

    // If the solution changed (i.e., we managed to find some new
    // upgrades), we should try again; otherwise we've found the
    // solution that we'll return to the user.
    if(!(last_sol.valid() &&
	 sol.get_choices() == last_sol.get_choices()))
      {
	// The solution changed; prepare the resolver for the next
	// stage.


	// Mandate all the upgrades and installs in this solution,
	// then ask for the next solution.  The goal is to try to
	// find the best solution, not just some solution, but to
	// preserve our previous decisions in order to avoid
	// thrashing around between alternatives.
	for(choice_set::const_iterator it = sol.get_choices().begin();
	    it != sol.get_choices().end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		{
		  const pkgCache::PkgIterator p(it->get_ver().get_pkg());

		  if(it->get_ver().get_ver() != p.CurrentVer())
		    {
		      LOG_DEBUG(logger,
				"safe_resolve_deps: Mandating the upgrade or install " << it->get_ver()
				<< ", since it was produced as part of a solution.");
		      manager->mandate_version(it->get_ver());
		    }
		  else
		    {
		      LOG_DEBUG(logger,
				"safe_resolve_deps: Not mandating " << it->get_ver()
				<< ", since it is a hold.");
		    }
		}

		break;

	      default:
		// Nothing to do otherwise.
		break;
	      }
	  }


	// NULL out the sub-continuation so that we don't accidentally
	// trigger it twice.
	boost::shared_ptr<background_continuation> k = real_continuation;
	real_continuation.reset();


	boost::shared_ptr<safe_resolver_continuation> safe_resolver_k;
	safe_resolver_k.reset(new safe_resolver_continuation(k, manager, sol, post_thunk));

	// Hold the global lock on the solution so that we can
	// guarantee that we're calculating the next solution.
	// (otherwise there's a potential race between
	// generated_solutions_count() and get_solution_background())
	cwidget::threads::mutex::lock l(manager->mutex);

	const int limit = aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", defaultStepLimit);

	manager->get_solution_background(manager->generated_solution_count(),
					 limit, safe_resolver_k, post_thunk);
      }
    else
      {
	// Internal error that should never happen, but try to survive
	// if it does.
	LOG_FATAL(logger,
		  "safe_resolve_deps: Internal error: the resolver unexpectedly produced the same result twice: " << last_sol << " = " << sol);

	// Note: no need to use post_thunk, since we've already passed
	// through it to get here.
	if(real_continuation.get() != NULL)
	  real_continuation->success(sol);
	else
	  LOG_WARN(logger, "safe_resolve_deps: should send the end solution to the real continuation, but it's NULL.");
      }
  }

  void no_more_solutions()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolver());

    if(last_sol.valid())
      {
	LOG_TRACE(logger, "safe_resolve_deps: no more solutions; returning the last seen solution.");

	if(real_continuation.get() != NULL)
	  real_continuation->success(last_sol);
	else
	  LOG_WARN(logger, "safe_resolve_deps: should return the last seen solution, but the continuation is NULL.");
      }
    else
      {
	LOG_TRACE(logger, "safe_resolve_deps: unable to find any solutions.");

	if(real_continuation.get() != NULL)
	  real_continuation->no_more_solutions();
	else
	  LOG_WARN(logger, "safe_resolve_deps: should report that there are no solutions, but the continuation is NULL.");
      }
  }

  void no_more_time()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolver());

    if(last_sol.valid())
      {
	LOG_TRACE(logger, "safe_resolve_deps: ran out of time searching for a solution; returning the last one that was computed.");

	if(real_continuation.get() != NULL)
	  real_continuation->success(last_sol);
	else
	  LOG_WARN(logger, "safe_resolve_deps: should return the last seen solution, but the continuation is NULL.");
      }
    else
      {
	LOG_TRACE(logger, "safe_resolve_deps: ran out of time before finding any solutions.");

	if(real_continuation.get() != NULL)
	  real_continuation->no_more_time();
	else
	  LOG_WARN(logger, "safe_resolve_deps: should report that we ran out of time, but the continuation is NULL.");
      }
  }

  void interrupted()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolver());

    // Should never happen.  In fact, no code calls interrupted() --
    // is that a vestigial function?
    LOG_WARN(logger, "safe_resolve_deps: someone interrupted the solution search!");
  }

  void aborted(const std::string &errmsg)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolver());

    // Should we try to return the current solution if there is one?
    LOG_FATAL(logger, "safe_resolve_deps: aborted by exception: " << errmsg);
    if(real_continuation.get() != NULL)
      real_continuation->aborted(errmsg);
    else
      LOG_WARN(logger, "safe_resolve_deps: should report that we were interrupted by an exception, but the continuation is NULL.  Exception: " << errmsg);
  }
};

void resolver_manager::setup_safe_resolver(bool no_new_installs, bool no_new_upgrades)
{
  eassert(resolver_exists());

  logging::LoggerPtr logger(Loggers::getAptitudeResolverSafeResolverSetup());

  LOG_TRACE(logger,
	    "setup_safe_resolver: Setting up the resolver state for safe dependency resolution.");

  reset_resolver();

  background_suspender bs(*this);

  for(pkgCache::PkgIterator p = (*cache_file)->PkgBegin();
      !p.end(); ++p)
    {
      // Forbid the resolver from removing installed packages.
      if(!p.CurrentVer().end())
	{
	  aptitude_resolver_version remove_p =
	    aptitude_resolver_version::make_removal(p,
						    *cache_file);

	  LOG_DEBUG(logger,
		    "setup_safe_resolver: Rejecting the removal of the package " << remove_p.get_package() << ".");

	  reject_version(remove_p);
	}

      // Look up the version that's to-be-installed according to the
      // resolver's initial state.
      aptitude_resolver_package aptitude_p(p, *cache_file);
      aptitude_resolver_version p_initial_version =
	resolver->get_initial_state().version_of(aptitude_p);

      // Forbid all real versions that aren't the current version or
      // the candidate version.
      for(pkgCache::VerIterator v = p.VersionList();
	  !v.end(); ++v)
	{
	  // For our purposes, all half-unpacked etc states are
	  // installed.
	  const bool p_is_installed =
	    p->CurrentState != pkgCache::State::NotInstalled &&
	    p->CurrentState != pkgCache::State::ConfigFiles;

	  bool p_will_install;
	  if(!p_initial_version.get_ver().end())
	    {
	      if(p->CurrentState == pkgCache::State::NotInstalled ||
		 p->CurrentState == pkgCache::State::ConfigFiles)
		p_will_install = true;
	      else
		p_will_install = (p.CurrentVer() != p_initial_version.get_ver());
	    }
	  else
	    p_will_install = false;

	  const bool v_is_a_new_install = !p_is_installed && !p_will_install;
	  const bool v_is_a_new_upgrade = p_is_installed && p_will_install;
	  const bool v_is_a_non_default_version = v != (*cache_file)[p].CandidateVerIter(*cache_file);

	  if(v != p.CurrentVer() &&
	     // Disallow installing not-installed packages that
	     // aren't marked for installation if
	     // no_new_installs is set.
	     ((v_is_a_new_install && no_new_installs) ||
	      (v_is_a_new_upgrade && no_new_upgrades) ||
	      v_is_a_non_default_version))
	    {
	      aptitude_resolver_version p_v =
		aptitude_resolver_version::make_install(v, *cache_file);

	      if(logger->isEnabledFor(logging::DEBUG_LEVEL))
		{
		  if(v_is_a_non_default_version)
		    LOG_DEBUG(logger, "setup_safe_resolver: Rejecting " << p_v << " (it is a non-default version).");
		  else if(v_is_a_new_install)
		    LOG_DEBUG(logger, "setup_safe_resolver: Rejecting " << p_v << " (it is a new install).");
		  else // if(v_is_a_new_upgrade)
		    LOG_DEBUG(logger, "setup_safe_resolver: Rejecting " << p_v << " (it is a new upgrade).");
		}

	      reject_version(p_v);
	    }
	}
    }
}

void resolver_manager::safe_resolve_deps_background(bool no_new_installs, bool no_new_upgrades,
						    const boost::shared_ptr<background_continuation> &k,
						    post_thunk_f post_thunk)
{
  setup_safe_resolver(no_new_installs, no_new_upgrades);
  maybe_start_solution_calculation(boost::make_shared<safe_resolver_continuation>(k, this, post_thunk),
				   post_thunk);
}

