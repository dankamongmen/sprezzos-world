// cmdline_resolver.cc
//
//   Copyright (C) 2005-2010 Daniel Burrows
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


// Local includes:
#include "cmdline_resolver.h"

#include "cmdline_action.h"
#include "cmdline_common.h"
#include "cmdline_prompt.h"
#include "cmdline_show.h"
#include "cmdline_show_broken.h"
#include "cmdline_spinner.h"
#include "cmdline_util.h"
#include "terminal.h"

#include <aptitude.h>
#include <solution_fragment.h>

#include <generic/apt/aptcache.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/resolver_manager.h>
#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>
#include <generic/util/util.h>


// System includes:
#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>
#include <boost/ref.hpp>
#include <boost/shared_ptr.hpp>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/ssprintf.h>

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <iostream>
#include <fstream>
#include <sstream>

#include <algorithm>

using namespace std;

using aptitude::cmdline::terminal_metrics;
using boost::shared_ptr;

typedef generic_solution<aptitude_universe> aptitude_solution;
typedef generic_choice<aptitude_universe> choice;
typedef generic_choice_set<aptitude_universe> choice_set;

namespace cw = cwidget;

/** Generate a cw::fragment describing a solution as an ordered sequence
 *  of actions.
 *
 *  \param ids If not NULL, updated with a mapping from the string
 *  keys identifying choices in the solution to the associated
 *  choices.
 */
static cw::fragment *solution_story(const aptitude_solution &s,
				    std::map<std::string, choice> *ids)
{
  std::vector<choice> choices;
  for(choice_set::const_iterator it = s.get_choices().begin();
      it != s.get_choices().end(); ++it)
    choices.push_back(*it);

  sort(choices.begin(), choices.end(), aptitude_solution::choice_id_compare());


  vector<cw::fragment *> fragments;

  for(vector<choice>::const_iterator i = choices.begin();
      i != choices.end(); ++i)
    {
      fragments.push_back(cw::fragf("%ls",
				    dep_text(i->get_dep().get_dep()).c_str()));
      fragments.push_back(cw::fragf(" -> %F%n",
				    indentbox(0, 4, choice_fragment(*i))));
    }

  if(ids == NULL)
    return cw::sequence_fragment(fragments);
  else
    {
      std::vector<cw::fragment *> id_fragments, flag_fragments;

      for(std::vector<choice>::size_type i = 0; i < choices.size(); ++i)
	{
	  std::string key = boost::lexical_cast<std::string>(i + 1);
	  id_fragments.push_back(NULL);
	  id_fragments.push_back(cw::fragf("%s)", key.c_str()));
	  (*ids)[key] = choices[i];

          flag_fragments.push_back(NULL);
	  flag_fragments.push_back(choice_state_fragment(choices[i]));
	}


      std::vector<cw::fragment_column_entry> columns;
      columns.push_back(cw::fragment_column_entry(false, true,
						  1, cw::fragment_column_entry::top,
						  id_fragments));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  NULL));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  flag_fragments));
      columns.push_back(cw::fragment_column_entry(false, false,
						  1, cw::fragment_column_entry::top,
						  NULL));
      columns.push_back(cw::fragment_column_entry(false, true,
						  1, cw::fragment_column_entry::top,
						  fragments));

      return cw::fragment_columns(columns);
    }
}

void cmdline_dump_resolver()
{
  string dumpfile = aptcfg->Find(PACKAGE "::CmdLine::Resolver-Dump", "");
  if(!dumpfile.empty())
    {
      ofstream f(dumpfile.c_str());
      if(!f)
	_error->Errno("dump_resolver", _("Unable to open %s for writing"), dumpfile.c_str());
      else
	{
	  resman->dump(f);

	  if(!f)
	    _error->Errno("dump_resolver", _("Error writing resolver state to %s"), dumpfile.c_str());
	  else
	    cout << _("Resolver state successfully written!");
	}
    }
}

static void setup_resolver(pkgset &to_install,
			   pkgset &to_hold,
			   pkgset &to_remove,
			   pkgset &to_purge,
			   bool force_no_change)
{
  if(!resman->resolver_exists())
    return;

  // Make sure the resolver is in the initial state so we can twiddle
  // it.
  resman->reset_resolver();

  if(aptcfg->FindB(PACKAGE "::CmdLine::Resolver-Debug", false))
    resman->set_debug(true);

  // For all packages that the user listed on the command-line (i.e.,
  // all in to_install, to_hold, to_remove, and to_purge), tell the
  // resolver to try *really really hard* to avoid altering their
  // state.
  if(force_no_change && resman->resolver_exists())
    {
      pkgset *sets[4]={&to_install, &to_hold, &to_remove, &to_purge};
      int tweak_amt=aptcfg->FindI(PACKAGE "::CmdLine::Request-Strictness", 10000);

      for(int i=0; i<4; ++i)
	{
	  pkgset &S=*sets[i];

	  for(pkgset::const_iterator p=S.begin();
	      p!=S.end(); ++p)
	    {
	      pkgDepCache::StateCache &state=(*apt_cache_file)[*p];
	      pkgCache::VerIterator instver=state.InstVerIter(*apt_cache_file);

	      for(pkgCache::VerIterator v=p->VersionList();
		  !v.end(); ++v)
		if(instver == v)
		  resman->tweak_score(*p, v,
				     tweak_amt);

	      if(instver.end())
		resman->tweak_score(*p, pkgCache::VerIterator(*apt_cache_file),
				   tweak_amt);
	    }
	}
    }

  cmdline_dump_resolver();
}

static inline cw::fragment *flowindentbox(int i1, int irest, cw::fragment *f)
{
  return indentbox(i1, irest, flowbox(f));
}

static void resolver_help(ostream &out,
                          const shared_ptr<terminal_metrics> &term_metrics)
{
  cw::fragment *f=indentbox(2, 2,
			cw::fragf(_("y: %F"
				"n: %F"
				"q: %F"
				".: %F"
				",: %F"
				"o: %F"
				"e: %F"
				"x: %F"
				"r (ID|pkg ver) ...: %F%n"
				"a (ID|pkg ver) ...: %F%n"
                                "<ID>: %F%n"
				"<ACTION> pkg... : %F%n"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"),
			      flowindentbox(0, 3,
					    cw::fragf(_("accept the proposed changes"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("reject the proposed changes and search for another solution"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("give up and quit the program"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("move to the next solution"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("move to the previous solution"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("toggle between the contents of the solution and an explanation of the solution"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("examine the solution in the visual user interface"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("abort automatic dependency resolution; resolve dependencies by hand instead"))),
			      flowindentbox(0, 15,
					    cw::fragf(_("reject the given package versions; don't display any solutions in which they occur.  Enter UNINST instead of a version to reject removing the package.  ID is the integer printed to the left of the action."))),
			      flowindentbox(0, 15,
					    cw::fragf(_("accept the given package versions; display only solutions in which they occur.  Enter UNINST instead of a version to accept removing the package.  ID is the integer printed to the left of the action."))),
                              flowindentbox(0, 15,
                                            cw::fragf(_("display information about the action labeled ID from the solution.  The label is the integer printed to the left of the action."))),
			      flowindentbox(0, 3,
					    cw::fragf(_("adjust the state of the listed packages, where ACTION is one of:"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'+' to install packages"))),
			      flowindentbox(0, 5,
					    cw::fragf(_("'+M' to install packages and immediately flag them as automatically installed"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'-' to remove packages"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'_' to purge packages"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'=' to place packages on hold"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("':' to keep packages in their current state without placing them on hold"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'&M' to mark packages as automatically installed"))),
			      flowindentbox(0, 4,
					    cw::fragf(_("'&m' to mark packages as manually installed"))),
			      flowindentbox(0, 3,
					    cw::fragf(_("Adjustments will cause the current solution to be discarded and recalculated as necessary.")))));

  const unsigned int screen_width = term_metrics->get_screen_width();
  out << f->layout(screen_width, screen_width, cwidget::style());
  delete f;
}

// Given several versions with the same VerStr, choose one to output.
static pkgCache::VerIterator choose_version(const vector<pkgCache::VerIterator> &choices)
{
  eassert(!choices.empty());

  if(choices.size() == 1)
    return choices.front();

  cout << ssprintf(_("The version %s is available in the following archives:"), choices.front().VerStr()) << endl;

  for(vector<pkgCache::VerIterator>::size_type i = 0;
      i < choices.size(); ++i)
    cout << ssprintf(" (%d) %s", (int)(i+1), archives_text(choices[i]).c_str()) << endl;

  while(1)
    {
      string response = prompt_string(ssprintf(_("Select the version of %s that should be used: "), choices.front().ParentPkg().FullName(true).c_str()));

      int i;
      istringstream in(response);
      in >> ws >> i >> ws;

      if(!in || !in.eof() || i < 1 || i > (signed)choices.size())
	cerr << ssprintf(_("Invalid response.  Please enter an integer between 1 and %d."), (int)choices.size()) << endl;
      else
	return choices[i];
    }
}

static void reject_or_mandate_version(const string &s,
				      const std::map<std::string, choice> &ids,
				      bool is_reject)
{
  istringstream in(s);

  in >> ws;

  if(in.eof())
    {
      cerr << ssprintf(_("Expected at least one package/version pair following '%c'"),
			 is_reject ? 'R' : 'A') << endl;
      return;
    }

  string pkgname;
  string vername;

  while(!in.eof())
    {
      in >> pkgname >> ws;

      choice c; // Initialized below.

      std::map<std::string, choice>::const_iterator found =
	ids.find(pkgname);
      if(found != ids.end())
	c = found->second;
      else
	{
	  if(in.eof())
	    {
	      cerr << ssprintf(_("Expected a version or \"%s\" after \"%s\""), "UNINST", pkgname.c_str()) << endl;
	      return;
	    }

	  in >> vername >> ws;

	  pkgCache::PkgIterator pkg((*apt_cache_file)->FindPkg(pkgname));

	  if(pkg.end())
	    {
	      cerr << ssprintf(_("No such package \"%s\""), pkgname.c_str()) << endl;
	      continue;
	    }

	  aptitude_universe::version ver =
	    aptitude_universe::version::make_removal(pkg,
						 *apt_cache_file);
	  if(stringcasecmp(vername, "UNINST") != 0)
	    {
	      vector<pkgCache::VerIterator> found;
	      for(pkgCache::VerIterator vi = pkg.VersionList(); !vi.end(); ++vi)
		if(vi.VerStr() == vername)
		  found.push_back(vi);

	      if(found.empty())
		{
		  cerr << ssprintf(_("%s has no version named \"%s\""),
				   pkgname.c_str(), vername.c_str()) << endl;
		  continue;
		}

	      ver = aptitude_universe::version::make_install(choose_version(found),
							     *apt_cache_file);

	      eassert(!ver.get_ver().end());
	      eassert(ver.get_pkg() == ver.get_ver().ParentPkg());
	    }

          c = choice::make_install_version(ver, -1);
	}

      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    aptitude_resolver_version ver(c.get_ver());
	    pkgCache::PkgIterator pkg(ver.get_pkg());
	    if(is_reject)
	      {
		if(resman->is_rejected(ver))
		  {
		    if(ver.get_ver().end())
		      cout << ssprintf(_("Allowing the removal of %s"),
				       pkg.FullName(true).c_str()) << endl;
		    else
		      cout << ssprintf(_("Allowing the installation of %s version %s (%s)"),
				       pkg.FullName(true).c_str(),
				       ver.get_ver().VerStr(),
				       archives_text(ver.get_ver()).c_str()) << endl;

		    resman->unreject_version(ver);
		  }
		else
		  {
		    if(ver.get_ver().end())
		      cout << ssprintf(_("Rejecting the removal of %s"),
				       pkg.FullName(true).c_str()) << endl;
		    else
		      cout << ssprintf(_("Rejecting the installation of %s version %s (%s)"),
				       pkg.FullName(true).c_str(),
				       ver.get_ver().VerStr(),
				       archives_text(ver.get_ver()).c_str()) << endl;

		    resman->reject_version(ver);
		  }
	      }
	    else
	      {
		if(resman->is_mandatory(ver))
		  {
		    if(ver.get_ver().end())
		      cout << ssprintf(_("No longer requiring the removal of %s"),
				       pkg.FullName(true).c_str()) << endl;
		    else
		      cout << ssprintf(_("No longer requiring the installation of %s version %s (%s)"),
				       pkg.FullName(true).c_str(), ver.get_ver().VerStr(),
				       archives_text(ver.get_ver()).c_str()) << endl;

		    resman->unmandate_version(ver);
		  }
		else
		  {
		    if(ver.get_ver().end())
		      cout << ssprintf(_("Requiring the removal of %s"),
				       pkg.FullName(true).c_str()) << endl;
		    else
		      cout << ssprintf(_("Requiring the installation of %s version %s (%s)"),
				       pkg.FullName(true).c_str(), ver.get_ver().VerStr(),
				       archives_text(ver.get_ver()).c_str()) << endl;

		    resman->mandate_version(ver);
		  }
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  {
	    aptitude_resolver_dep d(c.get_dep());

	    pkgCache::DepIterator start, end;
	    surrounding_or(d.get_dep(), start, end, d.get_dep().Cache());
	    std::ostringstream dep_rendering_stream;

	    if(start.end()) // Sanity-check.
	      dep_rendering_stream << "(??\?)";
	    else
	      {
		dep_rendering_stream << start.ParentPkg().FullName(true)
				     << " "
				     << start.ParentVer().VerStr()
				     << " "
				     << start.DepType()
				     << " ";
		bool first = true;

		while(!start.end() && start != end)
		  {
		    if(first)
		      first = false;
		    else
		      dep_rendering_stream << " | ";

		    dep_rendering_stream << start.TargetPkg().FullName(true).c_str()
					 << " "
					 << start.TargetVer();

		    ++start;
		  }
		std::string dep_rendering = dep_rendering_stream.str();

		if(is_reject)
		  {
		    if(resman->is_hardened(d))
		      {
			cout << ssprintf(_("Allowing this recommendation to be ignored: %s"),
					 dep_rendering.c_str()) << endl;
			resman->unharden_dep(d);
		      }
		    else
		      {
			cout << ssprintf(_("Always obeying this recommendation: %s"),
					 dep_rendering.c_str()) << endl;
			resman->harden_dep(d);
		      }
		  }
		else
		  {
		    if(resman->is_approved_broken(d))
		      {
			cout << ssprintf(_("No longer ignoring this recommendation: %s"),
					 dep_rendering.c_str()) << endl;
			resman->unapprove_broken_dep(d);
		      }
		    else
		      {
			cout << ssprintf(_("Ignoring this recommendation: %s"),
					 dep_rendering.c_str()) << endl;
			resman->approve_broken_dep(d);
		      }
		  }
	      }
	  }
	}
    }
}

void cmdline_resolver_show_choice(const choice &c,
				  const std::string &tag,
                                  const shared_ptr<terminal_metrics> &term_metrics)
{
  cw::fragment *info_fragment = NULL;
  bool is_rejected = false;
  bool is_approved = false;

  switch(c.get_type())
    {
    case choice::install_version:
      {
        pkgCache::VerIterator disp_ver = c.get_ver().get_ver();

        if(disp_ver.end())
          disp_ver = c.get_ver().get_pkg().CurrentVer();

        if(disp_ver.end())
          disp_ver = c.get_ver().get_pkg().VersionList();

        if(disp_ver.end())
          info_fragment = cw::fragf("Package: %s\n\n",
                                    c.get_ver().get_pkg().FullName(true).c_str());
        else
          info_fragment = cw::fragf("%F\n\n",
                                    version_file_fragment(disp_ver,
                                                          disp_ver.FileList(),
                                                          0));
      }

      is_rejected = resman->is_rejected(c.get_ver());
      is_approved = resman->is_mandatory(c.get_ver());
      break;

    case choice::break_soft_dep:
      info_fragment = cw::text_fragment("");
      is_rejected = resman->is_hardened(c.get_dep());
      is_approved = resman->is_approved_broken(c.get_dep());
      break;

    default:
      info_fragment = cw::text_fragment("");
      break;
    }

  cw::fragment *f =
    cw::fragf("%F%F%F",
	      flowbox(cw::fragf("%F: %F\n\n",
				cw::fragf(_("Action \"%s\""), tag.c_str()),
				choice_fragment(c))),
	      info_fragment,
	      flowbox(cw::fragf("%F\n\n%F%F\n%F\n",
                                // TODO: maybe this message could be
                                // improved -- we could probably print
                                // the dependency in a more
                                // traditional way and maybe explain
                                // which part failed?
                                //
                                // ForTranslators: the substitution is
                                // replaced in English with something
                                // like "Pkg1 depends upon Pkg2".
                                cw::fragf(_("This action was selected because %ls."),
                                          dep_text(c.get_dep().get_dep()).c_str()),
				is_rejected ? cw::fragf("%s\n", _("This action is currently rejected; it will not appear in new solutions.")) :
				is_approved ? cw::fragf("%s\n", _("This action is currently approved; it will be selected whenever possible.")) :
				cw::text_fragment(""),
				// ForTranslators: the "r" here is
				// for "reject", but should not be
				// translated since the commands in
				// aptitude's internal command-line
				// aren't translated.
				is_rejected ? cw::fragf(_("Enter \"r %s\" to allow this action to appear in new solutions."), tag.c_str())
				// ForTranslators: the "r" here is
				// for "reject", but should not be
				// translated since the commands in
				// aptitude's internal command-line
				// aren't translated.
                                            : cw::fragf(_("Enter \"r %s\" to prevent this action from appearing in new solutions."), tag.c_str()),
				// ForTranslators: the "a" here is
				// for "approve", but should not be
				// translated since the commands in
				// aptitude's internal command-line
				// aren't translated.
				is_approved ? cw::fragf(_("Enter \"a %s\" to cease requiring that new solutions include this action if possible."), tag.c_str())
				// ForTranslators: the "a" here is
				// for "approve", but should not be
				// translated since the commands in
				// aptitude's internal command-line
				// aren't translated.
                                            : cw::fragf(_("Enter \"a %s\" to require that new solutions include this action if possible."), tag.c_str()))));

  const unsigned int screen_width = term_metrics->get_screen_width();
  cwidget::fragment_contents lines = f->layout(screen_width, screen_width, cwidget::style());

  delete f;

  cout << lines << endl;
}

// The command-line blocks until the continuation hits, so we can just
// run continuations in the background thread.
void cmdline_resolver_trampoline(const sigc::slot<void> &f)
{
  f();
}

// TODO: make this generic?
class cmdline_resolver_continuation : public resolver_manager::background_continuation
{
public:
  struct resolver_result
  {
    /** If \b true, then NoMoreSolutions was thrown. */
    bool out_of_solutions;

    /** If \b true, then NoMoreTime was thrown. */
    bool out_of_time;

    /** If \b true, then we got a fatal exception. */
    bool aborted;

    /** If aborted is \b true, then this is the message associated
     *  with the deadly exception.
     */
    std::string abort_msg;

    /** If out_of_solutions, out_of_time, and aborted are \b false,
    this is * the result returned by the resolver.
     */
    aptitude_solution sol;

    resolver_result()
      : out_of_solutions(false), out_of_time(false)
    {
    }

  private:
    resolver_result(bool _out_of_solutions,
		    bool _out_of_time,
		    bool _aborted,
		    std::string _abort_msg,
		    const aptitude_solution &_sol)
      : out_of_solutions(_out_of_solutions),
	out_of_time(_out_of_time),
	aborted(_aborted),
	abort_msg(_abort_msg),
	sol(_sol)
    {
    }

  public:
    static resolver_result OutOfSolutions()
    {
      return resolver_result(true, false, false, "", aptitude_solution());
    }

    static resolver_result OutOfTime()
    {
      return resolver_result(false, true, false, "", aptitude_solution());
    }

    static resolver_result Aborted(const std::string &msg)
    {
      return resolver_result(false, false, true, msg, aptitude_solution());
    }

    static resolver_result Success(const aptitude_solution &sol)
    {
      return resolver_result(false, false, false, "", sol);
    }
  };

private:
  cwidget::threads::box<resolver_result> &retbox;

public:
  cmdline_resolver_continuation(cwidget::threads::box<resolver_result> &_retbox)
    : retbox(_retbox)
  {
  }

  void success(const aptitude_solution &sol)
  {
    retbox.put(resolver_result::Success(sol));
  }

  void no_more_solutions()
  {
    retbox.put(resolver_result::OutOfSolutions());
  }

  void no_more_time()
  {
    retbox.put(resolver_result::OutOfTime());
  }

  void interrupted()
  {
    abort();
  }

  void aborted(const std::string &errmsg)
  {
    retbox.put(resolver_result::Aborted(errmsg));
  }
};

class CmdlineSearchAbortedException : public cwidget::util::Exception
{
  std::string msg;

public:
  CmdlineSearchAbortedException(const std::string &_msg)
    : msg(_msg)
  {
  }

  std::string errmsg() const { return msg; }
};

class CmdlineSearchDisabledException : public cwidget::util::Exception
{
  std::string msg;

public:
  CmdlineSearchDisabledException(const std::string &_msg)
    : msg(_msg)
  {
  }

  std::string errmsg() const { return msg; }
};

// Displays a spinner while we wait for the background thread to spit
// out a solution.
static aptitude_solution wait_for_solution(cwidget::threads::box<cmdline_resolver_continuation::resolver_result> &retbox,
					   cmdline_spinner &spin)
{
  cmdline_resolver_continuation::resolver_result res;
  bool done = false;
  // The number of milliseconds to step per display.
  long spin_step = aptcfg->FindI(PACKAGE "::Spin-Interval", 500);

  do
    {
      timeval until;
      gettimeofday(&until, 0);

      until.tv_usec += spin_step * 1000L;
      until.tv_sec += until.tv_usec / (1000L * 1000L);
      until.tv_usec = until.tv_usec % (1000L * 1000L);

      timespec until_ts;
      until_ts.tv_sec = until.tv_sec;
      until_ts.tv_nsec = until.tv_usec * 1000;

      done = retbox.timed_take(res, until_ts);

      if(!done)
	{
	  resolver_manager::state state = resman->state_snapshot();

	  spin.set_msg(ssprintf(_("open: %zd; closed: %zd; defer: %zd; conflict: %zd"),
				state.open_size, state.closed_size,
				state.deferred_size, state.conflicts_size));
	  spin.display();
	  spin.tick();
	}
    } while(!done);

  if(res.out_of_time)
    throw NoMoreTime();
  else if(res.out_of_solutions)
    throw NoMoreSolutions();
  else if(res.aborted)
    throw CmdlineSearchAbortedException(res.abort_msg);
  else
    return res.sol;
}

aptitude_solution calculate_current_solution(bool suppress_message,
                                             const shared_ptr<terminal_metrics> &term_metrics)
{
  const int step_limit = aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000);
  if(step_limit <= 0)
    {
      const std::string msg = ssprintf(_("Would resolve dependencies, but dependency resolution is disabled.\n   (%s::ProblemResolver::StepLimit = 0)\n"), PACKAGE);

      // It's important that the code path leading to here doesn't
      // access resman: the resolver won't exist in this case.
      throw CmdlineSearchDisabledException(msg);
    }

  if(!resman->resolver_exists())
    {
      const std::string msg = _("I want to resolve dependencies, but no dependency resolver was created.");

      throw CmdlineSearchAbortedException(msg);
    }



  if(resman->get_selected_solution() < resman->generated_solution_count())
    return resman->get_solution(resman->get_selected_solution(), 0);


  cmdline_spinner spin(aptcfg->FindI("Quiet", 0), term_metrics);

  if(!suppress_message)
    std::cout << _("Resolving dependencies...") << std::endl;

  cwidget::threads::box<cmdline_resolver_continuation::resolver_result> retbox;

  resman->get_solution_background(resman->generated_solution_count(),
				  step_limit,
				  boost::make_shared<cmdline_resolver_continuation>(boost::ref(retbox)),
				  cmdline_resolver_trampoline);

  return wait_for_solution(retbox, spin);
}

aptitude::cmdline::cmdline_resolver_result
cmdline_resolve_deps(pkgset &to_install,
		     pkgset &to_hold,
		     pkgset &to_remove,
		     pkgset &to_purge,
		     bool assume_yes,
		     bool force_no_change,
		     int verbose,
		     pkgPolicy &policy,
		     bool arch_only,
                     const shared_ptr<terminal_metrics> &term_metrics)
{
  bool story_is_default = aptcfg->FindB(PACKAGE "::CmdLine::Resolver-Show-Steps", false);

  while(!show_broken())
    {
      setup_resolver(to_install, to_hold, to_remove, to_purge,
		     force_no_change);
      aptitude_solution lastsol;

      // Stores the string IDs that can be used for accept/reject
      // commands.  Filled in when the solution is being rendered
      // (since the IDs are integers counting from the first entry in
      // the displayed lists).
      std::map<std::string, choice> ids;


      // The inner loop tries to generate solutions until some
      // packages are modified by the user (then the new set of broken
      // packages, if any, is displayed and we start over)
      bool modified_pkgs=false;

      bool redisplay = false;
      while(!modified_pkgs)
	try
	  {
	    try
	      {
		aptitude_solution sol = calculate_current_solution(true, term_metrics);

		if(_error->PendingError())
                  _error->DumpErrors();

		if(sol != lastsol || redisplay)
		  {
		    ids.clear();
		    // \todo display "the following actions..." only
		    // the first time through.
		    cw::fragment *f=cw::sequence_fragment(flowbox(cwidget::text_fragment(_("The following actions will resolve these dependencies:"))),
						  cwidget::newline_fragment(),
						  story_is_default
						    ? solution_story(sol, &ids)
						    : solution_fragment_with_ids(sol, ids),
						  NULL);

                    const unsigned int screen_width = term_metrics->get_screen_width();
		    cwidget::fragment_contents lines=f->layout(screen_width, screen_width, cwidget::style());

		    delete f;

		    cout << lines << endl;
		    lastsol=sol;
		  }

		redisplay = false;

		string response=assume_yes?"Y":prompt_string(_("Accept this solution? [Y/n/q/?] "));

		string::size_type loc=0;
		while(loc<response.size() && isspace(response[loc]))
		  ++loc;
		if(loc == response.size())
		  {
		    response='Y';
		    loc=0;
		  }

		switch(toupper(response[loc]))
		  {
		  case 'Y':
		    (*apt_cache_file)->apply_solution(calculate_current_solution(true, term_metrics), NULL);
		    modified_pkgs=true;
		    break;
		  case 'N':
		    {
		      unsigned int curr_count = resman->generated_solution_count();

		      if(curr_count>0)
			while(resman->get_selected_solution() < curr_count)
			  resman->select_next_solution();
		    }
		    break;
		  case 'Q':
		    cout << _("Abandoning all efforts to resolve these dependencies.") << endl;
		    return aptitude::cmdline::resolver_user_exit;
		  case 'X':
		    cout << _("Abandoning automatic dependency resolution and reverting to manual resolution.") << endl;
		    return aptitude::cmdline::resolver_incomplete;
		  case 'O':
		    {
		      story_is_default = !story_is_default;
		      ids.clear();
		      cw::fragment *f = story_is_default
			? solution_story(sol, &ids)
			: solution_fragment_with_ids(sol, ids);
                      const unsigned int screen_width = term_metrics->get_screen_width();
		      cout << f->layout(screen_width, screen_width,
					cwidget::style()) << endl;
		      delete f;
		      break;
		    }
		  case 'E':
		    ui_solution_screen();
		    break;
		  case 'R':
		    reject_or_mandate_version(string(response, 1), ids, true);
		    redisplay = true;
		    break;
		  case 'A':
		    reject_or_mandate_version(string(response, 1), ids, false);
		    redisplay = true;
		    break;
		  case '.':
		    resman->select_next_solution();
		    break;
		  case ',':
		    resman->select_previous_solution();
		    break;
		  case '?':
		    cout << _("The following commands are available:") << endl;
		    resolver_help(cout, term_metrics);
		    break;
		  case '+':
		  case '-':
		  case '=':
		  case '_':
		  case ':':
		    {
		      std::set<pkgCache::PkgIterator> seen_virtual_packages;
		      cmdline_parse_action(response, seen_virtual_packages,
					   to_install, to_hold,
					   to_remove, to_purge, verbose,
					   policy, arch_only, false,
                                           term_metrics);
		      modified_pkgs=true;
		    }
		    break;
		    // Undocumented debug feature:
		  case '~':
		    {
		      string fn=prompt_string(_("File to write resolver state to: "));
		      ofstream f(fn.c_str());
		      if(!f)
			_error->Errno("dump_resolver", _("Unable to open %s for writing"), fn.c_str());
		      else
			{
			  resman->dump(f);
			  if(!f)
			    _error->Errno("dump_resolver", _("Error writing resolver state to %s"), fn.c_str());
			  else
			    cout << _("Resolver state successfully written!");
			}
		    }
		    break;
		  default:
		    // Look for a choice tag in the first word of the
		    // string.  NB: checking only the first word
		    // requires special knowledge that choice tags are
		    // in fact single words.
		    {
		      std::string first_word;

		      while(loc < response.size() && !isspace(response[loc]))
			{
			  first_word += response[loc];
			  ++loc;
			}

		      std::map<std::string, choice>::const_iterator found =
			ids.find(first_word);
		      if(found == ids.end())
			{
			  cout << _("Invalid response; please enter one of the following commands:") << endl;
			  resolver_help(cout, term_metrics);
			}
		      else
			{
			  const choice &c = found->second;
			  cmdline_resolver_show_choice(c, first_word, term_metrics);
			}
		    }
		    break;
		  }
	      }
	    catch(NoMoreTime)
	      {
		bool done=false;
		while(!done)
		  {
		    string response;
// FIXME: translate Y, N
		    if(!assume_yes)
		      response = prompt_string(_("No solution found within the allotted time.  Try harder? [Y/n] "));

		    string::size_type loc=0;
		    while(loc<response.size() && isspace(response[loc]))
		      ++loc;
		    if(loc == response.size())
		      {
			loc=0;
			response='Y';
		      }
// FIXME: translate Y, N
		    switch(toupper(response[loc]))
		      {
		      case 'Y':
			try
			  {
			    calculate_current_solution(false, term_metrics);
			    done=true;
			  }
			catch(NoMoreTime)
			  {
			    // ignore and continue looping.
			  }
			// NoMoreSolutions flows to the outer catch.
			break;
		      case 'N':
			cout << _("Abandoning all efforts to resolve these dependencies.") << endl;
			return aptitude::cmdline::resolver_incomplete;
		      default:
			cout << _("Invalid response; please enter 'y' or 'n'.") << endl;
		      }
		  }
	      }
	  }
	catch(NoMoreSolutions)
	  {
	    if(resman->generated_solution_count()==0)
	      {
		cout << _("Unable to resolve dependencies!  Giving up...") << endl;
		return aptitude::cmdline::resolver_incomplete;
	      }
	    else
	      {
		cout << endl
		     << _("*** No more solutions available ***")
		     << endl
		     << endl;
		// Force it to re-print the last solution.
		resman->select_previous_solution();
		lastsol.nullify();
	      }
	  }
	catch(StdinEOFException)
	  {
	    throw;
	  }
	catch(const CmdlineSearchDisabledException &e)
	  {
	    cout << e.errmsg();
	    return aptitude::cmdline::resolver_incomplete;
	  }
	catch(cwidget::util::Exception &e)
	  {
            cout << _("*** ERROR: search aborted by fatal exception.  You may continue\n"
                      "           searching, but some solutions will be unreachable.")
		 << endl
		 << endl
		 << e.errmsg();

	    if(resman->generated_solution_count() == 0)
	      return aptitude::cmdline::resolver_incomplete;
	    else
	      {
		cout << endl << endl;

		// Force it to re-print the last solution.
		if(resman->get_selected_solution() == resman->generated_solution_count())
		  resman->select_previous_solution();
		lastsol.nullify();
	      }
	  }
    }

  return aptitude::cmdline::resolver_success;
}


namespace aptitude
{
  namespace cmdline
  {
    // Implements the --show-resolver-actions command-line parameters.
    void show_resolver_actions(const generic_solution<aptitude_universe> &solution,
                               const shared_ptr<terminal_metrics> &term_metrics)
    {
      if(solution.get_choices().size() > 0)
	{
	  // The previous line will say "resolving dependencies...";
	  // separate the solution from this message..
	  std::cout << std::endl;
	  std::auto_ptr<cw::fragment> story(solution_story(solution, NULL));
          const unsigned int screen_width = term_metrics->get_screen_width();
	  std::cout << story->layout(screen_width, screen_width, cwidget::style());
	}
    }

    // Take the first solution we can compute, returning false if we
    // failed to find a solution.
    bool safe_resolve_deps(int verbose,
                           bool no_new_installs,
                           bool no_new_upgrades,
                           bool show_story,
                           const shared_ptr<terminal_metrics> &term_metrics)
    {
      if(!resman->resolver_exists())
	return true;

      cmdline_dump_resolver();

      try
	{
	  cwidget::threads::box<cmdline_resolver_continuation::resolver_result> retbox;

	  resman->safe_resolve_deps_background(no_new_installs, no_new_upgrades,
					       boost::make_shared<cmdline_resolver_continuation>(boost::ref(retbox)),
					       cmdline_resolver_trampoline);

	  cmdline_spinner spin(aptcfg->FindI("Quiet", 0), term_metrics);
	  // TODO: maybe we should say "calculating upgrade" if we're
	  // running safe-upgrade?
	  std::cout << _("Resolving dependencies...") << std::endl;
	  generic_solution<aptitude_universe> sol = wait_for_solution(retbox, spin);

	  if(show_story)
	    show_resolver_actions(sol, term_metrics);

	  (*apt_cache_file)->apply_solution(sol, NULL);
	}
      // If anything goes wrong, we give up (silently if verbosity is disabled).
      catch(NoMoreTime)
	{
	  std::cout << cw::util::ssprintf(_("Unable to resolve dependencies for the upgrade because the resolver timed out.\n  You may be able to solve this problem by increasing\n  Aptitude::ProblemResolver::StepLimit (currently %d)."),
					  // TODO: bad magic number
					  // here -- get it from the
					  // backend directly instead.
					  aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 500000))
		    << std::endl;
	  return false;
	}
      catch(NoMoreSolutions)
	{
	  std::cout << _("Unable to resolve dependencies for the upgrade: no solution found.")
		    << std::endl;
	  return false;
	}
      catch(const cw::util::Exception &e)
	{
	  std::cout << cw::util::ssprintf(_("Unable to resolve dependencies for the upgrade: %s"), e.errmsg().c_str())
		    << std::endl;
	  return false;
	}

      return true;
    }
  }
}
