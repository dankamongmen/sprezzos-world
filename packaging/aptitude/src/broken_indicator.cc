// broken_indicator.cc
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

#include "broken_indicator.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptcache.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <generic/util/util.h>

#include <cwidget/config/colors.h>
#include <cwidget/config/keybindings.h>
#include <cwidget/fragment.h>
#include <cwidget/widgets/text_layout.h>
#include <cwidget/toplevel.h>

#include <apt-pkg/pkgsystem.h>

#include <string>
#include <vector>

using namespace std;
namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

typedef generic_solution<aptitude_universe> aptitude_solution;
typedef generic_choice<aptitude_universe> choice;
typedef generic_choice_set<aptitude_universe> choice_set;

/** A simple indicator, usually placed at the bottom of the screen,
 *  that describes the current state of the problem resolver.  Hidden
 *  if no problem resolver is active.
 *
 *  \todo write a simple table cw::fragment class and use that to
 *  right-justify the text that obviously should be
 */
class broken_indicator : public cw::text_layout
{
  aptitude_solution last_sol;

  /** Records whether the last solution is the "keep-all" one. */
  bool last_sol_is_keep_all : 1;

  /** Records whether we had generated all solutions at the time of
   *  the last update.
   */
  bool last_complete : 1;

  /** Records whether the background thread was active at the time of
   *  the last update.
   */
  bool last_background_active : 1;

  /** Tracks the phase of the visual "spinner". */
  int spin_count;

  void handle_cache_reload()
  {
    if(resman != NULL)
      resman->state_changed.connect(sigc::mem_fun(*this, &broken_indicator::post_update));

    update();
  }

protected:
  broken_indicator()
    :spin_count(0)
  {
    if(resman != NULL)
      resman->state_changed.connect(sigc::mem_fun(*this, &broken_indicator::post_update));

    cache_closed.connect(sigc::mem_fun(*this, &broken_indicator::update));
    cache_reloaded.connect(sigc::mem_fun(*this, &broken_indicator::handle_cache_reload));

    set_bg_style(cw::get_style("Error"));

    update();

    cw::toplevel::addtimeout(new cw::toplevel::slot_event(sigc::mem_fun(this, &broken_indicator::tick_timeout)),
			     aptcfg->FindI(PACKAGE "::Spin-Interval", 500));
  }

private:
  static cw::fragment *key_hint_fragment(const resolver_manager::state &state)
  {
    wstring next = cw::config::global_bindings.readable_keyname("NextSolution");
    wstring prev = cw::config::global_bindings.readable_keyname("PrevSolution");
    wstring examine = cw::config::global_bindings.readable_keyname("ExamineSolution");
    wstring apply = cw::config::global_bindings.readable_keyname("ApplySolution");



    cw::style st_normal;
    cw::style st_disabled;
    st_disabled.attrs_off(A_BOLD);
    st_disabled.attrs_on(A_DIM);
    st_disabled.set_fg(COLOR_BLACK);

    vector<cw::fragment *> key_hints;

    key_hints.push_back(cw::fragf(_("%ls: Examine"),
				  examine.c_str()));


    bool can_apply = (state.selected_solution < state.generated_solutions);
    bool can_next = (state.selected_solution < state.generated_solutions &&
		     !(state.selected_solution + 1 == state.generated_solutions && state.solutions_exhausted));

    cw::fragment *apply_fragment =
      cw::style_fragment(cw::fragf(_("%ls: Apply"),
				   apply.c_str()),
			 can_apply ? st_normal : st_disabled);

    cw::fragment *next_fragment =
      cw::style_fragment(cw::fragf(_("%ls: Next"),
				   next.c_str()),
			 can_next ? st_normal : st_disabled);

    const bool can_prev = (state.selected_solution > 0);
    cw::fragment *prev_fragment =
      cw::style_fragment(cw::fragf(_("%ls: Previous"),
				   prev.c_str()),
			 can_prev ? st_normal : st_disabled);

    key_hints.push_back(apply_fragment);
    key_hints.push_back(next_fragment);
    key_hints.push_back(prev_fragment);

    return cw::join_fragments(key_hints, L"  ");
  }

  void tick_timeout()
  {
    cw::widget_ref tmpref(this);

    if(resman != NULL && resman->background_thread_active())
      {
 	++spin_count;
	update();
	cw::toplevel::update();
      }

    cw::toplevel::addtimeout(new cw::toplevel::slot_event(sigc::mem_fun(this, &broken_indicator::tick_timeout)),
			     aptcfg->FindI(PACKAGE "::Spin-Interval", 500));
  }

  std::string spin_string(const resolver_manager::state &state) const
  {
    if(!state.background_thread_active)
      return " ";

    switch(spin_count % 4)
      {
      case 0:
	return ".";
      case 1:
	return "o";
      case 2:
	return "O";
      case 3:
	return "o";
      default:
	return "?";
      }
  }

  struct update_event : public cw::toplevel::event
  {
    broken_indicator *b;
  public:
    update_event(broken_indicator *_b)
      : b(_b)
    {
    }

    void dispatch()
    {
      b->update();
    }
  };

  /** Post an update to run in the main thread; needed since the
   *  selected_signal_changed signal might theoretically run from a
   *  background thread.  (at the moment it shouldn't, but this will
   *  help avoid nasty surprises)
   */
  void post_update()
  {
    cw::toplevel::post_event(new update_event(this));
  }
public:
  static cw::util::ref_ptr<broken_indicator> create()
  {
    cw::util::ref_ptr<broken_indicator> rval(new broken_indicator);
    rval->decref();
    return rval;
  }

  // TODO: split this monster up.
  void update()
  {
    cw::widget_ref tmpref(this);

    if(resman == NULL || !resman->resolver_exists())
      {
	set_fragment(cw::fragf(""));
	last_sol.nullify();
	hide();
	return;
      }

    // Use our special knowledge that this means no solution will ever
    // be generated.
    if(aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000) <= 0)
      {
	set_fragment(cw::text_fragment(_("Dependency resolution disabled.")));
	last_sol.nullify();
	show();
	return;
      }

    // Take a snapshot of the state.
    resolver_manager::state state = resman->state_snapshot();

    if(state.solutions_exhausted && state.generated_solutions == 0)
      {
	set_fragment(cw::fragf(_("Unable to resolve dependencies.")));
	last_sol.nullify();
	show();
	return;
      }

    // Handle the case where the resolver is churning away.
    if(state.selected_solution >= state.generated_solutions)
      {
	if(state.background_thread_aborted)
	  {
	    set_fragment(cw::fragf(_("Fatal error in resolver")));
	    last_sol.nullify();
	    show();
	    return;
	  }

	// TODO: add a column-generating cw::fragment that can
	//       left/right justify stuff.

	vector<cw::fragment_column_entry> columns;

	cw::fragment *col1_fragment =
	  cw::flowbox(cw::text_fragment(ssprintf(_("[%d(%d)/...] Resolving dependencies"),
						 state.selected_solution + 1,
						 state.generated_solutions)));
	columns.push_back(cw::fragment_column_entry(true, false, 1, cw::fragment_column_entry::top, col1_fragment));

	columns.push_back(cw::fragment_column_entry(false, false, 1, cw::fragment_column_entry::top, NULL));

	cw::fragment *col3_fragment = cw::text_fragment(spin_string(state));
	columns.push_back(cw::fragment_column_entry(false, false, 1, cw::fragment_column_entry::top, col3_fragment));

	set_fragment(cw::sequence_fragment(cw::fragment_columns(columns),
					   key_hint_fragment(state),
					   NULL));
	last_sol.nullify();
	show();
	return;
      }

    aptitude_solution sol = resman->get_solution(state.selected_solution, 0);

    // This test always fails the first time update() is called, since
    // sol is never NULL and last_sol is initialized to NULL.
    if(sol == last_sol && state.solutions_exhausted == last_complete &&
       // If there's an active thread we need to redraw the widget to
       // include the spinner.
       !last_background_active &&
       state.background_thread_active == last_background_active)
      return;

    last_sol = sol;
    last_complete = state.solutions_exhausted;
    last_background_active = state.background_thread_active;
    last_sol_is_keep_all = resman->get_is_keep_all_solution(state.selected_solution, 0);

    if(sol.get_choices().size() == 0)
      {
	set_fragment(cw::fragf("%s", _("Internal error: unexpected null solution.")));
	show();
	return;
      }

    int install_count=0, remove_count=0, keep_count=0, upgrade_count=0, downgrade_count=0;

    for(choice_set::const_iterator i = sol.get_choices().begin();
	i != sol.get_choices().end(); ++i)
      {
	// Ignore broken recommendations for now.
	if(i->get_type() != choice::install_version)
	  continue;

	pkgCache::PkgIterator pkg = i->get_ver().get_pkg();
	pkgCache::VerIterator curver=pkg.CurrentVer();
	pkgCache::VerIterator instver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);
	pkgCache::VerIterator newver = i->get_ver().get_ver();

	// If not, we have a problem.
	eassert(instver!=newver);

	if(newver == curver)
	  ++keep_count;
	else if(curver.end())
	  ++install_count;
	else if(newver.end())
	  ++remove_count;
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
	      ++upgrade_count;
	    else if(cmp>0)
	      ++downgrade_count;
	  }
      }

    vector<cw::fragment *> fragments;

    string countstr
      = ssprintf(state.solutions_exhausted?"[%d/%d]":"[%d(%d)/...]",
		 state.selected_solution + 1,
		 state.generated_solutions);
    fragments.push_back(cw::fragf("%s ", countstr.c_str()));


    if(last_sol_is_keep_all)
      fragments.push_back(cw::text_fragment(_("Suggest keeping all packages at their current version.")));
    else
      {
	vector<cw::fragment *> suggestions;

	if(install_count>0)
	  {
	    cw::fragment *install_count_fragment =
	      cw::text_fragment(ssprintf(ngettext("%d install",
						  "%d installs",
						  install_count),
					 install_count));
	    suggestions.push_back(install_count_fragment);
	  }

	if(remove_count>0)
	  {
	    cw::fragment *remove_count_fragment =
	      cw::text_fragment(ssprintf(ngettext("%d removal",
						  "%d removals",
						  remove_count),
					 remove_count));
	    suggestions.push_back(remove_count_fragment);
	  }

	if(keep_count>0)
	  {
	    cw::fragment *keep_count_fragment =
	      cw::text_fragment(ssprintf(ngettext("%d keep",
						  "%d keeps",
						  keep_count),
					 keep_count));
	    suggestions.push_back(keep_count_fragment);
	  }

	if(upgrade_count>0)
	  {
	    cw::fragment *upgrade_count_fragment =
	      cw::text_fragment(ssprintf(ngettext("%d upgrade",
						  "%d upgrades",
						  upgrade_count),
					 upgrade_count));
	    suggestions.push_back(upgrade_count_fragment);
	  }

	if(downgrade_count>0)
	  {
	    cw::fragment *downgrade_count_fragment =
	      cw::text_fragment(ssprintf(ngettext("%d downgrade",
						  "%d downgrades",
						  downgrade_count),
					 downgrade_count));

	    suggestions.push_back(downgrade_count_fragment);
	  }

	/* ForTranslators: %F is replaced with a comma separated list such as
	   "n1 installs, n2 removals", ...
	*/
	fragments.push_back(cw::fragf(_("Suggest %F"), cw::join_fragments(suggestions, L", ")));
      }

    if(state.background_thread_active)
      {
	vector<cw::fragment_column_entry> columns;

	cw::fragment *col1_fragment =
	  cw::hardwrapbox(cw::sequence_fragment(fragments));
	columns.push_back(cw::fragment_column_entry(true, false, 1, cw::fragment_column_entry::top, col1_fragment));

	columns.push_back(cw::fragment_column_entry(false, false, 1, cw::fragment_column_entry::top, NULL));

	cw::fragment *col3_fragment =
	  cw::text_fragment(spin_string(state));
	columns.push_back(cw::fragment_column_entry(false, false, 1, cw::fragment_column_entry::top, col3_fragment));


	fragments.clear();
	fragments.push_back(cw::fragment_columns(columns));
      }
    else
      fragments.push_back(cw::newline_fragment());
    fragments.push_back(cw::hardwrapbox(key_hint_fragment(state)));

    cw::fragment *f = cw::sequence_fragment(fragments);
    set_fragment(f);

    show();
  }
};

cw::util::ref_ptr<cw::widget> make_broken_indicator()
{
  return broken_indicator::create();
}
