// dashboardtab.h            -*-c++-*-
//
//   Copyright (C) 2008-2009 Obey Arthur Liu
//   Copyright (C) 2008 Daniel Burrows
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

#ifndef DASHBOARD_TAB_H
#define DASHBOARD_TAB_H

#include <cwidget/generic/util/ref_ptr.h>

#include <gtkmm.h>

#include "tab.h"

/** \file dashboardtab.h */

namespace aptitude
{
  namespace matching
  {
    class pattern;
  };
}

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/problemresolver/solution.h>
class resolver_manager;

namespace gui
{
  class PkgView;
  class PackageSearchEntry;
  class ResolverTab;

  /** \brief The main dashboard tab. */
  class DashboardTab : public Tab
  {
    // The internal resolver that we use to calculate an upgrade
    // solution.  By convention, this is set to NULL if there's no
    // upgrade to calculate.  Note that although this handles
    // restarting the resolver when a package's state changes, we must
    // actually discard and recreate it ourselves as well: e.g., if a
    // "hold" state changes, that can affect the upgrade calculation.
    resolver_manager *upgrade_resolver;

    // Collects code that keeps track of the "fixing upgrade" state.
    class fixing_upgrade_info
    {
      // A separate internal resolver that's used if the user asks to
      // fix the initial solution manually.  The first upgrade
      // solution that was produced is used for the initial state of
      // this resolver.  This manager is set to NULL until (1) an
      // upgrade solution is produced AND (2) there is a "fixing
      // upgrade" tab.
      resolver_manager *fixing_upgrade_resolver;

      // The "fixing upgrade" tab, if one exists; otherwise NULL.
      ResolverTab *fixing_upgrade_tab;

      /** \brief Invoked when the tab is closed. */
      void handle_fixing_upgrade_tab_closed();

      /** \brief Initialize the internal resolver manager from the
       *  given solution.
       *
       *  If there is a fixing upgrade tab, notifies it of the new
       *  manager.
       */
      void create_resolver(const generic_solution<aptitude_universe> &sol);

    public:
      fixing_upgrade_info();

      // Discards the resolver and NULLs it out in the tab if there is
      // one.
      ~fixing_upgrade_info();

      /** \brief Throw away the resolver and NULL it out in the tab.
       *
       *  Use this to get rid of the resolver after a cache reload,
       *  for instance.
       */
      void discard_resolver();

      /** \brief Pulse the progress bar on the "fixing upgrade" tab,
       *  if there is one.
       */
      void pulse();

      /** \brief Tell the "fixing upgrade" tab, if there is one, that
       *  the given solution was found.
       *
       *  If there is a tab, creates a new resolver manager and hands
       *  it off to the tab.
       *
       *  \param sol The solution that was calculated, or an invalid
       *             solution if the resolver failed.
       */
      void solution_calculated(const generic_solution<aptitude_universe> &sol);

      /** \brief Show the "fixing upgrade" tab and populate it with
       *  the given solution.
       *
       *  Like solution_calculated(), but creates the tab if it
       *  doesn't exist.
       *
       *  \param sol The solution that was calculated, or an invalid
       *             solution if the resolver failed.
       */
      void show_fixing_upgrade(const generic_solution<aptitude_universe> &sol);
    };

    fixing_upgrade_info fixing_upgrade;

    // The internal resolver solution that we use to calculate an
    // upgrade solution.  If this is invalid, the solution hasn't been
    // calculated yet.
    generic_solution<aptitude_universe> upgrade_solution;
    // The timeout connection that's used to pulse the progress bar
    // while we're calculating the upgrade.
    sigc::connection pulse_progress_connection;

    // An internal object that serves as the connection point for
    // slots from the background thread.  The only reason for this to
    // exist is so that we can easily disconnect all those slots at
    // once (by deleting it) and avoid "stale" events from a previous
    // resolver run.
    class redirect_from_background : public sigc::trackable
    {
      DashboardTab &parent;
    public:
      redirect_from_background(DashboardTab &_parent)
	: parent(_parent)
      {
      }

      void success(generic_solution<aptitude_universe> sol)
      {
	parent.upgrade_resolver_success(sol);
      }

      void no_more_solutions()
      {
	parent.upgrade_resolver_no_more_solutions();
      }

      void aborted(std::string errmsg)
      {
	parent.upgrade_resolver_aborted(errmsg);
      }
    };

    redirect_from_background *background_upgrade_redirect;


    cwidget::util::ref_ptr<PkgView> upgrades_pkg_view;
    Gtk::TextView *upgrades_changelog_view;
    Gtk::TextView *upgrades_summary_textview;

    cwidget::util::ref_ptr<PackageSearchEntry> package_search_entry;

    Gtk::ProgressBar *upgrade_resolver_progress;
    Gtk::Label *upgrade_resolver_label;
    Gtk::Button *fix_manually_button;
    Gtk::Button *upgrade_button;

    Gtk::Label *available_upgrades_label;

    // Maps each version displayed in the upgrade list to the location
    // of its changelog in the changelog text view.
    std::map<pkgCache::VerIterator, Glib::RefPtr<Gtk::TextBuffer::Mark> > changelog_locations;

    void do_search();

    void do_upgrade();

    void do_fix_manually();

    // Download all the changelogs and show the new entries.
    void create_upgrade_summary();

    void handle_cache_closed();
    void handle_cache_reloaded();

    void handle_upgrades_store_reloaded();

    void activated_upgrade_package_handler();

    bool pulse_progress_timeout();


    // NB: the resolver callbacks pass their argument by value to
    // avoid any possible confusion about whether they could refer to
    // a destroyed object.

    /** \brief Invoked in the foreground when the background thread
     *  computes an upgrade solution.
     *
     *  \param sol   The solution that was calculated, or an invalid
     *               solution to indicate that nothing needs to be
     *               calculated.
     *
     *  Hides the progress bar, shows the label, and activates the
     *  button; changes the label text to suggest to the user that
     *  they can press the button to install the upgrades that are
     *  available.
     */
    void upgrade_resolver_success(generic_solution<aptitude_universe> sol);

    /** \brief Invoked in the foreground when the background thread
     *  can't find an upgrade solution.
     *
     *  Hides the progress bar, shows the label, deactivates the
     *  button, and informs the user that it was unable to find an
     *  upgrade solution.
     */
    void upgrade_resolver_no_more_solutions();

    /** \brief Invoked in the foreground when the background thread
     *  is aborted (by an exception) while searching for an upgrade solution.
     */
    void upgrade_resolver_aborted(std::string errmsg);



    class upgrade_continuation;
    /** \brief Create the internal resolver if it doesn't exist and
     *  start its calculation.
     *
     *  Shows the progress bar and hides the label; makes the button
     *  inactive and kicks off a timer to pulse the progress bar.
     */
    void make_resolver();
    /** \brief Throw away the internal resolver and solution, and hide
     *  the progress bar and label.
     */
    void discard_resolver();

  public:
    DashboardTab(Glib::ustring label);
    ~DashboardTab();

    bool get_edit_columns_available();
    void dispatch_edit_columns();
  };
}



#endif // DASHBOARD_TAB_H

