// -*-c++-*-

// resolver.h
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef RESOLVER_H_
#define RESOLVER_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>
#include <generic/problemresolver/choice.h>
#include <generic/problemresolver/solution.h>
#include <generic/util/maybe.h>
#include <generic/util/util.h>

#include <gtk/tab.h>

namespace gui
{

  class ResolverColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<pkgCache::PkgIterator> PkgIterator;
      Gtk::TreeModelColumn<pkgCache::VerIterator> VerIterator;
    Gtk::TreeModelColumn<Glib::ustring> ActionMarkup;
    Gtk::TreeModelColumn<bool> BgSet;
    Gtk::TreeModelColumn<Glib::ustring> BgColor;
    Gtk::TreeModelColumn<Glib::ustring> PreferenceIcon;
    Gtk::TreeModelColumn<Glib::ustring> PreferenceIconTooltip;
    Gtk::TreeModelColumn<maybe<generic_choice<aptitude_universe> > > Choice;

      ResolverColumns();
  };

  class AlreadyGeneratedSolutionColumns : public Gtk::TreeModel::ColumnRecord
  {
  public:
    // A column showing the index of the solution (as a string).
    Gtk::TreeModelColumn<Glib::ustring> Index;
    // The same information, as an integer (for internal use).
    Gtk::TreeModelColumn<int> IndexNum;
    // The main "markup" column.
    Gtk::TreeModelColumn<Glib::ustring> Markup;
    // The column storing the "tooltip" associated with each solution.
    Gtk::TreeModelColumn<Glib::ustring> TooltipMarkup;
    Gtk::TreeModelColumn<generic_solution<aptitude_universe> > Solution;

    AlreadyGeneratedSolutionColumns();
  };

  // Wrapper that adds automatic updates of resolver item states to
  // the resolver view.
  class ResolverView : public aptitude::util::refcounted_base_threadsafe
  {
  private:
    Gtk::TreeViewColumn * ActionMarkup;
    Gtk::TreeViewColumn * PreferenceIcon;

    Gtk::TreeView *view;
    ResolverColumns resolver_columns;

    // These backpointers track the current state of the store; they
    // are used to respond to signals from the resolver manager about
    // changes to reject/approve states.
    std::map<aptitude_resolver_version, Gtk::TreeModel::iterator> version_backpointers;
    std::map<aptitude_resolver_dep, Gtk::TreeModel::iterator> dep_backpointers;

    // Set up some common column properties.
    void set_column_properties(Gtk::TreeViewColumn *treeview_column,
			       int size);

    template<typename ColumnType>
    int append_column(const Glib::ustring &title,
		      Gtk::TreeViewColumn *out_treeview_column,
		      const Gtk::TreeModelColumn<ColumnType> &model_column,
		      int size);

    bool add_backpointer(const Gtk::TreeModel::Path &path,
			 const Gtk::TreeModel::iterator &iter);

    bool sanity_check_iter(const Gtk::TreeModel::iterator &iter) const;

    // Routines and structures that manage the accepted/rejected
    // states of rows; placed here so that the incremental update can
    // be a member function.

    /** \brief Information about how to display the user's preference
     *  about a package.
     */
    struct preference_info
    {
      Glib::ustring icon;
      Glib::ustring icon_tooltip;
      Glib::ustring background;
      bool background_set;

      preference_info()
	: icon(),
	  icon_tooltip(),
	  background(),
	  background_set(false)
      {
      }

      preference_info(const Glib::ustring &_icon,
		      const Glib::ustring &_icon_tooltip,
		      const Glib::ustring &_background,
		      bool _background_set)
	: icon(_icon),
	  icon_tooltip(_icon_tooltip),
	  background(_background),
	  background_set(_background_set)
      {
      }
    };
    preference_info get_preference_info(const generic_choice<aptitude_universe> &c,
					resolver_manager *manager) const;
    void set_preference_info(Gtk::TreeModel::Row &row,
			     const preference_info &pref_inf) const;
    void set_preference_info(Gtk::TreeModel::Row &row,
			     const generic_choice<aptitude_universe> &c,
			     resolver_manager *manager) const;

    // Used to iterate over the model when it's first set and update
    // its preference-information columns.
    bool update_preference_info(const Gtk::TreeModel::iterator &iter,
				resolver_manager *manager);

    ResolverView(Gtk::TreeView *view);
  public:
    /** \brief Create a new resolver view.
     *
     *  The given view is customized with the standard set of columns
     *  for a resolver-view.
     */
    static cwidget::util::ref_ptr<ResolverView> create(Gtk::TreeView *view);

    /** \brief Retrieve the column structure associated with this view. */
    const ResolverColumns &get_columns() const { return resolver_columns; }

    /** \brief Set the model displayed by this view.
     *
     *  \param model    The new tree-model to display.
     *  \param manager  The resolver-manager to use to flesh out
     *                  the model.
     *
     *  This routine updates the tree-view and builds the backpointer
     *  maps; it also fills in preference information columns.
     */
    void set_model(const Glib::RefPtr<Gtk::TreeModel> &model,
		   resolver_manager *manager);

    /** \brief Update the preference state of rows attached to the
     *  given version.
     */
    void update_version(const aptitude_resolver_version &version,
			resolver_manager *manager);

    /** \brief Update the preference state of rows attached to the
     *  given dependency.
     */
    void update_dep(const aptitude_resolver_dep &dep,
		    resolver_manager *manager);

    Gtk::TreeView *get_treeview() const { return view; }
  };

  class ResolverTab : public Tab
  {
    private:
      typedef generic_solution<aptitude_universe> aptitude_solution;

    // The resolver manager that we're using.
    resolver_manager *resolver;

    // True if we're using an internal resolver; false otherwise.
    bool using_internal_resolver;

    resolver_manager *get_resolver() const
    {
      return using_internal_resolver ? resolver : resman;
    }

    sigc::connection resolver_state_changed_connection;
    sigc::connection resolver_version_accept_reject_changed_connection;
    sigc::connection resolver_break_dep_accept_reject_changed_connection;

    cwidget::util::ref_ptr<ResolverView> solution_view;

    // The columns and tree-view for the list of previously generated
    // solutions.
    AlreadyGeneratedSolutionColumns already_generated_columns;
    Gtk::TreeView * already_generated_view;
    Glib::RefPtr<Gtk::ListStore> already_generated_model;

      Gtk::Label * pResolverStatus;
      Gtk::Button * pResolverApply;
    Gtk::Button *find_next_solution_button;

    // The top container for the "fixing upgrade..." message.  Used to
    // show and hide it.
    Gtk::Widget * resolver_fixing_upgrade_message;

    Gtk::ProgressBar * resolver_fixing_upgrade_progress_bar;
    Gtk::Label * resolver_fixing_upgrade_label;

    Gtk::RadioButton * pButtonGroupByAction;
    Gtk::RadioButton * pButtonShowExplanation;

    Gtk::Label * acceptreject_label;
    Gtk::ToggleButton * reject_button;
    Gtk::ToggleButton * no_preference_button;
    Gtk::ToggleButton * accept_button;

    // The solution, if any, that is currently displayed in the
    // solution pane.
    aptitude_solution displayed_solution;

    /** \brief Add a child of the given iterator and populate it from
     *  the given choice.
     */
    void append_choice(const Glib::RefPtr<Gtk::TreeStore> &store,
		       const cwidget::util::ref_ptr<ResolverView> &view,
		       const Gtk::TreeModel::Row &parent_row,
		       const generic_choice<aptitude_universe> &c) const;

    // These two routines manage the connections to the resolver and
    // other state that has to be thrown away and recreated when the
    // cache is reloaded.
    void handle_cache_closed();
    void setup_resolver_connections();

    /** \brief Create a new tree store and populate it with the given
     *  solution, rendered with actions collected by type.
     */
    Glib::RefPtr<Gtk::TreeStore> render_as_action_groups(const aptitude_solution &sol);

    /** \brief Create a new tree store and populate it with
     *  the given solution, rendered as a chronological explanation
     *  of each action.
     */
    Glib::RefPtr<Gtk::TreeStore> render_as_explanation(const aptitude_solution &sol);

    /** \brief Fill in a row in the list of generated solutions.
     *
     *  \param sol   The solution that the row represents.
     *  \param index The index of this solution.
     *  \param row   The row to fill in.
     */
    void render_already_generated_row(const aptitude_solution &sol,
				      int index,
				      Gtk::TreeModel::Row &row) const;

      std::string archives_text(const pkgCache::VerIterator &ver);
      std::string dep_targets(const pkgCache::DepIterator &start) const;
    std::string dep_text(const pkgCache::DepIterator &d) const;
    bool do_find_next_solution_enabled();
    bool do_find_next_solution_enabled_from_state(const resolver_manager::state &state);
    void do_find_next_solution();
      void do_apply_solution();

    /** \brief Update the solution pane from the currently selected
     *  solution in the solution list.
     *
     *  If there is no currently selected solution, the solution pane
     *  is always updated with an explanation of why; otherwise, it is
     *  only updated if the solution actually changed.
     *
     *  \param force_update   If true, the pane will be updated even if
     *                        there is no new solution.  Used to change
     *                        the view mode.
     */
    void update_solution_pane(bool force_update);

      /** \brief Updates the tab with the given resolver state.
       *
       *  \param force_update if \b true, the tree is rebuilt even if
       *  the current solution hasn't changed.
       */
    void update_from_state(const resolver_manager::state &state,
			   bool force_update);

      /** \brief Updates the tab with the current resolver state.
       *
       *  This is connected to the global state-changed signal; in
       *  functions that check or read the state before triggering an
       *  update, invoke update(state) instead to ensure
       *  consistency.
       *
       *  \param force_update if \b true, the tree is rebuilt even if
       *  the current solution hasn't changed.
       */
      void update(bool force_update);

    // Helper class used by update_reject_accept_buttons.
    class update_reject_accept_counter;

    void version_accept_reject_changed(const aptitude_resolver_version &ver);
    void break_dep_accept_reject_changed(const aptitude_resolver_dep &dep);

    // Updates the label by the reject/accept buttons and might change
    // the sensitivity of the buttons.  Invoked when the selection
    // state changes, when the resolver state changes, and when a new
    // solution is selected.
    void update_reject_accept_buttons();

    // We need to avoid responding to toggle messages while the
    // buttons are being updated; otherwise we could end up with
    // unexpected or inefficient behavior.  This Boolean value and the
    // accompanying RAII class are used to suppress toggle button
    // responses.
    bool toggle_signals_suppressed;
    class suppress_toggle_signals
    {
      ResolverTab &parent;
      bool old_toggle_signals_suppressed;

    public:
      suppress_toggle_signals(ResolverTab &_parent)
	: parent(_parent),
	  old_toggle_signals_suppressed(_parent.toggle_signals_suppressed)
      {
	parent.toggle_signals_suppressed = true;
      }

      ~suppress_toggle_signals()
      {
	parent.toggle_signals_suppressed = false;
      }
    };

    // Helpers, applied to each element in the selection to implement
    // the various action buttons.
    void do_reject_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c);
    void do_no_preference_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c);
    void do_accept_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c);

    void reject_button_toggled();
    void no_preference_button_toggled();
    void accept_button_toggled();

    public:
      ResolverTab(const Glib::ustring &label);
    const cwidget::util::ref_ptr<ResolverView> &get_solution_view() const { return solution_view; };

    /** \brief Enable the "fix upgrade manually" mode on this resolver
     *  tab, using the given resolver manager.
     *
     *  This just adjusts the UI slightly to make it clear that the
     *  resolver is specifically focused on finding a solution for an
     *  upgrade problem and to provide feedback when the upgrade
     *  resolver's initial state is being recalculated for some reason
     *  (e.g., a cache reload).  It also prevents the resolver from
     *  being automatically destroyed when there are no more broken
     *  dependencies.
     *
     *  \param manager The manager to use in this tab, or \b NULL to
     *  disable the UI and show the "recalculating upgrade" progress
     *  bar.  The caller is responsible for invoking
     *  set_is_fix_upgrade_resolver() again, should "manager" ever
     *  become invalid, with a valid resolver manager object.
     */
    void set_fix_upgrade_resolver(resolver_manager *manager);

    /** \brief Invoked by the dashboard tab to pulse the progress bar
     *	in this tab.
     */
    void pulse_fix_upgrade_resolver_progress();
  };

  /** \brief Set up the global resolver tracker.
   *
   *  This is responsible for connecting up the signals that cause the
   *  resolver to be automatically triggered when there are broken
   *  packages, and for making sure that those signals are properly
   *  destroyed and recreated when the cache is closed and reopened.
   *  It should be called exactly once from main().
   *
   *  This also ensures that resman->state_changed() is triggered in
   *  the main thread whenever new resolver solutions are available.
   */
  void init_resolver();
}

#endif /* RESOLVER_H_ */
