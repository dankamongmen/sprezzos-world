// resolver.cc
//
//  Copyright 1999-2010 Daniel Burrows
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


#include "resolver.h"
#include "aptitude.h"

#include "gui.h"
#include "treeview_cell_tooltips.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <boost/make_shared.hpp>

#include <loggers.h>
#include <solution_fragment.h> // For archives_text.
#include <solution_item.h> // For action_type.

#include <generic/apt/apt_undo_group.h>
#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <cwidget/generic/util/ssprintf.h>

using aptitude::Loggers;

namespace gui
{
  namespace
  {
    typedef generic_choice<aptitude_universe> choice;
    typedef generic_choice_set<aptitude_universe> choice_set;

    void do_start_solution_calculation(resolver_manager *resolver);

    class gui_resolver_continuation : public resolver_manager::background_continuation
    {
      typedef generic_solution<aptitude_universe> aptitude_solution;

      resolver_manager *manager;

      // This is static because the continuation will be deleted
      // before it gets invoked.
      static void do_error(const std::string &msg,
			   resolver_manager& manager)
      {
	_error->Error(_("Error in dependency resolver: %s"), msg.c_str());

	std::string notification =
	  ssprintf(_("Fatal error in dependency resolver.  You can continue "
		     "searching, but some solutions might be impossible to generate.\n\n%s"),
		   msg.c_str());
	pMainWindow->get_notifyview()->add_notification(Gtk::manage(new Notification(notification.c_str())));
	manager.state_changed();
      }

    public:
      gui_resolver_continuation(resolver_manager *_manager)
	: manager(_manager)
      {
      }

      void success(const aptitude_solution &sol)
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: got solution: " << sol);

	// Tell the main loop to pretend the state changed.
	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void no_more_solutions()
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: ran out of solutions.");

	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void no_more_time()
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: ran out of time, restarting the calculation.");

	do_start_solution_calculation(manager);
      }

      void interrupted()
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: interrupted.");

	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void aborted(const std::string &errmsg)
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
	LOG_TRACE(logger, "Resolver tab: aborted by exception: " << errmsg);

	sigc::slot0<void> do_error_slot =
	  sigc::bind(sigc::ptr_fun(&gui_resolver_continuation::do_error),
		     errmsg, sigc::ref(*manager));
	post_event(make_safe_slot(do_error_slot));
      }
    };

    // Used by the upgrade resolver to automatically push itself to
    // the next solution.
    void do_start_solution_calculation(resolver_manager *resolver)
    {
      LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		"Restarting the resolver thread if necessary.");
      boost::shared_ptr<gui_resolver_continuation> k =
	boost::make_shared<gui_resolver_continuation>(resolver);
      resolver->maybe_start_solution_calculation(k, post_thunk);
    }

    // Start computing the first solution if it's not computed yet.
    void do_start_first_solution_calculation(resolver_manager *resolver)
    {
      if(resolver->generated_solution_count() == 0)
	{
	  // Note: there is no race condition here.  If the background
	  // thread computes a solution before we enqueue the job,
	  // then maybe_start_solution_calculation just won't do
	  // anything (because the selected solution is 0, so there's
	  // nothing to do -- the first solution is already
	  // generated).
	  LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		    "Making sure the first solution is computed.");
	  boost::shared_ptr<gui_resolver_continuation> k =
	    boost::make_shared<gui_resolver_continuation>(resolver);
	  resolver->maybe_start_solution_calculation(k,
						     post_thunk);
	}
    }

    // The global resolver will always be set to start calculating the
    // first solution as soon as it's created; after that we only
    // calculate solutions when the user presses the "find new
    // solution" button.
    void do_connect_resolver_callback()
    {
      LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		"Connecting the callback to compute the first solution when the resolver state changes.");
      resman->state_changed.connect(sigc::bind(sigc::ptr_fun(&do_start_first_solution_calculation), resman));
      // We may have missed a signal before making the connection:
      do_start_first_solution_calculation(resman);
    }

    std::string render_choice_description_markup(const choice &c)
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    aptitude_resolver_version ver(c.get_ver());
	    pkgCache::VerIterator descver;
	    if(!ver.get_ver().end())
	      descver = ver.get_ver();
	    else
	      {
		aptitude_resolver_version curr_ver(ver.get_package().current_version());
		if(!curr_ver.get_ver().end())
		  descver = curr_ver.get_ver();
		else if(!ver.get_pkg().CurrentVer().end())
		  descver = ver.get_pkg().CurrentVer();
		else if(!ver.get_pkg().VersionList().end())
		  descver = ver.get_pkg().VersionList();
		else
		  descver = pkgCache::VerIterator();
	      }

	    // Note that virtual packages shouldn't happen!  This is
	    // just a safeguard.
	    using cwidget::util::transcode;
	    std::string description_markup =
	      descver.end()
	      ? ssprintf("<i>%s</i>",
			 Glib::Markup::escape_text(_("Virtual package")).c_str()).c_str()
	      : Glib::Markup::escape_text(transcode(get_short_description(descver, apt_package_records))).c_str();

	    return description_markup;
	  }

	default:
	  return "";
	}
    }

    /** \brief Render the given choice in "short" form, not including
     *  a description of the choice.
     *
     *  The output is suitable for use in a list with a heading such
     *  as "Install the following packages".
     */
    std::string render_choice_brief_markup(const choice &c)
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    aptitude_resolver_version ver(c.get_ver());

	    switch(analyze_action(ver))
	      {
	      case action_remove:
		return Glib::Markup::escape_text(ver.get_pkg().Name());

	      case action_keep:
		if(ver.get_ver().end())
		  return ssprintf("%s [%s]",
				  Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
				  Glib::Markup::escape_text(_("Not Installed")).c_str());
		else
		  return ssprintf("%s [<big>%s</big> (%s)]",
				  Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
				  Glib::Markup::escape_text(ver.get_pkg().CurrentVer().VerStr()).c_str(),
				  Glib::Markup::escape_text(archives_text(ver.get_pkg().CurrentVer())).c_str());

	      case action_install:
		return ssprintf("%s [<big>%s</big> (%s)]",
				Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
				Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
				Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());

	      case action_downgrade:
	      case action_upgrade:
		return ssprintf("%s [<big>%s</big> (%s) -> <big>%s</big> (%s)]",
				Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
				Glib::Markup::escape_text(ver.get_pkg().CurrentVer().VerStr()).c_str(),
				Glib::Markup::escape_text(archives_text(ver.get_pkg().CurrentVer())).c_str(),
				Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
				Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());

	      default:
		return "<big>UNKNOWN ACTION TYPE</big>";
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  {
	    const std::string dep_render =
	      cwidget::util::transcode(dep_text(c.get_dep().get_dep()), "UTF-8");

	    return ssprintf("%s",
			    Glib::Markup::escape_text(dep_render).c_str());
	  }

	default:
	  return "<big>UNKNOWN CHOICE TYPE</big>";
	}
    }
  }

  void init_resolver()
  {
    cache_reloaded.connect(sigc::ptr_fun(&do_connect_resolver_callback));
    if(apt_cache_file)
      do_connect_resolver_callback();
  }

  //std::string glade_main_file;
  //AptitudeWindow * pMainWindow;

  ResolverColumns::ResolverColumns()
  {
    add(PkgIterator);
    add(VerIterator);
    add(ActionMarkup);
    add(BgSet);
    add(BgColor);
    add(PreferenceIcon);
    add(PreferenceIconTooltip);
    add(Choice);
  }

  AlreadyGeneratedSolutionColumns::AlreadyGeneratedSolutionColumns()
  {
    add(Index);
    add(IndexNum);
    add(Markup);
    add(TooltipMarkup);
    add(Solution);
  }

  void ResolverView::set_column_properties(Gtk::TreeViewColumn *treeview_column,
					   int size)
  {
    Glib::ListHandle<Gtk::CellRenderer *> renderers = treeview_column->get_cell_renderers();
    for(Glib::ListHandle<Gtk::CellRenderer *>::const_iterator it  =
	  renderers.begin(); it != renderers.end(); ++it)
      {
	treeview_column->add_attribute((*it)->property_cell_background_set(), resolver_columns.BgSet);
	treeview_column->add_attribute((*it)->property_cell_background(), resolver_columns.BgColor);
      }
    if (size < 0)
      treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_AUTOSIZE);
    else
      {
        treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
        treeview_column->set_fixed_width(size);
        treeview_column->set_resizable(true);
      }
    treeview_column->set_reorderable(true);
  }

  template<typename ColumnType>
  int ResolverView::append_column(const Glib::ustring &title,
				  Gtk::TreeViewColumn *out_treeview_column,
				  const Gtk::TreeModelColumn<ColumnType> &model_column,
				  int size)
  {
    out_treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    set_column_properties(out_treeview_column, size);

    return view->append_column(*out_treeview_column);
  }


  ResolverView::preference_info ResolverView::get_preference_info(const generic_choice<aptitude_universe> &c,
								  resolver_manager *manager) const
  {
    typedef generic_choice<aptitude_universe> choice;
    using cwidget::util::ssprintf;

    if(manager == NULL)
      return preference_info();
    else
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    pkgCache::VerIterator ver = c.get_ver().get_ver();
	    pkgCache::PkgIterator pkg = c.get_ver().get_pkg();

	    if(manager->is_rejected(c.get_ver()))
	      {
		Glib::ustring icon_tooltip;
		// Generate a friendly message about what's happening.
		switch(analyze_action(c.get_ver()))
		  {
		  case action_remove:
		    icon_tooltip = ssprintf(_("Removing %s is rejected."), pkg.Name());
		    break;

		  case action_keep:
		    if(ver.end())
		      icon_tooltip = ssprintf(_("Canceling the installation of %s is rejected."),
					      pkg.Name());
		    else
		      {
			aptitude_resolver_version previous_resolver_version =
			  c.get_ver().get_package().current_version();
			pkgCache::VerIterator prev_ver = previous_resolver_version.get_ver();

			if(prev_ver.end())
			  icon_tooltip = ssprintf(_("Canceling the removal of %s is rejected."),
						  pkg.Name());
			else
			  icon_tooltip = ssprintf(_("Keeping %s at version %s is rejected."),
						  pkg.Name(),
						  ver.VerStr());
		      }
		    break;

		  case action_install:
		    icon_tooltip = ssprintf(_("Installing %s version %s is rejected."),
					    pkg.Name(),
					    ver.VerStr());
		    break;

		  case action_downgrade:
		    icon_tooltip = ssprintf(_("Downgrading %s to version %s is rejected."),
					    pkg.Name(),
					    ver.VerStr());
		    break;

		  case action_upgrade:
		    icon_tooltip = ssprintf(_("Upgrading %s to version %s is rejected."),
					    pkg.Name(),
					    ver.VerStr());
		    break;
		  }

		return preference_info(Gtk::Stock::CANCEL.id,
				       icon_tooltip,
				       lightred_background_color,
				       true);
	      }
	    else if(manager->is_mandatory(c.get_ver()))
	      {
		Glib::ustring icon_tooltip;
		switch(analyze_action(c.get_ver()))
		  {
		  case action_remove:
		    icon_tooltip = ssprintf(_("Removing %s is preferred over all un-accepted alternatives."), pkg.Name());
		    break;

		  case action_keep:
		    if(ver.end())
		      icon_tooltip = ssprintf(_("Canceling the installation of %s is preferred over all un-accepted alternatives."),
					      pkg.Name());
		    else
		      {
			aptitude_resolver_version previous_resolver_version =
			  c.get_ver().get_package().current_version();
			pkgCache::VerIterator prev_ver = previous_resolver_version.get_ver();

			if(prev_ver.end())
			  icon_tooltip = ssprintf(_("Canceling the removal of %s is preferred over all un-accepted alternatives."),
						  pkg.Name());
			else
			  icon_tooltip = ssprintf(_("Keeping %s at version %s is preferred over all un-accepted alternatives."),
						  pkg.Name(),
						  ver.VerStr());
		      }
		    break;

		  case action_install:
		    icon_tooltip = ssprintf(_("Installing %s version %s is preferred over all un-accepted alternatives."),
					    pkg.Name(),
					    ver.VerStr());
		    break;

		  case action_downgrade:
		    icon_tooltip = ssprintf(_("Downgrading %s to version %s is preferred over all un-accepted alternatives."),
					    pkg.Name(),
					    ver.VerStr());
		    break;

		  case action_upgrade:
		    icon_tooltip = ssprintf(_("Upgrading %s to version %s is preferred over all un-accepted alternatives."),
					    pkg.Name(),
					    ver.VerStr());
		    break;
		  }

		return preference_info(Gtk::Stock::APPLY.id,
				       icon_tooltip,
				       lightgreen_background_color,
				       true);
	      }
	    else
	      return preference_info();
	  }

	case choice::break_soft_dep:
	  if(manager->is_hardened(c.get_dep()))
	    return preference_info(Gtk::Stock::CANCEL.id,
				   ssprintf(_("Leaving %ls unresolved is rejected."),
					    dep_text(c.get_dep().get_dep()).c_str()),
				   lightred_background_color,
				   true);
	  else if(manager->is_approved_broken(c.get_dep()))
	    return preference_info(Gtk::Stock::APPLY.id,
				   ssprintf(_("Leaving %ls unresolved is preferred over all un-accepted alternatives."),
					    dep_text(c.get_dep().get_dep()).c_str()),
				   lightgreen_background_color,
				   true);
	  else
	    return preference_info();

	default:
	  LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		    "Bad choice type " << c.get_type());
	  return preference_info(Gtk::Stock::DIALOG_ERROR.id,
				 "",
				 "",
				 false);
	}
  }

  void ResolverView::set_preference_info(Gtk::TreeModel::Row &row,
					 const ResolverView::preference_info &pref_inf) const
  {
    row[resolver_columns.PreferenceIcon] = pref_inf.icon;
    row[resolver_columns.PreferenceIconTooltip] = pref_inf.icon_tooltip;
    // Even if the color will have no effect because it isn't set, we
    // need to set a legal color name here; otherwise GTK+ will
    // complain that it doesn't know about the color "".  Black is
    // picked so it's really obvious when this has happened.
    if(pref_inf.background.empty())
      row[resolver_columns.BgColor] = "black";
    else
      row[resolver_columns.BgColor] = pref_inf.background;
    row[resolver_columns.BgSet] = pref_inf.background_set;

    // Sanity-check.
    if(pref_inf.background.empty() && pref_inf.background_set)
      LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		"The row \"" << row[resolver_columns.ActionMarkup] << "\" has no background but its background is set.");
  }

  void ResolverView::set_preference_info(Gtk::TreeModel::Row &row,
					 const generic_choice<aptitude_universe> &c,
					 resolver_manager *manager) const
  {
    preference_info pref_inf(get_preference_info(c, manager));
    set_preference_info(row, pref_inf);
  }

  bool ResolverView::update_preference_info(const Gtk::TreeModel::iterator &iter,
					    resolver_manager *manager)
  {
    Gtk::TreeModel::Row row(*iter);

    typedef generic_choice<aptitude_universe> choice;
    maybe<choice> maybe_c(row[resolver_columns.Choice]);
    choice c;
    if(maybe_c.extract(c))
      set_preference_info(row,
			  c,
			  manager);
    else
      set_preference_info(row, preference_info());

    return false;
  }

  void ResolverView::update_version(const aptitude_resolver_version &version,
				    resolver_manager *manager)
  {
    std::map<aptitude_resolver_version, Gtk::TreeModel::iterator>::iterator
      found = version_backpointers.find(version);

    if(found != version_backpointers.end())
      {
	typedef generic_choice<aptitude_universe> choice;
	Gtk::TreeModel::Row row(*found->second);
	set_preference_info(row,
			    choice::make_install_version(version, -1),
			    manager);
      }
  }

  void ResolverView::update_dep(const aptitude_resolver_dep &dep,
				resolver_manager *manager)
  {
    std::map<aptitude_resolver_dep, Gtk::TreeModel::iterator>::iterator
      found = dep_backpointers.find(dep);

    if(found != dep_backpointers.end())
      {
	typedef generic_choice<aptitude_universe> choice;
	Gtk::TreeModel::Row row(*found->second);
	set_preference_info(row,
			    choice::make_break_soft_dep(dep, -1),
			    manager);
      }
  }

  ResolverView::ResolverView(Gtk::TreeView *_view)
    : view(_view)
  {
    {
      Gtk::CellRendererPixbuf *preference_icon_renderer = manage(new Gtk::CellRendererPixbuf);
      PreferenceIcon = manage(new Gtk::TreeViewColumn("", *preference_icon_renderer));
      PreferenceIcon->add_attribute(preference_icon_renderer->property_stock_id(),
				    resolver_columns.PreferenceIcon);
      PreferenceIcon->set_sort_column(resolver_columns.PreferenceIcon);
      set_column_properties(PreferenceIcon, -1);
      view->append_column(*PreferenceIcon);
    }
    set_text_tooltip(view, PreferenceIcon, resolver_columns.PreferenceIconTooltip);

    {
      Gtk::CellRendererText *action_renderer = manage(new Gtk::CellRendererText);
      ActionMarkup = manage(new Gtk::TreeViewColumn("Action", *action_renderer));
      ActionMarkup->add_attribute(action_renderer->property_markup(),
				  resolver_columns.ActionMarkup);
      set_column_properties(ActionMarkup, -1);
      ActionMarkup->set_sort_column(resolver_columns.Choice);
      view->append_column(*ActionMarkup);
    }
    set_markup_tooltip(view, ActionMarkup, resolver_columns.ActionMarkup);

    view->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
    view->set_search_column(resolver_columns.ActionMarkup);
  }

  cwidget::util::ref_ptr<ResolverView>
  ResolverView::create(Gtk::TreeView *view)
  {
    return new ResolverView(view);
  }

  bool ResolverView::add_backpointer(const Gtk::TreeModel::Path &path,
				     const Gtk::TreeModel::iterator &iter)
  {
    typedef generic_choice<aptitude_universe> choice;
    const maybe<choice> &maybe_c((*iter)[resolver_columns.Choice]);
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    version_backpointers[c.get_ver()] = iter;
	    break;

	  case choice::break_soft_dep:
	    dep_backpointers[c.get_dep()] = iter;
	    break;
	  }
      }

    return false;
  }

  bool ResolverView::sanity_check_iter(const Gtk::TreeModel::iterator &iter) const
  {
    Glib::ustring bgColor = (*iter)[resolver_columns.BgColor];
    if((*iter)[resolver_columns.BgSet] && bgColor.empty())
      {
	LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		  "Sanity-check failed: the resolver row \""
		  << (*iter)[resolver_columns.ActionMarkup]
		  << "\" has an empty background color.");
      }

    return false;
  }

  void ResolverView::set_model(const Glib::RefPtr<Gtk::TreeModel> &model,
			       resolver_manager *manager)
  {
    model->foreach_iter(sigc::bind(sigc::mem_fun(*this, &ResolverView::update_preference_info),
				   manager));
    model->foreach_iter(sigc::mem_fun(*this, &ResolverView::sanity_check_iter));

    view->set_model(model);
    version_backpointers.clear();
    dep_backpointers.clear();

    model->foreach(sigc::mem_fun(*this, &ResolverView::add_backpointer));
  }

  ResolverTab::ResolverTab(const Glib::ustring &label) :
    Tab(Resolver, label, Gnome::Glade::Xml::create(glade_main_file, "resolver_main"), "resolver_main"),
    resolver(NULL),
    using_internal_resolver(false),
    toggle_signals_suppressed(false)
  {
    get_xml()->get_widget("main_resolver_status", pResolverStatus);
    get_xml()->get_widget("main_resolver_find_next", find_next_solution_button);
    find_next_solution_button->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_find_next_solution));
    get_xml()->get_widget("main_resolver_apply", pResolverApply);
    get_xml()->get_widget("resolver_fixing_upgrade_message", resolver_fixing_upgrade_message);
    get_xml()->get_widget("resolver_fixing_upgrade_progress_bar", resolver_fixing_upgrade_progress_bar);
    get_xml()->get_widget("resolver_fixing_upgrade_label", resolver_fixing_upgrade_label);
    pResolverApply->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_apply_solution));

    get_xml()->get_widget("resolver_group_by_action", pButtonGroupByAction);
    get_xml()->get_widget("resolver_show_explanation", pButtonShowExplanation);

    get_xml()->get_widget("resolver_acceptreject_label", acceptreject_label);
    get_xml()->get_widget("resolver_reject_button", reject_button);
    get_xml()->get_widget("resolver_no_preference_button", no_preference_button);
    get_xml()->get_widget("resolver_accept_button", accept_button);

    reject_button->set_image(*new Gtk::Image(Gtk::Stock::CANCEL, Gtk::ICON_SIZE_BUTTON));
    accept_button->set_image(*new Gtk::Image(Gtk::Stock::APPLY, Gtk::ICON_SIZE_BUTTON));

    // TODO: ideally, instead of rereading the state, we should
    // trigger an update using the last seen state.  Or maybe build two
    // models and swap between them.
    pButtonGroupByAction->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update_solution_pane),
							      true));
    pButtonShowExplanation->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update_solution_pane),
								true));

    reject_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::reject_button_toggled));
    no_preference_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::no_preference_button_toggled));
    accept_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::accept_button_toggled));

    Gtk::TreeView *solution_treeview;
    get_xml()->get_widget("main_resolver_treeview", solution_treeview);
    solution_view = ResolverView::create(solution_treeview);
    solution_treeview->get_selection()->signal_changed().connect(sigc::mem_fun(*this, &ResolverTab::update_reject_accept_buttons));

    // Set up the list of already-generated solutions.
    get_xml()->get_widget("already_generated_treeview",
			  already_generated_view);
    already_generated_model = Gtk::ListStore::create(already_generated_columns);
    already_generated_view->set_model(already_generated_model);
    already_generated_view->set_tooltip_column(already_generated_columns.TooltipMarkup.index());
    already_generated_view->get_selection()->set_mode(Gtk::SELECTION_BROWSE);
    already_generated_view->get_selection()->signal_changed().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update_solution_pane),
										 false));

    {
      Gtk::TreeViewColumn *index_column =
	manage(new Gtk::TreeViewColumn("Index", already_generated_columns.Index));
      index_column->set_sizing(Gtk::TREE_VIEW_COLUMN_AUTOSIZE);
      index_column->set_sort_column(already_generated_columns.IndexNum);

      Gtk::CellRendererText *markup_renderer = manage(new Gtk::CellRendererText);
      Gtk::TreeViewColumn *markup_column =
	manage(new Gtk::TreeViewColumn("Solution", *markup_renderer));
      markup_column->add_attribute(markup_renderer->property_markup(),
				   already_generated_columns.Markup);
      markup_column->set_sizing(Gtk::TREE_VIEW_COLUMN_AUTOSIZE);
      markup_column->set_sort_column(already_generated_columns.Markup);

      already_generated_view->append_column(*index_column);
      already_generated_view->append_column(*markup_column);
    }

    // Initialize the interface with the current state:
    update(true);

    get_widget()->show();

    cache_closed.connect(sigc::mem_fun(*this, &ResolverTab::handle_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &ResolverTab::setup_resolver_connections));

    setup_resolver_connections();
  }

  void ResolverTab::handle_cache_closed()
  {
    resolver_state_changed_connection.disconnect();
    resolver_version_accept_reject_changed_connection.disconnect();
    resolver_break_dep_accept_reject_changed_connection.disconnect();

    already_generated_model->clear();

    // TODO: what about encapsulated resolvers?
  }

  void ResolverTab::setup_resolver_connections()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

    if(get_resolver() == NULL)
      {
	LOG_TRACE(logger, "Not setting up resolver connections: the resolver is NULL.");
	return;
      }

    LOG_TRACE(logger, "Setting up connections on the resolver manager " << get_resolver());

    resolver_state_changed_connection.disconnect();
    resolver_version_accept_reject_changed_connection.disconnect();
    resolver_break_dep_accept_reject_changed_connection.disconnect();

    resolver_state_changed_connection =
      get_resolver()->state_changed.connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
						       false));

    resolver_version_accept_reject_changed_connection =
      get_resolver()->version_accept_reject_changed.connect(sigc::mem_fun(*this, &ResolverTab::version_accept_reject_changed));
    resolver_break_dep_accept_reject_changed_connection =
      get_resolver()->break_dep_accept_reject_changed.connect(sigc::mem_fun(*this, &ResolverTab::break_dep_accept_reject_changed));
  }

  void ResolverTab::update(bool force_update)
  {
    update_reject_accept_buttons();

    if(get_resolver() == NULL)
      return;

    resolver_manager::state state = get_resolver()->state_snapshot();
    update_from_state(state, force_update);
  }

  void ResolverTab::version_accept_reject_changed(const aptitude_resolver_version &ver)
  {
    solution_view->update_version(ver, get_resolver());
  }

  void ResolverTab::break_dep_accept_reject_changed(const aptitude_resolver_dep &dep)
  {
    solution_view->update_dep(dep, get_resolver());
  }

  class ResolverTab::update_reject_accept_counter
  {
    // Counts how many things are selected.
    int count;
    bool contains_rejected;
    bool contains_accepted;
    bool contains_no_preference;
    const ResolverTab &parent;
    const ResolverColumns &columns;

  public:
    update_reject_accept_counter(const ResolverTab &_parent,
				 const ResolverColumns &_columns)
      : count(0),
	contains_rejected(false),
	contains_accepted(false),
	contains_no_preference(false),
	parent(_parent),
	columns(_columns)
    {
    }

    void process(const Gtk::TreeModel::iterator &iter)
    {
      typedef generic_choice<aptitude_universe> choice;
      const maybe<choice> &maybe_c((*iter)[columns.Choice]);
      choice c;
      if(maybe_c.extract(c))
	{
	  ++count;

	  switch(c.get_type())
	    {
	    case choice::install_version:
	      if(parent.get_resolver()->is_rejected(c.get_ver()))
		contains_rejected = true;
	      else if(parent.get_resolver()->is_mandatory(c.get_ver()))
		contains_accepted = true;
	      else
		contains_no_preference = true;
	      break;

	    case choice::break_soft_dep:
	      if(parent.get_resolver()->is_hardened(c.get_dep()))
		contains_rejected = true;
	      else if(parent.get_resolver()->is_approved_broken(c.get_dep()))
		contains_accepted = true;
	      else
		contains_no_preference = true;
	      break;
	    }
	}
    }

    int get_count() const { return count; }
    bool get_contains_rejected() const { return contains_rejected; }
    bool get_contains_no_preference() const { return contains_no_preference; }
    bool get_contains_accepted() const { return contains_accepted; }
  };

  void ResolverTab::update_reject_accept_buttons()
  {
    suppress_toggle_signals suppressor(*this);

    if(get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	accept_button->set_sensitive(false);
	no_preference_button->set_sensitive(false);
	reject_button->set_sensitive(false);

	accept_button->set_active(false);
	no_preference_button->set_active(false);
	reject_button->set_active(false);

	return;
      }

    // Find out what's selected.
    update_reject_accept_counter counter(*this,
					 solution_view->get_columns());;

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    selection->selected_foreach_iter(sigc::mem_fun(counter,
						   &update_reject_accept_counter::process));

    const int count = counter.get_count();
    const bool contains_rejected = counter.get_contains_rejected();
    const bool contains_no_preference = counter.get_contains_no_preference();
    const bool contains_accepted = counter.get_contains_accepted();

    using cwidget::util::ssprintf;
    // TODO: this frame title is lame.
    acceptreject_label->set_markup(ssprintf("<b>%s</b>",
					    Glib::Markup::escape_text(ngettext("How to treat the selected action",
									       "How to treat the selected actions",
									       count)).c_str()));

    reject_button->set_tooltip_text(ngettext("Ignore solutions containing this action.",
					     "Ignore solutions containing these actions.",
					     count));

    no_preference_button->set_tooltip_text(ngettext("Allow solutions containing this action, but do not prefer them to other solutions.",
						    "Allow solutions containing these actions, but do not prefer them to other solutions.",
						    count));

    accept_button->set_tooltip_text(ngettext("Always prefer this action over alternatives that have not been accepted.",
					     "Always prefer these actions over alternatives that have not been accepted.",
					     count));

    if(contains_rejected)
      {
	reject_button->set_active(true);

	reject_button->set_inconsistent(contains_accepted ||
					contains_no_preference);
      }
    else
      {
	reject_button->set_active(false);
	reject_button->set_inconsistent(false);
      }

    if(contains_no_preference)
      {
	no_preference_button->set_active(true);

	no_preference_button->set_inconsistent(contains_rejected ||
					       contains_accepted);
      }
    else
      {
	no_preference_button->set_active(false);
	no_preference_button->set_inconsistent(false);
      }

    if(contains_accepted)
      {
	accept_button->set_active(true);

	accept_button->set_inconsistent(contains_rejected ||
					contains_no_preference);
      }
    else
      {
	accept_button->set_active(false);
	accept_button->set_inconsistent(false);
      }

    bool sensitive;

    if(count == 0)
      {
	// Maybe hide the buttons and/or show a label saying to select
	// an action?
	sensitive = false;

	accept_button->set_active(false);
	no_preference_button->set_active(false);
	reject_button->set_active(false);
      }
    else
      sensitive = true;

    accept_button->set_sensitive(sensitive);
    no_preference_button->set_sensitive(sensitive);
    reject_button->set_sensitive(sensitive);
  }

  void ResolverTab::do_reject_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c)
  {
    typedef generic_choice<aptitude_universe> choice;
    choice c;

    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->reject_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->harden_dep(c.get_dep());
	    break;
	  }
      }
  }

  namespace
  {
    struct collect_choices
    {
    public:
      typedef generic_choice<aptitude_universe> choice;

    private:
      std::vector<maybe<choice> > &target;
      cwidget::util::ref_ptr<ResolverView> solution_view;

    public:
      collect_choices(std::vector<maybe<choice> > &_target,
		      const cwidget::util::ref_ptr<ResolverView> &_solution_view)
	: target(_target),
	  solution_view(_solution_view)
      {
      }

      void visit(const Gtk::TreeModel::iterator &iter) const
      {
	const maybe<choice> &maybe_c((*iter)[solution_view->get_columns().Choice]);

	target.push_back(maybe_c);
      }
    };
  }

  void ResolverTab::do_no_preference_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c)
  {
    typedef generic_choice<aptitude_universe> choice;
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->unreject_version(c.get_ver());
	    get_resolver()->unmandate_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->unharden_dep(c.get_dep());
	    get_resolver()->unapprove_broken_dep(c.get_dep());
	    break;
	  }
      }
  }

  void ResolverTab::do_accept_choice(const maybe<generic_choice<aptitude_universe> > &maybe_c)
  {
    typedef generic_choice<aptitude_universe> choice;
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->mandate_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->approve_broken_dep(c.get_dep());
	    break;
	  }
      }
  }

  void ResolverTab::reject_button_toggled()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "reject_button_toggled: not rejecting packages.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    // Don't do anything unless the reject button is newly active.
    if(!reject_button->get_active())
      {
	LOG_TRACE(logger, "reject_button_toggled: the reject button was un-toggled; not doing anything.");
	return;
      }

    LOG_DEBUG(logger, "reject_button_toggled: rejecting the selected choices.");

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    typedef generic_choice<aptitude_universe> choice;
    std::vector<maybe<choice> > selected_choices;
    collect_choices collector(selected_choices, solution_view);
    selection->selected_foreach_iter(sigc::mem_fun(collector,
						   &collect_choices::visit));
    for(std::vector<maybe<choice> >::const_iterator it =
	  selected_choices.begin();
	it != selected_choices.end(); ++it)
      do_reject_choice(*it);
  }

  void ResolverTab::no_preference_button_toggled()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "no_preference_button_toggled: not clearing states.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    if(!no_preference_button->get_active())
      {
	LOG_TRACE(logger, "no_preference_button_toggled: the no-preference button was un-toggled; not doing anything.");
	return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    LOG_DEBUG(logger, "no_preference_button_toggled: clearing reject/accept states of the selected choices.");

    typedef generic_choice<aptitude_universe> choice;
    std::vector<maybe<choice> > selected_choices;
    collect_choices collector(selected_choices, solution_view);
    selection->selected_foreach_iter(sigc::mem_fun(collector,
						   &collect_choices::visit));
    for(std::vector<maybe<choice> >::const_iterator it =
	  selected_choices.begin();
	it != selected_choices.end(); ++it)
      do_no_preference_choice(*it);
  }

  void ResolverTab::accept_button_toggled()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "accept_button_toggled: not clearing states.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    if(!accept_button->get_active())
      {
	LOG_TRACE(logger, "accept_button_toggled: the accept button was un-toggled; not doing anything.");
	return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    LOG_DEBUG(logger, "accept_button_toggled: accepting the selected choices.");
    typedef generic_choice<aptitude_universe> choice;
    std::vector<maybe<choice> > selected_choices;
    collect_choices collector(selected_choices, solution_view);
    selection->selected_foreach_iter(sigc::mem_fun(collector,
						   &collect_choices::visit));
    for(std::vector<maybe<choice> >::const_iterator it =
	  selected_choices.begin();
	it != selected_choices.end(); ++it)
      do_accept_choice(*it);
  }

  string ResolverTab::archives_text(const pkgCache::VerIterator &ver)
  {
    string rval;

    bool is_first = true;

    for(pkgCache::VerFileIterator vf=ver.FileList(); !vf.end(); ++vf)
      {
        if(is_first)
          is_first = false;
        else
          rval += ", ";

        if(vf.File().Archive())
          rval += vf.File().Archive();
        else
          rval += _("<NULL>");
      }

    return rval;
  }

  std::string ResolverTab::dep_targets(const pkgCache::DepIterator &start) const
  {
    std::string rval;

    bool is_first = true;

    eassert(!start.end());

    for(pkgCache::DepIterator d = start; !d.end(); ++d)
      {
        if(is_first)
          is_first = false;
        else
          rval += " | ";

        rval += d.TargetPkg().Name();

        if(d.TargetVer())
          {
            rval += " (";
            rval += d.CompType();
            rval += " ";
            rval += d.TargetVer();
            rval += ")";
          }

        if((d->CompareOp & pkgCache::Dep::Or) == 0)
          break;
      }

    return rval;
  }

  std::string ResolverTab::dep_text(const pkgCache::DepIterator &d) const
  {
    const char *name = const_cast<pkgCache::DepIterator &>(d).ParentPkg().Name();

    std::string targets = dep_targets(d);

    switch(d->Type)
      {
      case pkgCache::Dep::Depends:
        return ssprintf(_("%s depends upon %s"),
			name, targets.c_str());
      case pkgCache::Dep::PreDepends:
        return ssprintf(_("%s pre-depends upon %s"),
			name, targets.c_str());
      case pkgCache::Dep::Suggests:
        return ssprintf(_("%s suggests %s"),
			name, targets.c_str());
      case pkgCache::Dep::Recommends:
        return ssprintf(_("%s recommends %s"),
			name, targets.c_str());
      case pkgCache::Dep::Conflicts:
        return ssprintf(_("%s conflicts with %s"),
			name, targets.c_str());
      case pkgCache::Dep::DpkgBreaks:
        return ssprintf(_("%s breaks %s"),
                         name, targets.c_str());
      case pkgCache::Dep::Replaces:
        return ssprintf(_("%s replaces %s"),
			name, targets.c_str());
      case pkgCache::Dep::Obsoletes:
        return ssprintf(_("%s obsoletes %s"),
			name, targets.c_str());
      case pkgCache::Dep::Enhances:
        return ssprintf(_("%s enhances %s"),
			name, targets.c_str());
      default:
        abort();
      }
  }

  void ResolverTab::append_choice(const Glib::RefPtr<Gtk::TreeStore> &store,
				  const cwidget::util::ref_ptr<ResolverView> &view,
				  const Gtk::TreeModel::Row &parent_row,
				  const choice &c) const
  {
    Gtk::TreeModel::iterator iter = store->append(parent_row.children());
    Gtk::TreeModel::Row row(*iter);

    row[view->get_columns().ActionMarkup] =
      "<b>" + render_choice_brief_markup(c) + "</b>\n<small>" +
      render_choice_description_markup(c) + "</small>";
    row[view->get_columns().Choice] = c;
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_action_groups(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(Gtk::TreeStore::create(solution_view->get_columns()));

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().ActionMarkup] =
	  Glib::Markup::escape_text(_("Internal error: unexpected null solution."));
	row[solution_view->get_columns().BgSet] = false;
      }
    else
      {
	typedef generic_choice<aptitude_universe> choice;

	// Bin packages according to what will happen to them.
	vector<choice> remove_packages;
	vector<choice> keep_packages;
	vector<choice> install_packages;
	vector<choice> downgrade_packages;
	vector<choice> upgrade_packages;
	vector<choice> unresolved;

	typedef generic_choice<aptitude_universe> choice;
	typedef generic_choice_set<aptitude_universe> choice_set;

	for(choice_set::const_iterator i = sol.get_choices().begin();
	    i != sol.get_choices().end(); ++i)
	  {
	    switch(i->get_type())
	      {
	      case choice::install_version:
		{
		  pkgCache::PkgIterator pkg = i->get_ver().get_pkg();
		  pkgCache::VerIterator curver=pkg.CurrentVer();
		  pkgCache::VerIterator newver = i->get_ver().get_ver();

		  if(curver.end())
		    {
		      if(newver.end())
			keep_packages.push_back(*i);
		      else
			install_packages.push_back(*i);
		    }
		  else if(newver.end())
		    remove_packages.push_back(*i);
		  else if(newver == curver)
		    keep_packages.push_back(*i);
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
			upgrade_packages.push_back(*i);
		      else if(cmp>0)
			downgrade_packages.push_back(*i);
		    }
		}

		break;

	      case choice::break_soft_dep:
		unresolved.push_back(*i);
		break;
	      }
	  }

	typedef generic_solution<aptitude_universe>::choice_name_lt choice_name_lt;
	sort(remove_packages.begin(), remove_packages.end(), choice_name_lt());
	sort(keep_packages.begin(), keep_packages.end(), choice_name_lt());
	sort(install_packages.begin(), install_packages.end(), choice_name_lt());
	sort(downgrade_packages.begin(), downgrade_packages.end(), choice_name_lt());
	sort(upgrade_packages.begin(), upgrade_packages.end(), choice_name_lt());
	sort(unresolved.begin(), unresolved.end(), choice_name_lt());

	if(!remove_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Remove the following packages:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;

	    for(vector<choice>::const_iterator i = remove_packages.begin();
		i!=remove_packages.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }

	if(!install_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Install the following packages:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = install_packages.begin();
		i!=install_packages.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }

	if(!keep_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Keep the following packages:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = keep_packages.begin();
		i!=keep_packages.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }

	if(!upgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Upgrade the following packages:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = upgrade_packages.begin();
		i!=upgrade_packages.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }

	if(!downgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Downgrade the following packages:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = downgrade_packages.begin();
		i!=downgrade_packages.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }

	if(!unresolved.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().ActionMarkup] =
	      ssprintf("<big><b>%s</b></big>",
		       Glib::Markup::escape_text(_("Leave the following dependencies unresolved:")).c_str());
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(std::vector<choice>::const_iterator i = unresolved.begin();
		i != unresolved.end(); ++i)
	      append_choice(store, solution_view, parent_row, *i);
	  }
      }

    return store;
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_explanation(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(Gtk::TreeStore::create(solution_view->get_columns()));

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().ActionMarkup] =
	  Glib::Markup::escape_text(_("Internal error: unexpected null solution."));
	row[solution_view->get_columns().BgSet] = false;
      }
    else
      {
	typedef generic_choice_set<aptitude_universe> choice_set;
	typedef generic_choice<aptitude_universe> choice;

	// Store just the choices that modify the state of the world.
	std::vector<choice> actions;

	for(choice_set::const_iterator it = sol.get_choices().begin();
	    it != sol.get_choices().end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		actions.push_back(*it);
		break;

	      default:
		break;
	      }
	  }

	std::sort(actions.begin(), actions.end(),
		  aptitude_solution::choice_id_compare());

	for(std::vector<choice>::const_iterator
	      it = actions.begin(); it != actions.end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		{
		  Gtk::TreeModel::iterator parent_iter = store->append();
		  Gtk::TreeModel::Row parent_row = *parent_iter;

		  parent_row[solution_view->get_columns().ActionMarkup] =
		    "<big><b>" +
		    Glib::Markup::escape_text(dep_text((*it).get_dep().get_dep())) +
		    "</b></big>";
		  parent_row[solution_view->get_columns().BgSet] = false;

		  Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		  Gtk::TreeModel::Row row = *iter;
		  Glib::ustring markup;

		  aptitude_resolver_version ver((*it).get_ver());
		  pkgCache::PkgIterator pkg = ver.get_pkg();
		  action_type action(analyze_action(ver));

		  using cwidget::util::ssprintf;

		  switch(action)
		    {
		    case action_remove:
		      markup = ssprintf(_("Remove %s [<big>%s</big> (%s)]"),
					Glib::Markup::escape_text(pkg.Name()).c_str(),
					Glib::Markup::escape_text(pkg.CurrentVer().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(pkg.CurrentVer())).c_str());
		      break;

		    case action_install:
		      markup = ssprintf(_("Install %s [<big>%s</big> (%s)]"),
					Glib::Markup::escape_text(pkg.Name()).c_str(),
					Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());
		      break;

		    case action_keep:
		      if(ver.get_ver().end())
			markup = ssprintf(_("Cancel the installation of %s"),
					  Glib::Markup::escape_text(pkg.Name()).c_str());
		      else if(ver.get_package().current_version().get_ver().end())
			markup = ssprintf(_("Cancel the removal of %s"),
					pkg.Name());
		      else
			markup = ssprintf(_("Keep %s at version <big>%s</big> (%s)"),
					  Glib::Markup::escape_text(pkg.Name()).c_str(),
					  Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
					  Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());
		      break;

		    case action_upgrade:
		      markup = ssprintf(_("Upgrade %s [<big>%s</big> (%s) -> <big>%s</big> (%s)]"),
					Glib::Markup::escape_text(pkg.Name()).c_str(),
					Glib::Markup::escape_text(pkg.CurrentVer().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(pkg.CurrentVer())).c_str(),
					Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());
		      break;

		    case action_downgrade:
		      markup = ssprintf(_("Downgrade %s [<big>%s</big> (%s) -> <big>%s</big> (%s)]"),
					Glib::Markup::escape_text(pkg.Name()).c_str(),
					Glib::Markup::escape_text(pkg.CurrentVer().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(pkg.CurrentVer())).c_str(),
					Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str(),
					Glib::Markup::escape_text(archives_text(ver.get_ver())).c_str());
		      break;

		    default:
		      markup = "Internal error: bad action type";
		      break;
		    }

		  std::string description_markup(render_choice_description_markup(*it));
		  row[solution_view->get_columns().ActionMarkup] =
		    "<b>" + markup + "</b>\n<small>" + description_markup + "</small>";
		}

		break;

	      default:
		// \todo Add items for breaking soft dependencies?
		break;
	      }
	  }
      }

    return store;
  }

  // Helpers to make render_already_generated_row less huge.
  namespace
  {
    std::string largenum(int n)
    {
      return cwidget::util::ssprintf("<big>%d</big>", n);
    }

    std::string render_solution_single_line(const generic_solution<aptitude_universe> &sol)
    {
      std::string list_text;

      bool first = true;
      for(choice_set::const_iterator it = sol.get_choices().begin();
	  it != sol.get_choices().end(); ++it)
	{
	  if(!first)
	    list_text += ", ";

	  switch(it->get_type())
	    {
	    case choice::install_version:
	      {
		aptitude_resolver_version ver(it->get_ver());
		switch(analyze_action(ver))
		  {
		  case action_remove:
		    {
		      const char * const
			tmpl(first ? _("<b>Remove</b> %s") : _("<b>remove</b> %s"));
		      list_text += ssprintf(tmpl,
					    Glib::Markup::escape_text(ver.get_pkg().Name()).c_str());
		    }
		    break;
		  case action_keep:
		    if(!ver.get_ver().end())
		      {
			const char * const
			  tmpl(first ? _("<b>Keep</b> %s at version %s") : _("<b>keep</b> %s at version %s"));
			list_text += ssprintf(tmpl,
					      Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
					      Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str());
		      }
		    else
		      {
			const char * const
			  tmpl(first ? _("<b>Cancel</b> the installation of %s") : _("<b>cancel</b> the installation of %s"));
			list_text += ssprintf(tmpl, Glib::Markup::escape_text(ver.get_pkg().Name()).c_str());
		      }
		    break;
		  case action_install:
		    {
		      const char * const
			tmpl(first ? _("<b>Install</b> %s %s") : _("<b>install</b> %s %s"));
		      list_text += ssprintf(tmpl,
					    Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
					    Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str());
		    }
		    break;
		  case action_downgrade:
		    {
		      const char * const
			tmpl(first ? _("<b>Downgrade</b> %s to version %s") : _("<b>downgrade</b> %s to version %s"));
		      list_text += ssprintf(tmpl,
					    Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
					    Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str());
		    }
		    break;
		  case action_upgrade:
		    {
		      const char * const
			tmpl(first ? _("<b>Upgrade</b> %s to version %s") : _("<b>upgrade</b> %s to version %s"));
		      list_text += ssprintf(tmpl,
					    Glib::Markup::escape_text(ver.get_pkg().Name()).c_str(),
					    Glib::Markup::escape_text(ver.get_ver().VerStr()).c_str());
		    }
		    break;
		  }
	      }
	      break;

	    case choice::break_soft_dep:
	      {
		const char * const
		  tmpl(first ? _("Leave %s unresolved") : _("leave %s unresolved"));
		const std::string dep_string =
		  cwidget::util::transcode(dep_text(it->get_dep().get_dep()), "UTF-8");
		list_text += ssprintf(tmpl,
				      Glib::Markup::escape_text(dep_string).c_str());
	      }
	      break;
	    }

	  first = false;
	}

      return list_text;
    }

    std::string render_solution_summary(const generic_solution<aptitude_universe> &sol,
					const std::string &separator)
    {
      int num_remove = 0;
      int num_keep = 0;
      int num_install = 0;
      int num_downgrade = 0;
      int num_upgrade = 0;
      int num_unresolved = 0;

      for(choice_set::const_iterator it = sol.get_choices().begin();
	  it != sol.get_choices().end(); ++it)
	{
	  switch(it->get_type())
	    {
	    case choice::install_version:
	      {
		aptitude_resolver_version ver(it->get_ver());

		switch(analyze_action(ver))
		  {
		  case action_remove:
		    ++num_remove;
		    break;

		  case action_keep:
		    ++num_keep;
		    break;

		  case action_install:
		    ++num_install;
		    break;

		  case action_downgrade:
		    ++num_downgrade;
		    break;

		  case action_upgrade:
		    ++num_upgrade;
		    break;
		  }
	      }
	      break;

	    case choice::break_soft_dep:
	      ++num_unresolved;
	      break;
	    }
	}

      using cwidget::util::ssprintf;
      std::vector<std::string> rval;
      if(num_install > 0)
	rval.push_back(ssprintf(ngettext("%s install",
					 "%s installs",
					 num_install),
				largenum(num_install).c_str()));
      if(num_remove > 0)
	rval.push_back(ssprintf(ngettext("%s remove",
					 "%s removes",
					 num_remove),
				largenum(num_remove).c_str()));

      if(num_keep > 0)
	rval.push_back(ssprintf(ngettext("%s keep",
					 "%s keeps",
					 num_keep),
				largenum(num_keep).c_str()));

      if(num_upgrade > 0)
	rval.push_back(ssprintf(ngettext("%s upgrade",
					 "%s upgrades",
					 num_upgrade),
				largenum(num_upgrade).c_str()));

      if(num_downgrade > 0)
	rval.push_back(ssprintf(ngettext("%s downgrade",
					 "%s downgrades",
					 num_downgrade),
				largenum(num_downgrade).c_str()));

      if(num_unresolved > 0)
	rval.push_back(ssprintf(ngettext("%s unresolved recommendation",
					 "%s unresolved recommendations",
					 num_unresolved),
				largenum(num_unresolved).c_str()));

      std::string tmp;
      for(std::vector<std::string>::const_iterator it =
	    rval.begin(); it != rval.end(); ++it)
	{
	  if(!tmp.empty())
	    tmp += separator;
	  tmp += *it;
	}
      return tmp;
    }

    std::string render_solution_long_markup(const generic_solution<aptitude_universe> &sol)
    {
      std::vector<choice> remove, keep, install,
	downgrade, upgrade, unresolved;

      for(choice_set::const_iterator it = sol.get_choices().begin();
	  it != sol.get_choices().end(); ++it)
	{
	  switch(it->get_type())
	    {
	    case choice::install_version:
	      {
		aptitude_resolver_version ver(it->get_ver());

		switch(analyze_action(ver))
		  {
		  case action_remove:
		    remove.push_back(*it);
		    break;

		  case action_keep:
		    keep.push_back(*it);
		    break;

		  case action_install:
		    install.push_back(*it);
		    break;

		  case action_downgrade:
		    downgrade.push_back(*it);
		    break;

		  case action_upgrade:
		    upgrade.push_back(*it);
		    break;
		  }
	      }
	      break;

	    case choice::break_soft_dep:
	      unresolved.push_back(*it);
	      break;
	    }
	}

      typedef generic_solution<aptitude_universe>::choice_name_lt choice_name_lt;
      std::sort(remove.begin(), remove.end(), choice_name_lt());
      std::sort(keep.begin(), keep.end(), choice_name_lt());
      std::sort(install.begin(), install.end(), choice_name_lt());
      std::sort(downgrade.begin(), downgrade.end(), choice_name_lt());
      std::sort(upgrade.begin(), upgrade.end(), choice_name_lt());
      std::sort(unresolved.begin(), unresolved.end(), choice_name_lt());

      std::string rval;
      using cwidget::util::ssprintf;
      if(!remove.empty())
	{
	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Remove the following packages:")).c_str());
	  for(std::vector<choice>::const_iterator it = remove.begin();
	      it != remove.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      if(!install.empty())
	{
	  if(!rval.empty())
	    rval += "\n\n";

	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Install the following packages:")).c_str());
	  for(std::vector<choice>::const_iterator it = install.begin();
	      it != install.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      if(!keep.empty())
	{
	  if(!rval.empty())
	    rval += "\n\n";

	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Keep the following packages:")).c_str());
	  for(std::vector<choice>::const_iterator it = keep.begin();
	      it != keep.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      if(!upgrade.empty())
	{
	  if(!rval.empty())
	    rval += "\n\n";

	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Upgrade the following packages:")).c_str());

	  for(std::vector<choice>::const_iterator it = upgrade.begin();
	      it != upgrade.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      if(!downgrade.empty())
	{
	  if(!rval.empty())
	    rval += "\n\n";

	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Downgrade the following packages:")).c_str());

	  for(std::vector<choice>::const_iterator it = downgrade.begin();
	      it != downgrade.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      if(!unresolved.empty())
	{
	  if(!rval.empty())
	    rval += "\n\n";

	  rval += ssprintf("<b>%s</b>",
			   Glib::Markup::escape_text(_("Leave the following dependencies unresolved:")).c_str());

	  for(std::vector<choice>::const_iterator it = unresolved.begin();
	      it != unresolved.end(); ++it)
	    rval += ssprintf("\n  %s", render_choice_brief_markup(*it).c_str());
	}

      return rval;
    }
  }

  void ResolverTab::render_already_generated_row(const aptitude_solution &sol,
						 int index,
						 Gtk::TreeModel::Row &row) const
  {
    // How large a solution can be before we give up on rendering it
    // fully in the list.
    const unsigned int list_render_limit = 3;
    // How large a solution can be before we give up on rendering it
    // fully in the tooltip.
    const unsigned int tooltip_render_limit = 20;

    row[already_generated_columns.Index]    = cwidget::util::ssprintf("%d", index);
    row[already_generated_columns.IndexNum] = index;

    if(sol.get_choices().size() <= list_render_limit)
      row[already_generated_columns.Markup] = render_solution_single_line(sol);
    else
      row[already_generated_columns.Markup] = render_solution_summary(sol, ", ");

    if(sol.get_choices().size() <= tooltip_render_limit)
      row[already_generated_columns.TooltipMarkup] = render_solution_long_markup(sol);
    else
      row[already_generated_columns.TooltipMarkup] = render_solution_summary(sol, "\n");

    row[already_generated_columns.Solution] = sol;
  }

  // The resolver tab manages the resolver state as follows.
  //
  // The resolver state is always set to the most recently generated
  // solution, or one past that point if a new solution is being
  // generated.  Pressing "find next" moves to the next solution after
  // the last generated solution, if possible.  If a solution is being
  // calculated, an appropriate message is shown to inform the user of
  // this fact and the "find next" button is disabled.
  void ResolverTab::update_from_state(const resolver_manager::state &state,
				      bool force_update)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    LOG_TRACE(logger, "Updating the resolver tab (" << (force_update ? "forced" : "not forced") << ")");
    // Maybe I should log more information about the resolver state as
    // we log it?

    Glib::RefPtr<Gtk::TreeStore> store = Gtk::TreeStore::create(solution_view->get_columns());

    bool added_anything = false;
    while(already_generated_model->children().size() < (unsigned)state.generated_solutions)
      {
	const int next_solution_num = already_generated_model->children().size();
	aptitude_solution sol = get_resolver()->get_solution(next_solution_num, 0);
	Gtk::TreeModel::iterator iter = already_generated_model->append();
	Gtk::TreeModel::Row row(*iter);

	LOG_DEBUG(logger, "Resolver tab: adding a solution: " << sol);

	render_already_generated_row(sol, next_solution_num + 1, row);
	added_anything = true;
      }

    // If we added a row, select the most recent solution (i.e., the
    // last one in the list).
    if(added_anything)
      {
	Gtk::TreeModel::iterator last = already_generated_model->children().end();
	--last;
	LOG_TRACE(logger, "Resolver tab: selecting the newly added solution.");
	already_generated_view->get_selection()->select(last);
      }

    update_solution_pane(false);

    find_next_solution_button->set_sensitive(do_find_next_solution_enabled_from_state(state));

    // WARNING:Minor hack.
    //
    // We should always hide and delete the resolver view when there
    // isn't a resolver yet.  But if one somehow gets created before
    // the resolver exists, we should show something sensible.  And
    // tab_del will really-and-truly destroy the parts of this tab
    // when it is invoked (not to mention the Tab object itself), so
    // we have to do the just-in-case setting up of the view before
    // calling tab_del.
    if(!state.resolver_exists)
      tab_del(this);
  }

  void ResolverTab::update_solution_pane(bool force_update)
  {
    LOG_TRACE(Loggers::getAptitudeGtkResolver(),
	      "Resolver tab: updating the solution pane.");

    resolver_manager::state state(get_resolver()->state_snapshot());
    aptitude_solution new_solution;
    // "index" is the "number" of the solution in the solutions list;
    // it has a meaningful value whenever new_solution is valid.
    int index = -1;

    if(already_generated_view->get_selection()->count_selected_rows() == 1)
      {
	Gtk::TreeModel::iterator selected_iter =
	  already_generated_view->get_selection()->get_selected();

	// Should never ever happen, but this is an extra
	// sanity-check.
	if(selected_iter)
	  {
	    new_solution = (*selected_iter)[already_generated_columns.Solution];
	    index = (*selected_iter)[already_generated_columns.IndexNum];
	  }
      }

    // If there is no new solution, always update the displayed text
    // in case the state changed.
    if(!new_solution.valid())
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		  "Resolver tab: there is no selected solution.");
	displayed_solution = new_solution;

#if 0
	/* This code is commented out because the solution treeview
           doesn't seem to be useful for displaying messages (they
           get truncated).  Maybe we could swap it out for a textview
           when there's nothing to show? */

	// Display some text to let the user know there's nothing
	// to see.
	Glib::RefPtr<Gtk::TreeStore> store = Gtk::TreeStore::create(solution_view->get_columns());
	Gtk::TreeModel::iterator text_row_iter = store->append();
	Gtk::TreeModel::Row text_row(*text_row_iter);

	if(already_generated_model->children().empty())
	  {
	    // It could be empty because we haven't found a
	    // solution yet, or because no solutions were found at
	    // all.  (if there are solutions that aren't in the
	    // list yet, I assume that we'll add them momentarily
	    // anyway)
	    if(state.generated_solutions > 0 ||
	       !state.solutions_exhausted)
	      {
		LOG_DEBUG(Loggers::getAptitudeGtkResolver(),
			  "Resolver tab: dependencies are still being resolved.");
		text_row[solution_view->get_columns().Name] =
		  _("Resolving dependencies...");
		pResolverStatus->set_text(_("No solutions yet."));
	      }
	    else
	      {
		LOG_DEBUG(Loggers::getAptitudeGtkResolver(),
			  "Resolver tab: no dependency solution was found.");
		text_row[solution_view->get_columns().Name] =
		  _("No dependency solution was found.");
		pResolverStatus->set_text(_("No solutions."));
	      }
	  }
	else
	  {
	    LOG_WARN(Loggers::getAptitudeGtkResolver(),
		     "Resolver tab: there are solutions, but no solution is selected!");
	    // This message shouldn't appear, but if it does the
	    // user would probably like to know how to make it go
	    // away. :-)
	    text_row[solution_view->get_columns().Name] =
	      _("To view a solution, select it from the list to the right.");
	    pResolverStatus->set_text(ssprintf(_("%u solutions."),
					       already_generated_model->children().size()));
	  }

	solution_view->set_model(store, get_resolver());
	solution_view->get_treeview()->expand_all();
#endif
      }
    else if(force_update || new_solution != displayed_solution)
      {
	if(Loggers::getAptitudeGtkResolver()->isEnabledFor(logging::DEBUG_LEVEL))
	  {
	    if(new_solution != displayed_solution)
	      LOG_DEBUG(Loggers::getAptitudeGtkResolver(),
			"Resolver tab: displaying a different solution: " << new_solution);
	    else
	      LOG_DEBUG(Loggers::getAptitudeGtkResolver(),
			"Resolver tab: the solution " << new_solution
			<< " is already displayed, but forcing an update as requested.");
	  }
	displayed_solution = new_solution;

	Glib::RefPtr<Gtk::TreeModel> store;
	if(pButtonShowExplanation->get_active())
	  store = render_as_explanation(displayed_solution);
	else
	  store = render_as_action_groups(displayed_solution);

	solution_view->set_model(store, get_resolver());
	solution_view->get_treeview()->expand_all();
	pResolverStatus->set_markup(ssprintf(_("Solution %s of %s."),
					     largenum(index).c_str(),
					     largenum(state.generated_solutions).c_str()));
      }
    else
      LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		"Resolver tab: the solution " << new_solution << " is already displayed.");

    pResolverApply->set_sensitive(displayed_solution.valid());
  }

  bool ResolverTab::do_find_next_solution_enabled_from_state(const resolver_manager::state &state)
  {
    if(!state.resolver_exists)
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		  "Resolver tab: disabling the find-next-solution button: there are no broken dependencies.");
	return false;
      }
    else if(state.background_thread_active)
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		  "Resolver tab: disabling the find-next-solution button: the resolver is already running.");
	return false;
      }
    else if(state.solutions_exhausted)
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		  "Resolver tab: disabling the find-next-solution button: there are no more solutions.");
	return false;
      }
    else
      return true;
  }

  bool ResolverTab::do_find_next_solution_enabled()
  {
    if (get_resolver() == NULL)
      return false;

    resolver_manager::state state = get_resolver()->state_snapshot();
    return do_find_next_solution_enabled_from_state(state);
  }

  void ResolverTab::do_find_next_solution()
  {
    resolver_manager::state state = get_resolver()->state_snapshot();

    if(do_find_next_solution_enabled_from_state(state))
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(), "Resolver tab: moving to solution number "
		  << state.generated_solutions);
	get_resolver()->discard_error_information();
	get_resolver()->select_solution(state.generated_solutions);
	do_start_solution_calculation(get_resolver());
      }
  }

  void ResolverTab::do_apply_solution()
  {
    if (!apt_cache_file || get_resolver() == NULL)
      return;

    if (displayed_solution.valid())
    {
      LOG_TRACE(Loggers::getAptitudeGtkResolver(),
		"Resolver tab: Applying the current solution: "
		<< displayed_solution);

      const bool safe_using_internal_resolver = using_internal_resolver;

      // BIG FAT WARNING: applying the solution will destroy this tab
      // as a side effect!  From here on out we can't refer to members
      // of this instance.
      undo_group *undo = new apt_undo_group;
      (*apt_cache_file)->apply_solution(displayed_solution, undo);

      if (!undo->empty())
        apt_undos->add_item(undo);
      else
        delete undo;

      // This is a bit of a hack, to ensure that the user doesn't get
      // dropped back at the dashboard after fixing an upgrade
      // manually.
      if(safe_using_internal_resolver)
	pMainWindow->do_preview();
      // No need to delete the tab manually here: it should already
      // have been deleted.  Deleting it here would introduce a
      // double-free bug.
    }
  }

  void ResolverTab::set_fix_upgrade_resolver(resolver_manager *manager)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    LOG_TRACE(logger, "Setting the resolver manager to " << manager);

    resolver_state_changed_connection.disconnect();

    using_internal_resolver = true;
    resolver = manager;
    if(manager != NULL)
      {
	manager->state_changed.connect(sigc::bind(sigc::ptr_fun(&do_start_first_solution_calculation), manager));
	manager->state_changed.connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
						  false));

	get_widget()->set_sensitive(true);

	boost::shared_ptr<gui_resolver_continuation> k =
	  boost::make_shared<gui_resolver_continuation>(manager);
	manager->maybe_start_solution_calculation(k, post_thunk);

	resolver_fixing_upgrade_label->show();
	resolver_fixing_upgrade_progress_bar->hide();
      }
    else
      {
	get_widget()->set_sensitive(false);
	resolver_fixing_upgrade_label->hide();
	resolver_fixing_upgrade_progress_bar->show();
	resolver_fixing_upgrade_progress_bar->pulse();
      }

    resolver_fixing_upgrade_message->show();
    setup_resolver_connections();
    update(true);
  }

  void ResolverTab::pulse_fix_upgrade_resolver_progress()
  {
    resolver_fixing_upgrade_progress_bar->pulse();
  }
}
