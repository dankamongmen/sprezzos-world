// dashboardtab.cc
//
//   Copyright (C) 2008-2009, 2011 Obey Arthur Liu
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

#include "dashboardtab.h"

#include "changelog.h"
#include "entitysummary.h"
#include "hyperlink.h"
#include "info.h"
#include "packagestab.h" // For PackageSearchEntry.
#include "pkgview.h"
#include "progress.h"
#include "resolver.h"

#include <aptitude.h>
#include <loggers.h>

#include <apt-pkg/pkgcache.h>

#include <boost/make_shared.hpp>
#include <boost/ref.hpp>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/resolver_manager.h>

#include <generic/util/undo.h>

#include <cwidget/generic/util/ssprintf.h>

namespace cw = cwidget;
using aptitude::Loggers;

namespace
{
  // Used to build the initial installations for the resolver.
  imm::map<aptitude_resolver_package, aptitude_resolver_version>
  get_upgradable()
  {
    imm::map<aptitude_resolver_package, aptitude_resolver_version> rval;

    std::set<pkgCache::PkgIterator> upgradable;
    (*apt_cache_file)->get_upgradable(true, upgradable);

    for(std::set<pkgCache::PkgIterator>::const_iterator it =
	  upgradable.begin(); it != upgradable.end(); ++it)
      {
	pkgCache::PkgIterator pkg(*it);
	pkgCache::VerIterator ver((*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file));

	aptitude_resolver_package resolver_pkg(pkg, *apt_cache_file);
	aptitude_resolver_version resolver_ver =
	  aptitude_resolver_version::make_install(ver, *apt_cache_file);

	rval.put(resolver_pkg, resolver_ver);
      }

    return rval;
  }
}

namespace gui
{
  void DashboardTab::fixing_upgrade_info::handle_fixing_upgrade_tab_closed()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    LOG_TRACE(logger, "The resolver tab for manually fixing the upgrade was closed.");

    fixing_upgrade_tab = NULL;
    // Don't destroy the resolver; the user might want to continue
    // from where they left off.
  }

  void DashboardTab::fixing_upgrade_info::create_resolver(const generic_solution<aptitude_universe> &sol)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    if(sol.valid())
      LOG_DEBUG(logger, "Manually fixing the remaining upgrades from the solution " << sol);
    else
      LOG_DEBUG(logger, "Manually fixing the upgrade.");

    // Figure out the initial set of installations.
    imm::map<aptitude_resolver_package, aptitude_resolver_version> initial_versions;

    if(sol.valid())
      {
	// Start with the initial versions of this solution.
	std::set<aptitude_resolver_version> sol_initial_versions;
	sol.get_initial_state().get_initial_versions(sol_initial_versions);
	for(std::set<aptitude_resolver_version>::const_iterator it
	      = sol_initial_versions.begin();
	    it != sol_initial_versions.end(); ++it)
	  initial_versions.put(it->get_package(), *it);

	typedef generic_choice<aptitude_universe> choice;
	typedef generic_choice_set<aptitude_universe> choice_set;

	// Now apply the changes described by this solution, but NOT
	// keeps!
	for(choice_set::const_iterator it = sol.get_choices().begin();
	    it != sol.get_choices().end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		if(it->get_ver() == it->get_ver().get_package().current_version())
		  LOG_TRACE(logger, "Not including " << it->get_ver() << " in the initial state of the upgrade-fixing resolver: it is a keep.");
		else
		  {
		    LOG_TRACE(logger, "Adding " << it->get_ver() << " to the initial state of the upgrade-fixing resolver.");
		    initial_versions.put(it->get_ver().get_package(), it->get_ver());
		  }
		break;

	      default:
		LOG_TRACE(logger, "Not adding " << (*it) << " to the initial state of the upgrade-fixing resolver: it does not modify a package state.");
		break;
	      }
	  }
      }
    else
      // Assume we just couldn't find any solutions (ow).
      initial_versions = get_upgradable();

    LOG_DEBUG(logger, "Initial versions for the manual fixer: " << initial_versions);
    if(fixing_upgrade_resolver)
      discard_resolver();
    LOG_TRACE(logger, "Creating a new resolver manager for manually fixing the upgrade.");
    fixing_upgrade_resolver = new resolver_manager(apt_cache_file,
						   initial_versions);

    if(fixing_upgrade_tab != NULL)
      {
	if(fixing_upgrade_resolver->resolver_exists())
	  {
	    LOG_TRACE(logger, "Sending the new resolver manager to the fixing-upgrade tab.");
	    fixing_upgrade_tab->set_fix_upgrade_resolver(fixing_upgrade_resolver);
	  }
	else
	  LOG_ERROR(logger, "No resolver exists in the new resolver manager -- not sending it to the fixing-upgrade tab.");
      }
  }

  DashboardTab::fixing_upgrade_info::fixing_upgrade_info()
    : fixing_upgrade_resolver(NULL),
      fixing_upgrade_tab(NULL)
  {
  }

  DashboardTab::fixing_upgrade_info::~fixing_upgrade_info()
  {
    discard_resolver();
  }

  void DashboardTab::fixing_upgrade_info::discard_resolver()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    LOG_TRACE(logger, "Discarding the resolver for manual upgrade fixing.");

    delete fixing_upgrade_resolver;
    fixing_upgrade_resolver = NULL;

    if(fixing_upgrade_tab != NULL)
      fixing_upgrade_tab->set_fix_upgrade_resolver(fixing_upgrade_resolver);
  }

  void DashboardTab::fixing_upgrade_info::pulse()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    LOG_TRACE(logger, "Pulsing the progress bar in the tab for manually fixing the upgrade.");

    if(fixing_upgrade_tab != NULL)
      fixing_upgrade_tab->pulse_fix_upgrade_resolver_progress();
  }

  void DashboardTab::fixing_upgrade_info::solution_calculated(const generic_solution<aptitude_universe> &sol)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    if(fixing_upgrade_tab != NULL)
      create_resolver(sol);
    else
      LOG_TRACE(logger, "Not creating a new resolver to fix upgrades manually: there is no active tab.");
  }

  void DashboardTab::fixing_upgrade_info::show_fixing_upgrade(const generic_solution<aptitude_universe> &sol)
  {
    if(fixing_upgrade_tab == NULL)
      {
	logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());
	LOG_TRACE(logger, "Creating new resolver tab to manually fix the upgrade.");

	fixing_upgrade_tab = new ResolverTab(_("Resolve Upgrade Manually"));
	fixing_upgrade_tab->closed.connect(sigc::mem_fun(*this, &fixing_upgrade_info::handle_fixing_upgrade_tab_closed));
	pMainWindow->tab_add(fixing_upgrade_tab);
      }

    solution_calculated(sol);
  }

  void DashboardTab::do_search()
  {
    pMainWindow->add_packages_tab(package_search_entry->get_text());
  }

  void DashboardTab::do_fix_manually()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    if(upgrade_resolver == NULL)
      // Should never happen.
      LOG_TRACE(logger, "Not fixing the upgrade manually: sanity-check failed: no upgrade resolver.");
    else
      fixing_upgrade.show_fixing_upgrade(upgrade_solution);
  }

  void DashboardTab::do_upgrade()
  {
    if(apt_cache_file == NULL)
      return;

    // NB: here we assume that the upgradable set is what the current
    // solution is based on.
    undo_group *undo = new apt_undo_group;
    {
      aptitudeDepCache::action_group group(*apt_cache_file, NULL);

      if(upgrade_solution.valid())
	(*apt_cache_file)->apply_solution(upgrade_solution, NULL);
      else
	// If there is no solution, assume that the upgrade can be
	// scheduled with no dependency problems.
	(*apt_cache_file)->mark_all_upgradable(false, true, NULL);
    }
    if(!undo->empty())
      apt_undos->add_item(undo);
    else
      delete undo;

    pMainWindow->do_preview();
  }

  bool DashboardTab::get_edit_columns_available()
  {
    return true;
  }

  void DashboardTab::dispatch_edit_columns()
  {
    upgrades_pkg_view->show_edit_columns_dialog();
  }

  // Download all the changelogs and show the new entries.
  void DashboardTab::create_upgrade_summary()
  {
    if(apt_cache_file == NULL)
      return;

    std::map<pkgCache::VerIterator, Glib::RefPtr<Gtk::TextBuffer::Mark> > changelog_locations_new;

    Glib::RefPtr<Gtk::TextBuffer> text_buffer = Gtk::TextBuffer::create();

    Glib::RefPtr<Gtk::TextBuffer::Tag> header_tag = text_buffer->create_tag();
    header_tag->property_weight() = Pango::WEIGHT_BOLD;
    header_tag->property_weight_set() = true;
    header_tag->property_scale() = Pango::SCALE_X_LARGE;
    header_tag->property_scale_set() = true;

    cwidget::util::ref_ptr<guiOpProgress> p(guiOpProgress::create());

    std::vector<pkgCache::VerIterator> versions;

    {
      int i = 0;
      p->OverallProgress(i, (*apt_cache_file)->Head().PackageCount, 1,
			 _("Preparing to download changelogs"));

      for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
	  !pkg.end(); ++pkg)
	{
	  if(pkg->CurrentState == pkgCache::State::Installed &&
	     (*apt_cache_file)[pkg].Upgradable())
	    {
	      pkgCache::VerIterator candver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);

	      if(!candver.end())
		versions.push_back(candver);
	    }

	  ++i;
	  p->Progress(i);
	}

      p->Done();
    }

    std::sort(versions.begin(), versions.end(), ver_name_lt());

    upgrades_summary_textview->set_buffer(text_buffer);

    for(std::vector<pkgCache::VerIterator>::const_iterator it =
	  versions.begin(); it != versions.end(); ++it)
      {
	// Write out the header: PACKAGE: VERSION -> VERSION
	pkgCache::VerIterator candver = *it;
	Gtk::TextBuffer::iterator where = text_buffer->end();

	Glib::RefPtr<Gtk::TextBuffer::Mark> header_begin_mark =
	  text_buffer->create_mark(where);
	changelog_locations_new[*it] = header_begin_mark;

	where = text_buffer->insert(where,
				    candver.ParentPkg().Name());

	where = text_buffer->insert(where, " ");

	pkgCache::VerIterator currver = candver.ParentPkg().CurrentVer();
	if(currver.end() ||
	   currver.VerStr() == NULL) // Just defensive; should never happen.
	  where = text_buffer->insert(where, "???");
	else
	  where = add_hyperlink(text_buffer, where,
				currver.VerStr(),
				sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					   currver.ParentPkg(), currver));

	where = text_buffer->insert(where, " -> ");

	where = add_hyperlink(text_buffer, where,
			      candver.VerStr(),
			      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					 candver.ParentPkg(), candver));

	where = text_buffer->insert(where, "\n");

	const Gtk::TextBuffer::iterator header_begin =
	  text_buffer->get_iter_at_mark(header_begin_mark);
	text_buffer->apply_tag(header_tag, header_begin, where);

	where = text_buffer->insert(where, "\n");

	const Gtk::TextBuffer::iterator changelog_begin_iter = where;

	where = fetch_and_show_changelog(candver,
					 text_buffer,
					 upgrades_summary_textview,
					 changelog_begin_iter,
					 true);

	where = text_buffer->insert(where, "\n\n");
      }

    upgrades_summary_textview->set_buffer(text_buffer);
    changelog_locations.swap(changelog_locations_new);
  }

  void DashboardTab::handle_cache_closed()
  {
    changelog_locations.clear();
    discard_resolver();
    available_upgrades_label->set_text(_("Available upgrades:"));
  }

  void DashboardTab::handle_cache_reloaded()
  {
    (*apt_cache_file)->pre_package_state_changed.connect(sigc::mem_fun(*this, &DashboardTab::discard_resolver));
    (*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &DashboardTab::make_resolver));
    make_resolver();
  }

  void DashboardTab::handle_upgrades_store_reloaded()
  {
    // Slightly lame: we know that the upgrade view will have as
    // many rows as there are upgrades, so just count the number of
    // rows (rather than re-calculating that and maybe being slow or
    // inconsistent).
    int num_upgrades = upgrades_pkg_view->get_model()->children().size();
    const char *text(ngettext("%d available upgrade:",
			      "%d available upgrades:",
			      num_upgrades));
    std::string formatted_text = cw::util::ssprintf(text, num_upgrades);
    available_upgrades_label->set_text(formatted_text);
  }

  class DashboardTab::upgrade_continuation : public resolver_manager::background_continuation
  {
    safe_slot1<void, generic_solution<aptitude_universe> > success_slot;
    safe_slot0<void> no_more_solutions_slot;
    safe_slot1<void, std::string> aborted_slot;

    resolver_manager &resolver;

  public:
    upgrade_continuation(const safe_slot1<void, generic_solution<aptitude_universe> > &_success_slot,
			 const safe_slot0<void> &_no_more_solutions_slot,
			 const safe_slot1<void, std::string> &_aborted_slot,
			 resolver_manager &_resolver)
      : success_slot(_success_slot),
	no_more_solutions_slot(_no_more_solutions_slot),
	aborted_slot(_aborted_slot),
	resolver(_resolver)
    {
    }

    void success(const generic_solution<aptitude_universe> &sol)
    {
      post_event(safe_bind(success_slot, sol));
    }

    void no_more_solutions()
    {
      post_event(no_more_solutions_slot);
    }

    void no_more_time()
    {
      boost::shared_ptr<upgrade_continuation> k =
	boost::make_shared<upgrade_continuation>(success_slot,
						 no_more_solutions_slot,
						 aborted_slot,
						 boost::ref(resolver));

      resolver.maybe_start_solution_calculation(k, &post_thunk);
    }

    void interrupted()
    {
    }

    void aborted(const std::string &errmsg)
    {
      post_event(safe_bind(aborted_slot, errmsg));
    }
  };

  bool DashboardTab::pulse_progress_timeout()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    LOG_TRACE(logger, "Pulsing the upgrade resolver's progress bar.");

    upgrade_resolver_progress->pulse();
    fixing_upgrade.pulse();
    return true;
  }

  void DashboardTab::make_resolver()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    if(apt_cache_file == NULL)
      {
	LOG_WARN(logger, "Not creating a resolver: the apt cache is closed.");
	discard_resolver(); // Just to be sure.
      }
    else if(upgrade_resolver != NULL)
      {
	// Shouldn't happen.
	LOG_WARN(logger, "Not creating a new resolver for the dashboard tab (one already exists).");
      }
    else
      {
	LOG_TRACE(logger, "Creating a new resolver for the dashboard tab.");
	upgrade_resolver = new resolver_manager(apt_cache_file, get_upgradable());

	if(!upgrade_resolver->resolver_exists())
	  {
	    LOG_TRACE(logger, "Not calculating an upgrade: there are no broken dependencies.");
	    // To ensure consistency, invoke success() with an invalid
	    // solution (indicating nothing to be done).
	    upgrade_resolver_success(generic_solution<aptitude_universe>());
	    return;
	  }

	LOG_TRACE(logger, "Starting to calculate the upgrade in the background.");
	if(background_upgrade_redirect != NULL)
	  {
	    // Shouldn't happen.
	    LOG_WARN(logger, "The background redirecter was unexpectedly not NULL.");
	    delete background_upgrade_redirect;
	    background_upgrade_redirect = NULL;
	  }
	background_upgrade_redirect = new redirect_from_background(*this);

	{
	  sigc::slot<void, generic_solution<aptitude_universe> > success_slot =
	    sigc::mem_fun(*background_upgrade_redirect, &redirect_from_background::success);
	  sigc::slot<void> no_more_solutions_slot =
	    sigc::mem_fun(*background_upgrade_redirect, &redirect_from_background::no_more_solutions);
	  sigc::slot<void, std::string> aborted_slot =
	    sigc::mem_fun(*background_upgrade_redirect, &redirect_from_background::aborted);

	  boost::shared_ptr<upgrade_continuation> k =
	    boost::make_shared<upgrade_continuation>(make_safe_slot(success_slot),
						     make_safe_slot(no_more_solutions_slot),
						     make_safe_slot(aborted_slot),
						     boost::ref(*upgrade_resolver));

	  upgrade_resolver->safe_resolve_deps_background(false, true, k, &post_thunk);
	}

	LOG_TRACE(logger, "Setting up the progress bar.");
	upgrade_resolver_progress->show();
	upgrade_resolver_progress->set_text(_("Calculating upgrade..."));
	upgrade_resolver_progress->pulse();
	upgrade_resolver_label->hide();
	fix_manually_button->set_sensitive(false);
	upgrade_button->set_sensitive(false);

	pulse_progress_connection.disconnect(); // Just extra paranoia.
	pulse_progress_connection = Glib::signal_timeout().connect_seconds(sigc::mem_fun(*this, &DashboardTab::pulse_progress_timeout),
									   3);
      }
  }

  void DashboardTab::discard_resolver()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    LOG_TRACE(logger, "Discarding the dashboard tab's internal resolver.");

    // Eliminate all the connections for resolver events by zapping
    // the object that they go through.
    delete background_upgrade_redirect;
    background_upgrade_redirect = NULL;
    pulse_progress_connection.disconnect();

    // Throw away the resolver itself.
    delete upgrade_resolver;
    upgrade_resolver = NULL;

    // Adjust the UI to show that there's no upgrade available.
    upgrade_resolver_progress->hide();
    upgrade_resolver_label->hide();
    fix_manually_button->set_sensitive(false);
    upgrade_button->set_sensitive(false);

    fixing_upgrade.discard_resolver();
  }

  void DashboardTab::upgrade_resolver_success(generic_solution<aptitude_universe> sol)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());

    pulse_progress_connection.disconnect();

    if(apt_cache_file == NULL)
      {
	LOG_WARN(logger, "Received a solution from the resolver when the cache was closed.");
	// ABORT!  Why did we get here?
	discard_resolver();
	return;
      }

    upgrade_solution = sol;

    if(sol.valid())
      LOG_TRACE(logger, "Upgrade solution computed: " << sol);
    else
      LOG_TRACE(logger, "No upgrade solution computed because there were no problems to solve.");

    fixing_upgrade.solution_calculated(upgrade_solution);

    std::set<pkgCache::PkgIterator> upgrades;
    (*apt_cache_file)->get_upgradable(true, upgrades);

    // Find out how many upgrades will be installed.
    int num_upgrades_selected;
    if(sol.valid())
      {
	num_upgrades_selected = 0;
	for(std::set<pkgCache::PkgIterator>::const_iterator it = upgrades.begin();
	    it != upgrades.end(); ++it)
	  {
	    aptitude_resolver_package pkg(*it, *apt_cache_file);
	    aptitude_resolver_version installed = sol.version_of(pkg);

	    // TODO: if we ever allow full replacements in safe upgrade,
	    // this will be wrong; when that happens, we need to account
	    // for them and include them in the explanatory text below.
	    if(pkg.get_pkg().CurrentVer() != installed.get_ver())
	      ++num_upgrades_selected;
	  }
      }
    else
      // If there are no problems, we upgrade all the currently
      // upgradable packages.
      num_upgrades_selected = upgrades.size();
    const int num_upgrades_not_selected
      = upgrades.size() - num_upgrades_selected;

    upgrade_resolver_progress->hide();
    upgrade_resolver_label->show();

    if(upgrades.empty())
      {
	upgrade_resolver_label->set_text(_("No upgrades are available."));
	fix_manually_button->set_sensitive(false);
	upgrade_button->set_sensitive(false);
      }
    else
      {
	// TODO: if we couldn't install all the upgrades, offer to let
	// the user start resolving dependencies by hand from the last
	// stopping point of the resolver.
	Glib::ustring markup;

	if(num_upgrades_selected == 0)
	  markup = Glib::Markup::escape_text(_("Unable to calculate an upgrade."));
	else
	  {
	    // This message doesn't say how many upgrades there are
	    // because (A) I can't come up with a straightforward way
	    // of stating that, and (B) that information is already
	    // available in the label of the list view.

	    if(num_upgrades_selected > 0)
	      markup += cw::util::ssprintf(ngettext("Press \"%s\" to install <span size='large'>%d</span> upgrade out of <span size='large'>%d</span>.",
						    "Press \"%s\" to install <span size='large'>%d</span> upgrades out of <span size='large'>%d</span>.",
						    num_upgrades_selected),
					   upgrade_button->get_label().c_str(),
					   num_upgrades_selected,
					   (int)upgrades.size());

	    if(num_upgrades_not_selected > 0)
	      {
		if(!markup.empty())
		  markup += "\n";

		markup += cw::util::ssprintf(ngettext("Press \"%s\" to manually attempt to install the remaining <span size='large'>%d</span> upgrade.",
						      "Press \"%s\" to manually attempt to install the remaining <span size='large'>%d</span> upgrades.",
						      num_upgrades_not_selected),
					     fix_manually_button->get_label().c_str(),
					     num_upgrades_not_selected);
	      }
	  }

	upgrade_resolver_label->set_markup(markup);
	fix_manually_button->set_sensitive(num_upgrades_not_selected > 0);
	upgrade_button->set_sensitive(num_upgrades_selected > 0);
      }
  }

  void DashboardTab::upgrade_resolver_no_more_solutions()
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());
    LOG_TRACE(logger, "The upgrade resolver was unable to calculate a solution.");

    pulse_progress_connection.disconnect();

    // TODO: add a button that lets the user start resolving
    // dependencies by hand.
    upgrade_resolver_progress->hide();
    upgrade_resolver_label->show();
    upgrade_resolver_label->set_text(cw::util::ssprintf(_("Unable to calculate an upgrade.  Press \"%s\" to manually search for a solution."),
							fix_manually_button->get_label().c_str()));
    fix_manually_button->set_sensitive(true);
    upgrade_button->set_sensitive(false);
    upgrade_solution.nullify();
    fixing_upgrade.solution_calculated(upgrade_solution);
  }

  void DashboardTab::upgrade_resolver_aborted(std::string errmsg)
  {
    logging::LoggerPtr logger(Loggers::getAptitudeGtkDashboardUpgradeResolver());
    LOG_ERROR(logger, "The upgrade resolver aborted with an error: " << errmsg);

    pulse_progress_connection.disconnect();

    upgrade_resolver_progress->hide();
    upgrade_resolver_label->show();
    upgrade_resolver_label->set_text(cw::util::ssprintf(_("Internal error encountered while calculating an upgrade: %s"),
							errmsg.c_str()));
    fix_manually_button->set_sensitive(false);
    upgrade_button->set_sensitive(false);
  }

  DashboardTab::DashboardTab(Glib::ustring label)
    : Tab(Dashboard, label,
	  Gnome::Glade::Xml::create(glade_main_file, "dashboard_main"),
	  "dashboard_main"),
      upgrade_resolver(NULL),
      background_upgrade_redirect(NULL)
  {
    get_xml()->get_widget("dashboard_upgrades_selected_package_textview",
			  upgrades_changelog_view);

    Gtk::Entry *search_entry;
    Gtk::Button *search_button;
    Gtk::Label *search_errors;

    get_xml()->get_widget("dashboard_search_entry",
			  search_entry);
    get_xml()->get_widget("dashboard_search_button",
			  search_button);
    get_xml()->get_widget("dashboard_search_errors",
			  search_errors);

    get_xml()->get_widget("dashboard_upgrade_button",
			  upgrade_button);
    get_xml()->get_widget("dashboard_fix_manually_button",
			  fix_manually_button);
    get_xml()->get_widget("dashboard_available_upgrades_label",
			  available_upgrades_label);
    get_xml()->get_widget("dashboard_upgrades_summary_textview",
			  upgrades_summary_textview);
    get_xml()->get_widget("dashboard_upgrade_calculation_progress",
			  upgrade_resolver_progress);
    get_xml()->get_widget("dashboard_upgrade_label",
			  upgrade_resolver_label);

    package_search_entry = PackageSearchEntry::create(search_entry,
						      search_errors,
						      search_button);
    package_search_entry->activated.connect(sigc::hide(sigc::mem_fun(*this, &DashboardTab::do_search)));

    upgrades_pkg_view = cwidget::util::ref_ptr<PkgView>(new PkgView(get_xml(), "dashboard_upgrades_treeview",
								    _("Dashboard"), "",
								    sigc::ptr_fun(&guiOpProgress::create)));
    upgrades_pkg_view->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &DashboardTab::activated_upgrade_package_handler));
    upgrades_pkg_view->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &DashboardTab::activated_upgrade_package_handler));

    cache_closed.connect(sigc::mem_fun(*this,
				       &DashboardTab::handle_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this,
					 &DashboardTab::handle_cache_reloaded));
    upgrades_pkg_view->store_reloaded.connect(sigc::mem_fun(*this,
							    &DashboardTab::handle_upgrades_store_reloaded));

    fix_manually_button->set_sensitive(false);
    upgrade_button->set_sensitive(false);

    if(apt_cache_file != NULL)
      {
	handle_cache_reloaded();
      }

    // TODO: use get_upgradable() to populate it with exactly the
    // packages that would be upgraded?
    upgrades_pkg_view->set_limit(aptitude::matching::pattern::make_upgradable());

    cache_reloaded.connect(sigc::mem_fun(*this, &DashboardTab::create_upgrade_summary));
    create_upgrade_summary();

    upgrade_button->set_image(*manage(new Gtk::Image(Gtk::Stock::GO_UP, Gtk::ICON_SIZE_BUTTON)));
    upgrade_button->signal_clicked().connect(sigc::mem_fun(*this, &DashboardTab::do_upgrade));

    fix_manually_button->signal_clicked().connect(sigc::mem_fun(*this, &DashboardTab::do_fix_manually));

    get_widget()->show_all();

    // TODO: start an "update" when the program starts, or not?

    // TODO: start downloading changelogs and display them when
    // packages are selected.

    // TODO: customize the displayed columns to display version /
    // size / archive information.
  }

  DashboardTab::~DashboardTab()
  {
    discard_resolver();
  }

  void DashboardTab::activated_upgrade_package_handler()
  {
    if(apt_cache_file == NULL)
      {
	upgrades_changelog_view->get_buffer()->set_text("");
	return;
      }

    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    upgrades_pkg_view->get_treeview()->get_cursor(path, focus_column);
    if (upgrades_pkg_view->get_treeview()->get_selection()->is_selected(path))
      {
	Gtk::TreeModel::iterator iter = upgrades_pkg_view->get_model()->get_iter(path);
	using cwidget::util::ref_ptr;
	ref_ptr<Entity> ent = (*iter)[upgrades_pkg_view->get_columns()->EntObject];

	show_entity_summary(ent, upgrades_changelog_view);



	// Try to scroll to its changelog in the big list.
	ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();

	if(!pkg_ent.valid())
	  {
	    upgrades_changelog_view->set_buffer(Gtk::TextBuffer::create());
	    return;
	  }

	pkgCache::PkgIterator pkg = pkg_ent->get_pkg();
	pkgCache::VerIterator candver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);


	std::map<pkgCache::VerIterator, Glib::RefPtr<Gtk::TextBuffer::Mark> >::const_iterator
	  found(changelog_locations.find(candver));
	if(found != changelog_locations.end())
	  upgrades_summary_textview->scroll_to(found->second, 0, 0, 0);
      }
    else
      {
	upgrades_changelog_view->set_buffer(Gtk::TextBuffer::create());
      }
  }
}


