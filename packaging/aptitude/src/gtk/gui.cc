// gui.cc
//
//  Copyright 1999-2010 Daniel Burrows
//  Copyright 2008-2009 Obey Arthur Liu
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gui.h"

#include "aptitude.h"
#include "loggers.h"

#include <signal.h>

#include <map>

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>
#include <generic/apt/download_update_manager.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/parse_dpkg_status.h>
#include <generic/apt/tags.h>

#include <generic/util/refcounted_wrapper.h>

#include <sigc++/signal.h>

#include <boost/make_shared.hpp>

#include <cwidget/generic/threads/event_queue.h>
#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

#include <gtk/dashboardtab.h>
#include <gtk/dependency_chains_tab.h>
#include <gtk/download.h>
#include <gtk/dpkg_terminal.h>
#include <gtk/hyperlink.h>
#include <gtk/packagestab.h>
#include <gtk/previewtab.h>
#include <gtk/progress.h>
#include <gtk/resolver.h>
#include <gtk/tab.h>

// \todo ui_download_manager should live in generic/.
#include "../ui_download_manager.h"

namespace cw = cwidget;

using aptitude::Loggers;

namespace gui
{
  // \todo Some of these icon choices suck.
  const entity_state_info not_installed_columns("p", N_("Not installed"), Gtk::StockID());
  const entity_state_info virtual_columns("p", N_("Virtual"), Gtk::StockID());
  const entity_state_info unpacked_columns("u", N_("Unpacked"), Gtk::Stock::DIALOG_WARNING);
  const entity_state_info half_configured_columns("C", N_("Half-configured"), Gtk::Stock::DIALOG_WARNING);
  const entity_state_info half_installed_columns("H", N_("Half-installed"), Gtk::Stock::DIALOG_WARNING);
  const entity_state_info config_files_columns("c", N_("Configuration files and data remain"), Gtk::Stock::PROPERTIES);
  const entity_state_info triggers_awaited_columns("W", N_("Triggers awaited"), Gtk::Stock::DIALOG_WARNING);
  const entity_state_info triggers_pending_columns("T", N_("Triggers pending"), Gtk::Stock::DIALOG_WARNING);
  const entity_state_info installed_columns("i", N_("Installed"), Gtk::Stock::YES);
  const entity_state_info error_columns("E", "Internal Error (bad state)", Gtk::Stock::DIALOG_ERROR);

  const entity_state_info install_columns("i", N_("Install"), Gtk::Stock::ADD);
  const entity_state_info reinstall_columns("r", N_("Reinstall"), Gtk::Stock::ADD);
  const entity_state_info upgrade_columns("u", N_("Upgrade"), Gtk::Stock::GO_UP);
  const entity_state_info downgrade_columns("u", N_("Downgrade"), Gtk::Stock::GO_DOWN);
  const entity_state_info remove_columns("d", N_("Remove"), Gtk::Stock::REMOVE);
  const entity_state_info purge_columns("p", N_("Remove and purge configuration/data"), Gtk::Stock::CLEAR);
  const entity_state_info hold_columns("h", N_("Hold (don't upgrade)"), Gtk::Stock::MEDIA_PAUSE);
  const entity_state_info forbid_columns("F", N_("Forbidden version"), Gtk::Stock::STOP);
  const entity_state_info broken_columns("B", N_("Unsatisfied dependencies"), Gtk::Stock::DIALOG_ERROR);
  const entity_state_info no_action_columns("", "", Gtk::StockID());


  const char *lightred_background_color = "#FFCCCC";
  const char *lightgreen_background_color = "#DDFFDD";

  Glib::RefPtr<Gnome::Glade::Xml> refXml;
  std::string glade_main_file;
  Gtk::Main * pKit;
  AptitudeWindow * pMainWindow;

  // True if a download or package-list update is proceeding.  This hopefully will
  // avoid the nasty possibility of collisions between them.
  // FIXME: uses implicit locking -- if multithreading happens, should use a mutex
  //       instead.
  static bool active_download;
  bool want_to_quit = false;

  namespace
  {
    // The Glib::dispatch mechanism only allows us to wake the main
    // thread up; it doesn't allow us to pass actual information across
    // the channel.  To hack this together, I borrow the event_queue
    // abstraction from cwidget, and use a dispatcher for the sole
    // purpose of waking the main thread up.
    //
    // I believe that putting sigc++ slots on this list should be OK:
    // from the sigc++ code, it looks like they get passed by value, not
    // by reference.  Once the slot is safely stuck into the list,
    // everything should be OK.

    cwidget::threads::mutex background_events_mutex;
    // Set to "true" if the background events function is currently
    // executing a thunk, in which case we don't need to invoke the
    // dispatcher.  This avoids trouble caused by the thunk invoking
    // post_event() and filling up the queue, so it deadlocks itself.
    bool run_background_events_active = false;
    bool background_events_dispatcher_signalled = false;
    std::deque<safe_slot0<void> > background_events;
    Glib::Dispatcher background_events_dispatcher;

    // Used to ensure that run_background_events_active is true only
    // while the inner portion of the "while" loop below is running.
    class set_bool_in_scope
    {
      bool &target;

    public:
      set_bool_in_scope(bool &_target)
	: target(_target)
      {
	target = true;
      }

      ~set_bool_in_scope()
      {
	target = false;
      }
    };

    void run_background_events()
    {
      cwidget::threads::mutex::lock l(background_events_mutex);
      while(!background_events.empty())
	{
	  set_bool_in_scope setter(run_background_events_active);
	  safe_slot0<void> f = background_events.front();
	  background_events.pop_front();

	  l.release();
	  f.get_slot()();
	  l.acquire();
	}
      background_events_dispatcher_signalled = false;
    }
  }

  std::string entity_state_info::get_description_i18n() const
  {
    if(description.empty())
      return "";
    else
      return _(description.c_str());
  }

  void post_event(const safe_slot0<void> &event)
  {
    // Ensure that the dispatcher is only invoked once.
    cwidget::threads::mutex::lock l(background_events_mutex);
    background_events.push_back(event);
    if(!run_background_events_active && !background_events_dispatcher_signalled)
      {
	background_events_dispatcher();
	background_events_dispatcher_signalled = true;
      }
  }

  void post_thunk(const sigc::slot<void> &thunk)
  {
    post_event(make_safe_slot(thunk));
  }

  void gtk_update()
  {
    while (Gtk::Main::events_pending())
      Gtk::Main::iteration();
  }
  progress_with_destructor make_gui_progress()
  {
    cw::util::ref_ptr<guiOpProgress> rval =
      guiOpProgress::create();
    return std::make_pair(rval,
			  sigc::mem_fun(*rval.unsafe_get_ref(),
					&guiOpProgress::destroy));
  }

  void start_download(const boost::shared_ptr<download_manager> &manager,
		      const std::string &title,
		      Gtk::Widget *image,
		      download_progress_mode progress_mode,
		      NotifyView *view,
		      const sigc::slot0<void> &download_starts_slot,
		      const sigc::slot0<void> &download_stops_slot)
  {
    cw::util::ref_ptr<download_list_model> model(download_list_model::create());
    download_signal_log *log = new download_signal_log;
    model->connect(log);

    Notification *n = make_download_notification(title,
						 progress_mode,
						 model,
						 log);
    n->set_image(image);

    view->add_notification(n);

    using aptitude::util::refcounted_wrapper;
    cwidget::util::ref_ptr<refcounted_wrapper<Notification> >
      n_wrapper(new refcounted_wrapper<Notification>(n));

    ui_download_manager *uim =
      new ui_download_manager(manager,
			      log,
			      n_wrapper,
			      sigc::ptr_fun(&make_gui_progress),
			      &post_event);

    uim->download_starts.connect(download_starts_slot);
    uim->download_stops.connect(download_stops_slot);

    uim->start();
  }

  void gui_finish_download()
  {
    active_download = false;
    // Update indicators that expect to know something about arbitrary
    // package states (e.g., the count of broken packages).
    (*apt_cache_file)->package_state_changed();
  }

  // \todo make this use the threaded download system.
  void really_do_update_lists()
  {
    boost::shared_ptr<download_update_manager> m(boost::make_shared<download_update_manager>());

    active_download = true;

    start_download(m,
		   _("Checking for updates"),
		   new Gtk::Image(Gtk::Stock::REFRESH,
				  Gtk::ICON_SIZE_LARGE_TOOLBAR),
		   download_progress_pulse,
		   pMainWindow->get_notifyview(),
		   sigc::ptr_fun(&gui_finish_download));
  }

  void do_update_lists()
  {
    if (!active_download)
      {
	// \todo We should offer to become root here.
	if (getuid()==0)
	  really_do_update_lists();
	else
	  {
	    Gtk::MessageDialog dialog(*pMainWindow,
				      _("Insufficient privileges."),
				      false,
				      Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
	    dialog.set_secondary_text(_("You must be root to update the package lists."));

	    dialog.run();
	  }
      }
    else
      {
        Gtk::MessageDialog dialog(*pMainWindow,
				  _("Download already running."), false,
				  Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
        dialog.set_secondary_text(_("A package-list update or install run is already taking place."));

        dialog.run();
      }
  }

  namespace
  {
    class DpkgTerminalTab : public Tab
    {
      Gtk::ScrolledWindow *terminal_scrolled_window;

    public:
      /** \brief Create a new dpkg terminal tab.
       *
       *  \param term The terminal widget; the tab does NOT take
       *  ownership of this widget, in order to allow it to be reused
       *  in another tab later.  See DpkgTerminalNotification.
       */
      DpkgTerminalTab(Gtk::Widget *term)
	: Tab(DpkgTerminalTabType, "Applying changes",
	      Gnome::Glade::Xml::create(glade_main_file, "main_apply_changes_scrolledwindow"),
	      "main_apply_changes_scrolledwindow",
	      false)
      {
	get_xml()->get_widget("main_apply_changes_scrolledwindow",
			      terminal_scrolled_window);
	terminal_scrolled_window->remove();
	terminal_scrolled_window->add(*term);

	get_widget()->show_all();
      }
    };

    /** \brief A tab that displays a diff between two conffiles and
     *  allows the user to choose whether to install a new version.
     *
     *  Making this be a tab may be a bit borderline.  On the other
     *  hand, I kind of like keeping popups to an absolute minimum.
     */
    class ConffileDiffTab : public Tab
    {
      int child_output_fd;

      bool saw_any_output;

      Glib::RefPtr<Gtk::TextBuffer> diff_buffer;

      Glib::RefPtr<Gtk::TextBuffer::Tag> deleted_tag, inserted_tag;

      // We only output whole lines, so that we can apply tags to each
      // one.
      std::string current_line;

    public:
      ConffileDiffTab(const std::string &conffile,
		      const std::string &existing_file,
		      const std::string &new_file)
	: Tab(ConffileDiff,
	      cw::util::ssprintf(_("Update %s?"),
				 conffile.c_str()),
	      Gnome::Glade::Xml::create(glade_main_file, "main_conffile_diff_tab"),
	      "main_conffile_diff_tab"),
	  saw_any_output(false)
      {
	std::vector<std::string> arguments;
	arguments.push_back("/usr/bin/diff");
	arguments.push_back("-u");
	arguments.push_back(existing_file);
	arguments.push_back(new_file);

	Gtk::Label *explanation_label, *expander_label;
	Gtk::TextView *diff_view;
	Gtk::Button *yes_button, *no_button;

	get_xml()->get_widget("main_conffile_explanation_label", explanation_label);
	get_xml()->get_widget("main_conffile_expander_label", expander_label);
	get_xml()->get_widget("main_conffile_diff_view", diff_view);
	get_xml()->get_widget("main_conffile_yes_button", yes_button);
	get_xml()->get_widget("main_conffile_no_button", no_button);

	// Language shamelessly stolen from Synaptic, all hail mvo.
	//
	//   -- dburrows 2008-11-16.
	const std::string header =
	  ssprintf(_("Replace configuration file\n'%s'?"),
		   conffile.c_str());

	const std::string detail =
	  ssprintf(_("The configuration file %s was modified (by "
		     "you or by a script). An updated version is shipped "
		     "in this package. If you want to keep your current "
		     "version, choose 'No'. Do you want to replace the "
		     "current file and install the new package "
		     "maintainers version? "),
		   conffile.c_str());

	const std::string markup =
	  ssprintf("<span weight=\"bold\" size=\"larger\">%s</span>\n\n%s",
		   Glib::Markup::escape_text(header).c_str(),
		   Glib::Markup::escape_text(detail).c_str());

	explanation_label->set_markup(markup);

	// \todo Maybe add a button / option to run a custom diffing
	// tool?

	Pango::FontDescription monospace("monospace");
	diff_view->modify_font(monospace);
	diff_buffer = diff_view->get_buffer();

	yes_button->signal_clicked().connect(yes_clicked.make_slot());
	no_button->signal_clicked().connect(no_clicked.make_slot());

	// Add some placeholder text that will appear while we wait
	// for diff to run.
	diff_buffer->insert(diff_buffer->end(),
			    cw::util::ssprintf(_("Comparing %s to %s..."),
					       existing_file.c_str(), new_file.c_str()));

	deleted_tag = diff_buffer->create_tag();
	deleted_tag->property_foreground() = "#330000";

	inserted_tag = diff_buffer->create_tag();
	inserted_tag->property_foreground() = "#003300";

	Glib::spawn_async_with_pipes(".",
				     arguments,
				     Glib::SpawnFlags(0),
				     sigc::slot0<void>(),
				     NULL,
				     NULL,
				     &child_output_fd,
				     NULL);

	Glib::signal_io().connect(sigc::mem_fun(*this, &ConffileDiffTab::handle_diff_output),
				  child_output_fd,
				  Glib::IO_IN | Glib::IO_ERR | Glib::IO_HUP | Glib::IO_NVAL);

	// \todo Connect up to the yes/no signals, maybe provide our
	// own signals, etc.
      }

      sigc::signal0<void> yes_clicked;
      sigc::signal0<void> no_clicked;

      void process_diff_output(const char *buf, int len)
      {
	if(!saw_any_output)
	  {
	    // Throw away the placeholder message.
	    diff_buffer->erase(diff_buffer->begin(), diff_buffer->end());
	    saw_any_output = true;
	  }

	const char *where = buf;
	const char * const end = buf + len;

	while(where != end)
	  {
	    const char * const begin = where;

	    while(where != end && *where != '\n')
	      ++where;

	    current_line.append(begin, where);

	    if(where != end)
	      {
		++where;

		if(current_line.size() > 0)
		  {
		    switch(current_line[0])
		      {
		      case '+':
			diff_buffer->insert_with_tag(diff_buffer->end(),
						     current_line,
						     inserted_tag);
			break;

		      case '-':
			diff_buffer->insert_with_tag(diff_buffer->end(),
						     current_line,
						     deleted_tag);
			break;

		      default:
			diff_buffer->insert(diff_buffer->end(), current_line);
			break;
		      }
		  }

		diff_buffer->insert(diff_buffer->end(), "\n");
		current_line.clear();
	      }
	  }
      }

      bool handle_diff_output(Glib::IOCondition condition)
      {
	bool rval = false;

	if(condition & Glib::IO_IN)
	  {
	    char buf[1024];
	    const int buf_len = sizeof(buf);

	    int amt;
	    bool read_anything = false;
	    do
	      {
		amt = read(child_output_fd, buf, buf_len);
		process_diff_output(buf, amt);
		if(amt > 0)
		  read_anything = true;
	      } while(amt > 0);

	    rval = read_anything;
	  }
	else if( (condition & Glib::IO_NVAL) ||
		 (condition & Glib::IO_ERR) ||
		 (condition & Glib::IO_HUP) )
	  rval = false;
	else
	  {
	    _error->Warning("Unexpected IO condition %d", condition);
	    rval = false;
	  }

	return rval;
      }
    };

    class DpkgTerminalNotification : public Notification
    {
      Gtk::Button *view_details_button;
      Gtk::ProgressBar *progress;
      // The active terminal information tab, or NULL if none.
      DpkgTerminalTab *tab;
      DpkgTerminal *terminal;
      // Invoked as an idle callback after dpkg finishes running.
      safe_slot1<void, pkgPackageManager::OrderResult> k;

      bool finished;
      bool abort_ok;
      Gtk::MessageDialog *abort_ok_prompt;

      // True if the child is currently suspended (meaning the "get
      // the user's attention" animations are active).
      bool child_is_suspended;

      // The connection representing the timout that flashes the
      // details button.
      sigc::connection flash_details_button_connection;
      // If "true", we're on the "flash fast" state of the flash
      // animation; otherwise we're "between flashes".  We flash the
      // button 10 times over 2 seconds, then wait 15 seconds before
      // flashing again.
      bool flashing;
      // The number of flashes: this counts from 0 to 19 (with even
      // numbers turning the flash on and odd numbers turning it off).
      int flash_count;

      // The connection representing the timeout that makes the
      // progress bar pulse.
      sigc::connection pulse_progress_bar_connection;

      // The text that was displayed in the progress bar when the
      // child stopped to wait for user input.
      Glib::ustring child_suspended_text;
      // The fractional progress that was displayed before the child was suspended.
      double child_suspended_fraction;

      logging::LoggerPtr logger, logger_backgrounding;

      static Gdk::Color mix_colors(const Gdk::Color &c1, const Gdk::Color &c2)
      {
	Gdk::Color rval;
	rval.set_red((c1.get_red() + c2.get_red()) / 2);
	rval.set_blue((c1.get_blue() + c2.get_blue()) / 2);
	rval.set_green((c1.get_green() + c2.get_green()) / 2);

	return rval;
      }

      Gdk::Color view_details_orig_base;
      Gdk::Color view_details_orig_bg;

      gboolean flash_details_button()
      {
	if(flashing)
	  {
	    if(flash_count == 20)
	      {
		LOG_TRACE(logger, "Button flash: stopping.");

		flashing = false;
		flash_count = 0;
		view_details_button->modify_base(Gtk::STATE_NORMAL, view_details_orig_base);
		view_details_button->modify_bg(Gtk::STATE_NORMAL, view_details_orig_bg);

		flash_details_button_connection =
		  Glib::signal_timeout().connect_seconds(sigc::mem_fun(*this, &DpkgTerminalNotification::flash_details_button),
							 15);
		return FALSE;
	      }
	    else
	      {
		// We flash to a lighter color than the default and a
		// darker color than the default.  This is because we
		// don't know what color the button is: if the style
		// is very bright, a bright color won't show up; if
		// the style is very dark, a dark color won't show up.
		// So we find two colors that should always be visible
		// (I hope).
		bool highlighted = (flash_count % 2 == 0);
		++flash_count;

		LOG_TRACE(logger, "Button flash: flashing "
			  << (highlighted ? "bright." : "dark."));

		if(highlighted)
		  {
		    Gdk::Color base(mix_colors(view_details_orig_base, Gdk::Color("#000000")));
		    Gdk::Color bg(mix_colors(view_details_orig_bg, Gdk::Color("#000000")));

		    view_details_button->modify_base(Gtk::STATE_NORMAL, base);
		    view_details_button->modify_bg(Gtk::STATE_NORMAL, bg);
		  }
		else
		  {
		    Gdk::Color base(mix_colors(view_details_orig_base, Gdk::Color("#FFFFFF")));
		    Gdk::Color bg(mix_colors(view_details_orig_bg, Gdk::Color("#FFFFFF")));

		    view_details_button->modify_base(Gtk::STATE_NORMAL, base);
		    view_details_button->modify_bg(Gtk::STATE_NORMAL, bg);
		  }

		return TRUE;
	      }
	  }
	else
	  {
	    LOG_TRACE(logger, "Button flash: paused.");
	    flash_details_button_connection =
	      Glib::signal_timeout().connect(sigc::mem_fun(*this, &DpkgTerminalNotification::flash_details_button),
					     1000 / 20);
	    flashing = true;
	    flash_count = 0;
	    return FALSE;
	  }
      }

      gboolean pulse_progress_bar()
      {
	LOG_TRACE(logger, "Pulsing the progress bar.");
	progress->pulse();
	return TRUE;
      }

      void child_suspended()
      {
	if(child_is_suspended)
	  {
	    LOG_TRACE(logger, "The dpkg-suspended animations are already running; not starting them again.");
	    return;
	  }

	LOG_TRACE(logger, "Placing the dpkg status widgets into the 'dpkg suspended' state.");

	flashing = true;
	flash_count = 0;

	child_suspended_text = progress->get_text();
	child_suspended_fraction = progress->get_fraction();
	progress->set_text(child_suspended_text + ": waiting for user input...");

	flash_details_button_connection = Glib::signal_timeout().connect(sigc::mem_fun(*this, &DpkgTerminalNotification::flash_details_button),
								  1000 / 20);
	pulse_progress_bar_connection = Glib::signal_timeout().connect_seconds(sigc::mem_fun(*this, &DpkgTerminalNotification::pulse_progress_bar),
									       1);

	child_is_suspended = true;
      }

      void child_resumed()
      {
	if(!child_is_suspended)
	  {
	    LOG_TRACE(logger, "The dpkg-suspended animations are already stopped; not stopping them again.");
	    return;
	  }

	LOG_TRACE(logger, "Returning the dpkg status widgets to the normal 'dpkg running' state.");

	progress->set_text(child_suspended_text);
	progress->set_fraction(child_suspended_fraction);
	flash_details_button_connection.disconnect();
	pulse_progress_bar_connection.disconnect();
	view_details_button->modify_base(Gtk::STATE_NORMAL, view_details_orig_base);
	view_details_button->modify_bg(Gtk::STATE_NORMAL, view_details_orig_bg);
	child_is_suspended = false;
      }

      void finish_prompt_abort_ok(int response)
      {
	LOG_TRACE(logger, "The user says that it " << (response == Gtk::RESPONSE_YES
						       ? "is" : "is not") << " OK to abort dpkg.");

	if(response == Gtk::RESPONSE_YES)
	  {
	    abort_ok = true;
	    close();
	  }

	if(abort_ok_prompt != NULL)
	  abort_ok_prompt->hide();
      }

      // \todo We should perhaps also prompt on delete_event() and
      // when the program quits.  I don't think delete_event() should
      // ask all active notifications to prompt, though (or should it?
      // I don't know -- it would be easy enough to arrange for that
      // to happen).
      bool prompt_abort_ok()
      {
	if(!finished && !abort_ok)
	  {
	    if(abort_ok_prompt == NULL)
	      {
		LOG_TRACE(logger, "Creating a new prompt to ask the user if aborting dpkg is OK.");
		// Hopefully this message is scary enough to make
		// people click "No" unless they really mean it. ;-) I
		// think the wording could be improved, though.

		// \todo We should find the real ultimate parent window of
		// the notification instead of assuming it's pMainWindow.
		abort_ok_prompt = new Gtk::MessageDialog(*pMainWindow, _("Interrupting this process could leave your system in an inconsistent state.  Are you sure you want to stop applying your changes?"),
							 false, Gtk::MESSAGE_QUESTION,
							 Gtk::BUTTONS_YES_NO, true);
		abort_ok_prompt->signal_response().connect(sigc::mem_fun(*this, &DpkgTerminalNotification::finish_prompt_abort_ok));
	      }
	    else
	      LOG_TRACE(logger, "Reusing the existing prompt to ask the user if aborting dpkg is OK.");

	    abort_ok_prompt->show();
	    return false;
	  }
	else
	  {
	    if(finished)
	      LOG_TRACE(logger, "Not asking whether to abort dpkg: it already finished on its own.");
	    else
	      LOG_TRACE(logger, "Not asking whether to abort dpkg: we already were told it's OK.");
	    return true;
	  }
      }

      void abort()
      {
	LOG_TRACE(logger,
		  "Aborting dpkg run.");
	delete terminal;
	terminal = NULL;

	finish_dpkg_run(pkgPackageManager::Incomplete);
      }

      void do_finish_dpkg_run(pkgPackageManager::OrderResult res)
      {
	LOG_TRACE(logger,
		  "Running post-dpkg actions (state " << res << ")");
	finished = true;
	progress->hide();
	Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();
	buffer->set_text(_("Done applying changes!"));
	set_buffer(buffer);

	k.get_slot()(res);
      }

      void finish_dpkg_run(pkgPackageManager::OrderResult res)
      {
	LOG_TRACE(logger,
		  "dpkg run finished (state " << res << ")");
	// Invoking this as an idle callback is a bit of a holdover
	// from when I tried (disastrously) to run the download
	// manager's finish() in a background thread.  Nonetheless, it
	// avoids any nasty surprises because of single-thread
	// reentrancy.
	sigc::slot1<void, pkgPackageManager::OrderResult> do_finish_dpkg_run_slot(sigc::mem_fun(*this, &DpkgTerminalNotification::do_finish_dpkg_run));
	post_event(safe_bind(make_safe_slot(do_finish_dpkg_run_slot), res));
      }

      void handle_subprocess_running_changed(bool state)
      {
	LOG_TRACE(logger,
		  "Subprocess is " << (state ? "running." : "suspended."));

	if(state)
	  child_resumed();
	else
	  child_suspended();
      }

    public:
	DpkgTerminalNotification(const safe_slot1<void, pkgPackageManager::OrderResult> &_k)
	: Notification(true),
	  progress(new Gtk::ProgressBar),
	  tab(NULL),
	  terminal(new DpkgTerminal),
	  k(_k),
	  finished(false),
	  abort_ok(false),
	  abort_ok_prompt(NULL),
	  child_is_suspended(false),
	  flashing(false),
	  flash_count(0),
	  logger(Loggers::getAptitudeDpkgTerminal())
      {
	LOG_TRACE(logger, "Creating dpkg terminal notification.");

	progress->set_text(_("Applying changes..."));
	progress->set_ellipsize(Pango::ELLIPSIZE_END);
	progress->show();
	prepend_widget(progress);

	terminal->finished.connect(sigc::mem_fun(*this, &DpkgTerminalNotification::finish_dpkg_run));
	terminal->status_message.connect(sigc::mem_fun(*this, &DpkgTerminalNotification::process_dpkg_message));
	terminal->subprocess_running_changed.connect(sigc::mem_fun(*this, &DpkgTerminalNotification::handle_subprocess_running_changed));

	view_details_button = new Gtk::Button(_("View Details"));
	view_details_button->signal_clicked().connect(sigc::mem_fun(*this, &DpkgTerminalNotification::view_details));
	view_details_button->show();
	add_button(view_details_button);

	view_details_orig_base = view_details_button->get_style()->get_base(Gtk::STATE_NORMAL);
	view_details_orig_bg = view_details_button->get_style()->get_bg(Gtk::STATE_NORMAL);

	closing.connect(sigc::mem_fun(*this, &DpkgTerminalNotification::prompt_abort_ok));
	closed.connect(sigc::mem_fun(*this, &DpkgTerminalNotification::abort));

	finalize();
	show();
      }

      void run_dpkg(const safe_slot1<pkgPackageManager::OrderResult, int> &f)
      {
	LOG_TRACE(logger, "Starting dpkg process.");
	terminal->run(f);
      }

      void view_details()
      {
	if(tab != NULL)
	  {
	    LOG_TRACE(logger, "Showing the existing dpkg tab.");
	    tab->get_widget()->show();
	  }
	else
	  {
	    LOG_TRACE(logger, "Creating and displaying a new dpkg tab.");
	    tab = new DpkgTerminalTab(terminal->get_widget());
	    tab_add(tab);
	  }
      }

      void process_dpkg_message(aptitude::apt::dpkg_status_message msg)
      {
	LOG_DEBUG(logger, "Received dpkg status message: " << msg);
	using aptitude::apt::dpkg_status_message;
	switch(msg.get_type())
	  {
	  case dpkg_status_message::error:
	    _error->Error(_("%s: %s"),
			  msg.get_package().c_str(),
			  msg.get_text().c_str());
	    progress->set_text(cw::util::ssprintf(_("Error in package %s"),
						  msg.get_package().c_str()));
	    break;

	  case dpkg_status_message::conffile:
	    // \todo Instead of always popping up the conffile prompt,
	    // should we show "yes" / "no" / "view details" buttons in
	    // the notification?
	    {
	      ConffileDiffTab *tab =
		new ConffileDiffTab(msg.get_text(),
				    msg.get_existing_filename(),
				    msg.get_new_filename());

	      if(terminal != NULL)
		{
		  tab->yes_clicked.connect(sigc::mem_fun(*terminal,
							 &DpkgTerminal::inject_yes));
		  tab->no_clicked.connect(sigc::mem_fun(*terminal,
							&DpkgTerminal::inject_no));
		}

	      tab->yes_clicked.connect(tab->close_clicked.make_slot());
	      tab->no_clicked.connect(tab->close_clicked.make_slot());

	      tab_add(tab);
	      tab->get_widget()->show();

	      progress->set_text(cw::util::ssprintf(_("Asking whether to replace the configuration file %s"),
						    msg.get_text().c_str()));
	    }
	    break;

	  case dpkg_status_message::status:
	    progress->set_text(msg.get_text());
	    break;
	  }

	progress->set_fraction(msg.get_percent() / 100.0);
      }

      ~DpkgTerminalNotification()
      {
	LOG_TRACE(logger, "Destroying the dpkg terminal notification and terminal.");

	delete abort_ok_prompt;
	delete tab;
	delete terminal;
      }
    };

    // Callback that kicks off a dpkg run.
    void gui_run_dpkg(sigc::slot1<pkgPackageManager::OrderResult, int> f,
		      sigc::slot1<void, pkgPackageManager::OrderResult> k)
    {
      DpkgTerminalNotification *n = new DpkgTerminalNotification(make_safe_slot(k));
      n->set_image(new Gtk::Image(Gtk::Stock::APPLY, Gtk::ICON_SIZE_LARGE_TOOLBAR));

      pMainWindow->get_notifyview()->add_notification(n);

      n->run_dpkg(make_safe_slot(f));
    }

    void install_or_remove_packages()
    {
      if(active_download)
	{
	  Gtk::MessageDialog dialog(*pMainWindow,
				    _("Download already running."), false,
				    Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
	  dialog.set_secondary_text(_("A package-list update or install run is already taking place."));

	  dialog.run();

	  return;
	}

      active_download = true;

      boost::shared_ptr<download_install_manager> m =
	boost::make_shared<download_install_manager>(false,
                                                     true,
						     sigc::ptr_fun(&gui_run_dpkg));

      start_download(m,
		     _("Downloading packages"),
		     new Gtk::Image(Gtk::Stock::APPLY,
				    Gtk::ICON_SIZE_LARGE_TOOLBAR),
		     download_progress_size,
		     pMainWindow->get_notifyview(),
		     sigc::slot0<void>(),
		     sigc::ptr_fun(&gui_finish_download));
    }
  }

  void do_mark_upgradable()
  {
    if(apt_cache_file)
    {
      aptitudeDepCache::action_group group(*apt_cache_file, NULL);
      undo_group *undo=new apt_undo_group;

      (*apt_cache_file)->mark_all_upgradable(true, true, undo);

      if(!undo->empty())
	apt_undos->add_item(undo);
      else
	delete undo;
    }
  }

  void do_keep_all()
  {
    std::auto_ptr<undo_group> undo(new apt_undo_group);
    aptitudeDepCache::action_group group(*apt_cache_file, undo.get());

    for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	!i.end(); ++i)
      (*apt_cache_file)->mark_keep(i, false, false, undo.get());

    if(!undo.get()->empty())
      apt_undos->add_item(undo.release());
  }

  /**
   * Adds a Tab_Type tab to the interface.
   * TODO: Get this one out of here!
   */
  void tab_add(Tab *tab)
  {
    pMainWindow->tab_add(tab);
  }

  void tab_del(Tab *tab)
  {
    pMainWindow->tab_del(tab);
  }

  void AptitudeWindow::tab_add(Tab *tab)
  {
    int new_page_idx = get_notebook()->append_page(*tab);
    get_notebook()->set_current_page(new_page_idx);
  }

  void AptitudeWindow::tab_del(Tab *tab)
  {
    get_notebook()->remove_page(*tab);
  }

  void AptitudeWindow::do_resolver()
  {
    tab_add(new ResolverTab(_("Resolver")));
  }

  void AptitudeWindow::do_preview()
  {
    tab_add(new PreviewTab(_("Preview")));
  }

  void AptitudeWindow::do_show_broken()
  {
    PackagesTab *tab = new PackagesTab(_("Broken packages"));
    tab->set_limit("?broken");
    tab_add(tab);
  }

  void AptitudeWindow::add_packages_tab(const std::string &pattern)
  {
    PackagesTab *tab = new PackagesTab("Packages " + pattern);
    tab_add(tab);
    tab->set_limit(pattern);
  }

  class BrokenPackagesNotification : public Notification
  {
  private:
    Gtk::Button *show_broken_button;
    Gtk::Button *resolve_dependencies_button;

    // Used to tell whether we need to update.
    int last_broken_count;

    void do_cache_reloaded()
    {
      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::update));
    }

  public:
    BrokenPackagesNotification(AptitudeWindow *main_window)
      : Notification(false)
    {
      last_broken_count = 0;

      show_broken_button = new Gtk::Button(_("Show broken packages"));
      show_broken_button->signal_clicked().connect(sigc::mem_fun(*main_window, &AptitudeWindow::do_show_broken));
      add_button(show_broken_button);

      resolve_dependencies_button = new Gtk::Button(_("Resolve dependencies"));
      resolve_dependencies_button->signal_clicked().connect(sigc::mem_fun(*main_window, &AptitudeWindow::do_resolver));
      add_button(resolve_dependencies_button);

      update();
      finalize();

      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::update));

      cache_reloaded.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::do_cache_reloaded));

      set_color(Gdk::Color("#FFE0E0"));
    }

    void update()
    {
      int broken_count = apt_cache_file ? (*apt_cache_file)->BrokenCount() : 0;

      if(broken_count == last_broken_count)
	return;

      Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();

      Glib::RefPtr<Gtk::TextBuffer::Tag> broken_tag = buffer->create_tag();
      broken_tag->property_weight() = Pango::WEIGHT_BOLD;

      buffer->insert_with_tag(buffer->end(),
			      ssprintf(ngettext("%d package is broken",
						"%d packages are broken.",
						broken_count),
				       broken_count),
			      broken_tag);

      bool something_is_broken = broken_count > 0;

      resolve_dependencies_button->set_sensitive(something_is_broken);

      property_visible() = something_is_broken;

      set_buffer(buffer);
      last_broken_count = broken_count;
    }
  };

  class NotificationInstallRemove : public Notification
  {
  private:
    Gtk::Button *preview_button;
    Gtk::Button *install_remove_button;

    // Used to tell whether we need to update.
    int last_broken_count;
    int last_download_size;
    int last_install_count;
    int last_remove_count;

    void do_cache_reloaded()
    {
      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &NotificationInstallRemove::update));
    }

  public:
    NotificationInstallRemove(AptitudeWindow *main_window)
      : Notification(false)
    {
      last_broken_count = 0;
      last_download_size = 0;
      last_install_count = 0;
      last_remove_count = 0;

      preview_button = new Gtk::Button(_("View changes"));
      preview_button->signal_clicked().connect(sigc::mem_fun(main_window, &AptitudeWindow::do_preview));
      add_button(preview_button);

      install_remove_button = new Gtk::Button(_("Apply changes"));
      install_remove_button->set_image(*new Gtk::Image(Gtk::Stock::APPLY,
						       Gtk::ICON_SIZE_BUTTON));
      install_remove_button->signal_clicked().connect(sigc::ptr_fun(&do_installremove));
      add_button(install_remove_button);

      update();
      finalize();

      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &NotificationInstallRemove::update));

      cache_reloaded.connect(sigc::mem_fun(*this, &NotificationInstallRemove::do_cache_reloaded));
    }

    void update()
    {
      int dl_size = apt_cache_file ? (*apt_cache_file)->DebSize() : 0;
      int broken_count = apt_cache_file ? (*apt_cache_file)->BrokenCount() : 0;
      int install_count = apt_cache_file ? (*apt_cache_file)->InstCount() : 0;
      int remove_count = apt_cache_file ? (*apt_cache_file)->DelCount() : 0;

      if(dl_size == last_download_size && broken_count == last_broken_count &&
	 install_count == last_install_count && remove_count == last_remove_count)
	return;

      Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();

      if(install_count > 0 || remove_count > 0)
	{
	  if(buffer->size() > 0)
	    buffer->insert(buffer->end(), "\n");

	  Glib::RefPtr<Gtk::TextBuffer::Mark> start_msg_mark = buffer->create_mark(buffer->end());

	  if(install_count > 0)
	    {
	      buffer->insert(buffer->end(),
			     // ForTranslators: any numbers in this
			     // string will be displayed in a larger
			     // font.
			     ssprintf(ngettext("%d package to install",
					       "%d packages to install",
					       install_count),
				      install_count));

	      if(remove_count > 0)
		buffer->insert(buffer->end(), "; ");
	    }

	  if(remove_count > 0)
	    {
	      buffer->insert(buffer->end(),
			     // ForTranslators: any numbers in this
			     // string will be displayed in a larger
			     // font.
			     ssprintf(ngettext("%d package to remove",
					       "%d packages to remove",
					       remove_count),
				      remove_count));

	      buffer->insert(buffer->end(), ".");
	    }

	  // HACK.  I want to make the numbers bigger, but I can't do
	  // that above because of translation considerations (we
	  // don't have a way of embedding markup into the string, so
	  // the markup can't be part of the string).  Instead I just
	  // magically know that the only digits in the string are the
	  // relevant numbers.
	  Gtk::TextBuffer::iterator start = buffer->get_iter_at_mark(start_msg_mark);
	  Gtk::TextBuffer::iterator end = buffer->end();
	  Glib::RefPtr<Gtk::TextBuffer::Tag> number_tag = buffer->create_tag();
	  number_tag->property_scale() = Pango::SCALE_LARGE;

	  while(start != end)
	    {
	      while(start != end && !isdigit(*start))
		++start;

	      if(start != end)
		{
		  Gtk::TextBuffer::iterator number_start = start;
		  while(start != end && isdigit(*start))
		    ++start;
		  buffer->apply_tag(number_tag, number_start, start);
		}
	    }
	}

      if(dl_size > 0)
	{
	  if(buffer->size() > 0)
	    buffer->insert(buffer->end(), "\n");
	  buffer->insert(buffer->end(),
			 ssprintf(_("Download size: %sB."),
				  SizeToStr(dl_size).c_str()));
	}

      bool something_is_broken = broken_count > 0;
      bool download_planned = install_count > 0 || remove_count > 0;

      preview_button->set_sensitive(download_planned);
      install_remove_button->set_sensitive(download_planned && !something_is_broken);

      property_visible() = !active_download && download_planned;

      set_buffer(buffer);
      last_broken_count = broken_count;
      last_download_size = dl_size;
      last_install_count = install_count;
      last_remove_count = remove_count;
    }
  };

  Gtk::TextBuffer::iterator add_debtags(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					Gtk::TextBuffer::iterator where,
					const pkgCache::PkgIterator &pkg,
					const Glib::RefPtr<Gtk::TextBuffer::Tag> &headerTag)
  {
    if(pkg.end())
      return where;

    using aptitude::apt::get_fullname;
    using aptitude::apt::get_tags;
    using aptitude::apt::tag;

    const std::set<tag> s(get_tags(pkg));

    if(s->empty() == false)
      {
	bool first = true;
	where = buffer->insert_with_tag(where,
					ssprintf(_("Tags of %s:\n"), pkg.Name()),
					headerTag);
	// TODO: indent all the tags.
	for(std::set<tag>::const_iterator it = s.begin();
	    it != s.end(); ++it)
	  {
	    const std::string name(get_fullname(*it));

	    if(first)
	      first = false;
	    else
	      where = buffer->insert(where, ", ");

	    where = add_hyperlink(buffer, where,
				  name,
				  sigc::bind(sigc::mem_fun(*pMainWindow, &AptitudeWindow::add_packages_tab),
					     "?tag(^" + backslash_escape_nonalnum(name) + "$)"));
	  }
      }

    return where;
  }

  void do_update()
  {
    do_update_lists();
  }

  void do_packages()
  {
    tab_add(new PackagesTab(_("Packages")));
  }

  void do_installremove()
  {
    install_or_remove_packages();
  }

  bool do_want_quit()
  {
    want_to_quit = true;
    return false;
  }

  void do_quit()
  {
    do_want_quit();
    pKit->quit();
  }

  void do_notimplemented_message()
  {
    do_notimplemented_message_custom(_("This feature is not implemented, yet."));
  }

  void do_notimplemented_message_custom(Glib::ustring msg)
  {
    Gtk::MessageDialog dialog(*pMainWindow,
                              _("Not implemented"),
                              false,
                              Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
    dialog.set_secondary_text(msg);
    dialog.run();
  }

  namespace
  {
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon,
                       sigc::slot0<void> callback,
                       bool sensitive = true,
                       int accel_key = 0,
                       Gdk::ModifierType accel_mods = Gdk::MODIFIER_MASK)
    {
      Gtk::Image *image = manage(new Gtk::Image(icon, Gtk::ICON_SIZE_MENU));
      Gtk::MenuItem *item = manage(new Gtk::ImageMenuItem(*image, label));

      if (sensitive)
      {
        item->signal_activate().connect(callback);
        if (accel_key || accel_mods)
        {
          item->add_accelerator("activate", pMainWindow->get_accel_group(), accel_key, accel_mods, Gtk::ACCEL_VISIBLE);
        }
      }
      else
        item->set_sensitive(false);

      menu->append(*item);

      item->show_all();
    }

    // for convenience
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon)
    {
      add_menu_item(menu, label, icon, sigc::slot0<void>(), false);
    }
  }

  void fill_package_menu(const std::set<PackagesAction> &actions,
			 const sigc::slot1<void, PackagesAction> &callback,
			 Gtk::Menu * menu)
  {
    if(actions.find(Upgrade) != actions.end())
      {
	if(actions.find(Install) != actions.end())
	  add_menu_item(menu, _("Install/Upgrade"), Gtk::Stock::ADD,
			sigc::bind(callback, Install),
			true,
	                GDK_i, Gdk::CONTROL_MASK);
	else
	  add_menu_item(menu, _("Upgrade"), Gtk::Stock::GO_UP,
			sigc::bind(callback, Install),
                        true,
	                GDK_i, Gdk::CONTROL_MASK);
      }
    else if(actions.find(Downgrade) != actions.end())
      add_menu_item(menu, _("Downgrade"), Gtk::Stock::GO_DOWN,
                    sigc::bind(callback, Install));
    else if(actions.find(Install) != actions.end())
      add_menu_item(menu, _("Install"), Gtk::Stock::ADD,
                    sigc::bind(callback, Install),
                    true,
                    GDK_i, Gdk::CONTROL_MASK);
    else
      add_menu_item(menu, _("Install/Upgrade"), Gtk::Stock::ADD); // Insensitive

    add_menu_item(menu, _("Remove"), Gtk::Stock::REMOVE,
                  sigc::bind(callback, Remove),
                  actions.find(Remove) != actions.end(),
                  GDK_minus, Gdk::CONTROL_MASK);

    add_menu_item(menu, _("Purge"), Gtk::Stock::CLEAR,
                  sigc::bind(callback, Purge),
                  actions.find(Purge) != actions.end(),
                  GDK_underscore, Gdk::CONTROL_MASK);

    add_menu_item(menu, _("Keep"), Gtk::Stock::MEDIA_REWIND,
                  sigc::bind(callback, Keep),
                  actions.find(Keep) != actions.end(),
                  GDK_colon, Gdk::CONTROL_MASK);

    add_menu_item(menu, _("Hold"), Gtk::Stock::MEDIA_PAUSE,
                  sigc::bind(callback, Hold),
                  actions.find(Hold) != actions.end(),
                  GDK_h, Gdk::CONTROL_MASK);

    if(actions.find(MakeAutomatic) != actions.end())
      add_menu_item(menu, _("Set as automatic"), Gtk::StockID(),
		    sigc::bind(callback, MakeAutomatic),
		    true,
	            GDK_M, Gdk::CONTROL_MASK|Gdk::SHIFT_MASK);
    else if(actions.find(MakeManual) != actions.end())
      add_menu_item(menu, _("Set as manual"), Gtk::StockID(),
		    sigc::bind(callback, MakeManual),
		    true,
                    GDK_m, Gdk::CONTROL_MASK);
    else
      add_menu_item(menu, _("Toggle automatic status"), Gtk::StockID(),
		    sigc::bind(callback, MakeManual),
		    false);
  }

  AptitudeWindow::AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Window(cobject)
  {
    refGlade->get_widget_derived("main_notebook", pNotebook);

    refGlade->get_widget("main_toolbutton_dashboard", pToolButtonDashboard);
    pToolButtonDashboard->signal_clicked().connect(sigc::mem_fun(*this, &AptitudeWindow::do_dashboard));

    refGlade->get_widget("main_toolbutton_update", pToolButtonUpdate);
    pToolButtonUpdate->signal_clicked().connect(&do_update);

    refGlade->get_widget("main_toolbutton_packages", pToolButtonPackages);
    pToolButtonPackages->signal_clicked().connect(&do_packages);

    refGlade->get_widget("main_toolbutton_preview", pToolButtonPreview);
    pToolButtonPreview->signal_clicked().connect(sigc::mem_fun(*this, &AptitudeWindow::do_preview));

    refGlade->get_widget("main_toolbutton_resolver", pToolButtonResolver);
    pToolButtonResolver->signal_clicked().connect(sigc::mem_fun(*this, &AptitudeWindow::do_resolver));

    refGlade->get_widget("main_toolbutton_installremove", pToolButtonInstallRemove);
    pToolButtonInstallRemove->signal_clicked().connect(&do_installremove);

    refGlade->get_widget("menu_do_package_run", pMenuFilePackageRun);
    pMenuFilePackageRun->signal_activate().connect(&do_installremove);

    refGlade->get_widget("menu_do_update_lists", pMenuFileUpdateLists);
    pMenuFileUpdateLists->signal_activate().connect(&do_update);

    refGlade->get_widget("menu_do_mark_upgradable", pMenuFileMarkUpgradable);
    pMenuFileMarkUpgradable->signal_activate().connect(&do_mark_upgradable);

    // Not implemented menu item placeholder
    refGlade->get_widget("menu_do_forget_new", pMenuFileForgetNew);
    pMenuFileForgetNew->signal_activate().connect(&do_notimplemented_message);

    refGlade->get_widget("menu_do_keep_all", pMenuFileKeepAll);
    pMenuFileKeepAll->signal_activate().connect(&do_keep_all);

    // Not implemented menu item placeholder
    refGlade->get_widget("menu_do_clean", pMenuFileClean);
    pMenuFileClean->signal_activate().connect(&do_notimplemented_message);

    // Not implemented menu item placeholder
    refGlade->get_widget("menu_do_autoclean", pMenuFileAutoclean);
    pMenuFileAutoclean->signal_activate().connect(&do_notimplemented_message);

    // Not implemented menu item placeholder
    refGlade->get_widget("menu_do_reload_cache", pMenuFileReloadCache);
    pMenuFileReloadCache->signal_activate().connect(&do_notimplemented_message);

    // Not implemented menu item placeholder
    refGlade->get_widget("menu_do_su_to_root", pMenuFileSuToRoot);
    pMenuFileSuToRoot->signal_activate().connect(&do_notimplemented_message);

    refGlade->get_widget("menu_do_quit", pMenuFileExit);
    pMenuFileExit->signal_activate().connect(&do_quit);

    refGlade->get_widget("menu_package", pMenuPackage);
    fill_package_menu(std::set<PackagesAction>(), sigc::slot1<void, PackagesAction>(),
		      pMenuPackage);

    {
      Gtk::MenuItem *menu_view_apt_errors;
      refGlade->get_widget("menu_view_apt_errors", menu_view_apt_errors);
      menu_view_apt_errors->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::show_apt_errors));
    }

    {
      Gtk::MenuItem *menu_view_dependency_chains;
      refGlade->get_widget("menu_item_find_dependency_paths", menu_view_dependency_chains);
      menu_view_dependency_chains->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::show_dependency_chains_tab));
    }

    refGlade->get_widget("menu_tab_previous", pMenuTabPrevious);
    pMenuTabPrevious->signal_activate().connect(sigc::mem_fun(pNotebook, &TabsManager::prev_page));

    refGlade->get_widget("menu_tab_next", pMenuTabNext);
    pMenuTabNext->signal_activate().connect(sigc::mem_fun(pNotebook, &TabsManager::next_page));

    // Tab closes are handled via post_event to ensure that tabs
    // aren't destroyed while we're still running methods on them.
    refGlade->get_widget("menu_tab_close", pMenuTabClose);
    sigc::slot<void> maybe_close_slot(sigc::mem_fun(pNotebook, &TabsManager::maybe_close_current_page));
    pMenuTabClose->signal_activate().connect(sigc::bind(sigc::ptr_fun(&post_event),
							make_safe_slot(maybe_close_slot)));

    refGlade->get_widget("menu_undo_undo", menu_undo_undo);
    menu_undo_undo->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::do_undo));

    refGlade->get_widget("menu_view_edit_columns", menu_view_edit_columns);
    menu_view_edit_columns->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::do_edit_columns));

    refGlade->get_widget_derived("main_notify_rows", pNotifyView);

    pNotifyView->add_notification(Gtk::manage(new BrokenPackagesNotification(this)));
    pNotifyView->add_notification(Gtk::manage(new NotificationInstallRemove(this)));

    refGlade->get_widget("main_progressbar", pProgressBar);
    refGlade->get_widget("main_statusbar", pStatusBar);
    pStatusBar->push("Aptitude-gtk v2", 0);

    activeErrorTab = NULL;
    errorStore.error_added.connect(sigc::mem_fun(*this, &AptitudeWindow::show_apt_errors));
    if(!errorStore.get_model()->children().empty())
      {
	// Show the apt error tab in the idle callback so we don't
	// kill ourselves.  The problem is that the global pointer to
	// this window isn't set up yet and show_apt_errors() expects
	// to be able to find it (ew).
	Glib::signal_idle().connect(sigc::bind_return(sigc::mem_fun(*this, &AptitudeWindow::show_apt_errors),
						      false));
      }

    // We need to be shown before we become not-sensitive, or GDK gets
    // cranky and spits out warnings.
    show();

    // Use a big global lock to keep the user from shooting themselves
    // while the cache is loading.
    if(!apt_cache_file)
      set_sensitive(false);
    cache_closed.connect(sigc::bind(sigc::mem_fun(*this, &Gtk::Widget::set_sensitive),
				    false));
    cache_reloaded.connect(sigc::bind(sigc::mem_fun(*this, &Gtk::Widget::set_sensitive),
				      true));

    // When the cache is reloaded, attach to the new resolver-manager.
    cache_reloaded.connect(sigc::mem_fun(*this, &AptitudeWindow::update_resolver_sensitivity_callback));
    update_resolver_sensitivity_callback();

    pNotebook->package_menu_actions_changed.connect(sigc::bind(sigc::ptr_fun(&AptitudeWindow::update_package_menu),
							       sigc::ref(*pNotebook),
							       sigc::ref(*pMenuPackage)));
    pNotebook->undo_available_changed.connect(sigc::bind(sigc::ptr_fun(&AptitudeWindow::update_undo_sensitivity),
							 sigc::ref(*pNotebook),
							 sigc::ref(*menu_undo_undo)));
    pNotebook->edit_columns_available_changed.connect(sigc::bind(sigc::ptr_fun(&AptitudeWindow::update_edit_columns_sensitivity),
								 sigc::ref(*pNotebook),
								 sigc::ref(*menu_view_edit_columns)));
  }

  void AptitudeWindow::do_dashboard()
  {
    tab_add(new DashboardTab(_("Dashboard")));
  }

  void AptitudeWindow::update_package_menu(TabsManager &notebook, Gtk::Menu &menu)
  {
    Tab *tab = notebook.get_current_tab();
    std::set<PackagesAction> actions;
    if(tab != NULL)
      actions = tab->get_package_menu_actions();

    menu.items().clear();
    fill_package_menu(actions,
		      sigc::mem_fun(*tab, &Tab::dispatch_package_menu_action),
		      &menu);
  }

  void AptitudeWindow::update_undo_sensitivity(TabsManager &notebook, Gtk::Widget &menu_undo_undo)
  {
    Tab *tab = notebook.get_current_tab();
    menu_undo_undo.property_sensitive() = tab != NULL && tab->get_undo_available();
  }

  void AptitudeWindow::do_undo()
  {
    Tab *tab = pNotebook->get_current_tab();
    if(tab != NULL)
      tab->dispatch_undo();
  }

  void AptitudeWindow::update_edit_columns_sensitivity(TabsManager &notebook, Gtk::Widget &menu_view_edit_columns)
  {
    Tab *tab = notebook.get_current_tab();
    menu_view_edit_columns.property_sensitive() = tab != NULL && tab->get_edit_columns_available();
  }

  void AptitudeWindow::do_edit_columns()
  {
    Tab *tab = pNotebook->get_current_tab();
    if(tab != NULL)
      tab->dispatch_edit_columns();
  }

  void AptitudeWindow::update_resolver_sensitivity_callback()
  {
    if(resman != NULL)
      resman->state_changed.connect(sigc::mem_fun(*this, &AptitudeWindow::update_resolver_sensitivity));

    update_resolver_sensitivity();
  }

  void AptitudeWindow::update_resolver_sensitivity()
  {
    bool resolver_exists = resman != NULL && resman->resolver_exists();

    pToolButtonResolver->set_sensitive(resolver_exists);
  }

  void AptitudeWindow::apt_error_tab_closed()
  {
    activeErrorTab = NULL;
  }

  void AptitudeWindow::show_apt_errors()
  {
    if(activeErrorTab != NULL)
      activeErrorTab->show();
    else
      {
	activeErrorTab = new ErrorTab("Errors", errorStore);
	activeErrorTab->closed.connect(sigc::mem_fun(this, &AptitudeWindow::apt_error_tab_closed));
	tab_add(activeErrorTab);
      }
  }

  void AptitudeWindow::show_dependency_chains_tab()
  {
    tab_add(new DependencyChainsTab("Find Dependency Chains"));
  }

  void init_glade(int argc, char *argv[])
  {
#ifndef DISABLE_PRIVATE_GLADE_FILE
    {
      // Use the basename of argv0 to find the Glade file.
      std::string argv0(argv[0]);
      std::string argv0_path;
      std::string::size_type last_slash = argv0.rfind('/');
      if(last_slash != std::string::npos)
	{
	  while(last_slash > 0 && argv0[last_slash - 1] == '/')
	    --last_slash;
	  argv0_path = std::string(argv0, 0, last_slash);
	}
      else
	argv0_path = '.';

      glade_main_file = argv0_path + "/gtk/aptitude.glade";

      //Loading the .glade file and widgets
      try
	{
	  refXml = Gnome::Glade::Xml::create(glade_main_file);
	}
      catch(Gnome::Glade::XmlError &)
	{
	}
    }
#endif

    if(!refXml)
      {
	glade_main_file = PKGDATADIR;
	glade_main_file += "/aptitude.glade";

	try
	  {
	    refXml = Gnome::Glade::Xml::create(glade_main_file);
	  }
	catch(Gnome::Glade::XmlError &)
	  {
	  }
      }
  }

  void init_style(void)
  {
    Gtk::RC::parse_string (
"style \"tiny-button-style\""
"{"
"  GtkWidget::focus-padding = 0"
"  xthickness = 0"
"  ythickness = 0"
"}"
"widget \"*.notebook_close_button\" style \"tiny-button-style\"");
  }

  namespace
  {
    void do_apt_init()
    {
      {
	cwidget::util::ref_ptr<guiOpProgress> p(guiOpProgress::create());
	apt_init(p.unsafe_get_ref(), true, NULL);
      }

      if(getuid() == 0 && aptcfg->FindB(PACKAGE "::Update-On-Startup", true))
	do_update();
    }
  }

  bool main(int argc, char *argv[])
  {
    // Don't crash if a subprocess breaks a pipe.
    signal(SIGPIPE, SIG_IGN);

    // GTK+ provides a perfectly good routine, gtk_init_check(), to
    // initialize GTK+ *and report whether the initialization
    // succeeded*.  gtkmm doesn't wrap it.  But initializing GTK+
    // twice won't hurt, so we do that.
    if(!gtk_init_check(&argc, &argv))
      return false;

    Glib::init();
    // If we don't check thread_supported() first, thread_init()
    // aborts with an error on some architectures. (see Debian bug
    // #555120)
    if(!Glib::thread_supported())
      Glib::thread_init();

    background_events_dispatcher.connect(sigc::ptr_fun(&run_background_events));

    pKit = new Gtk::Main(argc, argv);
    Gtk::Main::signal_quit().connect(&do_want_quit);
    init_glade(argc, argv);

    if(!refXml)
      {
	_error->Error(_("Unable to load the user interface definition file %s/aptitude.glade."),
		      PKGDATADIR);

	delete pKit;

	return false;
      }

    // Set up the style for GTK+ widgets.
    init_style();

    // Set up the resolver-triggering signals.
    init_resolver();

    // Postpone apt_init until we enter the main loop, so we get a GUI
    // progress bar.
    Glib::signal_idle().connect(sigc::bind_return(sigc::ptr_fun(&do_apt_init),
						  false));

    refXml->get_widget_derived("main_window", pMainWindow);

    pMainWindow->tab_add(new DashboardTab(_("Dashboard")));


    //This is the loop
    Gtk::Main::run(*pMainWindow);

    delete pMainWindow;
    delete pKit;

    return true;
  }
}
