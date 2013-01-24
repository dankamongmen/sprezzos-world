// ui.h    -*-c++-*-
//
//  Copyright 2000,2001 Daniel Burrows
//

#ifndef UI_H
#define UI_H

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>

#include <string>

#include <cwidget/generic/util/bool_accumulate.h>
#include <cwidget/generic/util/slotarg.h>

#include <cwidget/widgets/editline.h> // Included for history support.

/** \brief Global UI definitions and routines
 *
 * 
 *  Global UI definitions and routines.  A lot of things here just provide a
 *  single point for common actions so those actions can be customized later.
 *  (eg, the progress-bar can appear in various forms)
 * 
 *  \file ui.h
 */

class OpProgress;

class download_signal_log;
namespace cwidget
{
  class fragment;
}
class pkg_hier;
namespace cwidget
{
  namespace util
  {
    template<class T> class ref_ptr;
  }
  namespace widgets
  {
    class widget;

    typedef util::ref_ptr<widget> widget_ref;
  }
}

class download_list;
typedef cwidget::util::ref_ptr<download_list> download_list_ref;

class progress;
typedef cwidget::util::ref_ptr<progress> progress_ref;
/******************************************************************************
 * Global signals:
 *****************************************************************************/

// File menu
extern sigc::signal0<void> file_quit;

/* A signal that widgets watching package states should update themselves.
 *
 * This should be triggered at the end of any keystroke (or mouse-stroke)
 * handler that alters the package states.  At a minimum, it will
 * call vscreen_update().
 *
 * \todo this seems to be at least partly redundant with the similar
 * signal on the apt cache file.  Should one of them be removed or
 * scaled back?
 */
extern sigc::signal0<void> package_states_changed;

/** Tests whether Undo -> Undo is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> undo_undo_enabled;

/** Emitted for Undo -> Undo. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> undo_undo;

/** Used to determine whether a target is available for the "package actions".
 */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_menu_enabled;

/** Emitted for Package -> Install. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_install;

/** Emitted for Package -> Remove. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_remove;

/** Emitted for Package -> Purge. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_purge;

/** Emitted for Package -> Hold. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_hold;

/** Emitted for Package -> Keep. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_keep;

/** Emitted for Package -> Mark Auto */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_mark_auto;

/** Emitted for Package -> Mark Manual */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_unmark_auto;

/** Tests whether Package -> Forbid Version is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_forbid_enabled;

/** Emitted for Package -> Forbid. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_forbid;

/** Tests whether Package -> Package Information is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_information_enabled;

/** Emitted for Package -> Package Information. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_information;

/** Tests whether Package -> Cycle Information is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_cycle_information_enabled;

/** Emitted for Package -> Cycle Information. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_cycle_information;

/** Tests whether Package -> Changelog is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_changelog_enabled;

/** Emitted for Package -> Changelog. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> package_changelog;


/** Emitted for Resolver -> Toggle Rejected. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_toggle_rejected;

/** Tests whether Resolver -> Reject/Unreject Action is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_toggle_rejected_enabled;

/** Emitted for Resolver -> Toggle Approved. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_toggle_approved;

/** Tests whether Resolver -> Toggle Approved is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_toggle_approved_enabled;
/** Emitted for Resolver -> View Target */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_view_target;

/** Tests whether Resolver -> View Target is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> resolver_view_target_enabled;


/** Tests whether Search -> Find is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_search_enabled;

/** Tests whether Search -> Find Backwards is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_search_back_enabled;

/** Tests whether Search -> Find Again is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_research_enabled;

/** Tests whether Search -> Find Again Reverse is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_repeat_search_back_enabled;

/** Tests whether Search -> Limit is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_limit_enabled;

/** Tests whether Search -> Cancel Limit is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_cancel_limit_enabled;

/** Tests whether Searc -> Find Broken is enabled. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_broken_enabled;

/** Emitted for Search -> Find. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_search;

/** Emitted for Search -> Find Backwards. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_search_back;

/** Emitted for Search -> Find Again. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_research;

/** Emitted for Search -> Find Again Reverse. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_repeat_search_back;

/** Emitted for Search -> Limit. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_limit;

/** Emitted for Search -> Cancel Limit. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_cancel_limit;

/** Emitted for Search -> Find Broken. */
extern sigc::signal0<bool, cwidget::util::accumulate_or> find_broken;

/*****************************************************************************/

/** \brief Emitted when an install completes.  The parameter is \b
 *  true if the install succeeded and \b false otherwise.
 */
extern sigc::signal1<void, bool> install_finished;

/** \brief Emitted when a list update completes.  The parameter is \b
 *  true if the update succeeded and \b false otherwise.
 */
extern sigc::signal1<void, bool> update_finished;

void ui_init();
void ui_main();

// Displays a "popup" widget.  If the second argument is false, show_all
// will not be called on the widget.
void popup_widget(const cwidget::widgets::widget_ref &w, bool do_show_all=true);

// Displays a widget on the "main" program level, inserting the given string
// into the menu listing available "main" screens to reference it.
//
// The "insert" form adds the new widget directly after the currently visible
// widget.

/** Add a new widget to the main interaction area.
 *
 *  \param w the widget to add
 *  \param menuref the text to add in the corresponding menu
 *  \param menudesc the description of the generated menu item
 *  \param tablabel the label of the corresponding tab
 */
void add_main_widget(const cwidget::widgets::widget_ref &w,
		     const std::string &menuref,
		     const std::string &menudesc,
		     const std::string &tablabel);
void insert_main_widget(const cwidget::widgets::widget_ref &w,
			const std::string &menuref,
			const std::string &menudesc,
			const std::string &tablabel);

/** Returns the currently active main widget. */
cwidget::widgets::widget_ref active_main_widget();

// Generates a progress bar.
progress_ref gen_progress_bar();

// Generates an appropriate box to wrap text.
cwidget::fragment *wrapbox(cwidget::fragment *contents);

/** Generates a download progress object based on the user's current
 *  settings.
 *
 *  \param force_noninvasive if \b true, the generated UI object will
 *            always be "noninvasive" (typically a bar at the bottom of
 *            the screen).
 *  \param list_update       if \b true, this bar is for a list update
 *                           (meaning that we have to deal with apt's
 *                           utterly useless progress indication in this
 *                           case)
 *  \param title             if a new view is generated, this string is
 *                           used as its title; it will be cwidget::util::transcoded.
 *  \param longtitle         if a new view is generated, this string is
 *                           used as its long title; it will be cwidget::util::transcoded.
 *
 *  \return the new download manager and the download status widget.
 */
std::pair<download_signal_log *,
	  download_list_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const std::string &title,
		      const std::string &longtitle,
		      const std::string &tablabel);

// Asks the user for simple input (the question will appear in a "minibuffer"
// or in a dialog according to preferences)

void prompt_string(const std::string &prompt,
		   const std::string &text,
		   cwidget::util::slotarg<sigc::slot1<void, std::wstring> > slot,
		   cwidget::util::slotarg<sigc::slot0<void> > cancel_slot,
		   cwidget::util::slotarg<sigc::slot1<void, std::wstring> > changed_slot,
		   cwidget::widgets::editline::history_list *history);

void prompt_string(const std::wstring &prompt,
		   const std::wstring &text,
		   cwidget::util::slotarg<sigc::slot1<void, std::wstring> > slot,
		   cwidget::util::slotarg<sigc::slot0<void> > cancel_slot,
		   cwidget::util::slotarg<sigc::slot1<void, std::wstring> > changed_slot,
		   cwidget::widgets::editline::history_list *history);

void prompt_yesno(const std::string &prompt,
		  bool deflt,
		  cwidget::util::slot0arg yesslot,
		  cwidget::util::slot0arg noslot);

/** Display a popup dialog for a yes-no prompt.  Meant for prompts
 *  with large quantities of text.
 */
void prompt_yesno_popup(cwidget::fragment *f,
			bool deflt,
			cwidget::util::slot0arg yesslot,
			cwidget::util::slot0arg noslot);

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal).  For convenience, a
 *  wrapbox is implicitly placed around the message.
 *
 *  \param msg the message to display
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(cwidget::fragment *msg,
		  cwidget::util::slot0arg okslot,
		  const cwidget::style &st = cwidget::style_attrs_flip(A_REVERSE));

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal); it will be
 *  paragraph-wrapped as necessary.
 *
 *  \param msg the message to display
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(const std::wstring &msg,
		  cwidget::util::slot0arg okslot=NULL,
		  const cwidget::style &st = cwidget::style_attrs_flip(A_REVERSE));

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal); it will be
 *  paragraph-wrapped as necessary.
 *
 *  \param msg the message to display; will be cwidget::util::transcoded
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(const std::string &msg,
		  cwidget::util::slot0arg okslot=NULL,
		  const cwidget::style &st = cwidget::style_attrs_flip(A_REVERSE));

/** Call this when a download starts; it sets the flag associated with
 *  the existence of a download, destroys the active preview (if any),
 *  and maeks the apt cache read-only.
 *
 *  \param hide_preview if \b true, the preview screen will be hidden
 *  prior to starting the download.
 */
void ui_start_download(bool hide_preview = true);

/** Call this when a download finishes; it clears the flag associated
 *  with the existence of a download and puts the cache back into
 *  read-write mode.
 */
void ui_stop_download();

// Can be used to manually pop up an error dialog, if necessary.
void check_apt_errors();

/** Display the solution editor screen if it isn't visible. */
void do_examine_solution();

void do_new_package_view(OpProgress &progress);
// Displays a new package-view.

void do_new_flat_view(OpProgress &progress);
// Displays a new flat-view.

void do_package_run_or_show_preview();
// Shows a preview if previews are enabled (and why would you disable them?),
// otherwise does the same thing as install_or_remove_packages.

void install_or_remove_packages();
// Installs or removes packages.  (the thing that happens after you press
// "g" twice)

void do_update_lists();
// Updates the package lists.

/** \brief Delete obsolete downloaded files. */
void do_autoclean();

/** \brief Clean the package cache. */
void do_clean();

void do_forget_new();
// Forgets which packages are "new".

/** Advances to the next solution, if one exists. */
void do_next_solution();

/** Returns to the previous solution, if one exists. */
void do_previous_solution();

/** Applies the current solution, if it exists. */
void do_apply_solution();

// These generate particular screens of the UI:

// Info screen
cwidget::widgets::widget_ref make_info_screen(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver);
void show_info_screen(const pkgCache::PkgIterator &pkg,
		      const pkgCache::VerIterator &ver);
// Dependency screen
cwidget::widgets::widget_ref make_dep_screen(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      bool reverse=false);
void show_dep_screen(const pkgCache::PkgIterator &pkg,
		     const pkgCache::VerIterator &ver,
		     bool reverse=false);
// List of versions
cwidget::widgets::widget_ref make_ver_screen(const pkgCache::PkgIterator &pkg);
void show_ver_screen(const pkgCache::PkgIterator &pkg);

// Various defaults:
extern const char *default_pkgstatusdisplay;
extern const char *default_pkgheaderdisplay;
extern const char *default_grpstr;
extern const char *confirm_delete_essential_str;

#endif
