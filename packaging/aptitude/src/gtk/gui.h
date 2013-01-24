// -*-c++-*-

// gui.h
//
//  Copyright 1999-2009 Daniel Burrows
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

#ifndef GUI_H_
#define GUI_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <set>

#include <sigc++/slot.h>

#include <boost/shared_ptr.hpp>

#include <apt-pkg/pkgcache.h>

#include <generic/util/safe_slot.h>

#include <gtk/errortab.h>
#include <gtk/notify.h>

#include "constants.h"

class download_manager;

namespace gui
{
  // Local forward declarations:
  class AptitudeWindow;
  class TabsManager;
  class Tab;

  //This is a list of global and unique base widgets and other related stuff
  extern AptitudeWindow * pMainWindow;
  extern Glib::RefPtr<Gnome::Glade::Xml> refXml;
  extern std::string glade_main_file;
  extern bool want_to_quit;

  /** \brief Used to control how the main download progress bar is
   *  displayed.
   *
   *  This lets us adapt to the vagaries of the various download
   *  processes.
   */
  enum download_progress_mode
    {
      /** \brief Only pulse the download bar. */
      download_progress_pulse,

      /** \brief Display the progress based only on the number of
       *  items downloaded.
       */
      download_progress_item_count,

      /** \brief Display progress based on the number of bytes
       *  downloaded.
       */
      download_progress_size
    };

  /** \brief Dispatch the given thunk to the main loop. */
  void post_event(const safe_slot0<void> &thunk);

  /** \brief Wrap the given thunk in a safe_slot and post it to the
   *  main loop.
   */
  void post_thunk(const sigc::slot<void> &thunk);

  void gtk_update();

  /** \brief Display a "Not implemented, yet." message. */
  void do_notimplemented_message();

  /** \brief Display a custom "Not implemented, yet." message. */
  void do_notimplemented_message_custom(Glib::ustring msg);

  /** \brief Invoke tab_add on pMainWindow. */
  void tab_add(Tab *tab);

  /** \brief Invoke tab_del on pMainWindow. */
  void tab_del(Tab *tab);

  /** \brief Insert the tags of the given package into a TextBuffer.
   *
   *  Each tag will be a link that pops up a new packages view.
   *
   *  \param buffer The buffer into which the tags should be inserted.
   *  \param pkg    The package whose tags should be displayed.
   *  \param headerTag  A text-tag used to display the header that
   *                    leads off the tag list.
   *
   *  \return an iterator pointing to the end of the newly inserted text.
   */
  Gtk::TextBuffer::iterator add_debtags(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					Gtk::TextBuffer::iterator where,
					const pkgCache::PkgIterator &pkg,
					const Glib::RefPtr<Gtk::TextBuffer::Tag> &headerTag);

  /**
   * This is the main Aptitude custom window widget.
   */
  class AptitudeWindow : public Gtk::Window
  {
    private:
      Gtk::ToolButton * pToolButtonDashboard;
      Gtk::ToolButton * pToolButtonUpdate;
      Gtk::ToolButton * pToolButtonPackages;
      Gtk::ToolButton * pToolButtonPreview;
      Gtk::ToolButton * pToolButtonResolver;
      Gtk::ToolButton * pToolButtonInstallRemove;

      Gtk::ImageMenuItem * pMenuFilePackageRun;
      Gtk::ImageMenuItem * pMenuFileUpdateLists;
      Gtk::ImageMenuItem * pMenuFileMarkUpgradable;
      Gtk::ImageMenuItem * pMenuFileForgetNew;
      Gtk::ImageMenuItem * pMenuFileKeepAll;
      Gtk::ImageMenuItem * pMenuFileClean;
      Gtk::ImageMenuItem * pMenuFileAutoclean;
      Gtk::ImageMenuItem * pMenuFileReloadCache;
      Gtk::ImageMenuItem * pMenuFileSuToRoot;
      Gtk::ImageMenuItem * pMenuFileExit;

      Gtk::ImageMenuItem * pMenuTabPrevious;
      Gtk::ImageMenuItem * pMenuTabNext;
      Gtk::ImageMenuItem * pMenuTabClose;

      Gtk::Menu * pMenuPackage;
      Gtk::MenuItem *menu_undo_undo;
      Gtk::MenuItem *menu_view_edit_columns;

      Gtk::ProgressBar * pProgressBar;
      Gtk::Statusbar * pStatusBar;
      NotifyView * pNotifyView;
      TabsManager * pNotebook;

      // The "global" singleton for storing apt errors.
      ErrorStore errorStore;

      // If this is not NULL, it points at the currently active
      // errors-view.
      ErrorTab *activeErrorTab;

      // Nulls out the error tab when it's closed.
      void apt_error_tab_closed();

      void show_dependency_chains_tab();

      // Register the resolver-sensitivity callback.
      void update_resolver_sensitivity_callback();
      // Update the resolver sensistivity.
      void update_resolver_sensitivity();

      /** \brief Update the Package menu by querying the currently
       *  visible tab.
       *
       *  \param notebook  The notebook whose tabs should be examined.
       *
       *  \param packageMenu  The menu to update.
       *
       *  The parameters are passed mostly to ensure that the signal
       *  auto-disconnects when the menu and/or notebook are
       *  destroyed.
       */
      static void update_package_menu(TabsManager &notebook, Gtk::Menu &package_menu);

      /** \brief Update whether the Undo menu item is sensitive. */
      static void update_undo_sensitivity(TabsManager &notebook, Gtk::Widget &menu_undo_undo);

      /** \brief Update whether the "Edit Columns..." menu item is sensitive. */
      static void update_edit_columns_sensitivity(TabsManager &notebook, Gtk::Widget &menu_view_edit_columns);

    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)/* : Gtk::Window(cobject)*/;

    /** \brief Show the apt errors tab.
     *
     *  \todo This manually handles finding the active tab if there is
     *  one; perhaps this logic should be part of the tab manager?
     */
    void show_apt_errors();

    Gtk::Menu * get_menu_package() const { return pMenuPackage; }
    Gtk::ProgressBar * get_progress_bar() const { return pProgressBar; }
    Gtk::Statusbar * get_status_bar() const { return pStatusBar; }
    NotifyView *get_notifyview() const { return pNotifyView; }
    TabsManager * get_notebook() const { return pNotebook; }

    /** \brief Add a tab to the main notebook of this window. */
    void tab_add(Tab *tab);

    /** \brief Remove a tab from the main notebook of this window. */
    void tab_del(Tab *tab);

    /** \brief Undo the last action in the visible tab, as if Undo had
     *  been triggered from the Edit menu.
     */
    void do_undo();

    /** \brief Edit the visible columns of the main package list in
     *  the current tab, as if "Edit Columns..." had been triggered
     *  from the View menu.
     */
    void do_edit_columns();

    /** \brief Open a resolver tab, as if it had been triggered from the menu.
     */
    void do_resolver();

    /** \brief Open a dashboard tab, as if it had been triggered by
     *  pushing the toolbar button.
     */
    void do_dashboard();

    /** \brief Open a preview tab, as if it had been triggered from the menu. */
    void do_preview();

    /** \brief Open a list of currently broken packages. */
    void do_show_broken();

    /** \brief Open a new Packages tab to search for the given pattern. */
    void add_packages_tab(const std::string &pattern);
  };


  /** \brief Start a new download, creating the appropriate GUI
   *  elements.
   *
   *  \param manager The download manager defining the download
   *                 process to run.
   *
   *  \param title   A string describing the download.
   *
   *  \param image   An image to show in the download notification,
   *                 or NULL for none.
   *
   *  \param download_progress  How to display the download progress.
   *
   *  \param view    The notification view in which to place the
   *                 notification corresponding to this download.
   *
   *  \param download_starts_slot   A slot to be invoked in the
   *                                foreground thread when the
   *                                download is about to start.
   *
   *  \param download_stops_slot    A slot to be invoked in the
   *                                background thread after the
   *                                download and post-download
   *                                actions complete.
   */
  void start_download(const boost::shared_ptr<download_manager> &manager,
		      const std::string &title,
		      Gtk::Widget *image,
		      download_progress_mode download_progress,
		      NotifyView *view,
		      const sigc::slot0<void> &download_starts_slot = sigc::slot0<void>(),
		      const sigc::slot0<void> &download_stops_slot = sigc::slot0<void>());


  /** \brief Add package actions to a menu.
   *
   *  \param actions    The actions to include in the menu.
   *  \param callback   The callback to invoke when the actions are selected.
   *  \param menu       The menu in which to place the actions.
   *
   *  The actions are added to the end of the menu in a standard
   *  order.
   */
  void fill_package_menu(const std::set<PackagesAction> &actions,
			 const sigc::slot1<void, PackagesAction> &callback,
			 Gtk::Menu * menu);

  /** \brief Try to start up the user interface.
   *
   *  \return \b true if the interface was successfully started.
   */
  bool main(int argc, char *argv[]);

  /** \brief Start an install/remove run, as if it had been triggered
   *  from the menu.
   */
  void do_installremove();

  /** \return \b true if an install/remove run can be started. */
  void do_installremove_allowed();

  /** \brief Start a list update, as if it had been triggered from the menu.
   */
  void do_update();

  /** \brief Schedule all possible upgrades, as if "Mark Upgradable"
   *  had been selected from the menu or toolbar.
   */
  void do_mark_upgradable();

  /** @{
   *  Constants giving the textual and iconic representation of the
   *  current and selected states of packages.
   */

  /** \brief Represents information about a state that the packag can be in. */
  class entity_state_info
  {
    std::string flag;
    std::string description;
    Gtk::StockID icon;

    // Hidden.
    entity_state_info();
  public:
    entity_state_info(const std::string &_flag,
		      const std::string &_description,
		      const Gtk::StockID &_icon)
      : flag(_flag), description(_description), icon(_icon)
    {
    }

    const std::string &get_flag() const { return flag; }
    /** \brief Retrieve the translated description of this state.
     */
    std::string get_description_i18n() const;
    const Gtk::StockID &get_icon() const { return icon; }
  };

  extern const entity_state_info virtual_columns;
  extern const entity_state_info not_installed_columns;
  extern const entity_state_info unpacked_columns;
  extern const entity_state_info half_configured_columns;
  extern const entity_state_info half_installed_columns;
  extern const entity_state_info config_files_columns;
  extern const entity_state_info triggers_awaited_columns;
  extern const entity_state_info triggers_pending_columns;
  extern const entity_state_info installed_columns;
  extern const entity_state_info error_columns;

  extern const entity_state_info install_columns;
  extern const entity_state_info reinstall_columns;
  extern const entity_state_info upgrade_columns;
  extern const entity_state_info downgrade_columns;
  extern const entity_state_info remove_columns;
  extern const entity_state_info purge_columns;
  extern const entity_state_info hold_columns;
  extern const entity_state_info forbid_columns;
  extern const entity_state_info broken_columns;
  extern const entity_state_info no_action_columns;

  extern const char *lightred_background_color;
  extern const char *lightgreen_background_color;

  /** @} */
}

#endif /*GUI_H_*/
