/** \file area.h */    // -*-c++-*-

// Copyright (C) 2009-2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef APTITUDE_GTK_TOPLEVEL_MODEL_H
#define APTITUDE_GTK_TOPLEVEL_MODEL_H

#include <generic/util/dynamic_set.h>
#include <generic/util/enumerator.h>
#include <generic/util/progress_info.h>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

#include <cwidget/generic/util/bool_accumulate.h>

#include <gtkmm/image.h>

#include <sigc++/slot.h>

// "areas" are the broad functional categories that tabs are grouped
// into.  Each area contains one or more "tabs" (views the user can
// select), along with one or more "notifications" (warnings for the
// user, or indications of the progress of an ongoing background
// operation).
//
// This file defines the abstractions used to describe the areas of
// all active tabs and ongoing notifications within each area.
//
// Areas aren't especially performance-critical, so I've hidden the
// implementations behind abstract interfaces in order to better
// insulate the rest of the code from the implementation details.
//
// Areas are not thread-safe: only the main GUI thread should read and
// write them.
//
// Below, the "view implementation" refers to the code that displays
// the areas, tabs and so on.
//
// Some views take ownership of the widgets of the tabs they display.
// Exactly one of these views should be attached to any given tab at
// any time.  These views are generally intended to be the "main" view
// of the program (e.g., tabs_notebook) and are responsible for
// destroying the tab's widget when it is removed from the area's set.
//
// \todo the above paragraph is a description of how things are, but
// it's a pretty hideous design.  Can we do better within the bounds
// of gtk--?

namespace gui
{
  namespace toplevel
  {
    class area_info;
    class notification_info;
    class tab_info;


    /** \brief The abstract description of a static collection of areas.
     *
     *  It would be possible, and attractive from one point of view, to
     *  use a TreeModel here.  It would also make life way harder for
     *  consumers of the class, given that the list of areas is static.
     *
     *  (arguably it's overkill to have a generic interface this, but
     *   OTOH it avoids trouble if we, e.g., move to a mutable area list
     *   and change the representation)
     */
    class area_list : public sigc::trackable
    {
    public:
      virtual ~area_list() {}

      virtual int get_size() = 0;

      typedef aptitude::util::enumerator<boost::shared_ptr<area_info> >
      areas_enumerator;

      /** \brief Enumerate the list areas contained in this list. */
      virtual boost::shared_ptr<areas_enumerator> get_areas() = 0;
    };

    /** \brief Create an immutable area list from an STL vector of
     *  areas.
     */
    boost::shared_ptr<area_list> create_area_list(const std::vector<boost::shared_ptr<area_info> > &areas);

    /** \brief The abstract description of an area. */
    class area_info : public sigc::trackable
    {
    public:
      virtual ~area_info() {}

      /** \brief Get the name of this area (e.g., "Upgrade" or
       *	"Search").
       */
      virtual std::string get_name() = 0;

      /** \brief Get a description of this area. */
      virtual std::string get_description() = 0;

      /** \brief Get the icon of this area. */
      virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;

    public:
      typedef aptitude::util::writable_dynamic_set<boost::shared_ptr<tab_info> >
      tabs_set;

      typedef aptitude::util::writable_dynamic_set<boost::shared_ptr<notification_info> >
      notifications_set;

      /** \brief Get the tabs associated with this area. */
      virtual boost::shared_ptr<tabs_set> get_tabs() = 0;
      /** \brief Get the notifications associated with this area. */
      virtual boost::shared_ptr<notifications_set> get_notifications() = 0;
    };

    boost::shared_ptr<area_info> create_area_info(const std::string &name,
                                                  const std::string &description,
                                                  const Glib::RefPtr<Gdk::Pixbuf> &icon);

    /** \brief The interface that both the view and the tab
     *  implementation require from a tab.
     */
    class tab_info_base : public sigc::trackable
    {
    public:
      /** \brief Indicate that the user wants to close a tab.
       *
       *  For instance, perhaps the "close" button was clicked, or the
       *  user said "yes" when asked whether to close the tab.  Causes
       *  the "request_close" signal to be emitted.
       */
      virtual void request_close() = 0;
    };

    /** \brief The interface the view requires from a tab.
     *
     *  Split out from tab_info for clarity, and to keep the view
     *  honest.
     */
    class tab_display_info : virtual public tab_info_base
    {
    public:
      virtual ~tab_display_info() {}

      /** \brief Get the name of this tab. */
      virtual std::string get_name() = 0;

      /** \brief Get tooltip information for this tab.
       *
       *  \param tooltip_text A location in which to store the text of the
       *                      tooltip.
       *  \param tooltip_window A location in which to store a pointer to
       *                        a window that should be used to display
       *                        the tooltip.
       *
       *  tooltip_text is empty if there is no text; tooltip_window is
       *  NULL if there is no window.
       */
      virtual void get_tooltip(std::string &tooltip_text,
                               Gtk::Window * &tooltip_window) = 0;

      /** \brief Get the icon of this tab. */
      virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;

      /** \brief Get any progress information associated with this tab. */
      virtual aptitude::util::progress_info get_progress() = 0;

      /** \brief Get the main widget of this tab.
       *
       *  Returns NULL if the widget has already been destroyed.
       */
      virtual Gtk::Widget *get_widget() = 0;

      /** \brief Set whether the tab is currently visible.
       *
       *  Invoked by the view when it changes the currently active tab,
       *  including for changes arising from the "activate_tab" signal.
       */
      virtual void set_active(bool visible) = 0;

      /** \brief Set the widget to NULL to indicate that it has been
       *  destroyed.
       *
       *  The caller is responsible for actually destroying it.
       */
      virtual void reset_widget() = 0;

      /** \brief Forcibly close a tab.
       *
       *  This bypasses the first-chance check that first_close provides
       *  and is provided only as a way for the view to destroy itself.
       */
      virtual void force_close() = 0;

      /** \brief Signals */
      // @{

      /** \brief Register a slot to be invoked when the tooltip
       *  information changes.
       */
      virtual sigc::connection
      connect_tooltip_changed(const sigc::slot<void, boost::shared_ptr<tab_info>, std::string, Gtk::Window *> &
                              slot) = 0;

      /** \brief Register a slot to be invoked when the progress
       *  information changes.
       */
      virtual sigc::connection
      connect_progress_changed(const sigc::slot<void, boost::shared_ptr<tab_info>, aptitude::util::progress_info> &
                               slot) = 0;

      /** \brief Register a slot to be invoked when something asks for a
       *  tab to be made the currently visible tab by invoking activate().
       */
      virtual sigc::connection
      connect_activate_tab(const sigc::slot<void, boost::shared_ptr<tab_info> > &slot) = 0;

      // @}
    };

    /** \brief The tab information interface exposed to implementations. */
    class tab_controller_info : virtual public tab_info_base
    {
    public:
      virtual ~tab_controller_info() {}

      /** \brief Set the tooltip text for this tab.
       *
       *  Automatically deletes and clears the tooltip window.
       */
      virtual void set_tooltip(const std::string &tooltip_text) = 0;

      /** \brief Set the tooltip window for this tab.
       *
       *  As a side effect, deletes any old tooltip window and clears
       *  the old text.
       */
      virtual void set_tooltip(Gtk::Window *tooltip_window) = 0;

      /** \brief Update the progress information associated with this tab.
       *
       *  Invokes signal_progress_changed() as a side-effect.
       */
      virtual void set_progress(const aptitude::util::progress_info &info) = 0;


      /** \return \b true if this tab is currently visible.
       *
       *  This property is maintained by the view and read by the tab's
       *  implementation.  Note that it is \e not automatically
       *  maintained by set_active_tab(), because that routine lacks the
       *  information necessary to determine which tab is visible;
       *  normally an implementation of the view will hook into
       *  signal_active_tab_changed and emit this as appropriate.
       */
      virtual bool get_active() = 0;

      /** \brief Request that this tab be made the currently active tab.
       *
       *  Causes the activate_tab signal to be emitted.
       */
      virtual void activate() = 0;

      /** \brief Add the given tab to the same area as this tab.
       *
       *  If no parent area is stored, logs an error and does nothing.
       */
      virtual void add_sibling(const boost::shared_ptr<tab_info> &sibling) = 0;

      /** \brief Signals */
      // @{

      /** \brief Register a slot to be invoked when the tab becomes
       *  active or inactive.
       */
      virtual sigc::connection
      connect_active_changed(const sigc::slot<void, bool> &
                             slot) = 0;

      /** \brief Register a slot to be invoked when the tab is going to
       *  be closed.
       *
       *  If any slot connected to request_close returns "false", then
       *  the close is canceled.  Otherwise (e.g., if nothing attaches
       *  here), signal_closed will be invoked; that signal should
       *  remove the tab from its enclosing area, which in turn will
       *  cause views to remove it.  Once that happens, the widget
       *  associated with the tab is most likely destroyed.
       */
      virtual sigc::connection
      connect_request_close(const sigc::slot<bool> &slot) = 0;

      // @}
    };

    /** \brief The abstract description of a tab.
     */
    class tab_info : public tab_display_info, public tab_controller_info
    {
    public:
      /** \brief Register a slot to be invoked when the tab is to be
       *  closed.
       *
       *  The code that manages the set of tabs should catch this and
       *  remove the tab from the set.  That removal is the signal to
       *  other parts of the code that the tab is officially dead.  The
       *  set removal is used rather than using this signal to kill the
       *  tab because while both options have issues, this one seems
       *  conceptually cleaner to me and a bit less error-prone.
       */
      virtual sigc::connection
      connect_closed(const sigc::slot<void, boost::shared_ptr<tab_info> > &slot) = 0;

      /** \brief Set the area that this tab is stored in.
       *
       *  Should only be invoked by area_info.  If the area is set to
       *  a non-NULL value when it already has a non-NULL value, the
       *  new setting is discarded and an error is logged.
       */
      virtual void set_parent_area(const boost::shared_ptr<area_info> &area) = 0;
    };

    /** \brief Create a new tab_info.
     *
     *  \param name   The name to display in the tab.
     *  \param icon   The icon to display for the tab.
     *  \param widget The main widget of the tab.
     */
    boost::shared_ptr<tab_info> create_tab(const std::string &name,
                                           const Glib::RefPtr<Gdk::Pixbuf> &icon,
                                           Gtk::Widget *widget);

    /** \brief The abstract description of a notification. */
    class notification_info : public sigc::trackable
    {
    public:
      virtual ~notification_info() {}

      /** \brief Get the name of this notification. */
      virtual std::string get_name() = 0;

      /** \brief Get a longer description of this notification. */
      virtual std::string get_description() = 0;

      /** \brief Get the icon of this notification. */
      virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;


      /** \brief Retrieve the progress display associated with this
       *  notification.
       */
      virtual aptitude::util::progress_info get_progress() = 0;

      /** \brief Update the progress display associated with this
       *	notification.
       *
       *  Invokes signal_progress_changed as a side-effect.
       */
      virtual void set_progress(const aptitude::util::progress_info &progress) = 0;


      /** \brief Signals */

      // @{

      /** \brief Register a slot to be invoked when the progress
       *  information changes.
       */
      virtual sigc::connection
      connect_progress_changed(const sigc::slot<void, aptitude::util::progress_info> &slot) = 0;

      /** \brief Register a slot to be invoked when the user clicks the
       *  notification.
       */
      virtual sigc::connection
      connect_clicked(const sigc::slot<void> &slot) = 0;

      // @}
    };

    boost::shared_ptr<notification_info>
    create_notification(const std::string &name,
                        const std::string &description,
                        const Glib::RefPtr<Gdk::Pixbuf> &icon);
  }
}

#endif // APTITUDE_GTK_TOPLEVEL_MODEL_H

