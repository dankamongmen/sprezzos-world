// notify.h             -*-c++-*-
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

#ifndef NOTIFY_H_
#define NOTIFY_H_

#undef OK
#include <gtkmm.h>

#include <gtk/tab.h>

#include <cwidget/generic/util/bool_accumulate.h>

namespace gui
{
  /** \brief Basic notification class.
   *
   *  \note This derives from EventBox so that it can support changing
   *  the color of the widget (you need a GdkWindow to do that).
   *
   *  \todo The setup procedure could be more robust; right now we
   *  depend on the user invoking methods in the "right order".
   *  Ideally we would just take some collections as arguments to the
   *  constructor, eliminating all the "create an object and twiddle
   *  it" stuff.
   */
  class Notification : public Gtk::EventBox
  {
    private:
      bool onetimeuse;

      Gtk::HBox *hbox;
      Gtk::TextView * textview;
      Gtk::Widget * imageWidget;

      void init(const Glib::ustring &text, bool onetimeuse);
    public:
      /** \brief Create a notification.
       *
       * \param onetimeuse      If true, the notification is deleted when
       *                        the user clicks the close button; otherwise
       *                        it is only hidden.
       */
      Notification(bool onetimeuse);

      /** \brief Create a notification with the given text and no buttons. */
      Notification(const Glib::ustring &text, bool onetimeuse);
      bool is_onetimeuse() { return onetimeuse; };
      void add_button(Gtk::Button *);
      /** \brief Set the widget displayed as an image.
       *
       *  \param image    The widget to display as an image, or
       *                  NULL to destroy the existing image and
       *                  hide it.
       */
      void set_image(Gtk::Widget *image);
      /** \brief Set the text displayed in this notification.
       *
       *  \param buffer   The text buffer to display; if it is
       *                  empty, the text view contained in this
       *                  notification will be completely hidden.
       */
      void set_buffer(const Glib::RefPtr<Gtk::TextBuffer> &buffer);
      /** \brief Add an arbitrary widget to the notification, to the
       *  left of the text.
       *
       *  \param w The widget to add; will be set to managed mode so
       *  it gets destroyed when the notification is destroyed.
       */
      void prepend_widget(Gtk::Widget *w);
      /** \brief Change the background color of this notification. */
      void set_color(const Gdk::Color &color);
      /** \brief Add the elements of the notification that should appear to the
       *  right-hand side of the text it displays.
       *
       *  For instance, this adds the close button.  finalize() should
       *  be invoked after all the desired widgets and buttons have
       *  been added to the notification.
       */
      void finalize();

    /** \brief A signal emitted when the "close" button is clicked.
     *  Used internally to signal NotifyView that this notification
     *  was closed.
     */
  private:
    sigc::signal<void> close_clicked;
    friend class NotifyView;

  public:
    /** \brief A signal emitted when the user has asked to close the
     *  notification.
     *
     *  The return value will be \b true if every object connected to
     *  the signal returns \b true.  If the signal returns \b false
     *  (that is, if any object returns \b false), then the
     *  notification will not be closed.
     *
     *  For multi-use notifications, this signal is emitted when the
     *  notification is hiding; for single-use notification, it is
     *  emitted when the notification is being destroyed.
     */
    sigc::signal0<bool, cwidget::util::accumulate_and> closing;

    /** \brief A signal emitted when the notification is going to
     *  close.
     *
     *  For multi-use notifications, this signal is emitted when the
     *  notification is hiding; for single-use notification, it is
     *  emitted when the notification is being destroyed.
     */
    sigc::signal<void> closed;

    /** \brief Try to close this notification, exactly as if the user
     *  had clicked the "close" button.
     *
     *  This will invoke "closing" first to see whether it's OK to
     *  close the notification.
     */
    void close();
  };

  /** \brief Stores a stack of global and tab-local notifications.
   *
   *  Each notification is just a bunch of widgets packed into a box.
   */
  class NotifyView : public Gtk::VBox
  {
    private:
      Gtk::VBox * rows;

    void notification_close_clicked(Notification &notification);
    public:
      NotifyView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      ~NotifyView();
      /** \brief Add a new notification.
       *
       *  \param notification   The notification to add.
       *  The NotifyView will take ownership of this pointer.
       */
      void add_notification(Notification * notification);
      /** \brief Remove a notification.
       *
       *  \param notification   The notification to remove.
       */
      void remove_notification(Notification * notification);
      Gtk::VBox * get_rows() { return rows; };
  };

}

#endif /* NOTIFY_H_ */
