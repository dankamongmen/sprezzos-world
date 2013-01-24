// -*-c++-*-

// notify.h
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

#include "notify.h"
#include "aptitude.h"

namespace gui
{
  void Notification::init(const Glib::ustring &text, bool onetimeuse)
  {
    this->onetimeuse = onetimeuse;
    Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();
    buffer->set_text(text);
    hbox = manage(new Gtk::HBox());
    textview = manage(new Gtk::TextView(buffer));
    add(*hbox);
    hbox->show();
    hbox->pack_start(*textview, true, true);
  }

  Notification::Notification(bool onetimeuse)
    : imageWidget(NULL)
  {
    init("", onetimeuse);
  }

  Notification::Notification(const Glib::ustring &text, bool onetimeuse)
    : imageWidget(NULL)
  {
    init(text, onetimeuse);
    finalize();
  }

  void Notification::close()
  {
    close_clicked();
  }

  void Notification::set_image(Gtk::Widget *image)
  {
    delete imageWidget;
    imageWidget = image;

    if(image != NULL)
      {
	hbox->pack_start(*image, false, false);
	hbox->reorder_child(*image, 0);
	image->show();
      }
  }

  void Notification::add_button(Gtk::Button * button)
  {
    button->show();
    hbox->pack_start(*manage(button), false, true);
  }

  void Notification::prepend_widget(Gtk::Widget * widget)
  {
    widget->show();
    hbox->pack_start(*manage(widget));
  }

  void Notification::finalize()
  {
    if(textview->get_buffer()->size() > 0)
      textview->show();
    Gtk::Button * close_button = manage(new Gtk::Button());
    Gtk::Image * close_button_image = manage(new Gtk::Image(Gtk::Stock::CLOSE, Gtk::ICON_SIZE_MENU));
    close_button->property_image() = close_button_image;
    close_button->signal_clicked().connect(close_clicked);
    close_button->show();
    hbox->pack_start(*close_button, false, true);
  }

  void Notification::set_buffer(const Glib::RefPtr<Gtk::TextBuffer> &buffer)
  {
    textview->set_buffer(buffer);
    textview->property_visible() = buffer->size() > 0;
  }

  void Notification::set_color(const Gdk::Color &color)
  {
    modify_base(Gtk::STATE_NORMAL, color);
    textview->modify_base(Gtk::STATE_NORMAL, color);
    if(imageWidget != NULL)
      imageWidget->modify_base(Gtk::STATE_NORMAL, color);

    modify_bg(Gtk::STATE_NORMAL, color);
    textview->modify_bg(Gtk::STATE_NORMAL, color);
    if(imageWidget != NULL)
      imageWidget->modify_bg(Gtk::STATE_NORMAL, color);
  }

  NotifyView::NotifyView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::VBox(cobject)
  {
    refGlade->get_widget("main_notify_rows", rows);
  }

  NotifyView::~NotifyView()
  {
    // TODO Auto-generated destructor stub
  }

  void NotifyView::add_notification(Notification * notification)
  {
    notification->close_clicked.connect(sigc::bind(sigc::mem_fun(*this, &NotifyView::notification_close_clicked), sigc::ref(*notification)));
    rows->pack_start(*notification);
  }

  void NotifyView::notification_close_clicked(Notification & notification)
  {
    if(notification.closing())
      remove_notification(&notification);
  }

  void NotifyView::remove_notification(Notification * notification)
  {
    notification->closed();
    if (notification->is_onetimeuse())
      {
        remove(*notification);
      }
    else
      {
        notification->hide();
      }
  }

}
