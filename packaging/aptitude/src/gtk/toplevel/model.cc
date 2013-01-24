/** \file area.cc */

// Copyright (C) 2009-2010 Daniel Burrows

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

#include "model.h"

#include <loggers.h>

#include <cwidget/generic/util/bool_accumulate.h>

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/weak_ptr.hpp>

#include <generic/util/dynamic_set_impl.h>

#include <gtkmm/window.h>

using aptitude::Loggers;
using aptitude::util::dynamic_set;
using aptitude::util::dynamic_set_impl;
using aptitude::util::enumerator;
using aptitude::util::iterator_enumerator_with_keepalive;
using aptitude::util::progress_info;
using aptitude::util::writable_dynamic_set;

namespace gui
{
  namespace toplevel
  {
    // None of the connect_* methods in this file should be given slots
    // with outgoing strong references: doing so will likely result in a
    // reference loop.


    /** \brief Implements the static area list. */
    class area_list_impl : public area_list, public boost::enable_shared_from_this<area_list_impl>
    {
      std::vector<boost::shared_ptr<area_info> > areas;

    public:
      area_list_impl(const std::vector<boost::shared_ptr<area_info> > &_areas)
        : areas(_areas)
      {
      }

      int get_size()
      {
        return static_cast<int>(areas.size());
      }

      typedef enumerator<boost::shared_ptr<area_info> >
      area_enumerator;

      boost::shared_ptr<area_enumerator> get_areas();
    };

    boost::shared_ptr<area_list_impl::area_enumerator>
    area_list_impl::get_areas()
    {
      typedef iterator_enumerator_with_keepalive<std::vector<
      boost::shared_ptr<area_info> >::const_iterator, area_list_impl>
        real_area_enumerator;

      return boost::make_shared<real_area_enumerator>(areas.begin(), areas.end(), shared_from_this());
    }


    boost::shared_ptr<area_list> create_area_list(const std::vector<boost::shared_ptr<area_info> > &areas)
    {
      return boost::make_shared<area_list_impl>(areas);
    }

    class area_info_impl : public area_info, public boost::enable_shared_from_this<area_info_impl>
    {
      std::string name;
      std::string description;
      Glib::RefPtr<Gdk::Pixbuf> icon;

      typedef dynamic_set_impl<boost::shared_ptr<tab_info> > tabs_set_impl;
      typedef dynamic_set_impl<boost::shared_ptr<notification_info> > notifications_set_impl;

      // We need to write to the tab set so that we can remove each tab
      // from this area when it's closed.
      typedef writable_dynamic_set<boost::shared_ptr<tab_info> > writable_tabs_set;

      boost::shared_ptr<writable_tabs_set> tabs;
      boost::shared_ptr<notifications_set> notifications;

      void tab_inserted(const boost::shared_ptr<tab_info> &tab)
      {
        // Arrange for the tab to be dropped from the set when it's
        // closed.
        tab->connect_closed(sigc::mem_fun(*this, &area_info_impl::tab_closed));

        tab->set_parent_area(shared_from_this());
      }

      void tab_removed(const boost::shared_ptr<tab_info> &tab)
      {
        tab->set_parent_area(boost::shared_ptr<area_info>());
      }

      void tab_closed(const boost::shared_ptr<tab_info> &tab)
      {
        tabs->remove(tab);
      }

    public:
      area_info_impl(const std::string &_name,
                     const std::string &_description,
                     const Glib::RefPtr<Gdk::Pixbuf> &_icon)
        : name(_name),
          description(_description),
          icon(_icon),
          tabs(tabs_set_impl::create()),
          notifications(notifications_set_impl::create())
      {
        tabs->connect_inserted(sigc::mem_fun(*this,
                                             &area_info_impl::tab_inserted));

        tabs->connect_removed(sigc::mem_fun(*this,
                                            &area_info_impl::tab_removed));
      }

      std::string get_name() { return name; }
      std::string get_description() { return description; }
      Glib::RefPtr<Gdk::Pixbuf> get_icon() { return icon; }
      boost::shared_ptr<tabs_set> get_tabs() { return tabs; }
      boost::shared_ptr<notifications_set> get_notifications() { return notifications; }
    };

    boost::shared_ptr<area_info> create_area_info(const std::string &name,
                                                  const std::string &description,
                                                  const Glib::RefPtr<Gdk::Pixbuf> &icon)
    {
      return boost::make_shared<area_info_impl>(name, description, icon);
    }

    class tab_info_impl : public tab_info,
                          public boost::enable_shared_from_this<tab_info_impl>
    {
      std::string name;
      Glib::RefPtr<Gdk::Pixbuf> icon;

      std::string tooltip_text;
      Gtk::Window *tooltip_window;

      Gtk::Widget *tab;

      progress_info progress;

      bool active;

      boost::weak_ptr<area_info> parent_area_weak;

      sigc::signal<void, boost::shared_ptr<tab_info>, std::string, Gtk::Window *> signal_tooltip_changed;
      sigc::signal<void, boost::shared_ptr<tab_info>, aptitude::util::progress_info> signal_progress_changed;
      sigc::signal<void, boost::shared_ptr<tab_info> > signal_activate_tab;
      sigc::signal<void, bool> signal_active_changed;
      sigc::signal<bool>::accumulated<cwidget::util::accumulate_and> signal_request_close;
      sigc::signal<void, boost::shared_ptr<tab_info> > signal_closed;

    public:
      tab_info_impl(const std::string &_name,
                    const Glib::RefPtr<Gdk::Pixbuf> &_icon,
                    Gtk::Widget *_tab)
        : name(_name),
          icon(_icon),
          tooltip_window(NULL),
          tab(_tab),
          progress(progress_info::none()),
          active(false)
      {
      }

      ~tab_info_impl()
      {
        delete tooltip_window;
      }

      void set_parent_area(const boost::shared_ptr<area_info> &parent_area)
      {
        if(parent_area_weak.lock().get() != NULL &&
           parent_area.get() != NULL)
          LOG_ERROR(Loggers::getAptitudeGtkToplevel(),
                    "Parent area for the tab " << name << " set twice.");
        else
          parent_area_weak = parent_area;
      }

      void add_sibling(const boost::shared_ptr<tab_info> &sibling)
      {
        boost::shared_ptr<area_info> parent_area = parent_area_weak.lock();

        if(parent_area.get() != NULL)
          parent_area->get_tabs()->insert(sibling);
        else
          LOG_ERROR(Loggers::getAptitudeGtkToplevel(),
                    "Can't add a sibling to a tab with no parent.");
      }

      std::string get_name() { return name; }

      void get_tooltip(std::string &out_tooltip_text,
                       Gtk::Window * &out_tooltip_window)
      {
        out_tooltip_text = tooltip_text;
        out_tooltip_window = tooltip_window;
      }

      void set_tooltip(const std::string &new_tooltip_text)
      {
        const boost::shared_ptr<tab_info> this_ptr = shared_from_this();

        delete tooltip_window;
        tooltip_window = NULL;

        tooltip_text = new_tooltip_text;
        signal_tooltip_changed(this_ptr, tooltip_text, tooltip_window);
      }

      void set_tooltip(Gtk::Window *new_tooltip_window)
      {
        const boost::shared_ptr<tab_info> this_ptr = shared_from_this();

        tooltip_text.clear();

        delete tooltip_window;
        tooltip_window = new_tooltip_window;

        signal_tooltip_changed(this_ptr, tooltip_text, tooltip_window);
      }

      Glib::RefPtr<Gdk::Pixbuf> get_icon() { return icon; }

      progress_info get_progress() { return progress; }

      void set_progress(const progress_info &info)
      {
        const boost::shared_ptr<tab_info> this_ptr = shared_from_this();

        progress = info;
        signal_progress_changed(this_ptr, progress);
      }

      Gtk::Widget *get_widget() { return tab; }
      void reset_widget() { tab = NULL; }

      bool get_active() { return active; }
      void set_active(bool new_active)
      {
        if(active != new_active)
          {
            active = new_active;
            signal_active_changed(active);
          }
      }

      sigc::connection
      connect_tooltip_changed(const sigc::slot<void, boost::shared_ptr<tab_info>,
                              std::string, Gtk::Window *> &
                              slot)
      {
        return signal_tooltip_changed.connect(slot);
      }

      sigc::connection
      connect_progress_changed(const sigc::slot<void,
                               boost::shared_ptr<tab_info>,
                               aptitude::util::progress_info> &
                               slot)
      {
        return signal_progress_changed.connect(slot);
      }

      sigc::connection
      connect_activate_tab(const sigc::slot<void, boost::shared_ptr<tab_info> > &
                           slot)
      {
        return signal_activate_tab.connect(slot);
      }

      sigc::connection connect_closed(const sigc::slot<void, boost::shared_ptr<tab_info> > &slot)
      {
        return signal_closed.connect(slot);
      }

      sigc::connection
      connect_active_changed(const sigc::slot<void, bool> &
                             slot)
      {
        return signal_active_changed.connect(slot);
      }

      sigc::connection
      connect_request_close(const sigc::slot<bool> &slot)
      {
        return signal_request_close.connect(slot);
      }

      void activate()
      {
        const boost::shared_ptr<tab_info> this_ptr = shared_from_this();
        signal_activate_tab(this_ptr);
      }

      void request_close()
      {
        if(signal_request_close())
          force_close();
      }

      void force_close()
      {
        // Give the pointer an explicit scope to avoid surprises in case
        // it ends up being the last reference.
        const boost::shared_ptr<tab_info> this_ptr = shared_from_this();

        signal_closed(this_ptr);
      }
    };

    boost::shared_ptr<tab_info> create_tab(const std::string &name,
                                           const Glib::RefPtr<Gdk::Pixbuf> &icon,
                                           Gtk::Widget *widget)
    {
      return boost::make_shared<tab_info_impl>(name, icon, widget);
    }

    class notification_info_impl : public notification_info
    {
      std::string name, description;
      Glib::RefPtr<Gdk::Pixbuf> icon;

      progress_info progress;

      sigc::signal<void, aptitude::util::progress_info> signal_progress_changed;
      sigc::signal<void> signal_clicked;

    public:
      notification_info_impl(const std::string &_name,
                             const std::string &_description,
                             const Glib::RefPtr<Gdk::Pixbuf> &_icon)
        : name(_name),
          description(_description),
          icon(_icon),
          progress(progress_info::none())
      {
      }

      std::string get_name() { return name; }
      std::string get_description() { return description; }
      Glib::RefPtr<Gdk::Pixbuf> get_icon() { return icon; }

      progress_info get_progress() { return progress; }
      void set_progress(const progress_info &new_progress)
      {
        progress = new_progress;
        signal_progress_changed(progress);
      }

      sigc::connection
      connect_progress_changed(const sigc::slot<void, aptitude::util::progress_info> &
                               slot)
      {
        return signal_progress_changed.connect(slot);
      }

      sigc::connection
      connect_clicked(const sigc::slot<void> &slot)
      {
        return signal_clicked.connect(slot);
      }
    };

    boost::shared_ptr<notification_info>
    create_notification(const std::string &name,
                        const std::string &description,
                        const Glib::RefPtr<Gdk::Pixbuf> &icon)
    {
      return boost::make_shared<notification_info_impl>(name,
                                                        description,
                                                        icon);
    }
  }
}
