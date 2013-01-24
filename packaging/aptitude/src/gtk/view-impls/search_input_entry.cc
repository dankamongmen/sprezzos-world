/** \file search_input_entry.cc */

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

// Local includes:
#include "search_input_entry.h"

#include <generic/views/search_input.h>

// System includes:
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

using cwidget::util::transcode;

namespace gui
{
  namespace view_impls
  {
    namespace
    {
      class search_input_entry_impl : public aptitude::views::search_input
      {
        Gtk::Entry *search_entry;
        Gtk::Label *error_label;
        Gtk::Button *find_button;

        sigc::signal<void> signal_search;
        sigc::signal<void> signal_text_changed;

      public:
        // Only public for make_shared.
        search_input_entry_impl(Gtk::Entry *_search_entry,
                                Gtk::Label *_error_label,
                                Gtk::Button *_find_button);

        /** \brief Create a new search_input_entry_impl.
         *
         *  \param search_entry               The text entry to manage.
         *  \param error_label                The label in which to display error messages.
         *  \param find_button                A button that the user can use to perform a search.
         *
         *  \return The new search input entry view.
         */
        static boost::shared_ptr<search_input_entry_impl>
        create(Gtk::Entry *search_entry,
               Gtk::Label *error_label,
               Gtk::Button *find_button);

        std::wstring get_search_text();

        void set_search_text(const std::wstring &text);

        void set_error_message(const std::wstring &msg);

        void set_input_validity(bool valid);

        void set_find_sensitivity(bool value);

        sigc::connection
        connect_search_text_changed(const sigc::slot<void> &slot);

        sigc::connection connect_search(const sigc::slot<void> &slot);
      };

      search_input_entry_impl::search_input_entry_impl(Gtk::Entry *_search_entry,
                                                       Gtk::Label *_error_label,
                                                       Gtk::Button *_find_button)
        : search_entry(_search_entry),
          error_label(_error_label),
          find_button(_find_button)
      {
      }

      boost::shared_ptr<search_input_entry_impl>
      search_input_entry_impl::create(Gtk::Entry *search_entry,
                                      Gtk::Label *error_label,
                                      Gtk::Button *find_button)
      {
        return boost::make_shared<search_input_entry_impl>(search_entry,
                                                           error_label,
                                                           find_button);
      }

      std::wstring search_input_entry_impl::get_search_text()
      {
        return transcode(std::string(search_entry->get_text()), "UTF-8");
      }

      void search_input_entry_impl::set_search_text(const std::wstring &text)
      {
        search_entry->set_text(transcode(text, "UTF-8"));
      }

      void search_input_entry_impl::set_error_message(const std::wstring &msg)
      {
        if(!msg.empty())
          {
            Glib::ustring markup =
              Glib::ustring::compose("<span size=\"smaller\" color=\"red\">%1</span>",
                                     Glib::Markup::escape_text(transcode(msg, "UTF-8")));

            error_label->set_markup(markup);
            error_label->show();
          }
        else
          error_label->hide();
      }

      void search_input_entry_impl::set_input_validity(bool valid)
      {
        if(valid)
          search_entry->unset_base(Gtk::STATE_NORMAL);
        else
          search_entry->modify_base(Gtk::STATE_NORMAL, Gdk::Color("#FFD0D0"));
      }

      void search_input_entry_impl::set_find_sensitivity(bool value)
      {
        find_button->set_sensitive(value);
      }

      sigc::connection
      search_input_entry_impl::connect_search_text_changed(const sigc::slot<void> &slot)
      {
        return search_entry->signal_changed().connect(slot);
      }

      sigc::connection
      search_input_entry_impl::connect_search(const sigc::slot<void> &slot)
      {
        return search_entry->signal_activate().connect(slot);
        find_button->signal_clicked().connect(slot);
      }
    }
  }
}
