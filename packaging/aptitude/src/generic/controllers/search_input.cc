/** \file search_input.cc */    // -*-c++-*-

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
#include "search_input.h"

#include <aptitude.h>

#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/views/search_input.h>

// System includes:
#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

using cwidget::util::transcode;

namespace views = aptitude::views;

namespace aptitude
{
  namespace controllers
  {
    namespace
    {
      class search_input_impl : public search_input
      {
        boost::shared_ptr<views::search_input> view;

        // Parse the current entry and emit the activated() signal if
        // it's valid (otherwise show the error).
        void do_search();

        // Invoked when the search entry's text changes.
        void search_input_changed();

        /** \brief A signal emitted when the user searches for a package. */
        sigc::signal<void, std::wstring, cwidget::util::ref_ptr<aptitude::matching::pattern> > signal_activated;

      public:
        // Only public for make_shared.
        search_input_impl(const boost::shared_ptr<views::search_input> &_view);

        /** \brief Create a new search_input_impl.
         *
         *  \param view   The view to manage.
         *
         *  \return A reference-counting wrapper around the new package
         *  search entry.
         */
        static boost::shared_ptr<search_input_impl>
        create(const boost::shared_ptr<views::search_input> &view);

        std::wstring get_text() const;

        /** \brief Set the text of the view and emit a search
         *  immediately.
         */
        void enter_text(const std::wstring &text);

        sigc::connection
        connect_activated(const sigc::slot<void, std::wstring, cwidget::util::ref_ptr<aptitude::matching::pattern> > &slot);
      };

      search_input_impl::search_input_impl(const boost::shared_ptr<views::search_input> &_view)
        : view(_view)
      {
      }

      boost::shared_ptr<search_input_impl>
      search_input_impl::create(const boost::shared_ptr<views::search_input> &
                                view)
      {
        return boost::make_shared<search_input_impl>(view);
      }

      void search_input_impl::do_search()
      {
        const std::wstring search_term_w(view->get_search_text());
        const std::string search_term(transcode(search_term_w));
        cwidget::util::ref_ptr<aptitude::matching::pattern> p;
        try
          {
            p = aptitude::matching::parse_with_errors(search_term);
          }
        catch(aptitude::matching::MatchingException &ex)
          {
            std::string msg = (boost::format("%s: %s")
                               % _("Parse error")
                               % ex.errmsg()).str();
            view->set_error_message(transcode(msg));
            return;
          }

        view->set_error_message(std::wstring());
        signal_activated(search_term_w, p);
      }

      void search_input_impl::search_input_changed()
      {
        const std::wstring limit(view->get_search_text());
        bool valid;
        try
          {
            aptitude::matching::parse_with_errors(transcode(limit));
            valid = true;
          }
        catch(aptitude::matching::MatchingException &)
          {
            valid = false;
          }

        view->set_input_validity(valid);
      }

      void search_input_impl::enter_text(const std::wstring &text)
      {
        view->set_search_text(text);
        do_search();
      }

      sigc::connection
      search_input_impl::connect_activated(const sigc::slot<void, std::wstring, cwidget::util::ref_ptr<aptitude::matching::pattern> > &slot)
      {
        return signal_activated.connect(slot);
      }
    }

    search_input::~search_input()
    {
    }

    boost::shared_ptr<search_input>
    create_search_input(const boost::shared_ptr<views::search_input> &view)
    {
      return search_input_impl::create(view);
    }
  }
}
