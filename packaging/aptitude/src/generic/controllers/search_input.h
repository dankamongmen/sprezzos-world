/** \file search_input.h */    // -*-c++-*-

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

#ifndef APTITUDE_CONTROLLERS_SEARCH_INPUT_H
#define APTITUDE_CONTROLLERS_SEARCH_INPUT_H

#include <boost/shared_ptr.hpp>

#include <cwidget/generic/util/ref_ptr.h>

#include <sigc++/connection.h>
#include <sigc++/slot.h>
#include <sigc++/trackable.h>

namespace aptitude
{
  namespace matching
  {
    class pattern;
  }

  namespace views
  {
    class search_input;
  }

  namespace controllers
  {
    /** \brief Provides the logic to drive a search input box.
     *
     *  This handles providing feedback to the user about their
     *  current search expression, and triggering a search when they
     *  enter a well-formed search expression.
     */
    class search_input : public sigc::trackable
    {
    public:
      virtual ~search_input();

      /** \brief Set the text of the view to the given string, and
       *         immediately perform a search.
       *
       *  \todo This method is really only used to initialize new
       *  tabs.  It might be better to incorporate it into
       *  create_search_input().
       */
      virtual void enter_text(const std::wstring &text) = 0;

      /** \brief Register a slot to be invoked when a search should be
       *         performed.
       *
       *  The arguments to the slot are the actual text entered by the
       *  user and a search pattern parsed from that text.
       */
      virtual sigc::connection
      connect_activated(const sigc::slot<void, std::wstring, cwidget::util::ref_ptr<aptitude::matching::pattern> > &
                        slot) = 0;
    };

    /** \brief Create a new search input controller with the default
     *         behavior.
     *
     *  \param view The view whose behavior will be managed by the new
     *              controller.
     *
     *  \return A search input controller attached to the given view.
     */
    boost::shared_ptr<search_input>
    create_search_input(const boost::shared_ptr<aptitude::views::search_input> &view);
  }
}

#endif // APTITUDE_CONTROLLERS_SEARCH_INPUT_H
