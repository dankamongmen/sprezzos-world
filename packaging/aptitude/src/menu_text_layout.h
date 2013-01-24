// menu_text_layout.h                             -*-c++-*-
//
//   Copyright (C) 2005, 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef MENU_TEXT_LAYOUT_H
#define MENU_TEXT_LAYOUT_H

#include "menu_redirect.h"

#include <cwidget/widgets/editline.h>
#include <cwidget/widgets/text_layout.h>

/** \file menu_text_layout.h
 */

class menu_text_layout : public cwidget::widgets::text_layout, public menu_redirect
{
  std::wstring last_search;
  bool last_search_forward;
  cwidget::widgets::editline::history_list search_history;

  menu_text_layout(const menu_text_layout &other);

  /** Search forward for the given string (or the last search if s is
   *  empty); set last_search_forward to true and last_search to s if
   *  nonempty.
   */
  void do_find_search(const std::wstring &s);

  /** Search backward for the given string (or the last search if s is
   *  empty); set last_search_forward to false and last_search to s if
   *  nonempty.
   */
  void do_find_search_back(const std::wstring &s);
protected:
  menu_text_layout()
    : last_search_forward(true)
  {
  }

  menu_text_layout(cwidget::fragment *f)
    : cwidget::widgets::text_layout(f), last_search_forward(true)
  {
  }
public:
  static cwidget::util::ref_ptr<menu_text_layout> create()
  {
    cwidget::util::ref_ptr<menu_text_layout> rval = new menu_text_layout;
    rval->decref();
    return rval;
  }

  static cwidget::util::ref_ptr<menu_text_layout> create(cwidget::fragment *f)
  {
    cwidget::util::ref_ptr<menu_text_layout> rval = new menu_text_layout(f);
    rval->decref();
    return rval;
  }

  bool handle_key(const cwidget::config::key &k);

  bool find_search_enabled();
  bool find_search();
  bool find_search_back_enabled();
  bool find_search_back();
  bool find_research_enabled();
  bool find_research();
  bool find_repeat_search_back_enabled();
  bool find_repeat_search_back();
};
typedef cwidget::util::ref_ptr<menu_text_layout> menu_text_layout_ref;

#endif
