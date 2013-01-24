// pkg_view.h            -*-c++-*-
//
//  Copyright 2000-2002, 2004-2005 Daniel Burrows
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
//

#ifndef PKG_VIEW_H
#define PKG_VIEW_H

#include <cwidget/config/column_definition.h>
#include <cwidget/generic/util/ref_ptr.h>
#include <cwidget/style.h>
#include "pkg_grouppolicy.h"

#include <list>
#include <string>

/** \brief Stuff to manage "package views" and a way to create them
 *
 * 
 *  Stuff to manage "package views" and a way to create them.  Package views
 *  are a collection of information displays about a package laid out (by a
 *  table in our case).  A package view definition has a spot in itself for a
 *  "main" widget, which allows configurations to be shared between several
 *  different screens (unfortunately, it prevents me from, eg, having two
 *  separate package trees at once :-( )
 * 
 *  \file pkg_view.h
 */

class menu_redirect;
class package_view_item;
class pkg_grouppolicy_factory;

namespace cwidget
{
  namespace widgets
  {
    class widget;
    typedef util::ref_ptr<widget> widget_ref;
  }
}

// The available types of items are:
//
//  - Main widget: this is filled by the caller and can be any widget.
//  - Static widget: displays formatted text relating to the selected package
//   (eg: description, priority, etc)
//  - Description widget: displays the description of the selected package
enum package_view_itemtype {PACKAGE_VIEW_MAINWIDGET,
			    PACKAGE_VIEW_STATIC,
			    PACKAGE_VIEW_DESCRIPTION};
// NOTE: PACKAGE_VIEW_DESCRIPTION is a nasty temporary hack; I need a more
// flexible info displayer.
//
// Note to self: is the above really true?


// This is just a big structure that transfers information about the
// structure of a view around.  Not particularly pleasant or efficient,
// but it works.
struct package_view_item
{
  std::string name;
  // Name of this item.  (searches based on this are slow, but the list
  // should be short enough that it won't matter)

  package_view_itemtype type;

  cwidget::config::column_definition_list *columns;
  // For static items, a column-format
  std::string columns_cfg;
  // For static items, the name of a configuration setting which will be
  // tracked; the columns will be regenerated when this is modified.
  std::string columns_cfg_default;
  // The default value for columns_cfg.
  int row, col, w, h;
  int xopts, yopts;
  // obvious table stuff

  /** The cwidget::style with which this item should be displayed. */
  cwidget::style st;

  std::string popupdownkey;
  // The key used to toggle the visibility of this item.  ("" for none)
  std::string popupdownlinked;
  // Who are we linked to for our popupdown status?

  cwidget::widgets::widget_ref widget;
  // For the internal use of make_package_view (stores the widget this item
  // generated)

  bool visible;
  // True if the widget is visible immediately

  package_view_item(std::string _name,
		    cwidget::config::column_definition_list *_columns, std::string _columns_cfg,
		    int _row, int _col, int _w, int _h, int _xopts, int _yopts,
		    const cwidget::style &_st, std::string _popupdownkey,
		    std::string _popupdownlinked, bool _visible)
    :name(_name), type(PACKAGE_VIEW_STATIC), columns(_columns),
     columns_cfg(_columns_cfg), row(_row), col(_col), w(_w), h(_h),
     xopts(_xopts), yopts(_yopts), st(_st), popupdownkey(_popupdownkey),
     popupdownlinked(_popupdownlinked), widget(NULL), visible(_visible) {}

  package_view_item(std::string _name,
		    int _row, int _col, int _w, int _h, int _xopts, int _yopts,
		    const cwidget::style &_st, std::string _popupdownkey, 
		    std::string _popupdownlinked, bool _visible)
    :name(_name), type(PACKAGE_VIEW_DESCRIPTION), columns(NULL),
     row(_row), col(_col), w(_w), h(_h), xopts(_xopts), yopts(_yopts),
     st(_st), popupdownkey(_popupdownkey), popupdownlinked(_popupdownlinked),
     widget(NULL), visible(_visible) {}

  package_view_item(std::string _name,
		    int _row, int _col, int _w, int _h,
		    int _xopts, int _yopts, const cwidget::style &_st, bool _visible)
    :name(_name), type(PACKAGE_VIEW_MAINWIDGET), columns(NULL),
     row(_row), col(_col), w(_w), h(_h), xopts(_xopts), yopts(_yopts),
     st(_st), widget(NULL), visible(_visible) {}

  package_view_item()
    :type((package_view_itemtype) -1), columns(NULL),
     row(-1), col(-1), w(-1), h(-1),
     xopts(0), yopts(0), widget(NULL), visible(true)
  {
  }
};

/** Generates a package view from the given layout with the given main widget.
 *
 *  \param format a description of the layout of the view
 *  \param mainwidget the widget which will be used to fill in MAINWIDGET items
 *  \param menu_handler the handler to use for menu commands.
 *  \param sig the signal which will be called when a package is selected by
 *             the main widget.  If this is \b NULL, nothing will be connected
 *             to it.
 *  \param desc_sig an auxillary signal which will be used when something
 *                  which wants to display text in the description area is
 *                  selected by the main widget
 *  \param show_info_first if \b true, the reason subscreen will be initially
 *                         visible; otherwise, the description will be
 *                         initially visible.
 */
cwidget::widgets::widget_ref make_package_view(std::list<package_view_item> &format,
				const cwidget::widgets::widget_ref &mainwidget,
				menu_redirect *menu_handler,
				pkg_signal *sig, desc_signal *desc_sig,
				bool show_reason_first);

#endif
