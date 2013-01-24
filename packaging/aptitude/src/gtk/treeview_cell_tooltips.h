// treeview_cell_tooltips.h    -*-c++-*-
//
//  Copyright 2008 Daniel Burrows
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

#ifndef TREEVIEW_CELL_TOOLTIPS_H
#define TREEVIEW_CELL_TOOLTIPS_H

#include <gtkmm.h>

/** \file treeview_cell_tooltips.h
 *
 *  Code to display tooltips for individual cells in a TreeView.
 *
 *  The tooltips are attached to TreeViewColumns, and the text/markup
 *  to display in a tooltip is taken from a TreeModelColumn.  The
 *  first time that a tooltip is attached to a TreeView, a connection
 *  to signal_query_tooltip() is created to actually display the tooltips.
 */

namespace gui
{
  /** \brief Start displaying tooltips on columns for the given tree-view if they
   *  aren't already being displayed.
   *
   *  \param treeview    The TreeView on which to start displaying tooltips.
   */
  void enable_column_tooltips(Gtk::TreeView *treeview);

  /** \brief Stop displaying tooltips on columns for the given tree-view if they
   *  aren't already being displayed.
   *
   *  \param treeview    The TreeView on which to stop displaying tooltips.
   */
  void disable_column_tooltips(Gtk::TreeView *treeview);

  /** \brief Attach a textual tooltip to the given column.
   *
   *  Column tooltips will be enabled on the tree-view if they aren't already.
   *
   *  \param treeview     The tree-view in which the column exists.
   *  \param view_column  The column to set a tooltip on.
   *  \param model_column The column of the model that supplies the tooltip text.
   */
  void set_text_tooltip(Gtk::TreeView *treeview,
			Gtk::TreeViewColumn *view_column,
			const Gtk::TreeModelColumn<Glib::ustring> &model_column);

  /** \brief Attach a textual tooltip to the given column.
   *
   *  Column tooltips will be enabled on the tree-view if they aren't already.
   *
   *  \param treeview     The tree-view in which the column exists.
   *  \param view_column  The column to set a tooltip on.
   *  \param model_column The column of the model that supplies the tooltip text.
   */
  void set_markup_tooltip(Gtk::TreeView *treeview,
			  Gtk::TreeViewColumn *view_column,
			  const Gtk::TreeModelColumn<Glib::ustring> &model_column);
};

#endif // TREEVIEW_CELL_TOOLTIPS_H
