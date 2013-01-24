// download_item.h -- this is -*-c++-*-
//
//  Copyright 1999-2001, 2005 Daniel Burrows
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
//

#ifndef DOWNLOAD_ITEM_H
#define DOWNLOAD_ITEM_H

#include <apt-pkg/acquire-item.h>

#include <cwidget/widgets/treeitem.h>

/** \brief This item represents something that's in a download queue
 * 
 *  \file download_item.h
 */

class download_item:public cwidget::widgets::treeitem
{
  pkgAcquire::ItemDesc item;
  pkgAcquire::Worker *worker;
  // If the item is in the process of fetching, this is a pointer to the
  // worker fetching it; otherwise, it's NULL.

  bool hit;
  // When the download completes, this is set to either true or false depending
  // on whether the download was just a "hit".

  int doneColor;
  // The color to use to display the package after finishing.
public:
  download_item(const pkgAcquire::ItemDesc &_item):cwidget::widgets::treeitem(false), item(_item), worker(NULL) {}

  cwidget::style get_normal_style();

  virtual void paint(cwidget::widgets::tree *win, int y, bool hierarchical,
		     const cwidget::style &st);

  void download_done(bool _hit) {hit=_hit;}
  void set_worker(pkgAcquire::Worker *_worker) {worker=_worker;}

  // Nothing sensible to return here.
  const wchar_t *tag() {return L"";}
  const wchar_t *label() {return L"";}
};

#endif
