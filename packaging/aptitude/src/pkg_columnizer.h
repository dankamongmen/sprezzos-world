// pkg_columnizer.h  -*-c++-*-
//
//  Copyright 1999-2002, 2004-2005, 2007 Daniel Burrows
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

#ifndef PKG_COLUMNIZER_H
#define PKG_COLUMNIZER_H

#include "pkg_item.h"
#include <cwidget/config/column_definition.h>

/** \brief pkg_columnizer class and associated data
 *
 * 
 *  The rather hefty pkg_columnizer class and associated data (eg, info about
 *  column sizes) lives here.
 * 
 *  \file pkg_columnizer.h
 */

class pkg_item::pkg_columnizer : public cwidget::config::column_generator
{
  class pkg_genheaders;

  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator visible_ver;

  static cwidget::config::column_definition_list *columns;

  int basex;

  // Set up the translated format widths.
  static void init_formatting();
protected:
  const pkgCache::PkgIterator &get_pkg() {return pkg;}
  const pkgCache::VerIterator &get_visible_ver() {return visible_ver;}
public:
  static cwidget::column_disposition setup_column(const pkgCache::PkgIterator &pkg,
						  const pkgCache::VerIterator &ver,
						  int basex,
						  int type);
  virtual cwidget::column_disposition setup_column(int type);

  static const cwidget::config::column_definition_list &get_columns()
  {
    setup_columns();
    return *columns;
  }

  enum types {name, installed_size, debsize, stateflag, actionflag,
	      description, currver, candver, longstate, longaction,
	      maintainer, priority, shortpriority, section, revdepcount,
	      autoset, tagged, archive, sizechange,

              progname, progver, brokencount, diskusage, downloadsize,
	      pin_priority, hostname, trust_state, numtypes};
  static cwidget::config::column_type_defaults defaults[numtypes];
  static const char *column_names[numtypes];

  static const char *default_pkgdisplay;
  // The default values for the package column formatting.

  static int parse_column_type(char id);

  static void setup_columns(bool force_update=false);
  // This reads and sets up the necessary information about how to format the
  // display of packages.
  static std::wstring format_column_names(unsigned int width);
  // Returns a string which can serve as a header for the column-name
  // information for a screen of the given width.

  int get_basex() {return basex;}

  pkg_columnizer(const pkgCache::PkgIterator &_pkg,
		 const pkgCache::VerIterator &_visible_ver,
		 const cwidget::config::column_definition_list &_columns,
		 int _basex)
    : column_generator(_columns),
      pkg(_pkg),
      visible_ver(_visible_ver),
      basex(_basex)
  {
  }

};

#endif
