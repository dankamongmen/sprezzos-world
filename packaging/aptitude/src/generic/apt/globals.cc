/** \file globals.cc */

// Copyright (C) 2010 Daniel Burrows
//
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

#include <boost/shared_ptr.hpp>

class aptcfg;
class aptitudeCacheFile;
class pkgRecords;
class pkgSourceList;
class resolver_manager;
class signalling_config;
class undo_list;

namespace aptitude
{
  namespace util
  {
    class file_cache;
  }
}

// Definitions of global pointers exposed in apt.h.  They are defined
// here so that test code can get away with just linking in globals.o
// rather than apt.o and everything it requires.
aptitudeCacheFile *apt_cache_file=NULL;
signalling_config *aptcfg=NULL;
pkgRecords *apt_package_records=NULL;
pkgSourceList *apt_source_list=NULL;
undo_list *apt_undos=NULL;
resolver_manager *resman = NULL;

boost::shared_ptr<aptitude::util::file_cache> download_cache;

