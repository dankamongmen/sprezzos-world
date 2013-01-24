// description.h             -*-c++-*-
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef DESCRIPTION_H_
#define DESCRIPTION_H_

#include <string>

#include <apt-pkg/pkgcache.h>
#include <generic/apt/desc_parse.h>

namespace gui
{
  /** \brief A convenience class that collects all the code to extract
   *  package information.
   *
   *  \todo This should really be "PackageInformation" since the
   *  description is only one thing it does.
   *
   *  \todo Is this class really necessary / useful?
   */
  class PackageInformation
  {
    private:
      std::string name;
      std::string version;
      std::string priority;
      std::string section;
      std::string maintainer;
      std::string compressed_size;
      std::string uncompressed_size;
      std::string source_package;
      std::string short_description;
      std::string long_description;
  public:
      PackageInformation(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver);
      std::string Name() const { return name; }
      std::string Version() const { return version; }
      std::string Priority() const { return priority; }
      std::string Section() const { return section; }
      std::string Maintainer() const { return maintainer; }
      std::string CompressedSize() const { return compressed_size; }
      std::string UncompressedSize() const { return uncompressed_size; }
      std::string SourcePackage() const { return source_package; }
      std::string ShortDescription() const { return short_description; }
      std::string LongDescription() const { return long_description; }
  };

}

#endif /* DESCRIPTION_H_ */
