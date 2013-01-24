// -*-c++-*-

// description.cc
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

#include "packageinformation.h"
#include "aptitude.h"

#include <string>
#include <iostream>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/pkgrecords.h>

#include <generic/apt/apt.h>
#include <generic/apt/desc_parse.h>
#include <generic/apt/config_signal.h>

#include <glibmm/ustring.h>

namespace gui
{
  namespace
  {
    /** \todo This should spit text into a TextBuffer and use
     *  indentation tags to align bullets (so that paragraphs wrap
     *  properly).  Ideally the text of a bulleted item should align
     *  with the text *following* the bullet.
     */
    void make_desc_text(const std::vector<aptitude::description_element_ref> &elements,
			int level,
			std::string &out)
    {
      using cwidget::util::transcode;

      for(std::vector<aptitude::description_element_ref>::const_iterator it = elements.begin();
	  it != elements.end(); ++it)
	{
	  const aptitude::description_element_ref &elt(*it);

	  switch(elt->get_type())
	    {
	    case aptitude::description_element::blank_line:
	      out.push_back('\n');
	      break;
	    case aptitude::description_element::paragraph:
	      out.append(transcode(elt->get_string(), "UTF-8"));
	      out.push_back('\n');
	      break;
	    case aptitude::description_element::literal:
	      out.append(transcode(elt->get_string(), "UTF-8"));
	      out.push_back('\n');
	      break;
	    case aptitude::description_element::bullet_list:
	      {
		out.append(transcode(std::wstring() + L"\x2022-+"[level % 3],
				     "UTF-8"));

		make_desc_text(elt->get_elements(),
			       level + 1,
			       out);

		// Separate list items, unless there's a preceding
		// blank line.  The condition is a hack to avoid
		// appending too many blank lines when closing out a
		// description.
		if(!(out.size() >= 2 &&
		     out[out.size() - 1] == '\n' &&
		     out[out.size() - 2] == '\n'))
		  out.push_back('\n');
	      }
	      break;
	    }
	}
    }
  }

  PackageInformation::PackageInformation(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

    name = pkg.Name();

    version = ver.VerStr();

    priority = pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown");

    section = pkg.Section()?pkg.Section():_("Unknown");

    maintainer = rec.Maintainer().c_str();

    compressed_size = SizeToStr(ver->Size).c_str();

    uncompressed_size = SizeToStr(ver->InstalledSize).c_str();

    source_package = rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str();

    short_description = cwidget::util::transcode(get_short_description(ver, apt_package_records),
						 "UTF-8");
    const std::wstring fulldesc = get_long_description(ver, apt_package_records);

    std::vector<aptitude::description_element_ref> elements;

    aptitude::parse_desc(fulldesc, elements);
    make_desc_text(elements, 0, long_description);
  }
}
