// desc_render.h                                     -*-c++-*-
//
//   Copyright 2004-2005, 2008 Daniel Burrows
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

#ifndef DESC_RENDER_H
#define DESC_RENDER_H

#include <string>
#include <vector>

#include <apt-pkg/pkgcache.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/apt/desc_parse.h>

/** \file desc_render.h
 */

namespace cwidget
{
  class fragment;
}

namespace aptitude
{
  /** \brief Render a list of elements as a cwidget fragment.
   *
   *  \param elements   the list of description elements to be rendered.
   */
  cwidget::fragment *make_desc_fragment(const std::vector<description_element_ref> &elements);
}

/** Parses the given description string according to the standard
 *  formatting rules.
 *
 *  \param desc a Description tag to parse
 *  \return a cwidget::fragment representing that description
 *
 *  This just invokes aptitude::parse_desc() and aptitude::make_desc_fragment().
 */
cwidget::fragment *make_desc_fragment(const std::wstring &desc);

/** \return a cwidget::fragment listing the tags of the given package, or \b
 *  NULL if there are no tags.
 *
 *  The global cache, apt_cache_file, should be available when you
 *  call this routine.
 */
cwidget::fragment *make_tags_fragment(const pkgCache::PkgIterator &pkg);

#endif
