// tags.h                                            -*-c++-*-
//
//   Copyright (C) 2005, 2007, 2010 Daniel Burrows
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

#ifndef TAGS_H
#define TAGS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_EPT
#include <ept/debtags/debtags.h>
#endif // HAVE_EPT

#include <apt-pkg/pkgcache.h>

#include <set>
#include <string>

#ifndef HAVE_EPT
#define DEBTAGS_ARE_STRINGS 1
#else
#ifdef EPT_DEBTAGS_GETTAGSOFITEM_RETURNS_STRINGS
#define DEBTAGS_ARE_STRINGS 1
#endif
#endif

class OpProgress;

namespace aptitude
{
  namespace apt
  {
#ifdef DEBTAGS_ARE_STRINGS
    typedef std::string tag;
    inline std::string get_fullname(const std::string &t)
    {
      return t;
    }
#else
    // Probably means a new version of libept does something the
    // configure checks can't recognize.
#error "Don't know how to represent a debtags tag."
#endif

    /** \brief Initialize the cache of debtags information. */
    void load_tags(OpProgress *progress = NULL);

    /** /brief Grab the tags for the given package. */
    const std::set<tag> get_tags(const pkgCache::PkgIterator &pkg);

    /** \brief Get the name of the facet corresponding to a tag. */
    std::string get_facet_name(const tag &t);

    /** \brief Get the name of a tag (the full name minus the facet). */
    std::string get_tag_name(const tag &t);

    /** \brief Get the short description of a tag. */
    std::string get_tag_short_description(const tag &t);

    /** \brief Get the long description of a tag. */
    std::string get_tag_long_description(const tag &t);

    // \note This interface could be more efficient if it just used
    // facet names like libept does.  Using tags is a concession to
    // backwards compatibility (it's hard to implement one interface
    // that covers both cases without a lot of cruft).  In any event,
    // this shouldn't be called enough to matter.

    /** \brief Get the short description of the facet corresponding to a tag. */
    std::string get_facet_short_description(const tag &t);

    /** \brief Get the long description of the facet corresponding to a tag. */
    std::string get_facet_long_description(const tag &t);
  }
}

// If ept is unavailable, we use our own (broken!) code to build an
// in-memory database of package tags.  Otherwise, this code just
// handles initializing it, destroying it, and extracting information
// from it.  Note that this means that all callers have to be
// conditionalized on HAVE_EPT: the "tag" class this used to return is
// broken wrt hierarchies and just using ept is simpler.

#endif
