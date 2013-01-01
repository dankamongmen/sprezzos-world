/*
 * Ready-made utility classes for Debtags
 *
 * Copyright (C) 2003--2007  Enrico Zini <enrico@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef EPT_DEBTAGS_UTILS_H
#define EPT_DEBTAGS_UTILS_H

#include <wibble/mixin.h>
#include <ept/debtags/tag.h>

namespace ept {
namespace debtags {

/**
 * Convert a collection of ITEMs tagged with Tags to a collection of
 * ITEMs tagged with only the facets.
 */
template<typename OUT>
class TagToFacet : public wibble::mixin::OutputIterator< TagToFacet<OUT> >
{
protected:
	OUT out;

public:
	TagToFacet(const OUT& out) : out(out) {}

	template<typename ITEMS, typename TAGS>
	TagToFacet<OUT>& operator=(const std::pair<ITEMS, TAGS>& data)
	{
		std::set< Facet > facets;
		for (typename TAGS::const_iterator i = data.second.begin();
				i != data.second.end(); ++i)
			facets.insert(i->facet());
		*out = make_pair(data.first, facets);
		++out;
		return *this;
	}
};

template<typename OUT>
static TagToFacet<OUT> tagToFacet(const OUT& out)
{
	return TagToFacet<OUT>(out);
}

}
}

// vim:set ts=4 sw=4:
#endif
