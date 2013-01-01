#ifndef EPT_DEBTAGS_SOURCEDIR_TCC
#define EPT_DEBTAGS_SOURCEDIR_TCC

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Debtags data source directory access
 */
#include <ept/debtags/maint/sourcedir.h>

#include <tagcoll/input/zlib.h>
#include <tagcoll/input/stdio.h>

namespace ept {
namespace debtags {

template<typename OUT>
void SourceDir::readTags(OUT out)
{
	if (!valid()) return;

	for (const_iterator d = begin(); d != end(); ++d)
	{
		FileType type = fileType(d->d_name);
		if (type == TAG)
		{
			// Read uncompressed data
			tagcoll::input::Stdio in(path() + "/" + d->d_name);

			// Read the collection
			tagcoll::textformat::parse(in, out);
		}
		else if (type == TAGGZ)
		{
			// Read compressed data
			tagcoll::input::Zlib in(path() + "/" + d->d_name);

			// Read the collection
			tagcoll::textformat::parse(in, out);
		}
	}
}

}
}

#include <tagcoll/TextFormat.tcc>

#endif

// -*- C++ -*-
// vim:set ts=4 sw=4:
