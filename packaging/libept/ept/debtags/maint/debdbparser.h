#ifndef EPT_DEBTAGS_DEBDBPARSER_H
#define EPT_DEBTAGS_DEBDBPARSER_H

/** \file
 * Parser for debian database files
 */

/*
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

#include <tagcoll/input/base.h>

// TODO: is there a way to forward-declare this?
#include <map>

namespace tagcoll {
namespace input {
class Input;
}
}

namespace ept {
namespace debtags {

/*
class DebDBConsumer
{
public:
	virtual void consumeField(const std::string& name, const std::string& val) throw () = 0;
	virtual void consumeEndOfRecord() throw () = 0;
};
*/

/**
 * Parse Debian records from a parser input
 */
class DebDBParser
{
protected:
	tagcoll::input::Input& in;
	bool isBOL;
	bool isEOF;

	// Eat spaces and empty lines
	// Returns the number of '\n' encountered
	int eatSpacesAndEmptyLines();

	// Get the ^([A-Za-z0-9]+) field name
	std::string getFieldName();

	// Eat the \s*: characters that divide the field name and the field
	// data
	void eatFieldSep();

	// Get the \s*(.+?)\s*\n of a body line
	void appendFieldBody(std::string& body);

public:
	typedef std::map<std::string, std::string> Record;

	DebDBParser(tagcoll::input::Input& input);

	const std::string& fileName() const throw () { return in.fileName(); }
	int lineNumber() const throw () { return in.lineNumber(); }

	// Read a record and positions itself at the start of the next one
	// Returns false when there are no more records available
	bool nextRecord(Record& rec);
};

}
}

// vim:set ts=4 sw=4:
#endif
