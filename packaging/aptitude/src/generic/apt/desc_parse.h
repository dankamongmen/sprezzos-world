// desc_parse.h                                     -*-c++-*-
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

#ifndef DESC_PARSE_H
#define DESC_PARSE_H

#include <string>
#include <vector>

#include <cwidget/generic/util/ref_ptr.h>

/** \file desc_parse.h
 */

namespace aptitude
{
  /** \brief Represents a single piece of syntactic structure in a
   *  description.
   *
   *  This is a poor man's algebraic datatype.  Its structure is:
   *
   *    Element = Paragraph String | Literal String | BulletList [Element]
   *
   *  with blank lines being denoted by paragraphs containing an empty
   *  string.
   */
  class description_element
  {
  public:
    /** \brief The type tag of elements. */
    enum element_type
      {
	/** \brief Indicates that an element represents a blank line
	 *  between paragraphs.
	 *
	 *  The cwidget and command-line frontends need this because
	 *  vertical whitespace in these environments is dished out in
	 *  rather large quanta and user expectations are quite
	 *  specific.  Other frontends may want to experiment with
	 *  looser inter-paragraph spacing (ignoring this element
	 *  type).
	 */
	blank_line,
	/** \brief Indicates that an element represents a paragraph of text.
	 *
	 *  Paragraphs should be word-wrapped.
	 */
	paragraph,
	/** \brief Indicates that an element represents literally formatted text.
	 *
	 *  These elements should be formatted without word-wrapping
	 *  or other interpretation of the text.  Vertical whitespace
	 *  should not be used to offset literal elements from their
	 *  surrounding text.
	 */
	literal,
	/** \brief Indicates that an element represents a bulletted list.
	 *
	 *  Each entry in the list should be formatted by placing a
	 *  bullet to its left and indenting the whole entry.
	 */
	bullet_list
      };

  private:

    element_type type;

    std::wstring string_payload;
    std::vector<cwidget::util::ref_ptr<description_element> > list_payload;

    int refcount;

    description_element(element_type _type, const std::wstring &_string_payload,
			const std::vector<cwidget::util::ref_ptr<description_element> > &_list_payload)
      : type(_type),
	string_payload(_string_payload),
	list_payload(_list_payload),
	refcount(1)
    {
    }

  public:
    void incref()
    {
      eassert(refcount > 0);
      ++refcount;
    }

    void decref()
    {
      eassert(refcount > 0);

      --refcount;
      if(refcount == 0)
	delete this;
    }

    static cwidget::util::ref_ptr<description_element>
    make_blank_line()
    {
      return new description_element(blank_line, std::wstring(),
				     std::vector<cwidget::util::ref_ptr<description_element> >());
    }

    static cwidget::util::ref_ptr<description_element>
    make_paragraph(const std::wstring &text)
    {
      return new description_element(paragraph, text,
				     std::vector<cwidget::util::ref_ptr<description_element> >());
    }

    static cwidget::util::ref_ptr<description_element>
    make_literal(const std::wstring &text)
    {
      return new description_element(literal, text,
				     std::vector<cwidget::util::ref_ptr<description_element> >());
    }

    static cwidget::util::ref_ptr<description_element>
    make_bullet_list(const std::vector<cwidget::util::ref_ptr<description_element> > &elements)
    {
      return new description_element(bullet_list, std::wstring(), elements);
    }

    /** \brief Retrieve the type tag of this element. */
    element_type get_type() const { return type; }
    /** \brief Get the string of a paragraph or literal element.
     *
     *  It is an error to invoke this routine on any other element type.
     */
    const std::wstring &get_string() const
    {
      eassert(type == paragraph || type == literal);
      return string_payload;
    }

    /** \brief Get the list of elements associated with a bullet-list element.
     *
     *  It is an error to invoke this routine on any other element type.
     */
    const std::vector<cwidget::util::ref_ptr<description_element> > &get_elements()
    {
      eassert(type == bullet_list);
      return list_payload;
    }
  };
  typedef cwidget::util::ref_ptr<description_element> description_element_ref;

  /** \brief Parse a description-style string.
   *
   *  \param desc the raw text of the description.  The short
   *  description (i.e., everything up to and including the first
   *  newline) will be stripped from this string before it is parsed.
   *
   *  \param output the output list; all the top-level elements of the
   *  parsed description will be pushed onto the end of this vector.
   */
  void parse_desc(const std::wstring &desc,
		  std::vector<description_element_ref> &output);
}

#endif
