// changelog_parse.h                        -*-c++-*-
//
//   Copyright (C) 2005, 2008-2009 Daniel Burrows
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

#ifndef CHANGELOG_PARSE_H
#define CHANGELOG_PARSE_H

#include <time.h>

#include <apt-pkg/pkgcache.h>

#include <vector>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/post_thunk.h>
#include <generic/util/refcounted_base.h>
#include <generic/util/safe_slot.h>

/** \file changelog_parse.h
 */

class FileFd;

namespace cwidget
{
  class fragment;
}
namespace temp {class name;}

namespace aptitude
{
  namespace apt
  {
    class changelog_element
    {
    public:
      /** \brief The type of a changelog text element. */
      enum type
	{
	  /** \brief Some text that should be displayed literally. */
	  text_type,

	  /** \brief A beginning-of-the-line bullet. */
	  bullet_type,

	  /** \brief A Closes entry.
	   *
	   *  In this case, the text will be the bug number.
	   */
	  closes_type
	};

    private:
      /** \brief Where this element begins. */
      std::string::size_type begin;

      /** \brief Where this element ends. */
      std::string::size_type end;

      type tp;

    public:
      changelog_element(type _tp,
			std::string::size_type _begin,
			std::string::size_type _end)
	: begin(_begin), end(_end), tp(_tp)
      {
      }

      std::string::size_type get_begin() const { return begin; }
      std::string::size_type get_end() const { return end; }
      type get_type() const { return tp; }
    };


    /** \brief Represents a reference-counted vector of changelog text
     * 	elements.
     */
    class changelog_element_list : public util::refcounted_base_threadsafe
    {
      std::vector<changelog_element> elements;

      changelog_element_list(const std::vector<changelog_element> &_elements)
	: elements(_elements)
      {
      }
    public:
      static cwidget::util::ref_ptr<changelog_element_list>
      create(const std::vector<changelog_element> &elements)
      {
	return new changelog_element_list(elements);
      }

      const std::vector<changelog_element> &get_elements() const { return elements; }
    };

    /** \brief Represents a single entry in a Debian changelog. */
    class changelog_entry : public util::refcounted_base_threadsafe
    {
      std::string source;
      std::string version;
      std::string distribution;
      std::string urgency;
      std::string changes;
      cwidget::util::ref_ptr<changelog_element_list> elements;
      std::string maintainer;
      std::string date_str;
      bool could_parse_date;
      time_t date;

      changelog_entry(const std::string &_source,
		      const std::string &_version,
		      const std::string &_distribution,
		      const std::string &_urgency,
		      const std::string &_changes,
		      const cwidget::util::ref_ptr<changelog_element_list> &_elements,
		      const std::string &_maintainer,
		      const std::string &_date);

    public:
      /** \brief Create a new changelog entry.
       *
       *  \param source       The source package of the entry.
       *  \param version      The version number of the entry.
       *  \param distribution The distribution of the entry.
       *  \param urgency      The urgency of the entry.
       *  \param changes      The text of the entry.
       *  \param elements     The parsed elements of the entry.
       *  \param maintainer   The maintainer field of the entry.
       *  \param date         The date of the entry.
       */
      static cwidget::util::ref_ptr<changelog_entry>
      create(const std::string &source,
	     const std::string &version,
	     const std::string &distribution,
	     const std::string &urgency,
	     const std::string &changes,
	     const cwidget::util::ref_ptr<changelog_element_list> &elements,
	     const std::string &maintainer,
	     const std::string &date)
      {
	return new changelog_entry(source,
				   version,
				   distribution,
				   urgency,
				   changes,
				   elements,
				   maintainer,
				   date);
      }

      /** \return the source package name of the changelog entry. */
      const std::string &get_source() const { return source; }
      /** \return the version number of the changelog entry. */
      const std::string &get_version() const { return version; }
      /** \return the distribution of the changelog entry. */
      const std::string &get_distribution() const { return distribution; }
      /** \return the urgency of the changelog entry. */
      const std::string &get_urgency() const { return urgency; }
      /** \return the text of the changelog entry. */
      const std::string &get_changes() const { return changes; }
      /** \return the elements of the changelog entry text.
       *
       *  This excludes the first line, which can be programmatically
       *  generated from the other fields.
       */
      const cwidget::util::ref_ptr<changelog_element_list> &
      get_elements() const { return elements; }
      /** \return the maintainer field of the changelog entry. */
      const std::string &get_maintainer() const { return maintainer; }
      /** \return the date string of the changelog entry. */
      const std::string &get_date_str() const { return date_str; }
      /** \return \b true if the date string in this changelog entry
       *  was correctly parsed.
       */
      bool get_could_parse_date() const { return could_parse_date; }
      /** \return the date that was parsed from the date string in this
       *  changelog entry, if get_could_parse_date() returned \b true.
       */
      time_t get_date() const { return date; }
    };

    /** \brief Represents an entire changelog.
     *
     *  Changelogs are containers of changelog_entry objects.  The
     *  entries are stored in the same order that they appear in the
     *  original changelog.
     */
    class changelog : public util::refcounted_base_threadsafe
    {
      std::vector<cwidget::util::ref_ptr<changelog_entry> > entries;

      changelog(FileFd &file);

    public:
      static cwidget::util::ref_ptr<changelog> create(FileFd &file)
      {
	return new changelog(file);
      }

      /** \brief The type of an iterator over this changelog. */
      typedef std::vector<cwidget::util::ref_ptr<changelog_entry> >::const_iterator const_iterator;
      typedef std::vector<cwidget::util::ref_ptr<changelog_entry> >::size_type size_type;

      size_type size() const { return entries.size(); }
      const_iterator begin() const { return entries.begin(); }
      const_iterator end() const { return entries.end(); }
    };

    /** \brief Given a Debian changelog, parse it and generate a new
     *  file containing the changelog in an RFC822-style format.
     */
    temp::name digest_changelog(const temp::name &changelog,
				const std::string &from);

    /** \brief Given a digested changelog as produced by
     *  digest_changelog, parse it into a changelog structure.
     */
    cwidget::util::ref_ptr<changelog> parse_digested_changelog(const temp::name &digested);

    /** Parse the contents of the given file as a Debian changelog.
     *  for some reason the file cannot be parsed, returns \b NULL.
     *
     *  \param file a temporary file object containing the changelog.  A
     *         second temporary file will be created in the parent
     *         directory of this file, in which an intermediate form of
     *         the changelog will be stored.
     *
     *  \param from   a string giving the earliest version
     *                that is to be included in the changelog.
     *                If non-empty, nothing earlier than this version
     *                will be returned.
     */
    cwidget::util::ref_ptr<changelog> parse_changelog(const temp::name &file,
						      const std::string &from = "");



    /** \brief Start parsing a changelog in the background.
     *
     *  \param name The name of the file in which to find the
     *              changelog.
     *  \param slot A slot to invoke when teh changelog is parsed.  It
     *              will be invoked in the main thread.
     *  \param from The first version to parse, or an empty string
     *              to start at the beginning of the changelog.
     *  \param to   The last version to parse, or an empty string
     *              to parse until the end of the changelog.
     *  \param source_package The name of the source package whose
     *                        changelog is being parsed.
     *  \param digested       True if the changelog was already
     *                        digested.
     *  \param post_thunk     A function that should be used to post
     *                        thunks to the main thread.
     */
    void parse_changelog_background(const temp::name &name,
				    safe_slot1<void, cwidget::util::ref_ptr<aptitude::apt::changelog> > slot,
				    const std::string &from,
				    const std::string &to,
				    const std::string &source_package,
				    bool digested,
				    post_thunk_f post_thunk);
  }
}

#endif
