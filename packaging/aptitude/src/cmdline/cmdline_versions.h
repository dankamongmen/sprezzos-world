// \file cmdline_versions.h          -*-c++-*-
//
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

#ifndef CMDLINE_VERSIONS_H
#define CMDLINE_VERSIONS_H

#include <string>

/** \brief Represents the possible values of the "group-by"
 *  command-line option.
 */
enum group_by_option
  {
    /** \brief Group by archive ("stable", "unstable", etc). */
    group_by_archive,

    /** \brief Group by package unless there is exactly one pattern
     *  AND that pattern matches a single package exactly by name, in
     *  which case don't group at all.
     *
     *  This is the default value.  It means that
     *  "aptitude versions foo" shows the versions of "foo" without
     *  grouping, but a more complex search performs the grouping
     *  automatically.
     */
    group_by_auto,

    /** \brief No grouping. */
    group_by_none,

    /** \brief Group by package. */
    group_by_package,

    /** \brief Group by source package. */
    group_by_source_package,

    /** \brief Group by source package and version. */
    group_by_source_version,
  };

class GroupByParseException : public std::exception
{
  std::string msg;
public:
  GroupByParseException(const std::string &_msg);
  ~GroupByParseException() throw ();

  const char *what() const throw();
};

/** \brief Parse the argument to the "group-by" command-line option.
 *
 *  \note I do this instead of parsing all the way to the version
 *  grouping policy because the actual value of this option matters in
 *  multiple places in cmdline_versions(), in ways that I don't want
 *  to hide behind an abstract interface.
 *
 *  \param option   The option value to parse.
 *
 *  \throw GroupByParseException if the given option value can't be
 *  parsed.
 */
group_by_option parse_group_by_option(const std::string &option);

/** \brief Represents the possible values of the "show-package-names"
 *  command-line option.
 *
 *  This controls whether versions are listed with their package name
 *  or not.
 */
enum show_package_names_option
  {
    /** \brief Always show package names. */
    show_package_names_always,

    /** \brief Don't show package names if either there was only one
     *  input pattern and it selected a package by name, or results
     *  are being grouped by package; otherwise show them.
     *
     *  This is the default.
     */
    show_package_names_auto,

    /** \brief Don't show package names. */
    show_package_names_never,
  };

/** \brief Invoke the "versions" command-line action.
 *
 *  \param argc             The number of entries in argv.
 *
 *  \param argv             The command-line arguments not parsed by main().
 *
 *  \param status_fname     If non-NULL, a file-name from which to read status
 *                          information.
 *
 *  \param display_format   A string describing the columns to use in displaying
 *                          results.
 *
 *  \param width            The width to use in formatting results.
 *
 *  \param sort             A string describing how to sort results, using the
 *                          same syntax as the curses frontend.
 *
 *  \param disable_columns  \b true to disable columnar formatting and simply
 *                          separate fields with whitespace.
 *
 *  \param debug            \b true to print debugging information to stdout.
 *                          \todo  Should be handled by the logging subsystem.
 *
 *  \param group_by         Controls how to group versions; see group_by_option.
 *
 *  \param show_package_names  Controls whether package names are displayed; see
 *                             show_package_names_option.
 */
int cmdline_versions(int argc, char *argv[], const char *status_fname,
                     std::string display_format, std::string width,
                     std::string sort, bool disable_columns, bool debug,
                     group_by_option group_by,
                     show_package_names_option show_package_names);

#endif // CMDLINE_VERSIONS_H
