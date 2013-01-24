// cmdline_search.h                         -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SEARCH_H
#define CMDLINE_SEARCH_H

#include <string>

/** \file cmdline_search.h
 */


/** \brief Invoke the "search" and "versions" command-line actions.
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
 */
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   std::string display_format, std::string width, std::string sort,
		   bool disable_columns, bool debug);

#endif // CMDLINE_SEARCH_H
