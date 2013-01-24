// cmdline_versions.cc
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


// Local includes:
#include "cmdline_versions.h"

#include "cmdline_progress_display.h"
#include "cmdline_search_progress.h"
#include "cmdline_util.h"
#include "terminal.h"

#include <aptitude.h>
#include <pkg_ver_item.h>
#include <load_sortpolicy.h>

#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/matching/serialize.h>
#include <generic/util/progress_info.h>
#include <generic/util/throttle.h>
#include <generic/views/progress.h>


// System includes:
#include <apt-pkg/error.h>
#include <apt-pkg/aptconfiguration.h>

#include <boost/format.hpp>
#include <boost/make_shared.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>

#include <sigc++/bind.h>

#include <cwidget/generic/util/ref_ptr.h>
#include <cwidget/generic/util/ssprintf.h>

#include <vector>

namespace cw = cwidget;
namespace m = aptitude::matching;

using aptitude::cmdline::create_progress_display;
using aptitude::cmdline::create_search_progress;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::lessthan_1st;
using aptitude::cmdline::package_results_lt;
using aptitude::cmdline::search_result_column_parameters;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_locale;
using aptitude::cmdline::terminal_metrics;
using aptitude::cmdline::terminal_output;
using aptitude::cmdline::version_results_eq;
using aptitude::cmdline::version_results_lt;
using aptitude::matching::serialize_pattern;
using aptitude::util::create_throttle;
using aptitude::util::progress_info;
using aptitude::util::throttle;
using aptitude::views::progress;
using boost::shared_ptr;

namespace
{
  /** \brief A rule for how to group version results.
   *
   * Converts versions to strings that are used as the unique name of
   * the group they belong to (using strings is theoretically slower,
   * but it insulates callers from the type of object that's actually
   * doing the grouping).  Also, knows how to format the header of a
   * group.
   */
  class version_group_by_policy
  {
  public:
    /** \brief Get the groups of a match against the given version.
     *
     *  \param ver    The version that was matched.
     *
     *  \param match  How the given version was matched.
     *
     *  \param output A vector in which to store the groups of this
     *                version.  Each version should get at least one
     *                group, but some might get more than one.  If no
     *                version is produced, the default group of
     *                "<none>" will be used.
     */
    virtual void get_groups(const pkgCache::VerIterator &ver,
                            const cw::util::ref_ptr<m::structural_match> &match,
                            std::vector<std::string> &output) = 0;

    /** \brief Format a header line for the given group. */
    virtual std::string format_header(const std::string &group) = 0;
  };

  /** \brief Group versions by their package. */
  class version_group_by_package : public version_group_by_policy
  {
  public:
    void get_groups(const pkgCache::VerIterator &ver,
                    const cw::util::ref_ptr<m::structural_match> &match,
                    std::vector<std::string> &output)
    {
      output.push_back(ver.ParentPkg().FullName(true));
    }

    std::string format_header(const std::string &group)
    {
      return (boost::format(_("Package %s:")) % group).str();
    }
  };

  /** \brief Group versions by their source package. */
  class version_group_by_source_package : public version_group_by_policy
  {
  public:
    void get_groups(const pkgCache::VerIterator &ver,
                    const cw::util::ref_ptr<m::structural_match> &match,
                    std::vector<std::string> &output)
    {
      // I don't think FileList() *can* be invalid; this is just
      // paranoia.
      if(!ver.FileList().end())
        {
          std::string srcpkg = apt_package_records->Lookup(ver.FileList()).SourcePkg();

          if(srcpkg.empty())
            output.push_back(ver.ParentPkg().Name());
          else
            output.push_back(srcpkg);
        }
    }

    std::string format_header(const std::string &group)
    {
      return (boost::format(_("Source package %s:")) % group).str();
    }
  };

  /** \brief Group versions by their source version. */
  class version_group_by_source_version : public version_group_by_policy
  {
  public:
    void get_groups(const pkgCache::VerIterator &ver,
                    const cw::util::ref_ptr<m::structural_match> &match,
                    std::vector<std::string> &output)
    {
      // I don't think FileList() *can* be invalid; this is just
      // paranoia.
      if(!ver.FileList().end())
        {
          pkgRecords::Parser &rec = apt_package_records->Lookup(ver.FileList());

          std::string srcpkg = rec.SourcePkg();
          std::string srcver = rec.SourceVer();

          if(srcpkg.empty())
            srcpkg = ver.ParentPkg().Name();

          if(srcver.empty())
            srcver = ver.VerStr();

          std::string result;
          result.reserve(srcpkg.size() + srcver.size() + 1);
          result += srcpkg;
          result += " ";
          result += srcver;

          output.push_back(result);
        }
    }

    std::string format_header(const std::string &group)
    {
      return (boost::format(_("Source package %s:")) % group).str();
    }
  };

  /** \brief Group versions by their archive(s). */
  class version_group_by_archive : public version_group_by_policy
  {
  public:
    void get_groups(const pkgCache::VerIterator &ver,
                    const cw::util::ref_ptr<m::structural_match> &match,
                    std::vector<std::string> &output)
    {
      for(pkgCache::VerFileIterator vf = ver.FileList();
          !vf.end(); ++vf)
        if(vf.File().Archive() != NULL)
          output.push_back(vf.File().Archive());
        else
          output.push_back(_("<NULL>"));
    }

    std::string format_header(const std::string &group)
    {
      return (boost::format(_("Archive %s:")) % group).str();
    }
  };

  // Print the matches against a group of versions.
  void show_version_match_list(const std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > > &output,
                               const cw::config::column_definition_list &columns,
                               int format_width,
                               const unsigned int screen_width,
                               bool disable_columns,
                               bool show_package_names)
  {
    for(std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > >::const_iterator it = output.begin();
        it != output.end(); ++it)
      {
        boost::scoped_ptr<cw::config::column_parameters> p(new search_result_column_parameters(it->second));
        pkg_ver_columnizer columnizer(it->first,
                                      show_package_names,
                                      columns,
                                      0);
        if(disable_columns)
          printf("%ls\n", aptitude::cmdline::de_columnize(columns, columnizer, *p).c_str());
        else
          printf("%ls\n",
                 columnizer.layout_columns(format_width == -1 ? screen_width : format_width,
                                           *p).c_str());
      }
  }

  int do_search_versions(const std::vector<cw::util::ref_ptr<m::pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const cw::config::column_definition_list &columns,
                         int format_width,
                         const unsigned int screen_width,
                         bool disable_columns,
                         group_by_option group_by,
                         show_package_names_option show_package_names,
                         bool debug,
                         const shared_ptr<terminal_locale> &term_locale,
                         const shared_ptr<terminal_metrics> &term_metrics,
                         const shared_ptr<terminal_output> &term_output)
  {
    // Set to 100 if any exact-name matches fail.  Also set to 1 if
    // there are no results at all.
    int return_value = 0;

    typedef std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > >
      results_list;

    const shared_ptr<progress> search_progress_display =
      create_progress_display(term_locale, term_metrics, term_output);
    const shared_ptr<throttle> search_progress_throttle =
      create_throttle();

    results_list output;
    cw::util::ref_ptr<m::search_cache> search_info(m::search_cache::create());
    for(std::vector<cw::util::ref_ptr<m::pattern> >::const_iterator pIt = patterns.begin();
        pIt != patterns.end(); ++pIt)
      {
        const shared_ptr<progress> search_progress =
          create_search_progress(serialize_pattern(*pIt),
                                 search_progress_display,
                                 search_progress_throttle);

        std::size_t output_size = output.size();

        // Q: should I just wrap an ?or around them all?
        aptitude::matching::search_versions(*pIt,
                                            search_info,
                                            output,
                                            *apt_cache_file,
                                            *apt_package_records,
                                            debug,
                                            sigc::mem_fun(search_progress.get(),
                                                          &progress::set_progress));

        // Warn the user if an exact name pattern didn't produce a
        // result.
        if(output_size == output.size() &&
           (*pIt)->get_type() == m::pattern::exact_name)
          {
            return_value = 100;
            _error->Error(_("Unable to locate package %s"),
                          (*pIt)->get_exact_name_name().c_str());
          }
      }

    search_progress_display->done();

    if(output.empty() && return_value == 0)
      return_value = 1;

    // Decide how and whether to group the results.  Not initialized
    // so the compiler will check that we always assign a value.
    version_group_by_policy *group_by_policy;
    // Tracks whether package names should appear in the list if the
    // user wants us to automatically make that decision.
    bool package_names_should_auto_show;

    // HACK: on multiarch setups *always* assume more than one package
    // is returned.  Need a better way to count the packages returned,
    // but this works for now.
    const bool is_multiarch =
      APT::Configuration::getArchitectures().size() > 1;

    const bool arguments_select_exactly_one_package_by_exact_name =
      !is_multiarch &&
      (patterns.size() == 1 &&
       patterns[0]->get_type() == m::pattern::exact_name);

    switch(group_by)
      {
      case group_by_auto:
        if(!arguments_select_exactly_one_package_by_exact_name)
          group_by_policy = new version_group_by_package;
        else
          group_by_policy = NULL;

        // Note that in *both* cases, we don't need to show packages;
        // either we're grouping by packages, or only one package can
        // appear.
        package_names_should_auto_show = false;
        break;

      case group_by_archive:
        group_by_policy = new version_group_by_archive;
        package_names_should_auto_show =
          !arguments_select_exactly_one_package_by_exact_name;
        break;

      case group_by_none:
        group_by_policy = NULL;
        package_names_should_auto_show =
          !arguments_select_exactly_one_package_by_exact_name;
        break;

      case group_by_package:
        group_by_policy = new version_group_by_package;
        package_names_should_auto_show = false;
        break;

      case group_by_source_package:
        group_by_policy = new version_group_by_source_package;
        package_names_should_auto_show =
          !arguments_select_exactly_one_package_by_exact_name;
        break;

      case group_by_source_version:
        group_by_policy = new version_group_by_source_version;
        package_names_should_auto_show =
          !arguments_select_exactly_one_package_by_exact_name;
        break;

      default:
        _error->Error("Internal error: bad group-by value.");
        group_by_policy = NULL;
        package_names_should_auto_show =
          !arguments_select_exactly_one_package_by_exact_name;
        break;
      }

    // Now decide whether to show package names.
    bool do_show_package_names;
    switch(show_package_names)
      {
      case show_package_names_always:
        do_show_package_names = true;
        break;

      case show_package_names_auto:
        do_show_package_names = package_names_should_auto_show;
        break;

      case show_package_names_never:
        do_show_package_names = false;
        break;

      default:
        _error->Error("Internal error: invalid show-package-names option");
        do_show_package_names = package_names_should_auto_show;
        break;
      }

    _error->DumpErrors();

    // NB: sort the big list of results by version first so that we
    // don't have to sort lots of little lists later.  The code below
    // very carefully builds a list of the versions of each package in
    // a stable way, so the versions will continue to be in order.
    std::sort(output.begin(), output.end(), version_results_lt(sort_policy));
    output.erase(std::unique(output.begin(), output.end(), version_results_eq(sort_policy)),
                 output.end());

    if(group_by_policy != NULL)
      {
        typedef boost::unordered_map<std::string, boost::shared_ptr<results_list> >
          results_by_group_map;

        results_by_group_map by_groups;

        {
          // Avoid excessively allocating lots of short vectors.
          //
          // Possibly an excessive optimization.  This could be moved
          // to where groups.clear() happens.
         std::vector<std::string> groups;
         for(results_list::const_iterator results_it = output.begin();
             results_it != output.end(); ++results_it)
           {
             groups.clear();
             group_by_policy->get_groups(results_it->first, results_it->second, groups);
             if(groups.empty())
               // Shouldn't happen, but don't lose versions if it
               // does.
               groups.push_back("<none>");

             for(std::vector<std::string>::const_iterator groups_it =
                   groups.begin(); groups_it != groups.end(); ++groups_it)
               {
                 const std::string &group = *groups_it;
                 results_by_group_map::iterator found = by_groups.find(group);
                 if(found == by_groups.end())
                   {
                     boost::shared_ptr<results_list> cell = boost::make_shared<results_list>();

                     by_groups[group] = cell;
                     cell->push_back(*results_it);
                   }
                 else
                   found->second->push_back(*results_it);
               }
           }
        }

        typedef std::vector<std::pair<std::string, boost::shared_ptr<results_list> > >
          results_by_group_list;

        results_by_group_list by_groups_list(by_groups.begin(), by_groups.end());
        std::sort(by_groups_list.begin(), by_groups_list.end(),
                  lessthan_1st());

        for(results_by_group_list::const_iterator it = by_groups_list.begin();
            it != by_groups_list.end(); ++it)
          {
            if(it != by_groups_list.begin())
              printf("\n");
            printf("%s\n", group_by_policy->format_header(it->first).c_str());
            // No need to sort the versions in this list since we
            // sorted them above.
            show_version_match_list(*it->second,
                                    columns,
                                    format_width,
                                    screen_width,
                                    disable_columns,
                                    do_show_package_names);
          }
      }
    else
      show_version_match_list(output,
                              columns,
                              format_width,
                              screen_width,
                              disable_columns,
                              do_show_package_names);

    return return_value;
  }
}

GroupByParseException::GroupByParseException(const std::string &_msg)
  : msg(_msg)
{
}

GroupByParseException::~GroupByParseException() throw ()
{
}

const char *GroupByParseException::what() const throw ()
{
  return msg.c_str();
}

group_by_option parse_group_by_option(const std::string &option)
{
  // Translators: if you add synonyms to the possible values here,
  // please also use the translations in your manpage and in the error
  // string below.
  if(option == "archive" ||
     option == P_("--group-by|archive"))
    return group_by_archive;

  else if(option == "auto" ||
          option == P_("--group-by|auto"))
    return group_by_auto;

  else if(option == "none" ||
          option == P_("--group-by|none"))
    return group_by_none;

  else if(option == "package" ||
          option == P_("--group-by|package"))
    return group_by_package;

  else if(option == "source-package" ||
          option == P_("--group-by|source-package"))
    return group_by_source_package;

  else if(option == "source-version" ||
          option == P_("--group-by|source-version"))
    return group_by_source_version;

  else
    // ForTranslators: --group-by is the argument name and shouldn't
    // be translated.
    throw GroupByParseException((boost::format(_("Invalid package grouping mode \"%s\" (should be \"auto\", \"none\", \"package\", or \"source-package\")"))
                                 % option).str());
}

int cmdline_versions(int argc, char *argv[], const char *status_fname,
                     std::string display_format, std::string width,
                     std::string sort, bool disable_columns, bool debug,
                     group_by_option group_by,
                     show_package_names_option show_package_names)
{
  shared_ptr<terminal_io> term = create_terminal();

  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *sort_policy = parse_sortpolicy(sort);

  if(!sort_policy)
    return 100;

  consume_errors();

  const unsigned int screen_width = term->get_screen_width();
  if(!width.empty())
    {
      unsigned long tmp = screen_width;
      StrToNum(width.c_str(), tmp, width.size());
      real_width = tmp;
    }

  std::wstring wdisplay_format;

  if(!cw::util::transcode(display_format.c_str(), wdisplay_format))
    {
      _error->Error(_("iconv of %s failed"), display_format.c_str());
      return 100;
    }

  boost::scoped_ptr<cw::config::column_definition_list> columns;
  columns.reset(parse_columns(wdisplay_format,
                              pkg_item::pkg_columnizer::parse_column_type,
                              pkg_item::pkg_columnizer::defaults));

  if(columns.get() == NULL)
    return 100;

  if(argc <= 1)
    {
      _error->Error(_("versions: You must provide at least one package selector"));
      return 100;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    return 100;

  std::vector<cw::util::ref_ptr<m::pattern> > matchers;

  bool parsing_arguments_failed = false;

  for(int i = 1; i < argc; ++i)
    {
      const char * const arg = argv[i];

      cw::util::ref_ptr<m::pattern> m;
      const pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(arg);
      if(pkg.end() == false)
        {
          std::string name(arg);
          const std::string::size_type archfound = name.find_last_of(':');
          if(archfound != std::string::npos)
            {
              const std::string arch = name.substr(archfound + 1);
              name.erase(archfound);
              m = m::pattern::make_and(m::pattern::make_exact_name(pkg.Name()),
                                       m::pattern::make_architecture(arch));
            }
          else
            {
              m = m::pattern::make_exact_name(pkg.Name());
            }
        }
      else if(m::is_pattern(arg) == true)
        m = m::parse(arg);
      else
        _error->Error(_("Unable to locate package %s"), arg);

      if(m.valid() == false)
        parsing_arguments_failed = true;
      else
        matchers.push_back(m);
    }

  if(parsing_arguments_failed == true)
    return 100;

  return do_search_versions(matchers,
                            sort_policy,
                            *columns,
                            real_width,
                            screen_width,
                            disable_columns,
                            group_by,
                            show_package_names,
                            debug,
                            term,
                            term,
                            term);
}
