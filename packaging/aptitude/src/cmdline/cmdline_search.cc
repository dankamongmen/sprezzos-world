// cmdline_search.cc
//
//
// Copyright (C) 2004, 2010 Daniel Burrows
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
#include "cmdline_search.h"

#include "cmdline_common.h"
#include "cmdline_progress_display.h"
#include "cmdline_search_progress.h"
#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>
#include <load_sortpolicy.h>
#include <loggers.h>
#include <pkg_columnizer.h>
#include <pkg_item.h>
#include <pkg_sortpolicy.h>

#include <generic/apt/apt.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/matching/serialize.h>
#include <generic/util/progress_info.h>
#include <generic/util/throttle.h>
#include <generic/views/progress.h>

#include <cwidget/config/column_definition.h>
#include <cwidget/generic/util/transcode.h>


// System includes:
#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <boost/format.hpp>
#include <boost/scoped_ptr.hpp>

#include <sigc++/bind.h>

#include <algorithm>

using namespace std;
namespace cw = cwidget;
using aptitude::Loggers;
using aptitude::cmdline::create_search_progress;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_locale;
using aptitude::cmdline::terminal_metrics;
using aptitude::cmdline::terminal_output;
using aptitude::matching::serialize_pattern;
using aptitude::util::create_throttle;
using aptitude::util::progress_info;
using aptitude::util::progress_type_bar;
using aptitude::util::progress_type_none;
using aptitude::util::progress_type_pulse;
using aptitude::util::throttle;
using aptitude::views::progress;
using boost::format;
using boost::shared_ptr;
using cwidget::util::ref_ptr;
using cwidget::util::transcode;
using namespace aptitude::matching;
using namespace cwidget::config;

namespace
{
  int do_search_packages(const std::vector<ref_ptr<pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const column_definition_list &columns,
                         int format_width,
                         const unsigned int screen_width,
                         bool disable_columns,
                         bool debug,
                         const shared_ptr<terminal_locale> &term_locale,
                         const shared_ptr<terminal_metrics> &term_metrics,
                         const shared_ptr<terminal_output> &term_output)
  {
    const shared_ptr<progress> search_progress_display =
      create_progress_display(term_locale, term_metrics, term_output);
    const shared_ptr<throttle> search_progress_throttle =
      create_throttle();

    pkg_results_list output;
    ref_ptr<search_cache> search_info(search_cache::create());
    for(std::vector<ref_ptr<pattern> >::const_iterator pIt = patterns.begin();
        pIt != patterns.end(); ++pIt)
      {
        const shared_ptr<progress> search_progress =
          create_search_progress(serialize_pattern(*pIt),
                                 search_progress_display,
                                 search_progress_throttle);

        // Q: should I just wrap an ?or around them all?
        aptitude::matching::search(*pIt,
                                   search_info,
                                   output,
                                   *apt_cache_file,
                                   *apt_package_records,
                                   debug,
                                   sigc::mem_fun(*search_progress,
                                                 &progress::set_progress));
      }

    search_progress_display->done();

    std::sort(output.begin(), output.end(),
              aptitude::cmdline::package_results_lt(sort_policy));
    output.erase(std::unique(output.begin(), output.end(),
                             aptitude::cmdline::package_results_eq(sort_policy)),
                 output.end());

    for(pkg_results_list::const_iterator it = output.begin(); it != output.end(); ++it)
      {
        column_parameters *p =
          new aptitude::cmdline::search_result_column_parameters(it->second);
        pkg_item::pkg_columnizer columnizer(it->first,
                                            it->first.VersionList(),
                                            columns,
                                            0);
        if(disable_columns)
          printf("%ls\n", aptitude::cmdline::de_columnize(columns, columnizer, *p).c_str());
        else
          printf("%ls\n",
                 columnizer.layout_columns(format_width == -1 ? screen_width : format_width,
                                           *p).c_str());

        // Note that this deletes the whole result, so we can't re-use
        // the list.
        delete p;
      }

    return output.empty() ? 1 : 0;
  }
}

// FIXME: apt-cache does lots of tricks to make this fast.  Should I?
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   string display_format, string width, string sort,
		   bool disable_columns, bool debug)
{
  shared_ptr<terminal_io> term = create_terminal();

  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *s=parse_sortpolicy(sort);

  if(s == NULL)
    return 100;

  consume_errors();

  const unsigned int screen_width = term->get_screen_width();
  if(!width.empty())
    {
      unsigned long tmp=screen_width;
      StrToNum(width.c_str(), tmp, width.size());
      real_width=tmp;
    }

  wstring wdisplay_format;

  if(!cw::util::transcode(display_format.c_str(), wdisplay_format))
    {
      _error->Error(_("iconv of %s failed"), display_format.c_str());
      return 100;
    }

  boost::scoped_ptr<column_definition_list> columns;
  columns.reset(parse_columns(wdisplay_format,
                              pkg_item::pkg_columnizer::parse_column_type,
                              pkg_item::pkg_columnizer::defaults));

  if(columns.get() == NULL)
    return 100;

  if(argc<=1)
    {
      _error->Error(_("search: You must provide at least one search term"));
      return 100;
    }

  shared_ptr<OpProgress> progress =
    make_text_progress(true, term, term, term);

  apt_init(progress.get(), true, status_fname);

  if(_error->PendingError())
    return 100;

  vector<ref_ptr<pattern> > matchers;

  for(int i=1; i<argc; ++i)
    {
      const char * const arg = argv[i];

      ref_ptr<pattern> m = parse(arg);
      if(!m.valid())
        return 100;

      matchers.push_back(m);
    }

  return do_search_packages(matchers,
                            s,
                            *columns,
                            real_width,
                            screen_width,
                            disable_columns,
                            debug,
                            term,
                            term,
                            term);
}
