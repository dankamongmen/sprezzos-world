// cmdline_extract_cache_subset.cc
//
//   Copyright (C) 2008-2010 Daniel Burrows
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


// Local includes:
#include "cmdline_extract_cache_subset.h"

#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/dump_packages.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/util/util.h>


// System includes:
#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>

#include <stdio.h>

using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using boost::shared_ptr;

namespace aptitude
{
  namespace cmdline
  {
    int extract_cache_subset(int argc, char *argv[])
    {
      if(argc < 2)
	{
          _error->Error(_("extract-cache-entries: at least one argument is"
                          " required (the directory to which to write files)"));
          return 100;
	}

      const shared_ptr<terminal_io> term = create_terminal();

      std::string out_dir = argv[1];

      boost::shared_ptr<OpProgress> progress = make_text_progress(false, term, term, term);

      apt_init(progress.get(), true);
      if(_error->PendingError())
        return 100;

      pkgset packages;
      if(argc == 2)
	{
	  for(pkgCache::PkgIterator pIt = (*apt_cache_file)->PkgBegin();
	      !pIt.end(); ++pIt)
	    packages.insert(pIt);
	}
      else
	{
	  for(int i = 2; i < argc; ++i)
            pkgset_from_string(&packages, argv[i]);
	}

      if(_error->PendingError() == true)
        return 100;

      if(packages.size() == 0)
        {
          printf(_("No packages were selected by the given search pattern;"
                   " nothing to do.\n"));
          return 0;
        }

      aptitude::apt::make_truncated_state_copy(out_dir, packages);

      return _error->PendingError() == true ? 100 : 0;
    }
  }
}
