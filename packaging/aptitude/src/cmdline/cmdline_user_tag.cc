// cmdline_user_tag.cc
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
#include "cmdline_user_tag.h"

#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptcache.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/util/util.h>

#include <stdio.h>
#include <string.h>

using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using boost::shared_ptr;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      enum user_tag_action { action_add, action_remove };

      void do_user_tag(user_tag_action act,
		       const std::string &tag,
		       const pkgCache::PkgIterator &pkg,
		       int verbose)
      {
	switch(act)
	  {
	  case action_add:
	    if(verbose > 0)
				// Sometimes also "user-tag" is used!
	      printf(_("Adding user tag \"%s\" to the package \"%s\".\n"),
		     tag.c_str(), pkg.Name());

	    (*apt_cache_file)->attach_user_tag(pkg, tag, NULL);
	    break;
	  case action_remove:
	    if(verbose > 0)
	      printf(_("Removing user tag \"%s\" from the package \"%s\".\n"),
		     tag.c_str(), pkg.Name());

	    (*apt_cache_file)->detach_user_tag(pkg, tag, NULL);
	    break;
	  default:
	    fprintf(stderr, "Internal error: bad user tag action %d.", act);
	    break;
	  }
      }
    }

    int cmdline_user_tag(int argc, char *argv[], int quiet, int verbose)
    {
      const shared_ptr<terminal_io> term = create_terminal();

      user_tag_action action = (user_tag_action)-1;

      if(strcmp(argv[0], "add-user-tag") == 0)
	action = action_add;
      else if(strcmp(argv[0], "remove-user-tag") == 0)
	action = action_remove;
      else
	{
          _error->Error("Internal error: cmdline_user_tag encountered an"
                        " unknown command name \"%s\"\n",
                        argv[0]);
	  return 100;
	}

      if(argc < 3)
	{
          _error->Error(_("%s: too few arguments; expected at least a tag"
                          " name and a package.\n"),
                        argv[0]);
	  return 100;
	}

      consume_errors();

      OpProgress progress;

      apt_init(&progress, true);
      if(_error->PendingError())
        return 100;

      std::string tag(argv[1]);

      pkgset pkgset;
      for(int i = 2; i < argc; ++i)
        pkgset_from_string(&pkgset, argv[i]);

      if(pkgset.empty() == true)
        {
          _error->Error(_("No packages found"));
          return 100;
        }

      for(pkgset::const_iterator it = pkgset.begin();
          it != pkgset.end();
          ++it)
        do_user_tag(action, tag, *it, verbose);

      shared_ptr<OpProgress> text_progress = make_text_progress(false, term, term, term);
      if(!(*apt_cache_file)->save_selection_list(*text_progress))
	return 100;

      return _error->PendingError() == true ? 100 : 0;
    }
  }
}
