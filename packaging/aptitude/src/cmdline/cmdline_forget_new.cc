// cmdline_forget_new.cc
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
#include "cmdline_forget_new.h"

#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>


// System includes:
#include <apt-pkg/error.h>

#include <stdio.h>

using namespace std;

using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using boost::shared_ptr;

int cmdline_forget_new(int argc, char *argv[],
		       const char *status_fname, bool simulate)
{
  const shared_ptr<terminal_io> term = create_terminal();

  consume_errors();

  // NB: perhaps we should allow forgetting the new state of just
  // a few packages?
  if(argc != 1)
    {
      _error->Error(_("The forget-new command takes no arguments"));
      return 100;
    }  

  shared_ptr<OpProgress> progress = make_text_progress(false, term, term, term);

  apt_init(progress.get(), false, status_fname);

  if(_error->PendingError())
    return 100;

  // In case we aren't root.
  if(!simulate)
    apt_cache_file->GainLock();
  else
    apt_cache_file->ReleaseLock();

  if(_error->PendingError())
    return 100;

  if(simulate)
    printf(_("Would forget what packages are new\n"));
  else
    {
      (*apt_cache_file)->forget_new(NULL);

      (*apt_cache_file)->save_selection_list(*progress);

      if(_error->PendingError())
        return 100;
    }

  return 0;
}

