// cmdline_download.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_download.h"

#include "cmdline_common.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/pkg_acqfile.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>

#include <stdio.h>

using aptitude::controllers::acquire_download_progress;
using aptitude::cmdline::create_cmdline_download_progress;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_locale;
using boost::shared_ptr;

// Download stuff to the current directory
int cmdline_download(int argc, char *argv[])
{
  shared_ptr<terminal_io> term = create_terminal();

  if(argc<=1)
    {
      _error->Error(_("download: you must specify at least one package to download"));
      return 100;
    }

  consume_errors();

  shared_ptr<OpProgress> progress = make_text_progress(false, term, term, term);
  apt_init(progress.get(), false);

  if(_error->PendingError())
    return 100;

  pkgSourceList list;
  if(!list.ReadMainList())
    {
      _error->Error(_("Couldn't read source list"));
      return 100;
    }

  std::pair<download_signal_log *, boost::shared_ptr<acquire_download_progress> >
    progress_display = create_cmdline_download_progress(term, term, term, term);

  pkgAcquire fetcher;
  fetcher.Setup(progress_display.first);
  string filenames[(*apt_cache_file)->Head().PackageCount];

  for(int i=1; i<argc; ++i)
    {
      cmdline_version_source source = cmdline_version_cand;
      string name, sourcestr;
      if(!cmdline_parse_source(argv[i], source, name, sourcestr))
	continue;

      pkgset packages;
      if(aptitude::cmdline::pkgset_from_string(&packages, name) == false)
        continue;

      for(pkgset::const_iterator it = packages.begin();
          it != packages.end();
          ++it)
	{
	  const pkgCache::PkgIterator pkg = *it;

	  pkgCache::VerIterator ver=cmdline_find_ver(pkg, source, sourcestr);

	  if(ver.end())
	    continue;

	  if(!ver.Downloadable())
            _error->Error(_("No downloadable files for %s version %s;"
                            " perhaps it is a local or obsolete package?"),
			  name.c_str(), ver.VerStr());

	  get_archive(&fetcher, &list, apt_package_records,
		      ver, ".", filenames[pkg->ID]);
	}
    }

  if(fetcher.Run()!=pkgAcquire::Continue)
    // We failed or were cancelled
    return 100;

  return 0;
}
