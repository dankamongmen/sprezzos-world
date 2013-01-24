// cmdline_changelog.cc
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
#include "cmdline_changelog.h"

#include "cmdline_common.h"
#include "cmdline_main_loop.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"
#include "terminal.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_queue.h>
#include <generic/apt/pkg_changelog.h>

// System includes:
#include <apt-pkg/error.h>
#include <apt-pkg/metaindex.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/srcrecords.h>

#include <boost/format.hpp>
#include <boost/make_shared.hpp>
#include <boost/ref.hpp>

#include <sigc++/adaptors/bind.h>

#include <signal.h>
#include <stdlib.h>

#include <sstream>

using namespace std;

using aptitude::cmdline::create_terminal;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_metrics;
using boost::shared_ptr;

namespace
{

void set_name(temp::name n, temp::name *target)
{
  *target = n;
}

  // Temporary hack to display download progress for a single file.
  //
  // Eventually there should be a generic class to track the progress
  // of a group of downloads, but I don't have enough infrastructure
  // for that yet.
  class single_download_progress : public aptitude::download_callbacks
  {
    std::string description;
    bool quiet;
    shared_ptr<terminal_metrics> term_metrics;

  public:
    single_download_progress(const std::string &_description,
			     bool _quiet,
                             const shared_ptr<terminal_metrics> &_term_metrics)
      : description(_description),
        quiet(_quiet),
        term_metrics(_term_metrics)
    {
    }

    void partial_download(const temp::name &name,
			  unsigned long long currentSize,
			  unsigned long long totalSize)
    {
      // Hack: ripped this code from AcqTextStatus.

      if (quiet)
	return;

      const int screen_width = term_metrics->get_screen_width();

      unsigned long long TotalBytes = totalSize;
      unsigned long long CurrentBytes = currentSize;

      std::ostringstream bufferStream;

      bufferStream << long(double(CurrentBytes * 100.0) / double(TotalBytes));
      bufferStream << "% [" << description.c_str() << "]";

      std::string Buffer(bufferStream.str());

      sigset_t Sigs,OldSigs;
      sigemptyset(&Sigs);
      sigaddset(&Sigs,SIGWINCH);
      sigprocmask(SIG_BLOCK,&Sigs,&OldSigs);

      int Len = (int)Buffer.size();
      if (Len < screen_width)
	Buffer.insert(Buffer.end(), screen_width - Len, ' ');
      std::string BlankLine(screen_width, ' ');
      sigprocmask(SIG_SETMASK,&OldSigs,0);

      // Draw the current status
      if (Buffer.size() == BlankLine.size())
	std::cout << '\r' << Buffer << std::flush;
      else
	std::cout << '\r' << BlankLine << '\r' << Buffer << std::flush;
    }

    void success(const temp::name &filename)
    {
      if(!quiet)
	std::cout << "\r"
                  << std::string(term_metrics->get_screen_width(), ' ')
                  << "\r";

      std::cout << _("Get:") << " " << description << std::endl;
    }

    void failure(const std::string &msg)
    {
      if(!quiet)
	std::cout << "\r"
                  << std::string(term_metrics->get_screen_width(), ' ')
                  << "\r";

      std::cout << _("Err") << " " << description << std::endl;
    }
  };

  class changelog_download_callbacks : public single_download_progress
  {
    // Where to store the changelog file, if any.
    temp::name &out_changelog_file;

  public:
    changelog_download_callbacks(temp::name &_out_changelog_file,
				 const std::string &short_description,
                                 const shared_ptr<terminal_metrics> &term_metrics)
      : single_download_progress(short_description,
				 aptcfg->FindI("Quiet", 0) > 0,
                                 term_metrics),
	out_changelog_file(_out_changelog_file)
    {
    }

    void success(const temp::name &name)
    {
      single_download_progress::success(name);

      out_changelog_file = name;

      aptitude::cmdline::exit_main();
    }

    void failure(const std::string &msg)
    {
      single_download_progress::failure(msg);

      _error->Error(_("Changelog download failed: %s"), msg.c_str());

      out_changelog_file = temp::name();
      aptitude::cmdline::exit_main();
    }
  };

  void get_changelog(const pkgCache::VerIterator &ver,
		     temp::name &out_changelog_file,
                     const shared_ptr<terminal_metrics> &term_metrics)
  {
    const std::string short_description =
      (boost::format("Changelog of %s") % ver.ParentPkg().Name()).str();

    boost::shared_ptr<changelog_download_callbacks>
      callbacks = boost::make_shared<changelog_download_callbacks>(boost::ref(out_changelog_file),
								   short_description,
                                                                   term_metrics);

    get_changelog(aptitude::apt::changelog_info::create(ver),
		  callbacks,
		  aptitude::cmdline::post_thunk);

    aptitude::cmdline::main_loop();
  }

  /** \brief download a source package's changelog.
   *
   *  \param srcpkg the source package name
   *  \param ver the version of the source package
   *  \param section the section of the source package
   *  \param name the name of the package that the user provided
   *              (e.g., the binary package that the changelog command
   *               was executed on)
   *  \param out_changelog_file  Where to store the resulting file.
   *
   *  This routine exits once the download is complete.
   */
  void get_changelog_from_source(const std::string &srcpkg,
				 const std::string &ver,
				 const std::string &section,
				 const std::string &name,
				 temp::name &out_changelog_file,
                                 const shared_ptr<terminal_metrics> &term_metrics)
  {
    const std::string short_description =
      (boost::format("Changelog of %s") % name).str();

    boost::shared_ptr<changelog_download_callbacks>
      callbacks = boost::make_shared<changelog_download_callbacks>(boost::ref(out_changelog_file),
								   short_description,
                                                                   term_metrics);

    boost::shared_ptr<aptitude::apt::changelog_info>
      info = aptitude::apt::changelog_info::guess(srcpkg, ver, section, name);

    get_changelog(info,
		  callbacks,
		  aptitude::cmdline::post_thunk);

    aptitude::cmdline::main_loop();
  }


/** Try to find a particular package version without knowing the
 *  section that it occurs in.  The resulting name will be invalid if
 *  no changelog could be found.
 */
temp::name changelog_by_version(const std::string &pkg,
				const std::string &ver,
                                const shared_ptr<terminal_metrics> &term_metrics)
{
  // Try forcing the particular version that was
  // selected, using various sections.  FIXME: relies
  // on specialized knowledge about how get_changelog
  // works; in particular, that it only cares whether
  // "section" has a first component.

  temp::name rval;

  get_changelog_from_source(pkg, ver, "", pkg, rval, term_metrics);

  if(!rval.valid())
    {
      get_changelog_from_source(pkg, ver, "contrib/foo", pkg, rval, term_metrics);
    }

  if(!rval.valid())
    {
      get_changelog_from_source(pkg, ver, "non-free/foo", pkg, rval, term_metrics);
    }

  return rval;
}
}

void do_cmdline_changelog(const vector<string> &packages,
                          const shared_ptr<terminal_metrics> &term_metrics)
{
  const char *pager="/usr/bin/sensible-pager";

  if(access("/usr/bin/sensible-pager", X_OK)!=0)
    {
      _error->Warning(_("Can't execute sensible-pager, is this a working Debian system?"));

      pager=getenv("PAGER");

      if(pager==NULL)
	pager="more";
    }

  _error->PushToStack();
  for(vector<string>::const_iterator i=packages.begin(); i!=packages.end(); ++i)
    {
      // We need to do this because some code (see above) checks
      // PendingError to see whether everything is OK.
      _error->MergeWithStack();
      _error->PushToStack();
      string input=*i;

      cmdline_version_source source = cmdline_version_cand;
      string package, sourcestr;

      if(!cmdline_parse_source(input, source, package, sourcestr))
	continue;

      pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(package);

      temp::name filename;

      // For real packages/versions, we can do a sanity check on the
      // version and warn the user if it looks like it doesn't have a
      // corresponding source package.
      if(!pkg.end())
	{
	  pkgCache::VerIterator ver=cmdline_find_ver(pkg,
						     source, sourcestr);

	  if(!ver.end())
	    {
              get_changelog(ver, filename, term_metrics);
	    }
	}
      else
	{
	  aptitude::cmdline::source_package p =
	    aptitude::cmdline::find_source_package(package,
						   source,
						   sourcestr);

	  if(p.valid())
	    {
	      get_changelog_from_source(p.get_package(),
					p.get_version(),
					p.get_section(),
					p.get_package(),
					filename,
                                        term_metrics);
	    }
	}

      if(!filename.valid())
	_error->Error(_("Couldn't find a changelog for %s"), input.c_str());
      else
	// Run the user's pager.
	if(system((string(pager) + " " + filename.get_name()).c_str()) != 0)
          _error->Error(_("Couldn't run pager %s"), pager);
    }

  _error->MergeWithStack();
}

// TODO: fetch them all in one go.
int cmdline_changelog(int argc, char *argv[])
{
  shared_ptr<terminal_io> term = create_terminal();

  consume_errors();

  OpProgress progress;
  apt_init(&progress, false);

  if(_error->PendingError())
    return 100;

  vector<string> packages;
  for(int i=1; i<argc; ++i)
    packages.push_back(argv[i]);

  do_cmdline_changelog(packages, term);

  return _error->PendingError() ? 100 : 0;
}
