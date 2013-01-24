// main.cc  (neé testscr.cc)
//
//  Copyright (C) 1999-2011 Daniel Burrows
//  Copyright (C) 2012 Daniel Hartwig
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  Tests the various screen-output mechanisms

#include <signal.h>

#include "aptitude.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/problemresolver/exceptions.h>

#include <generic/util/logging.h>
#include <generic/util/temp.h>
#include <generic/util/util.h>

#ifdef HAVE_GTK
#include <gtkmm.h>
#endif

#ifdef HAVE_QT
#include <QtCore/qglobal.h> // To get the Qt version number
#endif

#include <cwidget/config/keybindings.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/dialogs.h>

#include <cmdline/cmdline_changelog.h>
#include <cmdline/cmdline_check_resolver.h>
#include <cmdline/cmdline_clean.h>
#include <cmdline/cmdline_common.h>
#include <cmdline/cmdline_do_action.h>
#include <cmdline/cmdline_download.h>
#include <cmdline/cmdline_dump_resolver.h>
#include <cmdline/cmdline_extract_cache_subset.h>
#include <cmdline/cmdline_forget_new.h>
#include <cmdline/cmdline_moo.h>
#include <cmdline/cmdline_prompt.h>
#include <cmdline/cmdline_search.h>
#include <cmdline/cmdline_show.h>
#include <cmdline/cmdline_update.h>
#include <cmdline/cmdline_user_tag.h>
#include <cmdline/cmdline_versions.h>
#include <cmdline/cmdline_why.h>
#include <cmdline/terminal.h>

#include <sigc++/functors/ptr_fun.h>

#include <apt-pkg/error.h>
#include <apt-pkg/cmndline.h>
#include <apt-pkg/init.h>

#include <boost/format.hpp>
#include <boost/optional.hpp>

#ifdef HAVE_GTK
#include "gtk/gui.h"
#include "gtk/init.h"
#endif

#ifdef HAVE_QT
#include "qt/qt_main.h"
#endif

#include <fstream>

#include "loggers.h"
#include "progress.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_view.h"
#include "ui.h"

namespace cw = cwidget;

using aptitude::Loggers;

using boost::optional;

using logging::DEBUG_LEVEL;
using logging::ERROR_LEVEL;
using logging::FATAL_LEVEL;
using logging::INFO_LEVEL;
using logging::OFF_LEVEL;
using logging::WARN_LEVEL;
using logging::TRACE_LEVEL;

using logging::Logger;
using logging::LoggerPtr;
using logging::log_level;

#if 0
// These are commented out so as to not punish users unduly for coding
// errors.  The cw::util::transcoder now substitutes conspicuous '?' characters
// into its output, which should be enough of a clue.


/** Handles a coding error using the apt error mechanism. */
std::wstring handle_mbtow_error(int error,
				const std::wstring &partial,
				const std::string &input)
{
  _error->Errno("iconv", _("Can't decode multibyte string after \"%ls\""),
		partial.c_str());
  return partial;
}

std::string handle_wtomb_error(int error,
			       const std::string &partial,
			       const std::wstring &input)
{
  _error->Errno("iconv", _("Can't decode wide-character string after \"%s\""),
		partial.c_str());
  return partial;
}
#endif

static void show_version()
{
  printf(_("%s %s compiled at %s %s\n"),
	   PACKAGE, VERSION, __DATE__, __TIME__);
#ifdef __GNUC__
  printf(_("Compiler: g++ %s\n"), __VERSION__);
#endif
  printf("%s", _("Compiled against:\n"));
  printf(_("  apt version %d.%d.%d\n"),
	 APT_PKG_MAJOR, APT_PKG_MINOR, APT_PKG_RELEASE);
#ifndef NCURSES_VERSION
  printf(_("  NCurses version: Unknown\n"));
#else
  printf(_("  NCurses version %s\n"), NCURSES_VERSION);
#endif
  printf(_("  libsigc++ version: %s\n"), SIGC_VERSION);
#ifdef HAVE_EPT
  printf(_("  Ept support enabled.\n"));
#else
  printf(_("  Ept support disabled.\n"));
#endif
#ifdef HAVE_GTK
  printf(_("  Gtk+ version %d.%d.%d\n"),
	 GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
  printf(_("  Gtk-- version %d.%d.%d\n"),
	 GTKMM_MAJOR_VERSION, GTKMM_MINOR_VERSION, GTKMM_MICRO_VERSION);
#else
  printf(_("  Gtk+ support disabled.\n"));
#endif
#ifdef HAVE_QT
  printf(_("  Compiled with Qt %s\n"), QT_VERSION_STR);
  printf(_("  Running on Qt %s\n"), qVersion());
#else
  printf(_("  Qt support disabled.\n"));
#endif
  printf("%s", _("\nCurrent library versions:\n"));
  printf(_("  NCurses version: %s\n"), curses_version());
  printf(_("  cwidget version: %s\n"), cwidget::version().c_str());
  printf(_("  Apt version: %s\n"), pkgLibVersion);
}

static void usage()
{
  printf(PACKAGE " " VERSION "\n");
  printf(_("Usage: aptitude [-S fname] [-u|-i]"));
  printf("\n");
  printf(_("       aptitude [options] <action> ..."));
  printf("\n");
  printf(_("  Actions (if none is specified, aptitude will enter interactive mode):\n\n"));
  printf(_(" install      - Install/upgrade packages.\n"));
  printf(_(" remove       - Remove packages.\n"));
  printf(_(" purge        - Remove packages and their configuration files.\n"));
  printf(_(" hold         - Place packages on hold.\n"));
  printf(_(" unhold       - Cancel a hold command for a package.\n"));
  printf(_(" markauto     - Mark packages as having been automatically installed.\n"));
  printf(_(" unmarkauto   - Mark packages as having been manually installed.\n"));
  printf(_(" forbid-version - Forbid aptitude from upgrading to a specific package version.\n"));
  printf(_(" update       - Download lists of new/upgradable packages.\n"));
  printf(_(" safe-upgrade - Perform a safe upgrade.\n"));
  printf(_(" full-upgrade - Perform an upgrade, possibly installing and removing packages.\n"));
  printf(_(" build-dep    - Install the build-dependencies of packages.\n"));
  printf(_(" forget-new   - Forget what packages are \"new\".\n"));
  printf(_(" search       - Search for a package by name and/or expression.\n"));
  printf(_(" show         - Display detailed information about a package.\n"));
  printf(_(" versions     - Displays the versions of specified packages.\n"));
  printf(_(" clean        - Erase downloaded package files.\n"));
  printf(_(" autoclean    - Erase old downloaded package files.\n"));
  printf(_(" changelog    - View a package's changelog.\n"));
  printf(_(" download     - Download the .deb file for a package.\n"));
  printf(_(" reinstall    - Download and (possibly) reinstall a currently installed package.\n"));
  printf(_(" why          - Show the manually installed packages that require a package, or\n"
           "                why one or more packages would require the given package.\n"));
  printf(_(" why-not      - Show the manually installed packages that lead to a conflict\n"
           "                with the given package, or why one or more packages would\n"
           "                lead to a conflict with the given package if installed.\n"));
  printf("\n");
  printf(_("  Options:\n"));
  printf(_(" -h             This help text.\n"));
#ifdef HAVE_GTK
  printf(_(" --gui          Use the GTK GUI even if disabled in the configuration.\n"));
#endif
  printf(_(" --no-gui       Do not use the GTK GUI even if available.\n"));
#ifdef HAVE_QT
  printf(_(" --qt           Use the Qt GUI.\n"));
  printf(_(" --no-qt        Do not use the Qt GUI even if enabled in the configuration.\n"));
#endif
  printf(_(" -s             Simulate actions, but do not actually perform them.\n"));
  printf(_(" -d             Only download packages, do not install or remove anything.\n"));
  printf(_(" -P             Always prompt for confirmation of actions.\n"));
  printf(_(" -y             Assume that the answer to simple yes/no questions is 'yes'.\n"));
  printf(_(" -F format      Specify a format for displaying search results; see the manual.\n"));
  printf(_(" -O order       Specify how search results should be sorted; see the manual.\n"));
  printf(_(" -w width       Specify the display width for formatting search results.\n"));
  printf(_(" -f             Aggressively try to fix broken packages.\n"));
  printf(_(" -V             Show which versions of packages are to be installed.\n"));
  printf(_(" -D             Show the dependencies of automatically changed packages.\n"));
  printf(_(" -Z             Show the change in installed size of each package.\n"));
  printf(_(" -v             Display extra information. (may be supplied multiple times).\n"));
  printf(_(" -t [release]   Set the release from which packages should be installed.\n"));
  printf(_(" -q             In command-line mode, suppress the incremental progress\n"
           "                indicators.\n"));
  printf(_(" -o key=val     Directly set the configuration option named 'key'.\n"));
  printf(_(" --with(out)-recommends	Specify whether or not to treat recommends as\n"
           "                strong dependencies.\n"));
  printf(_(" -S fname       Read the aptitude extended status info from fname.\n"));
  printf(_(" -u             Download new package lists on startup.\n"));
  printf(_("                  (terminal interface only)\n"));
  printf(_(" -i             Perform an install run on startup.\n"));
  printf(_("                  (terminal interface only)\n"));
  printf("\n");
  printf(_("                  This aptitude does not have Super Cow Powers.\n"));
}

CommandLine::Args opts[] = {
  {'h', "help", "help", 0},
  {0, "version", "version", 0},
  {'q', "quiet", "quiet", CommandLine::IntLevel},
  {'F', "display-format", PACKAGE "::%::display-format", CommandLine::HasArg},
  {'w', "width", PACKAGE "::CmdLine::Package-Display-Width", CommandLine::HasArg},
  {'s', "simulate", PACKAGE "::Simulate", 0},
  {'r', "with-recommends", "APT::Install-Recommends", 0},
  {'R', "without-recommends", PACKAGE "::%::without-recommends", 0},
  {0, "allow-untrusted", PACKAGE "::CmdLine::Ignore-Trust-Violations", 0},
  {'d', "download-only", PACKAGE "::CmdLine::Download-Only", 0},
  {'y', "yes", PACKAGE "::CmdLine::Assume-Yes", 0},
  {'y', "assume-yes", PACKAGE "::CmdLine::Assume-Yes", 0},
  {'v', "verbose", PACKAGE "::CmdLine::Verbose", CommandLine::IntLevel},
  {'V', "show-versions", PACKAGE "::CmdLine::Show-Versions", 0},
  {'D', "show-deps", PACKAGE "::CmdLine::Show-Deps", 0},
  {'W', "show-why", PACKAGE "::CmdLine::Show-Why", 0},
  {'P', "prompt", PACKAGE "::CmdLine::Always-Prompt", 0},
  {'O', "sort", PACKAGE "::CmdLine::Sorting", CommandLine::HasArg},
  {'t', "target-release", "APT::Default-Release", CommandLine::HasArg},
  {'t', "default-release", "APT::Default-Release", CommandLine::HasArg},
  {0, "disable-columns", PACKAGE "::CmdLine::Disable-Columns", 0},
  {0, "no-new-installs", PACKAGE "::Safe-Resolver::No-New-Installs", 0},
  {0, "no-new-upgrades", PACKAGE "::Safe-Resolver::No-New-Upgrades", 0},
  {0, "allow-new-installs", PACKAGE "::Safe-Resolver::No-New-Installs", CommandLine::InvBoolean},
  {0, "allow-new-upgrades", PACKAGE "::Safe-Resolver::No-New-Upgrades", CommandLine::InvBoolean},
  {0, "safe-resolver", PACKAGE "::%::safe-resolver", CommandLine::Boolean},
  {0, "full-resolver", PACKAGE "::%::full-resolver", CommandLine::Boolean},
  {0, "show-resolver-actions", PACKAGE "::Safe-Resolver::Show-Resolver-Actions", 0},
  {0, "visual-preview", PACKAGE "::CmdLine::Visual-Preview", 0},
  {0, "schedule-only", PACKAGE "::CmdLine::Schedule-Only", 0},
  {0, "purge-unused", PACKAGE "::Purge-Unused", 0},
  {0, "add-user-tag", PACKAGE "::CmdLine::Add-User-Tag::", CommandLine::HasArg},
  {0, "remove-user-tag", PACKAGE "::CmdLine::Remove-User-Tag::", CommandLine::HasArg},
  {0, "add-user-tag-to", PACKAGE "::CmdLine::Add-User-Tag-To::", CommandLine::HasArg},
  {0, "remove-user-tag-from", PACKAGE "::CmdLine::Remove-User-Tag-From::", CommandLine::HasArg},
  {0, "arch-only", "APT::Get::Arch-Only", CommandLine::Boolean},
  {0, "not-arch-only", "APT::Get::Arch-Only", CommandLine::InvBoolean},
#ifdef HAVE_GTK
  {0, "gui", PACKAGE "::%::start-gui", CommandLine::Boolean},
  {0, "new-gui", PACKAGE "::%::new-gui", 0},
#endif
  {0, "no-gui", PACKAGE "::%::start-gui", CommandLine::InvBoolean},
#ifdef HAVE_QT
  {0, "qt", PACKAGE "::%::qt-gui", 0},
#endif
  {0, "log-level", PACKAGE "::Logging::Levels::", CommandLine::HasArg},
  {0, "log-file", PACKAGE "::Logging::File", CommandLine::HasArg},
  {0, "log-resolver", PACKAGE "::%::log-resolver", 0},
  {0, "show-summary", PACKAGE "::CmdLine::Show-Summary",  CommandLine::HasArg},
  {0, "autoclean-on-startup", PACKAGE "::UI::Autoclean-On-Startup", 0},
  {0, "clean-on-startup", PACKAGE "::UI::Clean-On-Startup", 0},
  {'u', "update-on-startup", PACKAGE "::UI::Update-On-Startup", 0},
  {'i', "install-on-startup", PACKAGE "::UI::Install-On-Startup", 0},
  {0, "group-by", PACKAGE "::CmdLine::Versions-Group-By", CommandLine::HasArg},
  {0, "show-package-names", PACKAGE "::CmdLine::Versions-Show-Package-Names", CommandLine::HasArg},
  {'f', "fix-broken", PACKAGE "::CmdLine::Fix-Broken", 0},
  {'m', "ignore-missing", PACKAGE "::CmdLine::Fix-Missing", 0},
  {0, "fix-missing", PACKAGE "::CmdLine::Fix-Missing", 0},
  {'Z', "show-size-changes", PACKAGE "::CmdLine::Show-Size-Changes", 0},
  {'S', "pkgstates", PACKAGE "::%::pkgstates.in", CommandLine::HasArg},
  {'c', "config-file", 0, CommandLine::ConfigFile},
  {'o', "option", 0, CommandLine::ArbItem},
  {0,0,0,0}
};

const char *argv0;

namespace
{
  bool strncase_eq_with_translation(const std::string &s1, const char *s2)
  {
    if(strcasecmp(s1.c_str(), s2) == 0)
      return true;
    else if(strcasecmp(s1.c_str(), _(s2)) == 0)
      return true;
    else
      return false;
  }

  class log_level_map
  {
    std::map<std::string, log_level> levels;

    void add_level(const std::string &s, log_level level)
    {
      std::string tmp;
      for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
	tmp.push_back(toupper(*it));

      levels[tmp] = level;
    }

  public:
    log_level_map()
    {
      using namespace logging;

      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("trace"), TRACE_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("debug"), DEBUG_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("info"), INFO_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("warn"), WARN_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("error"), ERROR_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("fatal"), FATAL_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("off"), OFF_LEVEL);

      std::vector<std::pair<std::string, log_level> >
	tmp(levels.begin(), levels.end());

      for(std::vector<std::pair<std::string, log_level> >::const_iterator
	    it = tmp.begin(); it != tmp.end(); ++it)
	{
	  // Make sure that the untranslated entries always override
	  // the translated ones so that instructions in English
	  // always work.
	  std::map<std::string, log_level>::const_iterator found =
	    levels.find(it->first);

	  if(found == levels.end())
	    add_level(_(it->first.c_str()), it->second);
	}
    }

    typedef std::map<std::string, log_level>::const_iterator const_iterator;

    const_iterator find(const std::string &s) const
    {
      std::string tmp;
      for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
	tmp.push_back(toupper(*it));

      return levels.find(tmp);
    }

    const_iterator end() const
    {
      return levels.end();
    }
  };

  log_level_map log_levels;

  /** \brief Parse a logging level.
   *
   *  Logging levels have the form [<logger>:]level, where
   *  <logger> is an optional logger name.
   */
  void apply_logging_level(const std::string &s)
  {
    std::string::size_type colon_loc = s.rfind(':');
    std::string level_name;
    std::string logger_name;

    if(colon_loc == std::string::npos)
      level_name = s;
    else
      {
	level_name = std::string(s, colon_loc + 1);
	logger_name = std::string(s, 0, colon_loc);
      }

    optional<log_level> level;

    log_level_map::const_iterator found =
      log_levels.find(level_name);
    if(found != log_levels.end())
      level = found->second;

    if(!level)
      {
	// ForTranslators: both the translated and the untranslated
	// log level names are accepted here.
        _error->Error(_("Unknown log level name \"%s\" (expected \"trace\","
                        " \"debug\", \"info\", \"warn\", \"error\","
                        " \"fatal\", or \"off\")."),
		      level_name.c_str());
	return;
      }

    LoggerPtr targetLogger = Logger::getLogger(logger_name);

    if(!targetLogger)
      {
	_error->Error(_("Invalid logger name \"%s\""),
		      logger_name.c_str());
	return;
      }

    targetLogger->setLevel(*level);
  }

  /** \brief Apply logging levels from the configuration file. */
  void apply_config_file_logging_levels(signalling_config *config)
  {
    const Configuration::Item *tree = config->Tree(PACKAGE "::Logging::Levels");
    if(tree == NULL)
      return;

    for(Configuration::Item *item = tree->Child; item != NULL;
	item = item->Next)
      apply_logging_level(item->Value);
  }

  /** \brief Read the default set of user tag application requests. */
  bool read_user_tag_applications(std::vector<aptitude::cmdline::tag_application> &user_tags)
  {
    struct tag_application_source
    {
      const char *config_item;
      bool is_add;
      bool implicit;
    };

    struct tag_application_source sources[] = {
      {PACKAGE "::CmdLine::Add-User-Tag", true, true},
      {PACKAGE "::CmdLine::Remove-User-Tag", false, true},
      {PACKAGE "::CmdLine::Add-User-Tag-To", true, false},
      {PACKAGE "::CmdLine::Remove-User-Tag-From", false, false},
      {0,0,0}
    };

    using aptitude::cmdline::read_user_tag_applications;
    for(size_t i = 0; sources[i].config_item != 0; ++i)
      {
        if(read_user_tag_applications(user_tags, sources[i].config_item,
                                      sources[i].is_add, sources[i].implicit) == false)
          return false;
      }

    return true;
  }

  /** \brief Set some standard logging levels to output log
   *  information about the resolver.
   *
   *  The information this generates is enough to let the log parser
   *  show a visualization of what happened.
   */
  void enable_resolver_log()
  {
    Loggers::getAptitudeResolverSearch()->setLevel(TRACE_LEVEL);
    Loggers::getAptitudeResolverSearchCosts()->setLevel(INFO_LEVEL);
  }
}

void do_message_logged(std::ostream &out,
                       const char *sourceFilename,
                       int sourceLineNumber,
                       log_level level,
                       LoggerPtr logger,
                       const std::string &msg)
{
  time_t current_time = 0;
  struct tm local_current_time;

  time(&current_time);
  localtime_r(&current_time, &local_current_time);

  out << sstrftime("%F %T", &local_current_time)
      << " [" << pthread_self() << "] "
      << sourceFilename << ":" << sourceLineNumber
      << " " << describe_log_level(level)
      << " " << logger->getCategory()
      << " - " << msg << std::endl << std::flush;
}

void handle_message_logged(const char *sourceFilename,
                           int sourceLineNumber,
                           log_level level,
                           LoggerPtr logger,
                           const std::string &msg,
                           const std::string &filename)
{
  if(filename == "-")
    {
      // HACK: Block logging to stdout if running in curses (c.f. problemresolver.h)
      if(cw::rootwin == (cw::cwindow) NULL)
        do_message_logged(std::cout,
                          sourceFilename,
                          sourceLineNumber,
                          level,
                          logger,
                          msg);
    }
  else
    {
      std::ofstream f(filename.c_str(), std::ios::app);
      if(f)
        do_message_logged(f,
                          sourceFilename,
                          sourceLineNumber,
                          level,
                          logger,
                          msg);
      // Since logging is just for debugging, I don't do anything if
      // the log file can't be opened.
    }
}

int main(int argc, const char *argv[])
{
  // Block signals that we want to sigwait() on by default and put the
  // signal mask into a known state.  This ensures that unless threads
  // deliberately ask for a signal, they don't get it, meaning that
  // sigwait() should work as expected.  (the alternative, blocking
  // all signals, is troublesome: we would have to ensure that fatal
  // signals and other things that shouldn't be blocked get removed)
  //
  // When aptitude used log4cxx, we ran into the fact that it doesn't
  // ensure that its threads don't block signals, so cwidget wasn't
  // able to sigwait() on SIGWINCH without this.  (cwidget is guilty
  // of the same thing, but that doesn't cause problems for aptitude)
  {
    sigset_t mask;

    sigemptyset(&mask);

    sigaddset(&mask, SIGWINCH);

    pthread_sigmask(SIG_BLOCK, &mask, NULL);
  }

  srandom(time(0));

  // See earlier note
  //
  //cw::util::transcode_mbtow_err=handle_mbtow_error;
  //cw::util::transcode_wtomb_err=handle_wtomb_error;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);

  // An environment variable is used mostly because of the utterly
  // lame option parsing that aptitude does.  A better option parser
  // would pre-parse the options and remember them in a structure of
  // some sort, meaning that I could parse the command-line options
  // before I set up the apt configuration structures.
  const char * const rootdir = getenv("APT_ROOT_DIR");
  apt_preinit(rootdir);

  // Ensure that the cache is always closed when main() exits.  Without
  // this, there might be dangling flyweights hanging around, and those
  // can trigger aborts when the static flyweight pool is destroyed.
  //
  // TBH, I think it might be worth writing our own flyweight stand-in
  // to avoid this particular bit of stupid.  On the other hand, it
  // might be better to fully shut down the cache all the time, to
  // better detect leaks and so on?  I'm undecided -- and it shouldn't
  // take too long to clear out the cache.
  atexit(&apt_shutdown);

  // NOTE: this can of course be spoofed.  Anyone bothering to is off their
  //      rocker.
  argv0=argv[0];

  // Backwards bug-compatibility; old versions used a misleading name
  // for this option.
  if(aptcfg->Find(PACKAGE "::Keep-Unused-Pattern", "") == "")
    {
      aptcfg->Set(PACKAGE "::Keep-Unused-Pattern",
		  aptcfg->Find(PACKAGE "::Delete-Unused-Pattern", ""));
      aptcfg->Set(PACKAGE "::Delete-Unused-Pattern", "");
    }
  else
    aptcfg->Set(PACKAGE "::Delete-Unused-Pattern", "");

  // By default don't log anything below WARN.
  Logger::getLogger("")->setLevel(WARN_LEVEL);

  // HACK: Support for optional values in CommandLine.
  for(int i = 1; i != argc; ++i)
    {
      if(strcasecmp("--show-summary", argv[i]) == 0)
        argv[i] = strdup("--show-summary=first-package");
    }

  // Push existing errors so we can check for only option errors
  // later.
  _error->PushToStack();

  // Parse the command line options.
  CommandLine cmdl(opts, _config);
  if(cmdl.Parse(argc, argv) == false)
    {
      _error->DumpErrors();
      return 100;
    }

  if(aptcfg->FindB("help", false) == true)
    {
      usage();
      return 0;
    }

  if(aptcfg->FindB("version", false) == true)
    {
      show_version();
      return 0;
    }

  // HACK: Multiplexed arguments not supported by CommandLine.
  {
    string display_format = _config->Find(PACKAGE "::%::display-format", "");
    if(display_format.empty() == false)
      {
        _config->Set(PACKAGE "::CmdLine::Package-Display-Format", display_format);
        _config->Set(PACKAGE "::CmdLine::Version-Display-Format", display_format);
      }

    if(_config->FindB(PACKAGE "::%::without-recommends", false) == true)
      {
        _config->Set("APT::Install-Recommends", false);
        _config->Set("APT::AutoRemove::Recommends-Important", true);
      }

    if(_config->FindB(PACKAGE "::%::log-resolver", false) == true)
      {
        _config->Set(PACKAGE "::Logging::Levels::", "aptitude.resolver.search:trace");
        _config->Set(PACKAGE "::Logging::Levels::", "aptitude.resolver.search.costs:info");
      }
  }

  // The filename to read status information from.
  string status_file = aptcfg->Find(PACKAGE "::%::pkgstates.in", "");
  char *status_fname = status_file.empty() ? NULL : strdup(status_file.c_str());
  string package_display_format = aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Format", "%c%a%M %p# - %d#");
  string version_display_format = aptcfg->Find(PACKAGE "::CmdLine::Version-Display-Format", "%c%a%M %p# %t %i");
  string group_by_mode_string = aptcfg->Find(PACKAGE "::CmdLine::Versions-Group-By", "auto");
  string show_package_names_mode_string = aptcfg->Find(PACKAGE "::CmdLine::Versions-Show-Package-Names", "auto");
  string sort_policy = aptcfg->Find(PACKAGE "::CmdLine::Sorting", "name,version");
  string width=aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Width", "");
  // Set to a non-empty string to enable logging simplistically; set
  // to "-" to log to stdout.
  string log_file = aptcfg->Find(PACKAGE "::Logging::File", "");
  bool simulate = aptcfg->FindB(PACKAGE "::CmdLine::Simulate", false) ||
    aptcfg->FindB(PACKAGE "::Simulate", false);
  bool download_only=aptcfg->FindB(PACKAGE "::CmdLine::Download-Only", false);;
  bool arch_only = aptcfg->FindB("Apt::Get::Arch-Only", false);

  bool update_only = aptcfg->FindB(PACKAGE "::UI::Update-On-Startup", false);
  bool install_only = aptcfg->FindB(PACKAGE "::UI::Install-On-Startup", false);
  bool queue_only = aptcfg->FindB(PACKAGE "::CmdLine::Schedule-Only", false);
  bool autoclean_only = aptcfg->FindB(PACKAGE "::UI::Autoclean-On-Startup", false);
  bool clean_only = aptcfg->FindB(PACKAGE "::UI::Clean-On-Startup", false);
  bool assume_yes=aptcfg->FindB(PACKAGE "::CmdLine::Assume-Yes", false);
  bool fix_broken=aptcfg->FindB(PACKAGE "::CmdLine::Fix-Broken", false);
  bool safe_resolver_no_new_installs = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Installs", false);
  bool safe_resolver_no_new_upgrades = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Upgrades", false);
  bool safe_resolver_show_resolver_actions = aptcfg->FindB(PACKAGE "::Safe-Resolver::Show-Resolver-Actions", false);

  resolver_mode_tp resolver_mode = resolver_mode_default;
  if(aptcfg->FindB(PACKAGE "::Always-Use-Safe-Resolver", false))
    resolver_mode = resolver_mode_safe;
  if(aptcfg->FindB(PACKAGE "::%::safe-resolver", false) == true
     && aptcfg->FindB(PACKAGE "::%::full-resolver", false) == true)
    _error->Error(_("Conflicting command line options --safe-resolver and --full-resolver"));
  if(aptcfg->FindB(PACKAGE "::%::safe-resolver", false) == true)
    resolver_mode = resolver_mode_safe;
  if(aptcfg->FindB(PACKAGE "::%::full-resolver", false) == true)
    resolver_mode = resolver_mode_full;

  bool disable_columns = aptcfg->FindB(PACKAGE "::CmdLine::Disable-Columns", false);

  bool showvers=aptcfg->FindB(PACKAGE "::CmdLine::Show-Versions", false);
  bool showdeps=aptcfg->FindB(PACKAGE "::CmdLine::Show-Deps", false);
  bool showsize=aptcfg->FindB(PACKAGE "::CmdLine::Show-Size-Changes", false);
  bool showwhy = aptcfg->FindB(PACKAGE "::CmdLine::Show-Why", false);
  string show_why_summary_mode = aptcfg->Find(PACKAGE "::CmdLine::Show-Summary", "no-summary");
  bool visual_preview=aptcfg->FindB(PACKAGE "::CmdLine::Visual-Preview", false);
  bool always_prompt=aptcfg->FindB(PACKAGE "::CmdLine::Always-Prompt", false);
  int verbose=aptcfg->FindI(PACKAGE "::CmdLine::Verbose", 0);

  std::vector<aptitude::cmdline::tag_application> user_tags;
  if(read_user_tag_applications(user_tags) == false)
    {
      _error->DumpErrors();
      return 100;
    }

  group_by_option group_by_mode = group_by_auto;
  try
    {
      group_by_mode = parse_group_by_option(group_by_mode_string);
    }
  catch(std::exception &ex)
    {
      _error->Error("%s", ex.what());
    }

  show_package_names_option show_package_names_mode = show_package_names_auto;
  if(show_package_names_mode_string == "never" ||
     show_package_names_mode_string == P_("--show-package-names|never"))
    show_package_names_mode = show_package_names_never;
  else if(show_package_names_mode_string == "auto" ||
          show_package_names_mode_string == P_("--show-package-names|auto"))
    show_package_names_mode = show_package_names_auto;
  else if(show_package_names_mode_string == "always" ||
          show_package_names_mode_string == P_("--show-package-names|always"))
    show_package_names_mode = show_package_names_always;
  else
    _error->Error(_("Invalid package names display mode \"%s\" (should be"
                    " \"never\", \"auto\", or \"always\")."),
                  show_package_names_mode_string.c_str());

  aptitude::why::roots_string_mode why_display_mode = aptitude::why::no_summary;
  if(show_why_summary_mode == "no-summary" || show_why_summary_mode == _("no-summary"))
    why_display_mode = aptitude::why::no_summary;
  else if(show_why_summary_mode == "first-package" || show_why_summary_mode == _("first-package"))
    why_display_mode = aptitude::why::show_requiring_packages;
  else if(show_why_summary_mode == "first-package-and-type" || show_why_summary_mode == _("first-package-and-type"))
    why_display_mode = aptitude::why::show_requiring_packages_and_strength;
  else if(show_why_summary_mode == "all-packages" || show_why_summary_mode == _("all-packages"))
    why_display_mode = aptitude::why::show_chain;
  else if(show_why_summary_mode == "all-packages-with-dep-versions" || show_why_summary_mode == _("all-packages-with-dep-versions"))
    why_display_mode = aptitude::why::show_chain_with_versions;
  else
    // ForTranslators: "why" here is the aptitude command name and
    // should not be translated.
    _error->Error(_("Invalid \"why\" summary mode \"%s\": expected"
                    " \"no-summary\", \"first-package\","
                    " \"first-package-and-type\", \"all-packages\","
                    " or \"all-packages-with-dep-versions\"."),
                  show_why_summary_mode.c_str());

  apply_config_file_logging_levels(aptcfg);

  if(!log_file.empty())
    Logger::getLogger("")
      ->connect_message_logged(sigc::bind(sigc::ptr_fun(&handle_message_logged),
                                          log_file));

  temp::initialize("aptitude");

  const bool debug_search = aptcfg->FindB(PACKAGE "::CmdLine::Debug-Search", false);

  if(!isatty(STDOUT_FILENO) && aptcfg->FindI("quiet", -1) == -1)
    aptcfg->SetNoUser("quiet", 1);
  int quiet = aptcfg->FindI("quiet");

  if(simulate)
    aptcfg->SetNoUser(PACKAGE "::Simulate", true);

  const bool cmdline_mode = cmdl.FileSize() > 0;

  // Sanity-check
  {
    int num_startup_actions = 0;
    if(update_only)
      ++num_startup_actions;
    if(install_only)
      ++num_startup_actions;
    if(autoclean_only)
      ++num_startup_actions;
    if(clean_only)
      ++num_startup_actions;

    if(num_startup_actions > 1)
      _error->Error(_("Only one of --auto-clean-on-startup,"
                      " --clean-on-startup, -i, and -u may be specified"));
  }

  if((update_only || install_only || autoclean_only || clean_only)
     && cmdline_mode == true)
    _error->Error(_("-u, -i, --clean-on-startup, and --autoclean-on-startup"
                    " may not be specified in command-line mode (eg, with"
                    " 'install')"));

  // Abort now if there were any errors.
  if(_error->PendingError() == true)
    {
      _error->DumpErrors();
      return 100;
    }

  _error->MergeWithStack();

  // Possibly run off and do other commands.
  if(cmdline_mode == true)
    {
      using namespace aptitude::cmdline;

      try
	{
	  // Connect up the "please consume errors" routine for the
	  // command-line.
	  consume_errors.connect(
            sigc::mem_fun(_error,
                          (void (GlobalError::*)()) &GlobalError::PushToStack));

          int filec = cmdl.FileSize();
          char **filev = const_cast<char **>(cmdl.FileList);
          int rval = 0;

	  // TODO: warn the user if they passed --full-resolver to
	  // something other than "upgrade" or do_action.

	  if(!strcasecmp(filev[0], "update"))
	    rval = cmdline_update(filec, filev, verbose);
	  else if(!strcasecmp(filev[0], "clean"))
	    rval = cmdline_clean(filec, filev, simulate);
	  else if(!strcasecmp(filev[0], "autoclean"))
	    rval = cmdline_autoclean(filec, filev, simulate);
	  else if(!strcasecmp(filev[0], "forget-new"))
	    rval = cmdline_forget_new(filec, filev,
				      status_fname, simulate);
	  else if(!strcasecmp(filev[0], "search"))
	    rval = cmdline_search(filec, filev,
				  status_fname,
				  package_display_format, width,
				  sort_policy,
				  disable_columns,
				  debug_search);
          else if(!strcasecmp(filev[0], "versions"))
            rval = cmdline_versions(filec, filev,
                                    status_fname,
                                    version_display_format, width,
                                    sort_policy,
                                    disable_columns,
                                    debug_search,
                                    group_by_mode,
                                    show_package_names_mode);
	  else if(!strcasecmp(filev[0], "why"))
	    rval = cmdline_why(filec, filev,
			       status_fname, verbose,
			       why_display_mode, false);
	  else if(!strcasecmp(filev[0], "why-not"))
	    rval = cmdline_why(filec, filev,
			       status_fname, verbose,
			       why_display_mode, true);
	  else if( (!strcasecmp(filev[0], "install")) ||
		   (!strcasecmp(filev[0], "reinstall")) ||
		   (!strcasecmp(filev[0], "dist-upgrade")) ||
		   (!strcasecmp(filev[0], "full-upgrade")) ||
		   (!strcasecmp(filev[0], "safe-upgrade")) ||
		   (!strcasecmp(filev[0], "upgrade")) ||
		   (!strcasecmp(filev[0], "remove")) ||
		   (!strcasecmp(filev[0], "purge")) ||
		   (!strcasecmp(filev[0], "hold")) ||
		   (!strcasecmp(filev[0], "unhold")) ||
		   (!strcasecmp(filev[0], "markauto")) ||
		   (!strcasecmp(filev[0], "unmarkauto")) ||
		   (!strcasecmp(filev[0], "forbid-version")) ||
		   (!strcasecmp(filev[0], "keep")) ||
		   (!strcasecmp(filev[0], "keep-all")) ||
		   (!strcasecmp(filev[0], "build-dep")) ||
		   (!strcasecmp(filev[0], "build-depends")))
            rval = cmdline_do_action(filec, filev,
                                     status_fname,
                                     simulate, assume_yes, download_only,
                                     fix_broken, showvers, showdeps,
                                     showsize, showwhy,
                                     visual_preview, always_prompt,
                                     resolver_mode, safe_resolver_show_resolver_actions,
                                     safe_resolver_no_new_installs, safe_resolver_no_new_upgrades,
                                     user_tags,
                                     arch_only, queue_only, verbose);
	  else if(!strcasecmp(filev[0], "add-user-tag") ||
		  !strcasecmp(filev[0], "remove-user-tag"))
	    rval = cmdline_user_tag(filec, filev,
                                    quiet, verbose);
	  else if(!strcasecmp(filev[0], "extract-cache-subset"))
	    rval = extract_cache_subset(filec, filev);
	  else if(!strcasecmp(filev[0], "download"))
	    rval = cmdline_download(filec, filev);
	  else if(!strcasecmp(filev[0], "changelog"))
	    rval = cmdline_changelog(filec, filev);
	  else if(!strcasecmp(filev[0], "moo"))
	    rval = cmdline_moo(filec, filev, verbose);
	  else if(!strcasecmp(filev[0], "show"))
	    rval = cmdline_show(filec, filev, verbose);
	  else if(!strcasecmp(filev[0], "dump-resolver"))
	    rval = cmdline_dump_resolver(filec, filev, status_fname);
	  else if(!strcasecmp(filev[0], "check-resolver"))
	    rval = cmdline_check_resolver(filec, filev, status_fname);
	  else if(!strcasecmp(filev[0], "help"))
            usage();
	  // Debugging/profiling commands:
	  else if(!strcasecmp(filev[0], "nop"))
	    {
	      OpTextProgress p(aptcfg->FindI("Quiet", 0));
	      apt_init(&p, true);
	    }
	  else if(!strcasecmp(filev[0], "nop-noselections"))
	    {
	      OpTextProgress p(aptcfg->FindI("Quiet", 0));
	      apt_init(&p, false);
	    }
          else if(!strcasecmp(filev[0], "dump-config"))
            {
              OpTextProgress p(aptcfg->FindI("Quiet", 0));
              apt_init(&p, false);
              _config->Dump(std::cout);
            }
	  else
	    {
              _error->Error(_("Unknown command \"%s\""), filev[0]);
              rval = 100;
	    }

          while(_error->StackCount() > 0)
            _error->MergeWithStack();

          if(_error->PendingError() == true && rval == 0)
            rval = 100;

          // Do not dump errors if user aborted.
          if(rval == 1)
            return rval;

          if(aptcfg->FindI("quiet", 0) > 0)
            _error->DumpErrors();
          else
            _error->DumpErrors(GlobalError::DEBUG);

          return rval;
	}
      catch(StdinEOFException)
	{
	  printf("%s", _("Abort.\n"));
	  return 1;
	}
      catch(const cwidget::util::Exception &e)
	{
	  fprintf(stderr, _("Uncaught exception: %s\n"), e.errmsg().c_str());

	  std::string backtrace = e.get_backtrace();
	  if(!backtrace.empty())
	    fprintf(stderr, _("Backtrace:\n%s\n"), backtrace.c_str());
	  return 100;
	}
    }

#ifdef HAVE_QT
  if(aptcfg->FindB(PACKAGE "::%::qt-gui", false) == true)
    {
      if(aptitude::gui::qt::main(argc, argv))
        return 0;

      // Otherwise, fall back to trying to start a curses interface
      // (assume that we can't contact the X server, or maybe that we
      // can't load the UI definition)
    }
#endif

#ifdef HAVE_GTK
  if(aptcfg->FindB(PACKAGE "::%::start-gui", false) == true)
    {
      if(aptcfg->FindB(PACKAGE "::%::new-gui", false) == true)
        {
          if(gui::init(argc, argv))
            return 0;
        }
      else if(gui::main(argc, argv))
	return 0;
      // Otherwise, fall back to trying to start a curses interface
      // (assume that we can't contact the X server, or maybe that we
      // can't load the UI definition)
    }
#endif

    {
      ui_init();

      try
        {
          progress_ref p=gen_progress_bar();
          // We can avoid reading in the package lists in the case that
          // we're about to update them (since they'd be closed and
          // reloaded anyway).  Obviously we still need them for installs,
          // since we have to get information about what to install from
          // somewhere...
          if(!update_only)
            apt_init(p->get_progress().unsafe_get_ref(), true, status_fname);
          if(status_fname)
            free(status_fname);
          check_apt_errors();

          file_quit.connect(sigc::ptr_fun(cw::toplevel::exitmain));

          if(apt_cache_file)
            {
              (*apt_cache_file)->package_state_changed.connect(sigc::ptr_fun(cw::toplevel::update));
              (*apt_cache_file)->package_category_changed.connect(sigc::ptr_fun(cw::toplevel::update));
            }

	  if(!aptcfg->FindB(PACKAGE "::UI::Flat-View-As-First-View", false))
	    do_new_package_view(*p->get_progress().unsafe_get_ref());
	  else
	    do_new_flat_view(*p->get_progress().unsafe_get_ref());

          p->destroy();
          p = NULL;

          if(update_only)
            do_update_lists();
          else if(install_only)
            do_package_run_or_show_preview();
	  else if(autoclean_only)
	    do_autoclean();
	  else if(clean_only)
	    do_clean();

          ui_main();
        }
      catch(const cwidget::util::Exception &e)
        {
          cw::toplevel::shutdown();

          fprintf(stderr, _("Uncaught exception: %s\n"), e.errmsg().c_str());

          std::string backtrace = e.get_backtrace();
          if(!backtrace.empty())
            fprintf(stderr, _("Backtrace:\n%s\n"), backtrace.c_str());

	  return 100;
        }

      // The cache is closed by apt_shutdown, which was registered
      // earlier with atexit(3).

      return 0;
    }
}
