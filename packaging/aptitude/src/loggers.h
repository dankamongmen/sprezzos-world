/** \file loggers.h    -*-c++-*- */

//   Copyright (C) 2009-2010 Daniel Burrows
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


#ifndef LOGGERS_H
#define LOGGERS_H

#include <generic/util/logging.h>

namespace logging = aptitude::util::logging;

namespace aptitude
{
  /** \brief A global repository for the loggers used in aptitude.
   *  The sole purpose of this file is to keep that information in one
   *  place.
   *
   *  It would be nicer if we could enforce that somehow.
   */
  class Loggers
  {
    // Could define static variables here, but that would have no real purpose.
  public:
    /** \brief The logger for events having to do with aptitude's
     *  global apt state.
     *
     *  Name: aptitude.apt.globals
     */
    static logging::LoggerPtr getAptitudeAptGlobals();

    /** \brief The logger for events having to do with aptitude's
     *  wrapper around the apt cache.
     *
     *  Name: aptitude.apt.cache
     */
    static logging::LoggerPtr getAptitudeAptCache();

    /** \brief The logger for events having to do with aptitude's
     *  backend changelog download code.
     *
     *  Name: aptitude.changelog
     */
    static logging::LoggerPtr getAptitudeChangelog();

    /** \brief The logger for events having to do with aptitude's
     *  backend changelog parser.
     *
     *  Name: aptitude.changelog.parse
     */
    static logging::LoggerPtr getAptitudeChangelogParse();

    /** \brief The logger for events having to do with command-line
     *  actions.
     *
     *  Includes both parsing and logic.
     *
     *  Name: aptitude.cmdline
     */
    static logging::LoggerPtr getAptitudeCmdline();

    /** \brief The logger for events having to do with the "search"
     *  command-line action.
     *
     *  Name: aptitude.cmdline.search
     */
    static logging::LoggerPtr getAptitudeCmdlineSearch();

    /** \brief The logger for events having to do with the progress
     *  display throttler in aptitude's command-line code.
     */
    static logging::LoggerPtr getAptitudeCmdlineThrottle();

    /** \brief The logger for events having to do with aptitude's
     *  caching of downloaded data (other than package lists and
     *  .debs).
     */
    static logging::LoggerPtr getAptitudeDownloadCache();

    /** \brief The logger for events having to do with aptitude's
     *  background download queue.
     */
    static logging::LoggerPtr getAptitudeDownloadQueue();

    /** \brief The logger for events having to do with how aptitude's
     *  background download queue checks the download cache.
     */
    static logging::LoggerPtr getAptitudeDownloadQueueCache();

    /** \brief The logger for events having to do with the dpkg
     *  status pipe.
     */
    static logging::LoggerPtr getAptitudeDpkgStatusPipe();

    /** \brief The logger for events having to do with the dpkg
     *  terminal in the GTK+ frontend.
     */
    static logging::LoggerPtr getAptitudeDpkgTerminal();

    /** \brief The logger for events having to do with the dpkg
     *  terminal and subprocess activity/inactivity in the GTK+
     *  frontend.
     */
    static logging::LoggerPtr getAptitudeDpkgTerminalInactivity();

    /** \brief The logger for the GUI dashboard tab's upgrade
     *         resolver.
     *
     *  Name: aptitude.gtk.dashboard.upgrade.resolver
     */
    static logging::LoggerPtr getAptitudeGtkDashboardUpgradeResolver();

    /** \brief The logger for GUI changelog handling.
     *
     *  Name: aptitude.gtk.changelog
     */
    static logging::LoggerPtr getAptitudeGtkChangelog();

    /** \brief The logger for the GUI changelog cache thread.
     *
     *  Name: aptitude.gtk.changelog.cache
     */
    static logging::LoggerPtr getAptitudeGtkChangelogCache();

    /** \brief The logger for the GUI changelog parse thread.
     *
     *  Name: aptitude.gtk.changelog.parse
     */
    static logging::LoggerPtr getAptitudeGtkChangelogParse();

    /** \brief The logger for the module of globals.
     *
     *  Name: aptitude.gtk.globals
     */
    static logging::LoggerPtr getAptitudeGtkGlobals();

    /** \brief The logger for the GTK+ main window.
     *
     *  Name: aptitude.gtk.mainwindow
     */
    static logging::LoggerPtr getAptitudeGtkMainWindow();

    /** \brief The logger for the GUI package view (PkgView).
     *
     *  Name: aptitude.gtk.pkgview
     */
    static logging::LoggerPtr getAptitudeGtkPkgView();

    /** \brief The logger for the GUI resolver tab.
     *
     *  Name: aptitude.gtk.resolver
     */
    static logging::LoggerPtr getAptitudeGtkResolver();

    /** \brief The logger for events related to the screenshot cache
     *  in the GUI.
     *
     *  Name: aptitude.gtk.screenshot.cache
     */
    static logging::LoggerPtr getAptitudeGtkScreenshotCache();

    /** \brief The logger for events related to the screenshot image
     *  object in the GUI.
     *
     *  Name: aptitude.gtk.screenshot.image
     */
    static logging::LoggerPtr getAptitudeGtkScreenshotImage();

    /** \brief The logger for events related to GUI tabs.
     *
     *  Name: aptitude.gtk.tabs
     */
    static logging::LoggerPtr getAptitudeGtkTabs();

    /** \brief The logger for events related to the top-level GTK+ UI.
     *
     *  Name: aptitude.gtk.toplevel
     */
    static logging::LoggerPtr getAptitudeGtkToplevel();

    /** \brief The logger for events related to a collection of tabs
     *  in the top-level GTK+ UI.
     *
     *  Name: aptitude.gtk.toplevel.tabs
     */
    static logging::LoggerPtr getAptitudeGtkToplevelTabs();

    /** \brief The logger for the initialization of the Qt frontend.
     *
     *  Name: aptitude.qt.init
     */
    static logging::LoggerPtr getAptitudeQtInit();

    /** \brief The logger for the dependency resolver.
     *
     *  Name: aptitude.resolver
     */
    static logging::LoggerPtr getAptitudeResolver();

    /** \brief The logger for the resolver cost settings.
     *
     *  Name: aptitude.resolver.costs
     */
    static logging::LoggerPtr getAptitudeResolverCosts();

    /** \brief The logger for resolver hints.
     *
     *  Name: aptitude.resolver.hints
     */
    static logging::LoggerPtr getAptitudeResolverHints();

    /** \brief The logger for comparing resolver hints.
     *
     *  Name: aptitude.resolver.hints.compare
     */
    static logging::LoggerPtr getAptitudeResolverHintsCompare();

    /** \brief The logger for matching resolver hints against
     * packages.
     *
     *  Name: aptitude.resolver.hints.match
     */
    static logging::LoggerPtr getAptitudeResolverHintsMatch();

    /** \brief The logger for parsing resolver hints.
     *
     *  Name: aptitude.resolver.hints.parse
     */
    static logging::LoggerPtr getAptitudeResolverHintsParse();

    /** \brief The logger for describing which packages are initially
     *  considered "manual".
     *
     *  Name: aptitude.resolver.initialManualFlags
     */
    static logging::LoggerPtr getAptitudeResolverInitialManualFlags();

    /** \brief The logger for the "upgrade/install only" logic in the
     *  dependency resolver.
     *
     *  Name: aptitude.resolver.safeResolver
     */
    static logging::LoggerPtr getAptitudeResolverSafeResolver();

    /** \brief The logger for the setup routine for the safe resolver
     * logic.
     *
     *  Name: aptitude.resolver.safeResolver.setup
     */
    static logging::LoggerPtr getAptitudeResolverSafeResolverSetup();

    /** \brief The logger for setting up scores for the dependency
     *  resolver.
     *
     *  Name: aptitude.resolver.scores
     */
    static logging::LoggerPtr getAptitudeResolverScores();

    /** \brief The logger for the resolver's search logic.
     *
     *  Name: aptitude.resolver.search
     */
    static logging::LoggerPtr getAptitudeResolverSearch();

    /** \brief The logger for the resolver's search-graph maintenance.
     */
    static logging::LoggerPtr getAptitudeResolverSearchGraph();

    /** \brief The logger for the cost component of the resolver's
     *  search logic.
     *
     *  Name: aptitude.resolver.search.costs
     */
    static logging::LoggerPtr getAptitudeResolverSearchCosts();

    /** \brief The logger for events related to setting up and running
     *  the background resolver thread.
     */
    static logging::LoggerPtr getAptitudeResolverThread();

    /** \brief The logger for messages related to temporary files. */
    static logging::LoggerPtr getAptitudeTemp();

    /** \brief The logger for the "why" command.
     *
     *  Name: aptitude.why
     */
    static logging::LoggerPtr getAptitudeWhy();

    /** \brief The logger for the GTK+ interface to the "why" command.
     *
     *  Name: aptitude.why
     */
    static logging::LoggerPtr getAptitudeWhyGtk();
  };
}

#endif // LOGGERS_H
