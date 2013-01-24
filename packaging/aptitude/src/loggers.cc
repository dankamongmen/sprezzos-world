/** \file loggers.cc */

//   Copyright (C) 2009-2010 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "loggers.h"

using logging::Logger;
using logging::LoggerPtr;

namespace aptitude
{
  LoggerPtr Loggers::getAptitudeAptCache()
  {
    return Logger::getLogger("aptitude.apt.cache");
  }

  LoggerPtr Loggers::getAptitudeAptGlobals()
  {
    return Logger::getLogger("aptitude.apt.globals");
  }

  LoggerPtr Loggers::getAptitudeChangelog()
  {
    return Logger::getLogger("aptitude.changelog");
  }

  LoggerPtr Loggers::getAptitudeChangelogParse()
  {
    return Logger::getLogger("aptitude.changelog.parse");
  }

  LoggerPtr Loggers::getAptitudeCmdline()
  {
    return Logger::getLogger("aptitude.cmdline");
  }

  LoggerPtr Loggers::getAptitudeCmdlineSearch()
  {
    return Logger::getLogger("aptitude.cmdline.search");
  }

  LoggerPtr Loggers::getAptitudeCmdlineThrottle()
  {
    return Logger::getLogger("aptitude.cmdline.throttle");
  }

  LoggerPtr Loggers::getAptitudeDownloadCache()
  {
    return Logger::getLogger("aptitude.downloadCache");
  }

  LoggerPtr Loggers::getAptitudeDownloadQueue()
  {
    return Logger::getLogger("aptitude.downloadQueue");
  }

  LoggerPtr Loggers::getAptitudeDownloadQueueCache()
  {
    return Logger::getLogger("aptitude.downloadQueue.cache");
  }

  LoggerPtr Loggers::getAptitudeDpkgStatusPipe()
  {
    return Logger::getLogger("aptitude.dpkg.statusPipe");
  }

  LoggerPtr Loggers::getAptitudeDpkgTerminal()
  {
    return Logger::getLogger("aptitude.dpkg.terminal");
  }

  LoggerPtr Loggers::getAptitudeDpkgTerminalInactivity()
  {
    return Logger::getLogger("aptitude.dpkg.terminal.inactivity");
  }

  LoggerPtr Loggers::getAptitudeGtkChangelog()
  {
    return Logger::getLogger("aptitude.gtk.changelog");
  }

  LoggerPtr Loggers::getAptitudeGtkChangelogCache()
  {
    return Logger::getLogger("aptitude.gtk.changelog.cache");
  }

  LoggerPtr Loggers::getAptitudeGtkChangelogParse()
  {
    return Logger::getLogger("aptitude.gtk.changelog.parse");
  }

  LoggerPtr Loggers::getAptitudeGtkDashboardUpgradeResolver()
  {
    return Logger::getLogger("aptitude.gtk.dashboard.upgrade.resolver");
  }

  LoggerPtr Loggers::getAptitudeGtkGlobals()
  {
    return Logger::getLogger("aptitude.gtk.globals");
  }

  LoggerPtr Loggers::getAptitudeGtkMainWindow()
  {
    return Logger::getLogger("aptitude.gtk.mainwindow");
  }

  LoggerPtr Loggers::getAptitudeGtkPkgView()
  {
    return Logger::getLogger("aptitude.gtk.pkgview");
  }

  LoggerPtr Loggers::getAptitudeGtkResolver()
  {
    return Logger::getLogger("aptitude.gtk.resolver");
  }

  LoggerPtr Loggers::getAptitudeGtkScreenshotCache()
  {
    return Logger::getLogger("aptitude.gtk.screenshot.cache");
  }

  LoggerPtr Loggers::getAptitudeGtkScreenshotImage()
  {
    return Logger::getLogger("aptitude.gtk.screenshot.image");
  }

  LoggerPtr Loggers::getAptitudeGtkTabs()
  {
    return Logger::getLogger("aptitude.gtk.tabs");
  }

  LoggerPtr Loggers::getAptitudeGtkToplevel()
  {
    return Logger::getLogger("aptitude.gtk.toplevel");
  }

  LoggerPtr Loggers::getAptitudeGtkToplevelTabs()
  {
    return Logger::getLogger("aptitude.gtk.toplevel.tabs");
  }

  LoggerPtr Loggers::getAptitudeQtInit()
  {
    return Logger::getLogger("aptitude.qt.init");
  }

  LoggerPtr Loggers::getAptitudeResolver()
  {
    return Logger::getLogger("aptitude.resolver");
  }

  LoggerPtr Loggers::getAptitudeResolverCosts()
  {
    return Logger::getLogger("aptitude.resolver.costs");
  }

  LoggerPtr Loggers::getAptitudeResolverHints()
  {
    return Logger::getLogger("aptitude.resolver.hints");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsCompare()
  {
    return Logger::getLogger("aptitude.resolver.hints.compare");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsMatch()
  {
    return Logger::getLogger("aptitude.resolver.hints.match");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsParse()
  {
    return Logger::getLogger("aptitude.resolver.hints.parse");
  }

  LoggerPtr Loggers::getAptitudeResolverInitialManualFlags()
  {
    return Logger::getLogger("aptitude.resolver.initialManualFlags");
  }

  LoggerPtr Loggers::getAptitudeResolverSafeResolver()
  {
    return Logger::getLogger("aptitude.resolver.safeResolver");
  }
 
  LoggerPtr Loggers::getAptitudeResolverSafeResolverSetup()
  {
    return Logger::getLogger("aptitude.resolver.safeResolver.setup");
  }

  LoggerPtr Loggers::getAptitudeResolverScores()
  {
    return Logger::getLogger("aptitude.resolver.scores");
  }

  LoggerPtr Loggers::getAptitudeResolverSearch()
  {
    return Logger::getLogger("aptitude.resolver.search");
  }

  LoggerPtr Loggers::getAptitudeResolverThread()
  {
    return Logger::getLogger("aptitude.resolver.thread");
  }

  LoggerPtr Loggers::getAptitudeResolverSearchGraph()
  {
    return Logger::getLogger("aptitude.resolver.search.graph");
  }

  LoggerPtr Loggers::getAptitudeResolverSearchCosts()
  {
    return Logger::getLogger("aptitude.resolver.search.costs");
  }

  LoggerPtr Loggers::getAptitudeTemp()
  {
    return Logger::getLogger("aptitude.temp");
  }

  LoggerPtr Loggers::getAptitudeWhy()
  {
    return Logger::getLogger("aptitude.why");
  }

  LoggerPtr Loggers::getAptitudeWhyGtk()
  {
    return Logger::getLogger("aptitude.why.gtk");
  }
}
