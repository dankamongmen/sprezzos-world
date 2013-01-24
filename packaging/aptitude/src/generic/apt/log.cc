// log.cc
//
//   Copyright (C) 2005-2008, 2010 Daniel Burrows
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

#include "log.h"

#include "apt.h"
#include "config_signal.h"

#include <aptitude.h>

#include <generic/util/util.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/strutl.h>

#include <errno.h>
#include <stdio.h>

#include <algorithm>

using namespace std;

typedef std::pair<pkgCache::PkgIterator, pkg_action_state> logitem;
typedef std::vector<logitem> loglist;

bool do_log(const string &log,
	    const loglist &changed_packages)
{
  FILE *f = NULL;

  if(log[0] == '|')
    f = popen(log.c_str()+1, "w");
  else
    f = fopen(log.c_str(), "a");

  if(!f)
    {
      _error->Errno("do_log", _("Unable to open %s to log actions"), log.c_str());

      return false;
    }

  time_t curtime = time(NULL);
  tm ltime;
  string timestr;

  if(localtime_r(&curtime, &ltime) != NULL)
    // ForTranslators: This is a date and time format.  See strftime(3).
    timestr = sstrftime(_("%a, %b %e %Y %T %z"), &ltime);
  else
    timestr = ssprintf(_("Error generating local time (%s)"),
		       sstrerror(errno).c_str());

  fprintf(f, "Aptitude " VERSION ": %s\n%s\n\n",
	  _("log report"), timestr.c_str());
  fprintf(f, _("IMPORTANT: this log only lists intended actions; actions which fail due to\ndpkg problems may not be completed.\n\n"));
  fprintf(f, _("Will install %li packages, and remove %li packages.\n"),
	  (*apt_cache_file)->InstCount(), (*apt_cache_file)->DelCount());

  if((*apt_cache_file)->UsrSize() > 0)
    fprintf(f, _("%sB of disk space will be used\n"),
	    SizeToStr((*apt_cache_file)->UsrSize()).c_str());
  else if((*apt_cache_file)->UsrSize() < 0)
    fprintf(f, _("%sB of disk space will be freed\n"),
	    SizeToStr((*apt_cache_file)->UsrSize()).c_str());

  fprintf(f, "===============================================================================\n");


  for(loglist::const_iterator i = changed_packages.begin();
      i != changed_packages.end(); ++i)
    {
      if(i->second == pkg_upgrade)
	fprintf(f, _("[UPGRADE] %s %s -> %s\n"), i->first.FullName(false).c_str(),
		i->first.CurrentVer().VerStr(),
		(*apt_cache_file)[i->first].CandidateVerIter(*apt_cache_file).VerStr());
      else if(i->second == pkg_downgrade)
	fprintf(f, _("[DOWNGRADE] %s %s -> %s\n"), i->first.FullName(false).c_str(),
		i->first.CurrentVer().VerStr(),
		(*apt_cache_file)[i->first].CandidateVerIter(*apt_cache_file).VerStr());
      else
	if(i->second != pkg_unchanged)
	  {
	    const char *tag = NULL;
	    switch(i->second)
	      {
	      case pkg_remove:
		tag = _("REMOVE");
		break;
		//case pkg_upgrade:
		//tag=_("UPGRADE");
		//break;
	      case pkg_install:
		tag = _("INSTALL");
		break;
	      case pkg_reinstall:
		tag = _("REINSTALL");
		break;
	      case pkg_hold:
		tag = _("HOLD");
		break;
	      case pkg_broken:
		tag = _("BROKEN");
		break;
	      case pkg_unused_remove:
		tag = _("REMOVE, NOT USED");
		break;
	      case pkg_auto_remove:
		tag = _("REMOVE, DEPENDENCIES");
		break;
	      case pkg_auto_install:
		tag = _("INSTALL, DEPENDENCIES");
		break;
	      case pkg_auto_hold:
		tag = _("HOLD, DEPENDENCIES");
		break;
	      case pkg_unconfigured:
		tag = _("UNCONFIGURED");
		break;
	      default:
		tag = _("????????");
		break;
	      }

	    fprintf(f, _("[%s] %s\n"), tag, i->first.FullName(false).c_str());
	  }
    }
  fprintf(f, _("===============================================================================\n\nLog complete.\n"));

  if(log[0] == '|')
    pclose(f);
  else
    fclose(f);

  return true;
}

struct log_sorter
{
  pkg_name_lt plt;
public:
  inline bool operator()(const logitem &a, const logitem &b)
  {
    if(a.second<b.second)
      return true;
    else if(a.second>b.second)
      return false;
    else
      return plt(a.first, b.first);
  }
};

void log_changes()
{
  vector<string> logs;

  string main_log = aptcfg->Find(PACKAGE "::Log", "/var/log/" PACKAGE);

  if(!main_log.empty())
    logs.push_back(main_log);

  const Configuration::Item *parent = aptcfg->Tree(PACKAGE "::Log");

  if(parent != NULL)
    for(const Configuration::Item *curr = parent->Child;
	curr != NULL; curr = curr->Next)
      {
	if(!curr->Value.empty())
	  logs.push_back(curr->Value);
      }

  if(!logs.empty())
    {
      loglist changed_packages;
      for(pkgCache::PkgIterator i
	    = (*apt_cache_file)->PkgBegin(); !i.end(); i++)
	{
	  pkg_action_state s = find_pkg_state(i, *apt_cache_file);
	  if(s != pkg_unchanged)
	    changed_packages.push_back(logitem(i, s));
	}

      sort(changed_packages.begin(), changed_packages.end(), log_sorter());

      for(vector<string>::iterator i
	    = logs.begin(); i != logs.end(); ++i)
	do_log(*i, changed_packages);
    }
}
