// tasks.cc
//
//  Copyright (C) 2001 Daniel Burrows
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
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//

#include "tasks.h"
#include "apt.h"

#include <aptitude.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/tagfile.h>

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/transcode.h>

#include <errno.h>
#include <ctype.h>

#include <map>
#include <vector>
#include <iterator>
#include <algorithm>
#include <sstream>

namespace cw = cwidget;

using namespace std;

namespace aptitude {
namespace apt {

// Stores the various tasks.
map<string, task> *task_list=new map<string, task>;

task *find_task(const std::string &name)
{
  if(task_list->find(name) == task_list->end())
    return NULL;

  return &((*task_list)[name]);
}

// This is an array indexed by package ID, managed by load_tasks.
// (as usual, it's initialized to NULL)
set<string> *tasks_by_package;

// Now this is just a wrapper, as you can see..
std::set<std::string> *get_tasks(const pkgCache::PkgIterator &pkg)
{
  if(!tasks_by_package)
    return NULL;

  return tasks_by_package+pkg->ID;
}

// Based on task_packages in tasksel.pl.
bool get_task_packages(std::set<pkgCache::PkgIterator> * const pkgset,
                       const task &task, const string &arch)
{
  // key packages are always included
  for(set<string>::const_iterator it = task.keys.begin();
      it != task.keys.end();
      ++it)
    {
      pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*it, arch);
      if(pkg.end() != false)
        pkgset->insert(pkg);
    }

  if(task.packages.empty() == true)
    {
      // only key
    }
  else if(task.packages[0] == "task-fields")
    {
      // task-fields method is built-in for speed
      for(pkgCache::GrpIterator grp = (*apt_cache_file)->GrpBegin();
          grp.end() == false;
          ++grp)
        {
          pkgCache::PkgIterator pkg = grp.FindPkg(arch);
          if(pkg.end() == true)
            continue;
          set<string> *tasks = get_tasks(pkg);
          if(tasks->find(task.name) != tasks->end())
            pkgset->insert(pkg);
        }
    }
  else if(task.packages[0] == "list")
    {
      // list method is build-in for speed
      for(vector<string>::const_iterator it = task.packages.begin() + 1;
          it != task.packages.end();
          ++it)
        {
          const pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*it, arch);
          if(pkg.end() != false)
            pkgset->insert(pkg);
        }
    }
  else
    {
      // TODO: Handle other methods.  One option is to just call
      // 'tasksel --task-packages' and process the list of search
      // patterns returned.
      return _error->Warning(_("Unhandled packages method in task %s: %s"),
                             task.name.c_str(),
                             task.packages[0].c_str());
    }

  return true;
}

bool is_task_package(const pkgCache::PkgIterator &pkg)
{
  // TODO: Enable this optimization on Debian systems.
  // if(pkg.Section() != NULL && strcmp(pkg.Section(), "tasks") == 0)
  //   return true;
  set<string> *pkg_tasks = get_tasks(pkg);
  for(set<string>::const_iterator it = pkg_tasks->begin();
      it != pkg_tasks->end();
      ++it)
    {
      if(task_list->find(*it) != task_list->end())
        {
          const task t = (*task_list)[*it];
          if(t.packages.empty() == true
             && t.keys.find(pkg.Name()) != t.keys.end())
            return true;
        }
    }

  return false;
}

/** \brief Add any tasks found in the given version-file pointer to
 *  the tasks of the given package.
 */
static void append_tasks(const pkgCache::PkgIterator &pkg,
			 const pkgCache::VerFileIterator &verfile)
{
  // This should never be called before load_tasks has initialized the
  // tasks structure.
  eassert(tasks_by_package);

  set<string> &task_set=tasks_by_package[pkg->ID];

  if(apt_package_records)
    {
      const char *start,*stop;
      pkgTagSection sec;

      // Pull out pointers to the underlying record.
      apt_package_records->Lookup(verfile).GetRec(start, stop);

      // Parse it as a section.
      sec.Scan(start, stop-start+1);

      string tasks=sec.FindS("Task");

      string::size_type loc=0, firstcomma=0;

      // Strip leading whitespace
      while(loc<tasks.size() && isspace(tasks[loc]))
	++loc;

      while( (firstcomma=tasks.find(',', loc))!=tasks.npos)
	{
	  // Strip trailing whitespace
	  string::size_type loc2=firstcomma-1;
	  while(isspace(tasks[loc2]))
	    --loc2;
	  ++loc2;

	  string taskname(tasks, loc, loc2-loc);
	  task_set.insert(taskname);
	  loc=firstcomma+1;

	  // Strip leading whitespace
	  while(loc<tasks.size() && isspace(tasks[loc]))
	    ++loc;
	}

      if(loc!=tasks.size())
	task_set.insert(string(tasks, loc));
    }
}

bool task::keys_present()
{
  if(!keys_present_cache_stale)
    return keys_present_cache;

  keys_present_cache_stale=false;

  for(set<string>::const_iterator i=keys.begin(); i!=keys.end(); ++i)
    {
      pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(*i);

      if(pkg.end())
	{
	  keys_present_cache=false;
	  return false;
	}
      else
	// Here it is assumed that all the tasks are loaded, because
	// we're going to look them up.
	{
	  set<string> *tasks = get_tasks(pkg);

	  if(!tasks)
	    {
	      keys_present_cache=false;
	      return false;
	    }

	  if(tasks->find(name) == tasks->end())
	    {
	      keys_present_cache=false;
	      return false;
	    }
	}
    }

  keys_present_cache=true;
  return true;
}

static string rfc822_process_paragraph(const string &textdomain,
				       string par)
{
  if (par.empty())
    return par;

  // Remove trailing whitespace
  string::size_type loc = par.size()-1;
  while(isspace(par[loc]))
    --loc;
  par.erase(loc+1, string::npos);

  return dgettext(textdomain.c_str(), par.c_str());
}

/** Returns msgid translated in the given text domain, with
 *  appropriate munging: paragraphs are translated individually after
 *  one leading and all trailing whitespace on each line is stripped.
 *
 *  \param textdomain the domain in which to translate
 *  \param msgid the formatted description which should be translated
 */
static string rfc822dgettext(string textdomain, string msgid)
{
  if (textdomain.empty())
    return msgid;
  string::size_type start=0, len=msgid.size(), nextnl=0;

  string thispar = "";
  string msgstr = "";

  // Remove leading whitespaces, i.e. replace "\n " by '\n'
  //
  // This assumes the trailing whitespace exists already.
  do
    {
      if (nextnl<len)
	nextnl=msgid.find('\n', start);
      if (nextnl==string::npos)
	nextnl=len-1;
      string thisline(msgid, start+1, nextnl-start);
      thispar+=thisline;
      start=nextnl+1;
    }
  while (start<len);

  // Reformat text (replace '\n' by ' ') and translate individual
  // paragraphs
  bool verbatimline = (thispar[0] == ' ');
  string::size_type loc=0;
  start=0, len=thispar.size(), nextnl=0;
  while((nextnl=thispar.find('\n', loc))!=string::npos)
    {
      // Verbatim line case
      if (thispar[nextnl+1] == ' ')
	verbatimline = true;
      // End of the paragraph
      else if (thispar[nextnl+1] == '.' && thispar[nextnl+2] == '\n')
	{
	  /* Translate current paragraph */
	  msgstr+=rfc822_process_paragraph(textdomain, string(thispar, start, nextnl-start));
	  msgstr+="\n.\n";
	  start = nextnl + 3;
	  nextnl += 2;
	  verbatimline = false;
	}
      // Add to the paragraph
      else
	{
	  if (!verbatimline)
	    thispar[nextnl] = ' ';
	  verbatimline = false;
	}
      loc = nextnl + 1;
    }
  msgstr+=rfc822_process_paragraph(textdomain, string(thispar, start));
  // Remove trailing whitespace
  loc=msgstr.size()-1;
  while(isspace(msgstr[loc]))
    --loc;
  ++loc;
  msgstr[loc] = '\0';
  // Reformat text, i.e. replace '\n' by "\n "
  start = 0;
  while((nextnl=msgstr.find('\n', start))!=string::npos)
    {
      msgstr.insert(nextnl+1, " ");
      start = nextnl+2;
    }
  return msgstr;
}

static void set_task_description(task &task, const wstring &desc)
{
  const wstring::size_type newline = desc.find(L'\n');
  task.shortdesc = wstring(desc, 0, newline);
  task.longdesc = wstring(L"\n ") + wstring(desc, newline+1);
}

static void read_task_desc(const string &filename, OpProgress &prog)
{
  FileFd fd;

  fd.Open(filename, FileFd::ReadOnly);
  if(!fd.IsOpen())
    return;

  const unsigned long long file_size = fd.Size();
  unsigned long long amt = 0;
  prog.SubProgress(file_size);

  pkgTagFile tagfile(&fd);
  pkgTagSection section;
  const string taskdomain("debian-tasks");

  while(tagfile.Step(section))
    {
      task newtask;
      const string taskname(section.FindS("Task"));

      if(!taskname.empty())
        {
          newtask.name = taskname;
          newtask.section = section.FindS("Section");
          newtask.relevance = section.FindI("Relevance", 5);

          istringstream keyss(section.FindS("Key"));
          for(istream_iterator<string> key(keyss);
              key != istream_iterator<string>();
              ++key)
            {
              newtask.keys.insert(*key);

              pkgCache::GrpIterator grp = (*apt_cache_file)->FindGrp(*key);
              if(grp.end() == true)
                continue;

              for(pkgCache::PkgIterator pkg = grp.PackageList();
                  pkg.end() == false;
                  pkg = pkg.Group().NextPkg(pkg))
                tasks_by_package[pkg->ID].insert(taskname);
            }

          istringstream packagess(section.FindS("Packages"));
          copy(istream_iterator<string>(packagess),
               istream_iterator<string>(),
               back_inserter(newtask.packages));

          string desc = section.FindS("Description");
          if(desc.empty() == false)
            {
              desc = rfc822dgettext(taskdomain, desc);
              set_task_description(newtask, cw::util::transcode(desc));
            }
          else
            {
              for(set<string>::const_iterator key = newtask.keys.begin();
                  key != newtask.keys.end();
                  ++key)
                {
                  const pkgCache::PkgIterator pkg((*apt_cache_file)->FindPkg(*key));
                  if(pkg.end() == true)
                    continue;
                  const pkgCache::VerIterator ver(pkg.VersionList());
                  if(ver.end() == true)
                    continue;

                  const wstring desc(get_long_description(ver, apt_package_records));
                  if(desc.empty() == true)
                    continue;

                  set_task_description(newtask, desc);
                  break;
                }
            }

          (*task_list)[taskname] = newtask;
        }

      amt += section.size();
      prog.Progress(amt);
    }
}

void load_tasks(OpProgress &progress)
{
  // Build a list for each package of the tasks that package belongs to.
  //
  // Sorting by location on disk is *critical* -- otherwise, this operation
  // will take ages.

  // This is done prior to loading the task descriptions so that I can just
  // bail if that fails.

  vector<loc_pair> versionfiles;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      for(pkgCache::VerIterator v = pkg.VersionList(); !v.end(); ++v)
	{
	  for(pkgCache::VerFileIterator vf = v.FileList(); !vf.end(); ++vf)
	    {
	      versionfiles.push_back(loc_pair(v, vf));
	    }
	}
    }

  sort(versionfiles.begin(), versionfiles.end(), location_compare());

  // Allocate and set up the table of task information.
  delete[] tasks_by_package;
  tasks_by_package = new set<string>[(*apt_cache_file)->Head().PackageCount];

  for(vector<loc_pair>::iterator i=versionfiles.begin();
      i!=versionfiles.end();
      ++i)
    append_tasks(i->first.ParentPkg(), i->second);

  // Load the task descriptions:
  const char *descdirs[] =
    {"/usr/share/tasksel/descs",
     "/usr/local/share/tasksel/descs",
     NULL};
  vector<string> descfiles;
  for(const char **it = descdirs; *it != NULL; ++it)
    {
      if(DirectoryExists(*it) == false)
        continue;
      const vector<string> v =
        GetListOfFilesInDir(*it, "desc", false, false);
      copy(v.begin(), v.end(), back_inserter(descfiles));
    }
  for(vector<string>::const_iterator it = descfiles.begin();
      it != descfiles.end();
      ++it)
    {
      progress.OverallProgress(it - descfiles.begin(), descfiles.size(), 1,
                               _("Reading task descriptions"));
      read_task_desc(*it, progress);
    }

  progress.Done();
}

void reset_tasks()
{
  task_list->clear();
  delete[] tasks_by_package;
  tasks_by_package=NULL;
}

}
}
