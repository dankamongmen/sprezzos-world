// tasks.h             -*-c++-*-
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

#ifndef TASKS_H
#define TASKS_H

#include <apt-pkg/pkgcache.h>

#include <string>
#include <set>
#include <map>

/** \brief Handles parsing the list of tasks and getting the task of a given
 *  package.
 * 
 *  \file tasks.h
 */

class OpProgress;

namespace aptitude {
namespace apt {

class task
{
private:
  bool keys_present_cache;
  bool keys_present_cache_stale;
public:
  task()
    : keys_present_cache(false),
      keys_present_cache_stale(true),
      relevance(0)
  {
  }

  std::string name;
  std::string section;
  std::wstring shortdesc;
  std::wstring longdesc;
  std::set<std::string> keys;
  std::vector<std::string> packages;

  bool keys_present();

  int relevance;
};

// Stores the various tasks.
extern std::map<std::string, task> *task_list;

task *find_task(const std::string &name);

/** \brief Get the set of tasks associated with the given package.
 *
 *  The caller should not delete this set; it's managed internally by
 *  the tasks module.
 */
std::set<std::string> *get_tasks(const pkgCache::PkgIterator &pkg);

bool get_task_packages(std::set<pkgCache::PkgIterator> * const pkgset,
                       const task &task,
                       const std::string &arch);

/** \brief Returns \b true if the given package is a task package.
 *
 *  A task package is one that appears in the Key stanza of a task
 *  definition which has no Packages stanza.  For example, the package
 *  task-ssh-server in the following definition is a task package:
 *
 *    Task: ssh-server
 *    Section: server
 *    Key: 
 *      task-ssh-server
 */
bool is_task_package(const pkgCache::PkgIterator &pkg);

// (re)loads in the current list of available tasks.  Necessary after a
// cache reload, for obvious reasons.  apt_reload_cache will call this.
void load_tasks(OpProgress &progress);

// Discards the current task list and readies a new one to be loaded.
// Since the task list contains package iterators, we have to do something
// in case they're still hanging around.
void reset_tasks();

}
}

#endif
