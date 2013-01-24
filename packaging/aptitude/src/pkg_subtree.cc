// pkg_subtree.cc
//
//  Copyright 1999-2005, 2007-2008 Daniel Burrows
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
//  (trivial :) ) implementations for pkg_subtree.h

#include "pkg_subtree.h"

#include <generic/apt/apt.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/widgets/tree.h>

#include "aptitude.h"

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

void pkg_subtree::paint(cw::tree *win, int y, bool hierarchical,
			const cw::style &st)
{
  std::wstring name_to_paint;
  if(num_packages_known)
    name_to_paint = cw::util::swsprintf(L"%ls (%d)",
					name.c_str(),
					num_packages);
  else
    name_to_paint = name;

  cw::subtree<pkg_tree_node>::paint(win, y, hierarchical, name_to_paint);
}

const wchar_t *pkg_subtree::tag()
{
  return name.c_str();
}

const wchar_t *pkg_subtree::label()
{
  return name.c_str();
}

bool pkg_subtree::dispatch_key(const cw::config::key &k, cw::tree *owner)
{
  if(pkg_tree_node::dispatch_key(k, owner))
    return true;
  else
    return cw::subtree<pkg_tree_node>::dispatch_key(k, owner);
}

void pkg_subtree::select(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->select(undo);
}

void pkg_subtree::hold(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->hold(undo);
}

void pkg_subtree::keep(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->keep(undo);
}

void pkg_subtree::remove(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->remove(undo);
}

void pkg_subtree::purge(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->purge(undo);
}

void pkg_subtree::reinstall(undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->reinstall(undo);
}

void pkg_subtree::set_auto(bool isauto, undo_group *undo)
{
  aptitudeDepCache::action_group group(*apt_cache_file, undo);

  for(child_iterator i=get_children_begin(); i!=get_children_end(); i++)
    (*i)->set_auto(isauto, undo);
}

void pkg_subtree::inc_num_packages()
{
  if(num_packages_known)
    {
      ++num_packages;
      if(num_packages_parent != NULL)
	num_packages_parent->inc_num_packages();
    }
}

void pkg_subtree::clear_num_packages()
{
  num_packages_known = false;
  num_packages = -1;
}

void pkg_subtree::set_num_packages(int num)
{
  num_packages_known = true;
  num_packages = num;
}

void pkg_subtree::do_highlighted_changed(bool highlighted)
{
  using cw::util::swsprintf;

  if(highlighted)
    {
      if(info_signal)
	{
	  if(num_packages_known)
	    {
	      const std::wstring numstr =
		swsprintf(cw::util::transcode(ngettext("This group contains %d package.",
						       "This group contains %d packages.",
						       num_packages)).c_str(),
			  num_packages);
	      if(description.empty())
		(*info_signal)(swsprintf(L"\n %ls", numstr.c_str()));
	      else
		(*info_signal)(swsprintf(L"%ls\n .\n %ls",
					 description.c_str(),
					 numstr.c_str()));
	    }
	  else
	    (*info_signal)(description);
	}
    }
  else
    {
      if(info_signal)
	(*info_signal)(L"");
    }
}
