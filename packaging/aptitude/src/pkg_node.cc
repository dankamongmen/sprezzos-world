// pkg_node.cc
//
//  Copyright 1999, 2000, 2002, 2005 Daniel Burrows
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
//  Implementations of stuff in pkg_node.h

#include <cwidget/config/keybindings.h>

#include "pkg_node.h"

#include "ui.h"

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/config_signal.h>

#include <generic/util/undo.h>

#include <cwidget/widgets/tree.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

cw::config::keybindings *pkg_tree_node::bindings=NULL;

void pkg_tree_node::init_bindings()
{
  bindings = new cw::config::keybindings(&cw::config::global_bindings);
}

// FIXME: add a do_action() command that takes a function pointer and does all
// the extra junk below.
bool pkg_tree_node::dispatch_key(const cw::config::key &k, cw::tree *owner)
{
  undo_group *grp=new apt_undo_group;

  if(bindings->key_matches(k, "Install"))
    select(grp);
  else if(bindings->key_matches(k, "Remove"))
    remove(grp);
  else if(bindings->key_matches(k, "Hold"))
    hold(grp);
  else if(bindings->key_matches(k, "Keep"))
    keep(grp);
  else if(bindings->key_matches(k, "Purge"))
    purge(grp);
  else if(bindings->key_matches(k, "Reinstall"))
    reinstall(grp);
  else if(bindings->key_matches(k, "SetAuto"))
    set_auto(true, grp);
  else if(bindings->key_matches(k, "ClearAuto"))
    set_auto(false, grp);
  else
    {
      delete grp;

      return false;
    }

  if(!grp->empty())
    apt_undos->add_item(grp);
  else
    delete grp;

  if(aptcfg->FindB(PACKAGE "::UI::Advance-On-Action", false))
    owner->level_line_down();

  package_states_changed();

  return true;
}

//////////////////////////// Menu Redirections //////////////////////////

bool pkg_tree_node::package_action(void (pkg_tree_node::* action)(undo_group *))
{
  undo_group *grp = new apt_undo_group;
  (this->*action)(grp);
  if(!grp->empty())
    apt_undos->add_item(grp);
  else
    delete grp;

  package_states_changed();

  return true;
}

bool pkg_tree_node::package_enabled()
{
  return true;
}

bool pkg_tree_node::package_install()
{
  return package_action(&pkg_tree_node::select);

}

bool pkg_tree_node::package_remove()
{
  return package_action(&pkg_tree_node::remove);
}

bool pkg_tree_node::package_purge()
{
  return package_action(&pkg_tree_node::purge);
}

bool pkg_tree_node::package_keep()
{
  return package_action(&pkg_tree_node::keep);
}

bool pkg_tree_node::package_hold()
{
  return package_action(&pkg_tree_node::hold);
}

bool pkg_tree_node::package_mark_auto()
{
  return package_action(&pkg_tree_node::mark_auto);
}

bool pkg_tree_node::package_unmark_auto()
{
  return package_action(&pkg_tree_node::unmark_auto);
}
