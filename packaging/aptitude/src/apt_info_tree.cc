// apt_info_tree.cc
//
//  Copyright 2000-2003, 2005, 2007 Daniel Burrows
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

#include "apt_info_tree.h"

#include <generic/apt/apt.h>

#include <sigc++/functors/mem_fun.h>

namespace cwidget
{
  using namespace widgets;
}

namespace cw = cwidget;

apt_info_tree::apt_info_tree(const string &_package, const string &_version)
  :package(_package), version(_version)
{
  cache_closed.connect(sigc::mem_fun(*this, &apt_info_tree::handle_cache_close));
  cache_reloaded.connect(sigc::mem_fun(*this, &apt_info_tree::restore_state));
}

void apt_info_tree::handle_cache_close()
{
  set_root(NULL);
}

void apt_info_tree::restore_state()
{
  cw::widget_ref tmpref(this);

  reset_incsearch();

  pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(package);
  if(pkg.end())
    destroy(); // Oopsie, go away
  else
    {
      pkgCache::VerIterator ver=pkg.VersionList();
      if(!version.empty())
	while(!ver.end() && ver.VerStr()!=version)
	  ++ver;
      if(ver.end() && !version.empty())
	// Our version of the package has vanished, pick any version.
	// (the alternative is to make the widget disappear, which I think is
	// uglier)
	{
	  version="";
	  set_root(setup_new_root(pkg, pkg.VersionList()), true);
	}
      else
	set_root(setup_new_root(pkg, ver), true);
    }

  reset_incsearch();
}

void apt_info_tree::repeat_signal()
{
  cw::widget_ref tmpref(this);

  get_selection()->highlighted_changed(true);
}
