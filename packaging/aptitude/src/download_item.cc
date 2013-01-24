// download_item.cc
//
//  Copyright 1999, 2004-2005, 2007 Daniel Burrows
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

#include "aptitude.h"

#include "download_item.h"

#include <apt-pkg/acquire-worker.h>
#include <apt-pkg/strutl.h>

#include <cwidget/generic/util/transcode.h>
#include <cwidget/widgets/tree.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

cw::style download_item::get_normal_style()
{
  switch(item.Owner->Status)
    {
    case pkgAcquire::Item::StatIdle:
      return cw::treeitem::get_normal_style() + cw::style_attrs_on(A_DIM);
    case pkgAcquire::Item::StatFetching:
      return cw::treeitem::get_normal_style();
    case pkgAcquire::Item::StatDone:
      if(!hit)
	return cw::treeitem::get_normal_style() + cw::get_style("Progress");
      else
	return cw::treeitem::get_normal_style() + cw::get_style("DownloadHit");
    case pkgAcquire::Item::StatError:
      return cw::treeitem::get_normal_style() + cw::get_style("Error");
    case pkgAcquire::Item::StatTransientNetworkError:
      return cw::treeitem::get_normal_style() + cw::get_style("Error");
    case pkgAcquire::Item::StatAuthError:
      return cw::treeitem::get_normal_style() + cw::get_style("Error");
    default:
      eassert(0);
    }
}

void download_item::paint(cw::tree *win, int y, bool hierarchical,
			  const cw::style &st)
  // A little confusing -- basically, there are two branches: either we display
  // a progress bar, or we don't.  If we don't, I can just display it as usual
  // (note, though, that I don't yet indent it according to the depth..); if we
  // do, a rather specialized set of code is called and then returns. (that's
  // the confusing bit :) )
{
  string output=((item.Owner->Status==pkgAcquire::Item::StatFetching)?item.ShortDesc:item.Description)+": ";
  int width,height;

  win->getmaxyx(height,width);

  const cw::style progress_style=st + cw::get_style("DownloadProgress");
  const cw::style normal_style=st + get_normal_style();
  int barsize=width;

  switch(item.Owner->Status)
    {
    case pkgAcquire::Item::StatIdle:
      output+=_(" [Working]");
      break;
    case pkgAcquire::Item::StatFetching:
      if(worker==NULL)
	output+=_(" [Working]");
      else
	{
	  eassert(worker->CurrentItem->Owner==item.Owner);

	  if(worker->TotalSize>0)
	    {
	      char intbuf[50]; // Waay more than enough.
	      barsize=(width*worker->CurrentSize)/worker->TotalSize;
	      win->apply_style(progress_style);

	      if(barsize>width)
		barsize=width;

	      win->apply_style(get_normal_style());
	      sprintf(intbuf,
		      "%sB/%sB",
		      SizeToStr(worker->CurrentSize).c_str(),
		      SizeToStr(worker->TotalSize).c_str());
	      output+=string(" [ ")+intbuf+" ]";
	    }
	  else
	    output+=_(" [Working]");
	}
      break;
    case pkgAcquire::Item::StatDone:
      output+=hit?_("[Hit]"):_("[Downloaded]");
      break;
    case pkgAcquire::Item::StatError:
      output+=item.Owner->ErrorText;
      break;
    case pkgAcquire::Item::StatTransientNetworkError:
      output += item.Owner->ErrorText;
      break;
    case pkgAcquire::Item::StatAuthError:
      output+=item.Owner->ErrorText;
      break;
    }

  win->show_string_as_progbar(0, y, cw::util::transcode(output),
			      progress_style, normal_style,
			      barsize, width);
}
