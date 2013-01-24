// cw::progress.cc
//
//  Copyright 2000, 2004-2008 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "aptitude.h"

#include "progress.h"

#include <sstream>

#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

namespace
{
  // Converts a percentage between 0 and 100 to an integer for
  // display purposes.  Out-of-range values are clamped, to avoid
  // nasty boundary problems.
  int convertPercent(double Percent)
  {
    int rval = (int)Percent;
    if(rval < 0)
      rval = 0;
    if(rval > 100)
      rval = 100;

    return rval;
  }
}

void progress::paint(const cw::style &st)
{
  int width=getmaxx();

  if(!p->Op.empty())
    {
      int truncPercent = convertPercent(p->Percent);

      std::ostringstream percentstream;
      percentstream << " " << truncPercent << "%";
      std::string percentstr = percentstream.str();

      mvaddstr(0, 0, cw::util::transcode(p->Op));
      mvaddstr(0, width - percentstr.size(), cw::util::transcode(percentstr));
      refresh();
    }
  else
    erase();
}

bool progress::get_cursorvisible()
{
  return false;
}

cw::point progress::get_cursorloc()
{
  double xd = (p->Percent * getmaxx()) / 100.0;
  int x     = static_cast<int>(xd);
  int maxx  = getmaxx();

  x = std::min(x, maxx);
  x = std::max(x, 0);
  return cw::point(x, 0);
}

void progress::Update()
{
  show();

  if(p->CheckChange(0.25))
    {
      cw::toplevel::update();
      cw::toplevel::updatecursor();
      cw::toplevel::tryupdate();
    }
}

void progress::Done()
{
  cw::toplevel::update();
  cw::toplevel::updatecursor();
  cw::toplevel::tryupdate();
  //hide();
}

int progress::width_request()
{
  return 40;
}

int progress::height_request(int w)
{
  return 1;
}

progress::progress()
  : p(progress_progress::create())
{
  p->Update_sig.connect(sigc::mem_fun(*this, &progress::Update));
  p->Done_sig.connect(sigc::mem_fun(*this, &progress::Done));
}

void progress::progress_progress::Update()
{
  Update_sig();
}

void progress::progress_progress::Done()
{
  Done_sig();
}
