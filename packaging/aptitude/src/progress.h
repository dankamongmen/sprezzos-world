// progress.h   -*-c++-*-
//
//  Copyright 2000, 2004-2005, 2007-2008 Daniel Burrows
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

#ifndef VS_PROGRESS_H
#define VS_PROGRESS_H

#include <cwidget/widgets/tree.h>
#include <cwidget/widgets/widget.h>

#include <apt-pkg/progress.h>

// \todo This should move to src/generic.
#include "ui_download_manager.h" // for refcounted_progress

/** \brief A cwidget::widgets::widget that also acts as a progress bar.
 * 
 *  \file progress.h
 */

// The two-level structure here is so we can derive from
// refcounted_progress (whose refcounting is incompatible with the
// widget class's refcounting).
//
// Probably at some point progress_widget should become a generic
// progress bar that lives in cwidget, at which point this would just
// be a small shim to hook it up.
class progress : public cwidget::widgets::widget
{
  class progress_progress : public 
refcounted_progress
  {
    progress_progress()
    {
    }

  public:
    static cwidget::util::ref_ptr<progress_progress> create()
    {
      return new progress_progress;
    };

    // I use signals to interconnect the two parts so that if the
    // widget dies we don't have bad things happen.
    sigc::signal<void> Update_sig;
    sigc::signal<void> Done_sig;

    void Update();
    void Done();

    friend class progress;
  };

  cwidget::util::ref_ptr<progress_progress> p;

protected:
  progress();

public:
  static cwidget::util::ref_ptr<progress> create()
  {
    return new progress;
  }

  virtual void paint(const cwidget::style &st);

  void Update();
  void Done();

  int width_request();
  int height_request(int w);

  bool get_cursorvisible();
  cwidget::widgets::point get_cursorloc();

  cwidget::util::ref_ptr<refcounted_progress> get_progress() const { return p; }
};

typedef cwidget::util::ref_ptr<progress> progress_ref;

#endif
