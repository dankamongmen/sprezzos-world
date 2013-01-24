/** \file screenshot.h */    // -*-c++-*-


// Copyright (C) 2009 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef GTK_SCREENSHOT_CACHE_H
#define GTK_SCREENSHOT_CACHE_H

// GTK+ classes and functions relating to the screenshot cache.

#include <gdkmm/pixbuf.h>

#include <generic/apt/screenshot.h>

namespace gui
{
  // An abstract base class is qused here to simplify the header
  // and avoid information leaks.
  //
  // Currently, the actual loading of screenshots takes place in a
  // *foreground* thread.  This is necessary because it's not clear
  // that it's safe for a foreground thread to display a partial
  // PixBuf while a background thread is possibly write()ing to it.
  // In fact, it's probably *not* safe to do so.

  /** \brief Handle to a package's screenshot.
   *
   *  Internally, we cache the image buffers for the most recently
   *  acquired screenshots for reuse.  When client code requests a
   *  screenshot, it either gets a previously acquired screenshot or
   *  gets a reference to a screenshot that's being loaded from the
   *  network.
   */
  class cached_screenshot
  {
    sigc::signal<void, std::string> failed;
    sigc::signal<void> prepared;
    sigc::signal<void, int, int, int, int> updated;
    sigc::signal<void> ready;

  public:
    cached_screenshot()
    {
    }

    virtual ~cached_screenshot();

    /** \brief Get the signal invoked when a screenshot can't be
     *  acquired.
     */
    sigc::signal<void, std::string> &get_signal_failed() { return failed; }

    /** \brief Get the signal invoked when we're ready to download a
     *  screenshot.
     *
     *  Once this signal is invoked, the pixbuf has been created but
     *  isn't filled in yet.  The client code can modify it in
     *  response to this signal.
     */
    sigc::signal<void> &get_signal_prepared() { return prepared; }

    /** \brief Get the signal invoked when a region of the screenshot
     *  is updated.
     *
     *  The arguments are (x, y, width, height).
     */
    sigc::signal<void, int, int, int, int> &get_signal_updated() { return updated; }

    /** \brief Get the signal invoked when the image is ready to be
     *  displayed.
     *
     *  In some corner cases, the image might have been changed to a
     *  newly allocated object since the last signal was emitted.  The
     *  recipient should use get_screenshot() to ensure that the most
     *  recent image is being used.
     */
    sigc::signal<void> &get_signal_ready() { return ready; }

    /** \brief Get the screenshot associated with this handle, or
     *  return an invalid pointer if it's not available yet.
     */
    virtual Glib::RefPtr<Gdk::Pixbuf> get_screenshot() = 0;

    /** \brief Cancel downloading this screenshot, if possible. */
    virtual void cancel() = 0;
  };

  /** \brief Retrieve the given screenshot. */
  boost::shared_ptr<cached_screenshot> get_screenshot(const aptitude::screenshot_key &key);
}

#endif // GTK_SCREENSHOT_CACHE_H
