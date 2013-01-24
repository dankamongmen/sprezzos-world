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

#ifndef GTK_SCREENSHOT_H
#define GTK_SCREENSHOT_H

// GTK+ classes and functions relating to screenshots.

#include <gtkmm/eventbox.h>
#include <gtkmm/image.h>

#include <boost/shared_ptr.hpp>

#include <generic/apt/screenshot.h>

namespace gui
{
  class cached_screenshot;

  class screenshot_image : public Gtk::EventBox
  {
    Gtk::Image image;

    // These are used to generate a new screenshot object when the
    // image is exposed.
    std::string package_name;
    aptitude::screenshot_type type;

    sigc::connection screenshot_prepared_connection;
    sigc::connection screenshot_failed_connection;
    sigc::connection screenshot_updated_connection;
    sigc::connection screenshot_ready_connection;

    boost::shared_ptr<cached_screenshot> screenshot;
    // Set to true when the download succeeds; used to decide whether
    // cancel_download() should blank the image.
    bool download_complete : 1;
    bool show_missing_image_icon : 1;
    // Remembers whether we would show a missing image icon.  Set to
    // true when a download fails, cleared when a download starts.
    bool image_missing : 1;

    bool clickable : 1;

    // If image_missing is true, stores the error message associated
    // with not being able to download a screenshot.
    std::string image_missing_error_message;

    std::string describe() const;

    /** \brief If there is an active screenshot download, detach from
     *	it.
     */
    void disconnect();

    /** \brief If there is not an active screenshot download, start
     *	one.
     */
    void connect();

    /** \brief Invoked when the screenshot download fails. */
    void failed(const std::string &msg);

    /** \brief Invoked when the screenshot object's pixbuf is created.
     */
    void prepared();

    /** \brief Invoked when the screenshot is entirely loaded.
     */

    void success();

    /** \brief Invoked when part of the screenshot has been loaded. */
    void updated(int x, int y, int width, int height);

    /** \brief Discard any partially downloaded screenshot.
     *
     *  If we should display the missing-image icon, this displays it
     *  and sets the tooltip text to image_missing_error_message.
     */
    void update_image_for_failed_download();

  protected:
    // Starts downloading a new screenshot if the last download didn't
    // fail.
    bool on_expose_event(GdkEventExpose *event);

    // Sets up the mouse cursor on the new window, if this is
    // clickable.
    void on_realize();

    // If this is clickable, dispatches mouse clicks.
    bool on_button_press_event(GdkEventButton *event);

  public:
    /** \brief Create a screenshot image widget.
     *
     *  \param _package_name  The name of the package whose screenshot
     *                        should be downloaded.
     *  \param _type          The type of screenshot to download.
     */
    screenshot_image(const std::string &_package_name,
		     aptitude::screenshot_type _type);

    /** \brief Enable or disable showing the missing image icon. */
    void set_show_missing_image_icon(bool new_value);

    /** \brief Make this screenshot clickable. */
    void enable_clickable();

    /** \brief Invoke to start a download, even if the widget is not
     *	visible.
     */
    void start_download();

    /** \brief Invoke to cancel any pending download.
     *
     *  If the screenshot was already downloaded, this has no effect.
     */
    void cancel_download();

    /** \brief Emitted when the screenshot is clicked,
     *  if this is a clickable image.
     */
    sigc::signal<void> clicked;
  };
}

#endif // GTK_SCREENSHOT_H
