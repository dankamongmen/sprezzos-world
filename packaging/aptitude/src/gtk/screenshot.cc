/** \file screenshot.cc */


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

#include "screenshot.h"

#include <loggers.h>

#include <gdkmm/cursor.h>
#include <gtkmm/stock.h>

#include <gtk/screenshot_cache.h>

using aptitude::Loggers;

namespace gui
{
  // TODO: this shows up in a couple places; it might be a sign that I
  // should have a "screenshot_id" class with its own operator<<,
  // hash, equality, etc.
  std::string screenshot_image::describe() const
  {
    std::string typestr;
    switch(type)
      {
      case aptitude::screenshot_thumbnail:
	typestr = "thumbnail";
	break;

      case aptitude::screenshot_full:
	typestr = "full-size";
	break;

      default:
	typestr = "BADTYPE";
	break;
      }

    return "the " + typestr + " screenshot of " + package_name;
  }

  void screenshot_image::failed(const std::string &msg)
  {
    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: failed to download "
	      << aptitude::screenshot_key(type, package_name)
	      << ": " << msg);

    image_missing = true;
    image_missing_error_message = msg;

    disconnect();
  }

  void screenshot_image::disconnect()
  {
    if(screenshot.get() != NULL && !download_complete)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "screenshot_image: disconnecting " << describe());

	screenshot_prepared_connection.disconnect();
	screenshot_failed_connection.disconnect();
	screenshot_updated_connection.disconnect();
	screenshot_ready_connection.disconnect();

	screenshot->cancel();
	screenshot.reset();
      }

    update_image_for_failed_download();
  }

  void screenshot_image::prepared()
  {
    image.set(screenshot->get_screenshot());

    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: pixbuf prepared for " << describe());
  }

  void screenshot_image::success()
  {
    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: registering success for " << describe());

    image.set(screenshot->get_screenshot());
    download_complete = true;
  }

  void screenshot_image::updated(int x, int y, int width, int height)
  {
    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: update for (" << x << ", " << y << ", " << width << ", " << height << ")");

    queue_draw_area(x, y, width, height);
  }

  void screenshot_image::update_image_for_failed_download()
  {
    image.clear();

    if(!image_missing)
      LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		"screenshot_image: not displaying the missing-image icon for "
		<< aptitude::screenshot_key(type, package_name)
		<< ": the image is not missing.");
    else if(!show_missing_image_icon)
      LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		"screenshot_image: not displaying the missing-image icon for "
		<< aptitude::screenshot_key(type, package_name)
		<< ": it is missing, but the missing image icon is disabled.");
    else
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "Displaying the missing-image icon for "
		  << aptitude::screenshot_key(type, package_name)
		  << ": " << image_missing_error_message);

	Gtk::Stock::lookup(Gtk::Stock::MISSING_IMAGE,
			   Gtk::ICON_SIZE_DIALOG,
			   image);
	set_tooltip_text(image_missing_error_message);
      }
  }

  void screenshot_image::connect()
  {
    if(screenshot.get() == NULL)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "screenshot_image: requesting " << describe());

	// Clear the missing-image flag and drop any associated
	// tooltip.
	image_missing = false;
	image_missing_error_message.clear();
	set_has_tooltip(false);

	screenshot = get_screenshot(aptitude::screenshot_key(type, package_name));

	screenshot_failed_connection = screenshot->get_signal_failed().connect(sigc::mem_fun(*this, &screenshot_image::failed));
	screenshot_prepared_connection = screenshot->get_signal_prepared().connect(sigc::mem_fun(*this, &screenshot_image::prepared));
	screenshot_updated_connection = screenshot->get_signal_updated().connect(sigc::mem_fun(*this, &screenshot_image::updated));
	screenshot_ready_connection = screenshot->get_signal_ready().connect(sigc::mem_fun(*this, &screenshot_image::success));

	// If the screenshot is already loaded, we reuse it; otherwise
	// we wait for a signal informing us that it's ready.
	Glib::RefPtr<Gdk::Pixbuf> current_screenshot = screenshot->get_screenshot();
	if(current_screenshot)
	  image.set(current_screenshot);
	else
	  image.clear();

	show();
      }
  }

  screenshot_image::screenshot_image(const std::string &_package_name,
				     aptitude::screenshot_type _type)
    : package_name(_package_name),
      type(_type),
      download_complete(false),
      show_missing_image_icon(false),
      image_missing(false),
      clickable(false)
  {
    image.show();
    add(image);
  }

  void screenshot_image::set_show_missing_image_icon(bool new_value)
  {
    show_missing_image_icon = new_value;

    if(screenshot.get() == NULL)
      update_image_for_failed_download();
  }

  void screenshot_image::start_download()
  {
    connect();
  }

  void screenshot_image::cancel_download()
  {
    disconnect();
  }



  bool screenshot_image::on_expose_event(GdkEventExpose *event)
  {
    // Note that we only try to start a download if we didn't fail the
    // last time.
    if(!image_missing)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "Starting to download " << aptitude::screenshot_key(type, package_name)
		  << " in response to an expose event.");
	connect();
      }
    else
      LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		"Not downloading " << aptitude::screenshot_key(type, package_name)
		<< ": the last download failed with this message: " << image_missing_error_message);

    return Gtk::EventBox::on_expose_event(event);
  }

  void screenshot_image::on_realize()
  {
    Gtk::EventBox::on_realize();

    // Check that the window really exists, just out of paranoia:
    if(clickable && is_realized() && get_window())
      get_window()->set_cursor(Gdk::Cursor(Gdk::HAND1));
  }

  bool screenshot_image::on_button_press_event(GdkEventButton *event)
  {
    switch(event->type)
      {
      case GDK_BUTTON_PRESS:
	if(clickable)
	  clicked();
	return true;
      default:
	return Gtk::EventBox::on_button_press_event(event);
      }
  }

  void screenshot_image::enable_clickable()
  {
    clickable = true;
    if(is_realized() && get_window())
      get_window()->set_cursor(Gdk::Cursor(Gdk::HAND1));
  }
}
