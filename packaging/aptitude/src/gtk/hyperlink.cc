// hyperlink.cc             -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
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

#include "hyperlink.h"

namespace gui
{
  namespace
  {
    class HyperlinkTag : public Gtk::TextBuffer::Tag
    {
      // Used to ensure we only have one connection to the hyperlink
      // motion-notify event.
      static Glib::Quark textview_hyperlink_motion_notify_signal_connection_set_property;

      static void setup_text_view_motion_notify(const Glib::RefPtr<Gtk::TextView> &text_view)
      {
	if(!text_view->get_data(textview_hyperlink_motion_notify_signal_connection_set_property))
	  {
	    // "You should never _need_ to dereference a RefPtr"
	    //   -- Murray Cumming, glibmm designer
	    //
	    // The following hack is here because I need to pass a
	    // reference to the text_view into
	    // text_view_motion_notify.  For some reason that I don't
	    // understand, gtkmm hides the self pointer (which GTK+
	    // provides!) when invoking this signal.  I can't pass a
	    // RefPtr because then the text view will never be
	    // destroyed due to circular references.  But I can't get
	    // a bare pointer from the RefPtr, due to the above design
	    // decision.  And unlike cwidget, glibmm doesn't provide
	    // any way of getting a sigc++ weak reference from a
	    // RefPtr.  Hence the following scary stuff.
	    //
	    // Note that because we don't have a proper sigc++-aware
	    // weak reference, this is ONLY safe because the text view
	    // won't trigger a motion notification after it's
	    // destroyed.
	    GtkTextView *text_view_raw = text_view->gobj();

	    text_view->signal_motion_notify_event().connect(sigc::bind(sigc::ptr_fun(&HyperlinkTag::text_view_motion_notify), text_view_raw));
	    text_view->set_data(textview_hyperlink_motion_notify_signal_connection_set_property, (void *)1);
	  }
      }

      // We use a global signal connection, not one tied to tags, to
      // detect mouseovers because we need to be able to reset the
      // pointer when it's not over a tag.
      static bool text_view_motion_notify(GdkEventMotion *event,
					  GtkTextView *text_view_raw)
      {
	// Is this safe?  I *think* we should get a second reference
	// to the text view, not a copy of it...
	Glib::RefPtr<Gtk::TextView> text_view(Glib::wrap(text_view_raw, true));

	int buffer_x, buffer_y;

	text_view->window_to_buffer_coords(Gtk::TEXT_WINDOW_TEXT,
					   (int)event->x, (int)event->y,
					   buffer_x, buffer_y);

	Gtk::TextBuffer::iterator iter;
	int trailing;

	text_view->get_iter_at_position(iter, trailing,
					buffer_x, buffer_y);

	typedef Glib::SListHandle<Glib::RefPtr<Gtk::TextTag> > tags_list;
	tags_list tags(iter.get_tags());
	bool is_hyperlink = false;
	for(tags_list::const_iterator it = tags.begin();
	    !is_hyperlink && it != tags.end(); ++it)
	  {
	    Glib::RefPtr<const HyperlinkTag> hyperlink =
	      Glib::RefPtr<const HyperlinkTag>::cast_dynamic(*it);

	    if(hyperlink)
	      is_hyperlink = true;
	  }

	// Ideally, we should only reset the cursor if we were the last
	// ones to touch it -- is there a good way to do that?
	//
	// We magically know that XTERM is the cursor that TextViews
	// use.  (eek)
	Gdk::Cursor new_cursor(is_hyperlink ? Gdk::HAND2 : Gdk::XTERM);
	text_view->get_window(Gtk::TEXT_WINDOW_TEXT)->set_cursor(new_cursor);

	return false;
      }

      bool do_event(const Glib::RefPtr<Glib::Object> &event_object,
		    GdkEvent *event,
		    const Gtk::TextBuffer::iterator &iter)
      {
	Glib::RefPtr<Gtk::TextView> text_view =
	  Glib::RefPtr<Gtk::TextView>::cast_dynamic(event_object);
	if(text_view)
	  setup_text_view_motion_notify(text_view);

	// TODO: draw a nice "box" / change the style on
	// GDK_BUTTON_PRESS.
	switch(event->type)
	  {
	  case GDK_BUTTON_RELEASE:
	    {
	      if(event->button.button == 1)
		link_action();
	    }
	  default:
	    break;
	  }

	return false;
      }

      sigc::slot0<void> link_action;

      HyperlinkTag(const sigc::slot0<void> &_link_action)
	: Gtk::TextBuffer::Tag(),
	  link_action(_link_action)
      {
	signal_event().connect(sigc::mem_fun(*this, &HyperlinkTag::do_event));

	property_foreground() = "#3030FF";
	property_underline() = Pango::UNDERLINE_SINGLE;
      }

    public:
      /** \brief Create a tag that hyperlinks to the given action. */
      static Glib::RefPtr<HyperlinkTag> create(const sigc::slot0<void> &link_action)
      {
	return Glib::RefPtr<HyperlinkTag>(new HyperlinkTag(link_action));
      }
    };

    // Used to ensure we only have one connection to the hyperlink
    // motion-notify event.
    Glib::Quark HyperlinkTag::textview_hyperlink_motion_notify_signal_connection_set_property("aptitude-textview-hyperlink-motion-notify-signal-connection-set");
  }

  // TODO: make the mouse cursor change on hyperlinks.  The only
  // advice I can find on how to do this is to connect a signal to the
  // TextView that examines all the tags under the mouse and sets the
  // pointer depending on whether it finds a special one; otherwise it
  // sets the cursor to "edit". (in our case this would involve
  // dynamic_casting each tag to a derived class that implements a
  // "get_cursor()" method) This is gross and will require our own
  // special version of TextView, but OTOH it should work pretty well.
  Gtk::TextBuffer::iterator add_hyperlink(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					  Gtk::TextBuffer::iterator where,
					  const Glib::ustring &link_text,
					  const sigc::slot0<void> &link_action)
  {
    Glib::RefPtr<HyperlinkTag> tag = HyperlinkTag::create(link_action);
    buffer->get_tag_table()->add(tag);

    return buffer->insert_with_tag(where, link_text, tag);
  }
}
