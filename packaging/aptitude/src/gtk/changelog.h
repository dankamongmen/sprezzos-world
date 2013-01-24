// changelog.h             -*-c++-*-
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef CHANGELOG_H_
#define CHANGELOG_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/acquire.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>
#include <generic/util/temp.h>
#include <generic/util/util.h>

namespace aptitude
{
  namespace apt
  {
    class changelog;
  }
}

namespace gui
{
  class dummyPkgAcquireStatus : public pkgAcquireStatus
  { // must also derive to read protected members..
    public:
      dummyPkgAcquireStatus();
      bool MediaChange(std::string, std::string);
  };

  /** \brief Render a changelog object into a buffer.
   *
   *  If the changelog is invalid, an error message will be displayed
   *  instead.
   *
   *  \param cl                The changelog to render.
   *  \param textBuffer        The text buffer in which to store
   *                           the rendered text.
   *  \param current_version   The currently installed source
   *                           version of the package whose
   *                           changelog this is (used to
   *                           determine which versions are
   *                           newer).
   *  \param where             The buffer location at which to render
   *                           the changelog.
   *  \param only_new          Set to \b true to only show entries that
   *                           are newer than current_version.  If this
   *                           is set, the changelog will be truncated
   *                           if version numbers that are out of order
   *                           are encountered.  (this avoids displaying
   *                           the whole changelog for packages where a
   *                           past version "reset" the version number)
   *
   *  \return A new iterator to the end of the rendered text.
   */
  Gtk::TextBuffer::iterator
  render_changelog(const cwidget::util::ref_ptr<aptitude::apt::changelog> &cl,
		   const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		   const std::string &current_version,
		   Gtk::TextBuffer::iterator where,
		   bool only_new);

  /** \brief Add a changelog to the queue of changelogs that are to be
   *  downloaded in the background.
   *  \param begin   The buffer location where the changelog should be inserted.
   *  \param text_buffer  The text buffer in which to insert the changelog.
   *  \param text_view The text view in which we will insert the changelog.
   *                   Used to display a progress bar during the download.
   *  \param ver     The version whose changelog should be downloaded.
   *  \param only_new  If \b true, only new entries will be displayed.
   *
   *  \return An iterator following any text inserted by this routine.
   */
  Gtk::TextBuffer::iterator fetch_and_show_changelog(const pkgCache::VerIterator &ver,
						     const Glib::RefPtr<Gtk::TextBuffer> &text_buffer,
						     Gtk::TextView *text_view,
						     const Gtk::TextBuffer::iterator &where,
						     bool only_new = false);
}

#endif /* CHANGELOG_H_ */
