// -*-c++-*-

// changelog.cpp
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

#include "changelog.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/ssprintf.h>

#include <loggers.h>

#include <generic/apt/apt.h>
#include <generic/apt/changelog_parse.h>
#include <generic/apt/download_queue.h>
#include <generic/apt/pkg_changelog.h>

#include <generic/util/file_cache.h>
#include <generic/util/job_queue_thread.h>

#include <gtk/hyperlink.h>
#include <gtk/gui.h>
#include <gtk/progress.h>

#include <boost/make_shared.hpp>

using aptitude::Loggers;
using aptitude::apt::get_changelog;

namespace cw = cwidget;

namespace gui
{
  namespace
  {
    void view_bug(const std::string &bug_number)
    {
      std::string URL = cw::util::ssprintf("http://bugs.debian.org/%s", bug_number.c_str());

      std::vector<std::string> arguments;
      arguments.push_back("/usr/bin/sensible-browser");
      arguments.push_back(URL);

      Glib::spawn_async(".", arguments);
    }
  }

  using aptitude::apt::changelog_element_list;
  using aptitude::apt::changelog_element;
  Gtk::TextBuffer::iterator
  render_change_elements(const std::string &text,
			 const std::vector<changelog_element> &elements,
			 const Glib::RefPtr<Gtk::TextBuffer> &buffer,
			 Gtk::TextBuffer::iterator where)
  {
    std::vector<changelog_element>::const_iterator elements_end = elements.end();

    Glib::RefPtr<Gtk::TextBuffer::Tag> bullet_tag = buffer->create_tag();
    bullet_tag->property_weight() = Pango::WEIGHT_BOLD;
    bullet_tag->property_weight_set() = true;

    for(std::vector<changelog_element>::const_iterator it =
	  elements.begin(); it != elements_end; ++it)
      {
	switch(it->get_type())
	  {
	  case changelog_element::text_type:
	    where = buffer->insert(where,
				   text.c_str() + it->get_begin(),
				   text.c_str() + it->get_end());
	    break;

	  case changelog_element::bullet_type:
	    where = buffer->insert_with_tag(where,
					    text.c_str() + it->get_begin(),
					    text.c_str() + it->get_end(),
					    bullet_tag);
	    break;

	  case changelog_element::closes_type:
	    {
	      const std::string bug_number(text, it->get_begin(), it->get_end() - it->get_begin());
	      where = add_hyperlink(buffer, where,
				    bug_number,
				    sigc::bind(sigc::ptr_fun(&view_bug),
					       bug_number));
	    }
	  }
      }

    return where;
  }

  namespace
  {
    class TextBufferUserAction
    {
      Glib::RefPtr<Gtk::TextBuffer> buffer;

    public:
      TextBufferUserAction(const Glib::RefPtr<Gtk::TextBuffer> &_buffer)
	: buffer(_buffer)
      {
	buffer->begin_user_action();
      }

      ~TextBufferUserAction()
      {
	buffer->end_user_action();
      }
    };
  }

  // \todo Maybe support hiding older versions by default, with a
  // clickable link at the end saying "show older versions...".
  //
  // NB: some changelogs aren't monotonically increasing, so that
  // support should put a link wherever the "newness state" goes from
  // newer to not-newer.
  static Gtk::TextBuffer::iterator
  do_render_changelog(const cwidget::util::ref_ptr<aptitude::apt::changelog> &cl,
		      const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		      const std::string &current_version,
		      Gtk::TextBuffer::iterator where,
		      bool only_new)
  {
    using aptitude::apt::changelog;

    // Don't update the display until we finish everything.
    TextBufferUserAction text_buffer_user_action_scope(textBuffer);

    Glib::RefPtr<Gtk::TextBuffer::Tag> newer_tag(textBuffer->create_tag());
    newer_tag->property_weight() = Pango::WEIGHT_SEMIBOLD;
    newer_tag->property_weight_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> number_tag = textBuffer->create_tag();
    number_tag->property_scale() = Pango::SCALE_LARGE;

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_low_tag = textBuffer->create_tag();

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_medium_tag = textBuffer->create_tag();
    urgency_medium_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_medium_tag->property_weight_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_high_tag = textBuffer->create_tag();
    urgency_high_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_high_tag->property_weight_set() = true;
    urgency_high_tag->property_foreground() = "#FF0000";
    urgency_high_tag->property_foreground_set() = true;

    // NB: "emergency" and "critical" are the same; thus saith
    // Policy.
    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_critical_tag = textBuffer->create_tag();
    urgency_critical_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_critical_tag->property_weight_set() = true;
    urgency_critical_tag->property_scale() = Pango::SCALE_LARGE;
    urgency_critical_tag->property_foreground() = "#FF0000";
    urgency_critical_tag->property_foreground_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> date_tag = textBuffer->create_tag();

    // Remember whether we added any changelog entries, so we can show
    // a message if there aren't any.
    bool added_at_least_one = false;
    std::string last_version;

    for(changelog::const_iterator it = cl->begin(); it != cl->end(); ++it)
      {
	cw::util::ref_ptr<aptitude::apt::changelog_entry> ent(*it);

	if(only_new)
	  {
	    // Check whether the version numbers in the changelog are
	    // out-of-order.  We start with the most recent changelog
	    // entry, so they should be decreasing.  If they aren't in
	    // order, stop generating output.
	    //
	    // This is necessary because in practice, some package
	    // changelogs contain many entries that are "newer" than
	    // the most recent entry.  Including those entries
	    // produces output that is both excessive and wrong.
	    const bool retrograde =
	      !last_version.empty() &&
	      _system->VS->CmpVersion(ent->get_version(), last_version) > 0;
	    if(retrograde)
	      break;
	    last_version = ent->get_version();
	  }

	bool newer =
	  !current_version.empty() &&
	  _system->VS->CmpVersion(ent->get_version(), current_version) > 0;

	// If the current entry isn't newer, we would have to have
	// out-of-order entries in order to see a newer one, so stop
	// scanning the changelog at that point.
	if(only_new && !newer)
	  break;

	added_at_least_one = true;

	const bool use_newer_tag = !only_new && newer;

	if(it != cl->begin())
	  where = textBuffer->insert(where, "\n\n");

	Glib::RefPtr<Gtk::TextBuffer::Mark> changelog_entry_mark;
	if(use_newer_tag)
	  changelog_entry_mark = textBuffer->create_mark(where);

	// Can't hyperlink to the package name because it's a
	// source package name.  Plus, it might not exist.
	where = textBuffer->insert(where, ent->get_source());
	where = textBuffer->insert(where, " (");
	where = textBuffer->insert_with_tag(where,
					    ent->get_version(),
					    number_tag);
	where = textBuffer->insert(where, ") ");
	where = textBuffer->insert(where, ent->get_distribution());
	where = textBuffer->insert(where, "; urgency=");
	Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_tag;
	const std::string &urgency = ent->get_urgency();
	if(urgency == "low")
	  urgency_tag = urgency_low_tag;
	else if(urgency == "medium")
	  urgency_tag = urgency_medium_tag;
	else if(urgency == "high")
	  urgency_tag = urgency_high_tag;
	else if(urgency == "critical" || urgency == "emergency")
	  urgency_tag = urgency_critical_tag;

	if(urgency_tag)
	  where = textBuffer->insert_with_tag(where, urgency, urgency_tag);
	else
	  where = textBuffer->insert(where, urgency);

	where = textBuffer->insert(where, "\n");

	where = render_change_elements(ent->get_changes(), ent->get_elements()->get_elements(), textBuffer, where);

	where = textBuffer->insert(where, "\n\n");
	where = textBuffer->insert(where, " -- ");
	where = textBuffer->insert(where, ent->get_maintainer());
	where = textBuffer->insert(where, " ");
	where = textBuffer->insert_with_tag(where, ent->get_date_str(), date_tag);

	if(use_newer_tag)
	  {
	    Gtk::TextBuffer::iterator start = textBuffer->get_iter_at_mark(changelog_entry_mark);
	    textBuffer->apply_tag(newer_tag, start, where);
	  }
      }

    if(!added_at_least_one)
      {
	if(cl->size() == 0)
	  where = textBuffer->insert(where, _("The changelog is empty."));
	else if((*cl->begin())->get_version() == current_version)
	  where = textBuffer->insert(where, _("No new changelog entries; it looks like you installed a locally compiled version of this package."));
	else
	  where = textBuffer->insert(where, _("No new changelog entries; this is likely due to a binary-only upload of this package."));
      }

    return where;
  }

  Gtk::TextBuffer::iterator
  render_changelog(const cw::util::ref_ptr<aptitude::apt::changelog> &cl,
		   const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		   const std::string &current_version,
		   Gtk::TextBuffer::iterator where,
		   bool only_new)
  {
    if(cl.valid())
      return do_render_changelog(cl, textBuffer, current_version, where, only_new);
    else
      {
        Glib::RefPtr<Gtk::TextBuffer::Tag> warning_tag = textBuffer->create_tag();
        warning_tag->property_weight() = Pango::WEIGHT_BOLD;
        warning_tag->property_weight_set() = true;
        warning_tag->property_foreground() = "#FF0000";
        warning_tag->property_foreground_set() = true;
        return textBuffer->insert_with_tag(where,
					   "Can't parse changelog, did you install the libparse-debianchangelog-perl package ?", warning_tag);
        // \todo Offer to install libparse-debianchangelog-perl if we
        // can't parse the changelog because it's missing.
        // Maybe we could add an action button that does that ?
      }
  }

  namespace
  {
    // Value type carrying information for the helper below.  It says
    // which region of the buffer should be replaced with the
    // changelog text.
    struct finish_changelog_download_info
    {
      Glib::RefPtr<Gtk::TextBuffer::Mark> begin, end;
      Glib::RefPtr<Gtk::TextBuffer> text_buffer;
      // Used for logging.
      aptitude::apt::changelog_info info;
      std::string current_version;
      bool only_new;

      finish_changelog_download_info(const Glib::RefPtr<Gtk::TextBuffer::Mark> &_begin,
				     const Glib::RefPtr<Gtk::TextBuffer::Mark> &_end,
				     const Glib::RefPtr<Gtk::TextBuffer> &_text_buffer,
                                     const aptitude::apt::changelog_info &_info,
				     const std::string &_current_version,
				     bool _only_new)
	: begin(_begin),
	  end(_end),
	  text_buffer(_text_buffer),
          info(_info),
	  current_version(_current_version),
	  only_new(_only_new)
      {
      }
    };

    // Deletes a range between two marks, inserting a changelog parsed
    // from the given file in the gap.
    //
    // This is invoked in the main thread (via the trampoline
    // do_view_changelog_trampoline) when a changelog is finished
    // downloading.
    void finish_changelog_download(const cw::util::ref_ptr<aptitude::apt::changelog> &cl,
				   const boost::shared_ptr<finish_changelog_download_info> &download_info)
    {
      LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
                "Finishing the download of "
                << download_info->info.get_source_package()
                << " "
                << download_info->info.get_source_version()
                << ".");

      const Gtk::TextBuffer::iterator begin = download_info->text_buffer->get_iter_at_mark(download_info->begin);
      const Gtk::TextBuffer::iterator end = download_info->text_buffer->get_iter_at_mark(download_info->end);

      // Zap the old text.
      const Gtk::TextBuffer::iterator where = download_info->text_buffer->erase(begin, end);

      const bool only_new = download_info->only_new;

      // Now insert the changelog.
      render_changelog(cl,
		       download_info->text_buffer,
		       download_info->current_version,
		       where,
		       only_new);
    }

    void changelog_download_error(const std::string &error,
				  const boost::shared_ptr<finish_changelog_download_info> download_info)
    {
      const Gtk::TextBuffer::iterator begin = download_info->text_buffer->get_iter_at_mark(download_info->begin);
      const Gtk::TextBuffer::iterator end = download_info->text_buffer->get_iter_at_mark(download_info->end);

      // Zap the old text.
      const Gtk::TextBuffer::iterator where = download_info->text_buffer->erase(begin, end);

      download_info->text_buffer->insert(where,
					 ssprintf(_("Failed to download the changelog: %s"), error.c_str()));
    }


    /** \brief The job structure after the frontend routine has
     *  processed it and before it's passed through
     *  process_changelog_job.
     *
     *  All references to structures in the apt cache are stripped
     *  out, so that it's safe to use this even after a cache reload.
     *  That avoids a world of hurt around trying to manage the job
     *  queue and ensure that it's entirely cleaned out when the cache
     *  is closed.
     */
    class preprocessed_changelog_job
    {
      // A slot to add a widget into the text view that we're
      // rendering into.  Used to hopefully avoid problems if the text
      // view is destroyed (the slot will become a NOP).  Note that
      // this calls back to a method on this object that the job is
      // bound to.
      sigc::slot<void, Gtk::Widget &, Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> > text_view_add_child_at_anchor;

      // The region of the text buffer that the previous bit of code
      // used to display the changelog's status.
      Glib::RefPtr<Gtk::TextBuffer::Mark> begin, end;
      Glib::RefPtr<Gtk::TextBuffer> text_buffer;

      bool only_new;

      std::string binary_package_name;

      boost::shared_ptr<aptitude::apt::changelog_info> target_info;
      boost::shared_ptr<aptitude::apt::changelog_info> current_info;

      std::set<std::string> origins;

    public:
      explicit preprocessed_changelog_job(const Glib::RefPtr<Gtk::TextBuffer::Mark> &_begin,
					  const Glib::RefPtr<Gtk::TextBuffer::Mark> &_end,
					  const Glib::RefPtr<Gtk::TextBuffer> &_text_buffer,
					  sigc::slot<void, Gtk::Widget &, Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> > _text_view_add_child_at_anchor,
					  const pkgCache::VerIterator &ver,
					  bool _only_new)
	: text_view_add_child_at_anchor(_text_view_add_child_at_anchor),
	  begin(_begin),
	  end(_end),
	  text_buffer(_text_buffer),
	  only_new(_only_new),
	  binary_package_name(ver.ParentPkg().Name()),
	  target_info(aptitude::apt::changelog_info::create(ver)),
	  current_info(aptitude::apt::changelog_info::create(ver.ParentPkg().CurrentVer()))
      {
	for(pkgCache::VerFileIterator vf = ver.FileList();
	    !vf.end(); ++vf)
	  {
	    if(!vf.File().end() && vf.File().Origin() != NULL)
	      origins.insert(vf.File().Origin());
	  }
      }
      const Glib::RefPtr<Gtk::TextBuffer::Mark> &get_begin() const { return begin; }
      const Glib::RefPtr<Gtk::TextBuffer::Mark> &get_end() const { return end; }
      const Glib::RefPtr<Gtk::TextBuffer> &get_text_buffer() const { return text_buffer; }
      //Gtk::TextView *get_text_view() const { return text_view; }
      bool get_only_new() const { return only_new; }
      const std::string &get_binary_package_name() const { return binary_package_name; }
      const boost::shared_ptr<aptitude::apt::changelog_info> &get_target_info() const { return target_info; }
      const boost::shared_ptr<aptitude::apt::changelog_info> &get_current_info() const { return current_info; }
      const std::set<std::string> &get_origins() const { return origins; }

      void add_child_at_anchor(Gtk::Widget &child,
			       const Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> &anchor)
      {
	text_view_add_child_at_anchor(child, anchor);
      }
    };

    std::ostream &operator<<(std::ostream &out,
                             const std::set<std::string> &s)
    {
      out << "{";
      for(std::set<std::string>::const_iterator it = s.begin();
          it != s.end(); ++it)
        {
          if(it != s.begin())
            out << ", ";
          out << *it;
        }
      out << "}";

      return out;
    }

    std::ostream &operator<<(std::ostream &out, const preprocessed_changelog_job &job)
    {
      const aptitude::apt::changelog_info &target_info = *job.get_target_info();

      return out << "(binaryPackageName = "
                 << job.get_binary_package_name()
                 << ", sourcePackage = "
                 << target_info.get_source_package()
                 << ", sourceVersion = "
                 << target_info.get_source_version()
                 << ", onlyNew = "
                 << (job.get_only_new() ? "true" : "false")
                 << ", origins = "
                 << job.get_origins()
                 << ")";
    }

    // \todo It would be uber-cool to have a progress bar for each
    // changelog being downloaded.  To do that, we need to start by
    // reporting download events here, or possibly by just
    // manipulating the progress bar directly (remember that all
    // callbacks happen in hte main thread).
    class changelog_download_callbacks : public aptitude::download_callbacks
    {
      // Information about the download that should eventually be
      // passed to finish_changelog_download.
      boost::shared_ptr<finish_changelog_download_info> download_info;

      // Information about the changelog that should be passed to the
      // parse thread.
      std::string from;
      std::string to;
      std::string source_package;

      // Information used to nicely display information about the
      // package.
      std::string source_version;

      boost::shared_ptr<Gtk::ProgressBar> progress_bar;

    public:
      changelog_download_callbacks(const boost::shared_ptr<finish_changelog_download_info> &_download_info,
				   const std::string &_from,
				   const std::string &_to,
				   const std::string &_source_package,
				   const std::string &_source_version,
				   const boost::shared_ptr<Gtk::ProgressBar> &_progress_bar)
	: download_info(_download_info),
	  from(_from),
	  to(_to),
	  source_package(_source_package),
	  source_version(_source_version),
	  progress_bar(_progress_bar)
      {
      }

      void success(const temp::name &name)
      {
	// Replace the progress bar and text with a message about the
	// download being complete.
	progress_bar->hide();

	Glib::RefPtr<Gtk::TextBuffer> text_buffer(download_info->text_buffer);

	Gtk::TextBuffer::iterator begin_iter =
	  text_buffer->get_iter_at_mark(download_info->begin);

	Gtk::TextBuffer::iterator end_iter =
	  text_buffer->get_iter_at_mark(download_info->end);

	end_iter = text_buffer->erase(begin_iter, end_iter);
	end_iter = text_buffer->insert(end_iter, cw::util::ssprintf(_("Parsing the changelog of %s version %s..."),
								    source_package.c_str(),
								    source_version.c_str()));

	Glib::RefPtr<Gtk::TextBuffer::Mark> end_mark = text_buffer->create_mark(end_iter);

	boost::shared_ptr<finish_changelog_download_info>
	  new_download_info = boost::make_shared<finish_changelog_download_info>(download_info->begin,
										 end_mark,
										 text_buffer,
                                                                                 download_info->info,
										 download_info->current_version,
										 download_info->only_new);


	const sigc::slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_slot =
	  sigc::bind(sigc::ptr_fun(&finish_changelog_download),
		     new_download_info);

	const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_safe_slot =
	  make_safe_slot(finish_changelog_download_slot);

	parse_changelog_background(name,
				   finish_changelog_download_safe_slot,
				   from,
				   to,
				   source_package,
				   false,
				   &post_thunk);
      }

      void failure(const std::string &msg)
      {
	progress_bar->hide();

	changelog_download_error(msg, download_info);
      }

      void partial_download(const temp::name &filename,
			    unsigned long currentSize,
			    unsigned long totalSize)
      {
	if(totalSize == 0)
	  progress_bar->set_fraction(0);
	else
	  progress_bar->set_fraction(((double)currentSize) / ((double)totalSize));
      }
    };

    /** \brief Try to generate a changelog-download object for the
     *  given job and start a background download for it if necessary.
     *
     *  \param entry         The download job to process.
     *
     *  \param digested_file The pre-digested changelog, if one is
     *                       available.
     *
     *  This function must be invoked in the main thread.
     */
    void process_changelog_job(const boost::shared_ptr<preprocessed_changelog_job> &entry,
			       const temp::name &digested_file)
    {
      logging::LoggerPtr logger = aptitude::Loggers::getAptitudeGtkChangelog();

      LOG_DEBUG(logger,
                "Processing the changelog job for " << entry << ".");

      //Gtk::TextView *textView = entry->get_text_view();
      Glib::RefPtr<Gtk::TextBuffer> textBuffer = entry->get_text_buffer();
      Glib::RefPtr<Gtk::TextBuffer::Mark> beginMark = entry->get_begin();

      // First, erase any old text that was displayed.
      {
	Glib::RefPtr<Gtk::TextBuffer::Mark> endMark = entry->get_end();

	textBuffer->erase(textBuffer->get_iter_at_mark(beginMark),
			  textBuffer->get_iter_at_mark(endMark));
      }


      const std::string &binary_package_name(entry->get_binary_package_name());
      const boost::shared_ptr<aptitude::apt::changelog_info> &target_info(entry->get_target_info());
      const boost::shared_ptr<aptitude::apt::changelog_info> &current_info(entry->get_current_info());
      const std::set<std::string> &origins(entry->get_origins());

      if(origins.find("Debian") == origins.end())
	{
	  std::string origins_str;
	  for(std::set<std::string>::const_iterator it = origins.begin();
	      it != origins.end(); ++it)
	    {
	      if(it->empty())
		continue;

	      if(!origins_str.empty())
		origins_str += ", ";
	      origins_str += "\"";
	      origins_str += *it;
	      origins_str += "\"";
	    }

	  const Gtk::TextBuffer::iterator begin_iter =
	    textBuffer->get_iter_at_mark(beginMark);
	  if(origins_str.empty())
	    textBuffer->insert(begin_iter,
			       ssprintf(_("You can only view changelogs of official Debian packages; the origin of %s is unknown."),
					binary_package_name.c_str()));
	  else
	    textBuffer->insert(begin_iter,
			       ssprintf(_("You can only view changelogs of official Debian packages; %s is from %s."),
					binary_package_name.c_str(), origins_str.c_str()));
	}
      else
	{

	  // The normal flow here is that the download invokes
	  // parse_and_view_changelog_download_trampoline, which tells
	  // the parse thread to parse the changelog and invoke
	  // finish_changelog_download_slot in the main thread when
	  // it's done.

	  // However, if we have a pre-digested changelog, we can call
	  // out to the parse thread directly, bypassing the download
	  // process.
	  if(digested_file.valid())
	    {
	      LOG_TRACE(aptitude::Loggers::getAptitudeGtkChangelog(),
			"Using a predigested file to display the changelog of "
			<< target_info->get_source_package() << " "
			<< target_info->get_source_version());

	      Glib::RefPtr<Gtk::TextBuffer::Mark> endMark;

	      {
		const Gtk::TextBuffer::iterator begin_iter =
		  textBuffer->get_iter_at_mark(beginMark);
		const Gtk::TextBuffer::iterator end_iter =
		  textBuffer->insert(begin_iter,
				     cw::util::ssprintf(_("Parsing the changelog of %s version %s..."),
							target_info->get_source_package().c_str(),
							target_info->get_source_version().c_str()));

		endMark = textBuffer->create_mark(end_iter);
	      }

	      // Bind up the buffer location, the buffer pointer, and the
	      // source version with the finalization code (so we can
	      // invoke it when the changelog downloads).
	      //
	      // The changelog continuation is triggered in the background
	      // thread, so we need to safely wrap it and post it to the
	      // main thread.
	      const bool only_new = entry->get_only_new();
	      boost::shared_ptr<finish_changelog_download_info> download_info =
		boost::make_shared<finish_changelog_download_info>(beginMark, endMark, textBuffer,
                                                                   *current_info,
								   only_new ? current_info->get_source_version() : "",
								   only_new);

	      const sigc::slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_slot =
		sigc::bind(sigc::ptr_fun(&finish_changelog_download),
			   download_info);

	      const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_safe_slot =
		make_safe_slot(finish_changelog_download_slot);

	      parse_changelog_background(digested_file,
					 finish_changelog_download_safe_slot,
					 only_new ? current_info->get_source_version() : "",
					 target_info->get_source_version(),
					 target_info->get_source_package(),
					 true,
					 &post_thunk);
	    }
	  else
	    {
	      Glib::RefPtr<Gtk::TextBuffer::Mark> endMark;
	      Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> anchor =
		Gtk::TextBuffer::ChildAnchor::create();

	      {
		const Gtk::TextBuffer::iterator begin_iter =
		  textBuffer->get_iter_at_mark(beginMark);
		Gtk::TextBuffer::iterator end_iter =
		  textBuffer->insert(begin_iter,
				     cw::util::ssprintf(_("Downloading the changelog of %s version %s..."),
							   target_info->get_source_package().c_str(),
							   target_info->get_source_version().c_str()));

		end_iter = textBuffer->insert(end_iter, "\n");

		end_iter = textBuffer->insert_child_anchor(end_iter, anchor);

		end_iter = textBuffer->insert(end_iter, "\n");

		endMark = textBuffer->create_mark(end_iter);
	      }

	      // A boost::shared_ptr is used here instead of manage()
	      // because I want to ensure that the C++ object is
	      // destroyed when it runs out of references.  In
	      // particular: if there is no text view any more, we
	      // won't be able to add the progress bar to it and it
	      // would never be deleted if I was relying on manage().
	      // This way, the progress bar will be valid as long as I
	      // need it, then get deleted.  (at least, I hope that's
	      // what will happen)
	      boost::shared_ptr<Gtk::ProgressBar> progressBar(boost::make_shared<Gtk::ProgressBar>());

	      entry->add_child_at_anchor(*progressBar, anchor);
	      progressBar->show();

	      const bool only_new = entry->get_only_new();
	      boost::shared_ptr<finish_changelog_download_info> download_info =
		boost::make_shared<finish_changelog_download_info>(beginMark, endMark, textBuffer,
                                                                   *current_info,
								   only_new ? current_info->get_source_version() : "",
								   only_new);

	      boost::shared_ptr<changelog_download_callbacks> callbacks =
		boost::make_shared<changelog_download_callbacks>(download_info,
								 only_new ? current_info->get_source_version() : "",
								 target_info->get_source_version(),
								 target_info->get_source_package(),
								 target_info->get_source_version(),
								 progressBar);

	      aptitude::apt::get_changelog(target_info, callbacks, &post_thunk);
	    }
	}
    }



    // The following code powers the thread that checks for parsed
    // (pre-digested) changelogs in the cache, handing each one that it
    // finds off to the "display parsed changelog" code.  Each request
    // contains a list of download jobs, and the ones whose parses are
    // not found in the cache are passed off to be downloaded and/or
    // parsed.

    class check_cache_for_parsed_changelogs_job
    {
      boost::shared_ptr<preprocessed_changelog_job> request;

    public:
      check_cache_for_parsed_changelogs_job(const boost::shared_ptr<preprocessed_changelog_job> &_request)
	: request(_request)
      {
      }

      const boost::shared_ptr<preprocessed_changelog_job> &get_request() const { return request; }
    };

    // TODO: show the individual requests.
    std::ostream &operator<<(std::ostream &out,
			     const check_cache_for_parsed_changelogs_job &job)
    {
      return out << "(job: " << *job.get_request() << ")";
    }

    /** \brief A thread that preprocesses changelog jobs by checking for
     *  parsed changelogs in the cache.
     */
    class check_cache_for_parsed_changelogs_thread
      : public aptitude::util::job_queue_thread<check_cache_for_parsed_changelogs_thread,
						check_cache_for_parsed_changelogs_job>
    {
      // Set to true when the global signal handlers are connected up.
      static bool signals_connected;
    public:
      static logging::LoggerPtr get_log_category()
      {
	return aptitude::Loggers::getAptitudeGtkChangelogCache();
      }

      check_cache_for_parsed_changelogs_thread()
      {
	if(!signals_connected)
	  {
	    cache_closed.connect(sigc::ptr_fun(&check_cache_for_parsed_changelogs_thread::stop));
	    cache_reloaded.connect(sigc::ptr_fun(&check_cache_for_parsed_changelogs_thread::start));
	    signals_connected = true;
	  }
      }

      void process_job(const check_cache_for_parsed_changelogs_job &job)
      {
	logging::LoggerPtr logger(get_log_category());

	const boost::shared_ptr<preprocessed_changelog_job> &req = job.get_request();

	temp::name predigested_file;

	std::string uri;
	if(!req->get_only_new())
	  uri = ssprintf("changelog://%s/%s",
			 req->get_target_info()->get_source_package().c_str(),
			 req->get_target_info()->get_source_version().c_str());
	else
	  uri = ssprintf("delta-changelog://%s/%s/%s",
			 req->get_target_info()->get_source_package().c_str(),
			 req->get_current_info()->get_source_version().c_str(),
			 req->get_target_info()->get_source_version().c_str());


	LOG_TRACE(logger,
		  "Changelog preparation thread: checking for the changelog of "
		  << req->get_target_info()->get_source_package()
		  << " " << req->get_target_info()->get_source_version()
		  << " under the cache URI "
		  << uri);

	predigested_file = download_cache->getItem(uri);
	if(predigested_file.valid())
	  LOG_TRACE(logger, "Changelog preparation thread: found a predigested changelog for "
		    << req->get_target_info()->get_source_package()
		    << " " << req->get_target_info()->get_source_version()
		    << " in the file " << predigested_file.get_name());

	sigc::slot<void, boost::shared_ptr<preprocessed_changelog_job>, temp::name>
	  process_changelog_job_slot = sigc::ptr_fun(&process_changelog_job);

	post_event(safe_bind(make_safe_slot(process_changelog_job_slot),
			     req,
			     predigested_file));
      }
    };
    bool check_cache_for_parsed_changelogs_thread::signals_connected = false;
  }




  // The top-level entry point for changelog fetching.
  //
  // The overall changelog fetch process has three steps:
  //
  // 1. First, we check whether the changelog exists as a parsed entry
  //    in the cache.  The URI used is one of:
  //
  //        changelog://PACKAGE/NEW_VERSION
  //        delta-changelog://PACKAGE/CURRENT_VERSION/NEW_VERSION
  //
  //    In the second case, only "new" entries are present.  The
  //    source package name is always used.
  //
  // 2. Second, we check whether the remaining changelogs exist on
  //    disk or in the file cache.
  //
  // 3. Finally, any changelogs not resolved by steps 1 and 2 are
  //    retrieved over the network.
  //
  // Step 1 is implemented in this file; 2 and 3 are implemented in
  // src/generic/pkg_changelog.{cc,h} using the central download
  // queue.  (TODO: the code in this file should probably be
  // generalized for use with, e.g., the curses frontend)
  //
  // Changelogs retrieved in steps 2 and 3 are parsed, then cached for
  // later use in step 1.  To conserve time, aptitude looks in the
  // file cache for the parsed changelog once more before proceeding
  // to parse it (the parse process is quite slow, hence all this
  // caching).




  Gtk::TextBuffer::iterator fetch_and_show_changelog(const pkgCache::VerIterator &ver,
						     const Glib::RefPtr<Gtk::TextBuffer> &text_buffer,
						     Gtk::TextView *text_view,
						     const Gtk::TextBuffer::iterator &where,
						     bool only_new)
  {
    Glib::RefPtr<Gtk::TextBuffer::Mark> begin_mark =
      text_buffer->create_mark(where);

    Gtk::TextBuffer::iterator end = text_buffer->insert(where,
							cw::util::ssprintf(_("Preparing to download the changelog of %s version %s."),
									   ver.ParentPkg().Name(),
									   ver.VerStr()));
    Glib::RefPtr<Gtk::TextBuffer::Mark> end_mark =
      text_buffer->create_mark(end);

    sigc::slot<void, Gtk::Widget &, Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> > text_view_add_child_at_anchor =
      sigc::mem_fun(*text_view, &Gtk::TextView::add_child_at_anchor);

    boost::shared_ptr<preprocessed_changelog_job> preprocessed =
      boost::make_shared<preprocessed_changelog_job>(begin_mark, end_mark, text_buffer,
						     text_view_add_child_at_anchor,
						     ver, only_new);

    check_cache_for_parsed_changelogs_job job(preprocessed);
    check_cache_for_parsed_changelogs_thread::add_job(job);


    return end;
  }
}
