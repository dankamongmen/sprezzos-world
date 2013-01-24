// changelog_parse.cc
//
//   Copyright (C) 2005, 2008-2009, 2011 Daniel Burrows
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
//
// At the moment this code uses parsechangelog to convert changelogs
// into something easier to read.

#include "changelog_parse.h"

#include "apt.h"
#include "desc_render.h"

#include <apt-pkg/fileutl.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/strutl.h>

#include <stdlib.h>

#include <generic/util/temp.h>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

#include <generic/util/file_cache.h>
#include <generic/util/job_queue_thread.h>
#include <generic/util/util.h>

namespace cw = cwidget;

namespace aptitude
{
  namespace apt
  {
    changelog_entry::changelog_entry(const std::string &_source,
				     const std::string &_version,
				     const std::string &_distribution,
				     const std::string &_urgency,
				     const std::string &_changes,
				     const cw::util::ref_ptr<changelog_element_list> &_elements,
				     const std::string &_maintainer,
				     const std::string &_date)
      : source(_source),
	version(_version),
	distribution(_distribution),
	urgency(_urgency),
	changes(_changes),
	elements(_elements),
	maintainer(_maintainer),
	date_str(_date),
	could_parse_date(false),
	date(0)
    {
      // I use RFC1123StrToTime instead of strptime because strptime
      // is locale-dependent.
      could_parse_date = RFC1123StrToTime(date_str.c_str(), date);
    }

    // Skips over whitespace in a text element, spilling elements as
    // needed to handle newline conditions.
    void skip_text_whitespace(const std::string &s,
			      std::string::size_type &start,
			      std::string::size_type &curr,
			      std::vector<changelog_element> &elements)
    {
      while(curr < s.size() && isspace(s[curr]))
	{
	  if(s[curr] != '\n')
	    ++curr;
	  else
	    {
	      ++curr;

	      elements.push_back(changelog_element(changelog_element::text_type,
						   start, curr));

	      if(curr < s.size() && s[curr] == ' ')
		++curr;

	      if(curr < s.size() && s[curr] == '.' &&

		 curr + 1 < s.size() && s[curr + 1] == '\n')
		++curr;

	      start = curr;
	    }
	}
    }

    cw::util::ref_ptr<changelog_element_list>
    parse_changes(const std::string &s)
    {
      std::vector<changelog_element> elements;

      std::string::size_type curr = 0;

      // The first line is generated mechanically before we get here, so skip it.
      {
	std::string::size_type first_nl = s.find('\n', 0);
	if(first_nl != std::string::npos)
	  curr = first_nl + 1;
	else
	  curr = s.size();
      }

      // Manually handle the start-of-line special casing here.
      if(curr < s.size() && s[curr] == ' ')
	++curr;
      if(curr < s.size() && s[curr] == '.' &&
	 curr + 1 < s.size() && s[curr + 1] == '\n')
	++curr;

      std::string::size_type start = curr;

      while(curr < s.size())
	{
	  const char c = s[curr];
	  switch(c)
	    {
	    case '\n':
	      {
		// Skipping whitespace might skip past several blank
		// lines, but that's OK (they don't contain bullets,
		// obviously).
		skip_text_whitespace(s, start, curr, elements);

		if(curr < s.size())
		  {
		    switch(s[curr])
		      {
		      case '*':
		      case '+':
		      case '-':
			if(curr != start)
			  elements.push_back(changelog_element(changelog_element::text_type,
							       start, curr));

			elements.push_back(changelog_element(changelog_element::bullet_type,
							     curr, curr + 1));

			++curr;
			start = curr;
			break;
		      }
		  }
	      }

	    case 'c':
	    case 'C':
	      {
		// Look for "closes:".
		++curr;
		if(!(curr < s.size() && (s[curr] == 'l' || s[curr] == 'L')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 'o' || s[curr] == 'O')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 's' || s[curr] == 'S')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 'e' || s[curr] == 'E')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 's' || s[curr] == 'S')))
		  break;

		++curr;
		if(!(curr < s.size() && s[curr] == ':'))
		  break;

		++curr;

		// Glom onto all the bug numbers we can find.
		bool done = false;
		while(!done)
		  {
		    skip_text_whitespace(s, start, curr, elements);

		    if(curr < s.size() && (s[curr] == 'b' || s[curr] == 'B'))
		      {
			++curr;
			if(!(curr < s.size() && (s[curr] == 'u' || s[curr] == 'U')))
			  break;

			++curr;
			if(!(curr < s.size() && (s[curr] == 'g' || s[curr] == 'G')))
			  break;

			++curr;
			skip_text_whitespace(s, start, curr, elements);
		      }

		    if(curr < s.size() && s[curr] == '#')
		      {
			++curr;

			skip_text_whitespace(s, start, curr, elements);
		      }

		    if(curr < s.size() && isdigit(s[curr]))
		      {
			// We have a digit for sure.  Spit out the
			// preceding text...
			elements.push_back(changelog_element(changelog_element::text_type,
							     start, curr));
			// Update the start pointer...
			start = curr;

			// Find the whole number...
			while(curr < s.size() && isdigit(s[curr]))
			  ++curr;

			// Put it onto the element list...
			elements.push_back(changelog_element(changelog_element::closes_type,
							     start, curr));

			// Move the start pointer past the number....
			start = curr;

			// Now see if there's a continuation (i.e., a
			// comma).  If there is we look for another
			// bug number; otherwise we stop.
			skip_text_whitespace(s, start, curr, elements);

			if(curr < s.size() && s[curr] == ',')
			  ++curr;
			else
			  done = true;
		      }
		    else // If there wasn't a digit, this isn't part of a Closes.
		      done = true;
		  }
	      }
	      break;

	    default:
	      ++curr;
	      break;
	    }
	}

      if(curr != start)
	elements.push_back(changelog_element(changelog_element::text_type,
					     start, curr));

      return changelog_element_list::create(elements);
    }

    changelog::changelog(FileFd &digest)
    {
      if(digest.IsOpen())
	{
	  pkgTagFile tagfile(&digest);

	  pkgTagSection sec;

	  std::vector<cw::fragment *> fragments;

	  while(tagfile.Step(sec))
	    {
	      std::string source(sec.FindS("Source"));
	      std::string version(sec.FindS("Version"));
	      std::string distribution(sec.FindS("Distribution"));
	      std::string urgency(sec.FindS("Urgency"));
	      std::string changes(sec.FindS("Changes"));
	      std::string maintainer(sec.FindS("Maintainer"));
	      std::string date(sec.FindS("Date"));

	      cw::util::ref_ptr<changelog_element_list> changelog_elements =
		parse_changes(changes);

	      entries.push_back(changelog_entry::create(source,
							version,
							distribution,
							urgency,
							changes,
							changelog_elements,
							maintainer,
							date));
	    }
	}
    }

    cw::util::ref_ptr<changelog> parse_digested_changelog(const temp::name &digested)
    {
      if(!digested.valid())
	return NULL;
      else
	{
	  FileFd digestedfd(digested.get_name(),
			    FileFd::ReadOnly);
	  if(!digestedfd.IsOpen())
	    return NULL;
	  else
	    return changelog::create(digestedfd);
	}
    }

    temp::name digest_changelog(const temp::name &changelog,
				const std::string &from)
    {
      temp::name rval("parsedchangelog");

      std::string version_fragment;
      if(from.empty())
	version_fragment = "--all";
      else
	{
	  version_fragment = "-f ";
	  // Note that escaping the version is *critical*, because
	  // it is untrusted data.
	  version_fragment += backslash_escape_nonalnum(from);
	}

      std::string cmd =
	cw::util::ssprintf("/usr/bin/parsechangelog --format rfc822 %s -l %s > %s 2> /dev/null",
			   version_fragment.c_str(),
			   changelog.get_name().c_str(),
			   rval.get_name().c_str());

      if(system(cmd.c_str()) == 0)
	return rval;
      else
	return temp::name();
    }

    cw::util::ref_ptr<changelog> parse_changelog(const temp::name &file,
						 const std::string &from)
    {
      temp::name digested = digest_changelog(file, from);
      return parse_digested_changelog(digested);
    }




    namespace
    {
      void invoke_safe_slot(safe_slot0<void> slot)
      {
	slot.get_slot()();
      }

      /** \brief Describes one job to be processed by the parse thread. */
      class parse_changelog_job
      {
	temp::name name;
	safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > slot;
	const std::string from;
	const std::string to;
	const std::string source_package;
	post_thunk_f post_thunk;
	bool digested;

      public:
	parse_changelog_job(const temp::name &_name,
			    const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &_slot,
			    const std::string &_from,
			    const std::string &_to,
			    const std::string &_source_package,
			    bool _digested,
			    post_thunk_f _post_thunk)
	  : name(_name), slot(_slot), from(_from),
	    to(_to), source_package(_source_package),
	    post_thunk(_post_thunk),
	    digested(_digested)
	{
	}

	/** \brief Return the temporary file name of the changelog. */
	const temp::name &get_name() const { return name; }
	/** \brief Return the slot that should be invoked when the changelog is parsed. */
	const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &get_slot() const { return slot; }
	/** \brief Return the earliest version that should be included in the parse. */
	const std::string &get_from() const { return from; }
	/** \brief Return the version whose changelog is being parsed. */
	const std::string &get_to() const { return to; }
	/** \brief Return the source package whose changelog is being parsed. */
	const std::string &get_source_package() const { return source_package; }
	/** \brief Return \b true if the input file has already been
	 *  passed through digest_changelog().
	 *
	 *  This is true, for instance, when retrieving a digested
	 *  changelog from the cache.
	 */
	bool get_digested() const { return digested; }
	/** \brief Get the function used to post thunks to the main
	 *  thread.
	 */
	post_thunk_f get_post_thunk() const { return post_thunk; }
      };

      std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<parse_changelog_job> &job);

      std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<parse_changelog_job> &job)
      {
	return out
	  << "(name=" << job->get_name().get_name()
	  << ", from=" << job->get_from()
	  << ", to=" << job->get_to()
	  << ", source_package=" << job->get_source_package()
	  << ", digested=" << (job->get_digested() ? "true" : "false")
	  << ")";
      }

      /** \brief Parses a queue of changelogs in the background.
       *
       *  The purpose of the queue is to ensure that aptitude only
       *  parses one changelog at a time and doesn't waste a ton of time
       *  starting new changelog parse threads and spawning copies of
       *  parsechangelog.
       *
       *  This is a self-terminating singleton thread.
       */
      class parse_changelog_thread : public aptitude::util::job_queue_thread<parse_changelog_thread,
									     boost::shared_ptr<parse_changelog_job> >
      {
	// Set to true when the global signal handlers are connected up.
	static bool signals_connected;

      public:
	// Tell the job_queue_thread what our log category is.
	static logging::LoggerPtr get_log_category()
	{
	  return aptitude::Loggers::getAptitudeChangelogParse();
	}

	parse_changelog_thread()
	{
	  if(!signals_connected)
	    {
	      cache_closed.connect(sigc::ptr_fun(&parse_changelog_thread::stop));
	      cache_reloaded.connect(sigc::ptr_fun(&parse_changelog_thread::start));
	      signals_connected = true;
	    }
	}

	void process_job(const boost::shared_ptr<parse_changelog_job> &job)
	{
	  std::string changelog_uri;
	  if(job->get_from().empty())
	    changelog_uri = ssprintf("changelog://%s/%s",
				     job->get_source_package().c_str(),
				     job->get_to().c_str());
	  else
	    changelog_uri = ssprintf("delta-changelog://%s/%s/%s",
				     job->get_source_package().c_str(),
				     job->get_from().c_str(),
				     job->get_to().c_str());

	  temp::name digested;
	  if(job->get_digested())
	    digested = job->get_name();
	  else
	    digested = aptitude::apt::digest_changelog(job->get_name(), job->get_from());

	  // Note that we don't re-cache the digested
	  // changelog if it was retrieved from the cache
	  // earlier (i.e., if job->get_digested() is true).
	  if(digested.valid() && !job->get_digested())
	    {
	      LOG_TRACE(get_log_category(),
			"Caching digested changelog as " << changelog_uri);
	      download_cache->putItem(changelog_uri, digested.get_name());
	    }

	  cw::util::ref_ptr<aptitude::apt::changelog> parsed =
	    aptitude::apt::parse_digested_changelog(digested);
	  job->get_post_thunk()(sigc::bind(sigc::ptr_fun(&invoke_safe_slot),
					   safe_bind(job->get_slot(), parsed)));
	}
      };
      bool parse_changelog_thread::signals_connected = false;
    }

    void parse_changelog_background(const temp::name &name,
				    safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > slot,
				    const std::string &from,
				    const std::string &to,
				    const std::string &source_package,
				    bool digested,
				    post_thunk_f post_thunk)
    {
      boost::shared_ptr<parse_changelog_job> job =
	boost::make_shared<parse_changelog_job>(name,
						slot,
						from,
						to,
						source_package,
						digested,
						post_thunk);

      parse_changelog_thread::add_job(job);
    }
  }
}
