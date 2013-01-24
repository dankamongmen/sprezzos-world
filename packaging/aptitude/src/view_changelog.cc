// view_changelog.cc
//
//   Copyright (C) 2004-2005, 2007-2010 Daniel Burrows
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

#include <cwidget/config/colors.h>
#include <cwidget/config/keybindings.h>
#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/widgets/pager.h>
#include <cwidget/widgets/scrollbar.h>
#include <cwidget/widgets/table.h>
#include <cwidget/widgets/text_layout.h>

#include "download_list.h"
#include "menu_redirect.h"
#include "menu_text_layout.h"
#include "progress.h"
#include "safe_slot_event.h"
#include "ui.h"

#include <generic/apt/apt.h>
#include <generic/apt/changelog_parse.h>
#include <generic/apt/pkg_changelog.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_queue.h>

#include <generic/util/util.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <boost/make_shared.hpp>

using namespace std;

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}


static
cw::fragment *change_text_fragment(const std::string &s)
{
  std::vector<cw::fragment *> lines;

  std::string::size_type start = 0;
  std::string::size_type next_nl;

  do
    {
      next_nl = s.find('\n', start);

      if(s[start] == ' ')
	++start;

      if(next_nl == start + 1 && s[start] == '.')
	{
	  lines.push_back(cw::newline_fragment());
	  start = next_nl + 1;
	  continue;
	}

      std::string this_line;
      if(next_nl != std::string::npos)
	this_line.assign(s, start, next_nl - start);
      else
	this_line.assign(s, start, std::string::npos);

      size_t first_nonspace = 0;
      while(first_nonspace < this_line.size() && isspace(this_line[first_nonspace]))
	++first_nonspace;

      bool has_bullet = false;
      if(first_nonspace < this_line.size())
	switch(this_line[first_nonspace])
	  {
	  case '*':
	  case '+':
	  case '-':
	    has_bullet = true;
	    break;
	  }

      if(has_bullet)
	{
	  cw::fragment *item =
	    cw::fragf("%s%F%s%n",
		      std::string(this_line, 0, first_nonspace).c_str(),
		      cw::text_fragment(std::string(this_line, first_nonspace, 1).c_str(),
					cw::get_style("Bullet")),
		      std::string(this_line, first_nonspace + 1).c_str());
	  lines.push_back(cw::hardwrapbox(item));
	}
      else
	lines.push_back(cw::hardwrapbox(cw::fragf("%s%n", this_line.c_str())));

      start = next_nl + 1;
    } while(next_nl != std::string::npos);

  return cw::sequence_fragment(lines);
}

static
cw::fragment *render_changelog(const cw::util::ref_ptr<aptitude::apt::changelog> &cl,
			       const std::string &curver)
{
  bool first = true;

  std::vector<cw::fragment *> fragments;

  for(aptitude::apt::changelog::const_iterator it = cl->begin();
      it != cl->end(); ++it)
    {
      const cw::util::ref_ptr<aptitude::apt::changelog_entry>ent(*it);

      cw::fragment *taglineFrag =
	cw::hardwrapbox(cw::fragf("%n -- %s  %s",
				  ent->get_maintainer().c_str(),
				  ent->get_date_str().c_str()));
      cw::fragment *f =
	cw::fragf(first ? "%F%F" : "%n%F%F",
		  change_text_fragment(ent->get_changes()),
		  taglineFrag);

      first = false;

      if(!curver.empty() && _system->VS->CmpVersion(ent->get_version(), curver) > 0)
	{
	  cw::style s = cw::get_style("ChangelogNewerVersion");
	  fragments.push_back(cw::style_fragment(f, s));
	}
      else
	fragments.push_back(f);
    }

  return cw::sequence_fragment(fragments);
}

class pkg_changelog_screen : public cw::file_pager, public menu_redirect
{
  bool last_search_forwards;

  void do_search()
  {
    last_search_forwards = true;

    prompt_string(W_("Search for: "),
		  get_last_search(),
		  cw::util::arg(sigc::mem_fun(*this, &cw::pager::search_for)),
		  NULL,
		  NULL,
		  NULL);
  }

  void do_search_back()
  {
    last_search_forwards = false;

    prompt_string(W_("Search backwards for: "),
		  get_last_search(),
		  cw::util::arg(sigc::mem_fun(*this, &cw::pager::search_back_for)),
		  NULL,
		  NULL,
		  NULL);
  }

  void do_repeat_search()
  {
    if(last_search_forwards)
      search_for(L"");
    else
      search_back_for(L"");
  }

  void do_repeat_search_back()
  {
    if(!last_search_forwards)
      search_for(L"");
    else
      search_back_for(L"");
  }

protected:
  pkg_changelog_screen(const temp::name &filename,
		       int x = 0, int y = 0,
		       int width = 0, int height = 0):
    cw::file_pager(filename.get_name()), last_search_forwards(true)
  {
    connect_key("Search", &cw::config::global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_search));
    connect_key("SearchBack", &cw::config::global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_search_back));
    connect_key("ReSearch", &cw::config::global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_repeat_search));
    connect_key("RepeatSearchBack", &cw::config::global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_repeat_search_back));
  }

public:
  static cw::util::ref_ptr<pkg_changelog_screen>
  create(const temp::name &filename,
	 int x = 0, int y = 0, int width = 0, int height = 0)
  {
    cw::util::ref_ptr<pkg_changelog_screen>
      rval(new pkg_changelog_screen(filename, x, y, width, height));
    rval->decref();
    return rval;
  }

  bool find_search_enabled()
  {
    return true;
  }

  bool find_search()
  {
    do_search();
    return true;
  }

  bool find_search_back_enabled()
  {
    return true;
  }

  bool find_search_back()
  {
    do_search_back();
    return true;
  }

  bool find_research_enabled()
  {
    return !get_last_search().empty();
  }

  bool find_research()
  {
    do_repeat_search();
    return true;
  }
};
typedef cw::util::ref_ptr<pkg_changelog_screen> pkg_changelog_screen_ref;


static void do_view_changelog(temp::name n,
			      string pkgname,
			      string curverstr)
{
  string menulabel =
    ssprintf(_("Changelog of %s"), pkgname.c_str());
  string tablabel = ssprintf(_("%s changes"), pkgname.c_str());
  string desclabel = _("View the list of changes made to this Debian package.");

  cw::util::ref_ptr<aptitude::apt::changelog> changelog(aptitude::apt::parse_changelog(n));
  cw::fragment *f = changelog.valid() ? render_changelog(changelog, curverstr) : NULL;

  cw::table_ref           t = cw::table::create();
  if(f != NULL)
    {
      cw::scrollbar_ref   s = cw::scrollbar::create(cw::scrollbar::VERTICAL);
      menu_text_layout_ref l = menu_text_layout::create();


      l->location_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &cw::scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(l.unsafe_get_ref(), &cw::text_layout::scroll));
      l->set_fragment(f);

      t->add_widget_opts(l, 0, 0, 1, 1,
			 cw::table::EXPAND|cw::table::SHRINK, cw::table::EXPAND);
      t->add_widget_opts(s, 0, 1, 1, 1, 0,
			 cw::table::EXPAND | cw::table::FILL);

      create_menu_bindings(l.unsafe_get_ref(), t);
    }
  else
    {
      pkg_changelog_screen_ref cs = pkg_changelog_screen::create(n);
      cw::scrollbar_ref          s = cw::scrollbar::create(cw::scrollbar::VERTICAL);

      cs->line_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &cw::scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(cs.unsafe_get_ref(), &pkg_changelog_screen::scroll_page));
      cs->scroll_top();

      t->add_widget_opts(cs, 0, 0, 1, 1,
			 cw::table::EXPAND|cw::table::SHRINK, cw::table::EXPAND);
      t->add_widget_opts(s, 0, 1, 1, 1, 0,
			 cw::table::EXPAND | cw::table::FILL);

      create_menu_bindings(cs.unsafe_get_ref(), t);
    }

  t->show_all();

  insert_main_widget(t, menulabel, desclabel, tablabel);
}

namespace
{
  void do_post_thunk(const sigc::slot<void> &thunk)
  {
    cw::toplevel::post_event(new aptitude::safe_slot_event(make_safe_slot(thunk)));
  }
}

class changelog_callbacks : public aptitude::download_callbacks
{
  progress_ref download_progress;
  std::string pkgname;
  std::string curverstr;

public:
  changelog_callbacks(const std::string &_pkgname,
		      const std::string &_curverstr)
    : download_progress(gen_progress_bar()),
      pkgname(_pkgname),
      curverstr(_curverstr)
  {
    cw::util::ref_ptr<refcounted_progress> p(download_progress->get_progress());

    p->OverallProgress(0, 0, 0,
                       ssprintf(_("Preparing to download the changelog of %s"), pkgname.c_str()));
  }

  ~changelog_callbacks()
  {
    if(download_progress.valid())
      download_progress->destroy();
  }

  void success(const temp::name &filename)
  {
    if(download_progress.valid())
      {
        download_progress->Done();
        download_progress->destroy();
        download_progress.clear();
      }
    do_view_changelog(filename, pkgname, curverstr);
  }

  void failure(const std::string &msg)
  {
    if(download_progress.valid())
      {
        download_progress->Done();
        download_progress->destroy();
        download_progress.clear();
      }
    show_message(ssprintf(_("Failed to download the changelog of %s: %s"),
                          pkgname.c_str(), msg.c_str()));
  }

  void partial_download(const temp::name &name,
			unsigned long long currentSize,
			unsigned long long totalSize)
  {
    cwidget::util::ref_ptr<refcounted_progress> p = download_progress->get_progress();

    p->OverallProgress(currentSize, totalSize, 1,
		       ssprintf(_("Downloading the changelog of %s"), pkgname.c_str()));
  }
};

void view_changelog(pkgCache::VerIterator ver)
{
  string pkgname = ver.ParentPkg().Name();


  pkgCache::VerIterator curver = ver.ParentPkg().CurrentVer();
  std::string current_source_ver;
  if(!curver.end())
    {
      pkgRecords::Parser &current_source_rec =
	apt_package_records->Lookup(curver.FileList());

      current_source_ver =
	current_source_rec.SourceVer().empty()
	? (curver.VerStr() == NULL ? "" : curver.VerStr())
	: current_source_rec.SourceVer();
    }

  // TODO: add a configurable association between origins and changelog URLs.

  boost::shared_ptr<changelog_callbacks> callbacks =
    boost::make_shared<changelog_callbacks>(ver.ParentPkg().Name(),
					    current_source_ver);
  boost::shared_ptr<aptitude::apt::changelog_info> info =
    aptitude::apt::changelog_info::create(ver);
  aptitude::apt::get_changelog(info, callbacks, do_post_thunk);
}
