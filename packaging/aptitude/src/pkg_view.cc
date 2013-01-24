// pkg_view.cc
//
//  Copyright 2000-2005, 2007-2010 Daniel Burrows
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

#include "pkg_view.h"

#include "aptitude.h"

#include "desc_render.h"
#include "edit_pkg_hier.h"
#include "menu_redirect.h"
#include "pkg_columnizer.h"
#include "reason_fragment.h"
#include "trust.h"
#include "ui.h"

#include <cmdline/cmdline_why.h>

#include <cwidget/config/keybindings.h>
#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/widgets/label.h>
#include <cwidget/widgets/multiplex.h>
#include <cwidget/widgets/scrollbar.h>
#include <cwidget/widgets/table.h>
#include <cwidget/widgets/text_layout.h>
#include <cwidget/widgets/widget.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/match.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/functors/ptr_fun.h>

#include <ctype.h>

#include <string>

using namespace std;

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

namespace matching = aptitude::matching;
using aptitude::why::why_callbacks;
using boost::shared_ptr;
using cw::util::ref_ptr;

class pkg_handling_label:public cw::label
{
  cw::config::column_definition_list *columns;

  bool have_pkg;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;

  void zap_package()
  {
    have_pkg=false;
  }

protected:
  pkg_handling_label(cw::config::column_definition_list *_columns)
    :cw::label(" "), columns(_columns), have_pkg(false)
  {
    cache_closed.connect(sigc::mem_fun(*this, &pkg_handling_label::zap_package));
  }

public:
  static cw::util::ref_ptr<pkg_handling_label>
  create(cw::config::column_definition_list *columns)
  {
    cw::util::ref_ptr<pkg_handling_label> rval(new pkg_handling_label(columns));
    rval->decref();
    return rval;
  }

  ~pkg_handling_label() {delete columns;}

  cw::size size_request() {return cw::size(1,1);}

  void set_columns(cw::config::column_definition_list *_columns)
  {
    delete columns;
    columns=_columns;
    cw::toplevel::update();
  }

  void do_columnify(const pkgCache::PkgIterator &_pkg,
		    const pkgCache::VerIterator &_ver)
  {
    pkg=_pkg;
    ver=_ver;

    have_pkg=!pkg.end();

    cw::toplevel::update();
  }

  void paint(const cw::style &st)
  {
    cw::widget_ref tmpref(this);

    if(apt_cache_file)
      {
	// Needed to initialize translated widths and stuff.
	pkg_item::pkg_columnizer::setup_columns();

	if(!have_pkg)
	  {
	    pkg=pkgCache::PkgIterator();
	    // Reinitialize it all the time to avoid the "magic autochanging
	    // pointers" bug.
	    ver=pkgCache::VerIterator(*apt_cache_file);
	  }

	cw::config::empty_column_parameters p;
	set_text(pkg_item::pkg_columnizer(pkg, ver, *columns, 0).layout_columns(getmaxx(), p));
      }
    else
      set_text("");

    cw::label::paint(st);
  }
};

typedef cw::util::ref_ptr<pkg_handling_label> pkg_handling_label_ref;

static void do_set_column_format(string key, string the_default,
				 pkg_handling_label &lBare)
{
  pkg_handling_label_ref l(&lBare);

  string format=aptcfg->Find(key, the_default.c_str());
  wstring wformat;

  cw::config::column_definition_list *columns=NULL;

  if(!cw::util::transcode(format.c_str(), wformat))
    _error->Errno("iconv", _("Couldn't cw::util::transcode column definition"));
  else
    columns=parse_columns(wformat,
			  pkg_item::pkg_columnizer::parse_column_type,
			  pkg_item::pkg_columnizer::defaults);

  if(!columns)
    _error->Error(_("Couldn't parse column definition"));
  else
    l->set_columns(columns);
}

class pkg_why_widget:public cw::text_layout
{
protected:
  pkg_why_widget()
  {
  }
public:
  static cw::util::ref_ptr<pkg_why_widget> create()
  {
    cw::util::ref_ptr<pkg_why_widget> rval(new pkg_why_widget);
    rval->decref();
    return rval;
  }

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver)
  {
    if(pkg.end())
      {
	set_no_package();
	return;
      }

    std::vector<ref_ptr<matching::pattern> > search_leaves;
    // Search for package (versions) that are not automatically
    // installed and that are or will be installed.
    search_leaves.push_back(matching::parse("?not(?automatic)?any-version(?or(?version(CANDIDATE)?action(install), ?version(CURRENT)?installed))"));
    try
      {
	bool success = false;
	set_fragment(do_why(search_leaves,
			    pkg,
			    aptitude::why::no_summary,
			    false,
			    false,
                            shared_ptr<why_callbacks>(),
			    success));
      }
    catch(...)
      {
	; // Eat and hide errors.
      }
  }

  void set_no_package()
  {
    set_fragment(wrapbox(cw::text_fragment(_("If you select a package, an explanation of why it should be installed or removed will appear in this space."))));
  }
};
typedef cw::util::ref_ptr<pkg_why_widget> pkg_why_widget_ref;

class pkg_description_widget:public cw::text_layout
{
protected:
  pkg_description_widget()
  {
  }
public:
  static cw::util::ref_ptr<pkg_description_widget> create()
  {
    cw::util::ref_ptr<pkg_description_widget> rval(new pkg_description_widget);
    rval->decref();
    return rval;
  }

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver)
  {
    // Check against pkg.end() to hack around #339533; if ver is a
    // default iterator, pkg.end() is true.
    wstring newdesc(pkg.end() ? L"" : get_long_description(ver, apt_package_records));

    cw::fragment *frag=make_desc_fragment(newdesc);

#ifdef APT_HAS_HOMEPAGE
    cw::fragment *homepage;
    if(!ver.end())
      {
	pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());
	string homepagestr = rec.Homepage();
	homepage = (homepagestr == ""
		    ? cw::fragf("")
		    : cw::dropbox(cw::fragf("%B%s%b", _("Homepage: ")),
			      cw::hardwrapbox(cw::text_fragment(rec.Homepage()))));
      }
    else
      homepage = cw::fragf("");
#else
    cw::fragment *homepage = cw::fragf("");
#endif

    cw::fragment *tags=make_tags_fragment(pkg);
    if(tags != NULL)
      tags = cw::fragf("%n%n%F", tags);
    else
      tags = cw::fragf("");

    cw::fragment *untrusted_frag;

    if(pkg.end() || ver.end())
      untrusted_frag=NULL;
    else
      untrusted_frag=make_untrusted_warning(ver);

    if(untrusted_frag == NULL)
      set_fragment(cw::fragf("%F%F%F", frag, homepage, tags));
    else
      set_fragment(cw::fragf("%F%n%F%F%F", untrusted_frag, frag,
			 homepage, tags));
  }
};

typedef cw::util::ref_ptr<pkg_description_widget> pkg_description_widget_ref;

// In order to properly dispatch line-up/down events to the sub-widgets,
// we need a meta-widget that knows about them.
//
// Note: this is not the most efficient way of doing things, but
// (a) it would be very error-prone to reproduce the multiplex's
//     behavior for this special case, and
// (b) only one of these is created per package view, so the overhead
//     of a few redundant pointers is acceptable.  In my opinion.
//
// This is still rather gross, and a better way would be nice.
class info_area_multiplex:public cw::multiplex
{
  hier_editor_ref editor;
  pkg_description_widget_ref description;
  cw::table_ref description_table;

  pkg_why_widget_ref why;
  cw::table_ref why_table;

  cw::text_layout_ref reasons;
  cw::table_ref reasons_table;

  /** True if the package members are valid.
   *
   *  This needs to be here because it's not always safe to check end()
   *  if the cache has been reloaded.
   */
  bool have_pkg;
  pkgCache::PkgIterator lastPkg;
  pkgCache::VerIterator lastVer;
  wstring lastDesc;

  /** True if the package had breakage the last time we checked. */
  bool hadBreakage;
  /** If the view was autoswitched to breakage reasons, this is set
   *  to the widget we switched away from; otherwise, it is \b NULL.
   */
  cw::widget_ref autoswitch;

  void clear_package()
  {
    have_pkg = false;
  }

protected:
  info_area_multiplex(const hier_editor_ref &_editor,
		      const pkg_description_widget_ref &_description,
		      const cw::table_ref &_description_table,
		      const pkg_why_widget_ref &_why,
		      const cw::table_ref &_why_table,
		      const cw::text_layout_ref &_reasons,
		      const cw::table_ref &_reasons_table)
    :cw::multiplex(false),
     editor(_editor),
     description(_description), description_table(_description_table),
     why(_why), why_table(_why_table),
     reasons(_reasons), reasons_table(_reasons_table),
     have_pkg(false), hadBreakage(false), autoswitch(NULL)
  {
    cache_closed.connect(sigc::mem_fun(*this,
				       &info_area_multiplex::clear_package));
    package_states_changed.connect(sigc::mem_fun(*this,
						 &info_area_multiplex::reset_package));
  }

public:
  static cw::util::ref_ptr<info_area_multiplex>
  create(const hier_editor_ref &editor,
	 const pkg_description_widget_ref &description,
	 const cw::table_ref &description_table,
	 const pkg_why_widget_ref &why,
	 const cw::table_ref &why_table,
	 const cw::text_layout_ref &reasons,
	 const cw::table_ref &reasons_table)
  {
    cw::util::ref_ptr<info_area_multiplex>
      rval(new info_area_multiplex(editor, description, description_table,
				   why, why_table, reasons, reasons_table));
    rval->decref();
    return rval;
  }

  void line_up()
  {
    cw::widget_ref tmpref(this);

    cw::widget_ref w=visible_widget();

    if(w==description_table)
      description->line_up();
    else if(w == why_table)
      why->line_up();
    else if(w==reasons_table)
      reasons->line_up();
  }

  void line_down()
  {
    cw::widget_ref tmpref(this);

    cw::widget_ref w=visible_widget();

    if(w==description_table)
      description->line_down();
    else if(w == why_table)
      why->line_down();
    else if(w==reasons_table)
      reasons->line_down();
  }

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver)
  {
    cw::widget_ref tmpref(this);

    bool hasBreakage;

    description->set_package(pkg, ver);
    reasons->set_fragment(reason_fragment(pkg, hasBreakage));
    editor->set_package(pkg, ver);
    why->set_package(pkg, ver);

    // autoswitch if a package is newly broken, or if we have just
    // moved to a broken package.
    if(hasBreakage &&
       (!hadBreakage ||
	!have_pkg ||
	!(pkg==lastPkg && ver==lastVer)) &&
       aptcfg->FindB(PACKAGE "::UI::Auto-Show-Reasons", true))
      {
	// Don't clobber the autoswitch bread crumb if we were
	// autoswitched and are still autoswitched.
	if(!autoswitch.valid())
	  autoswitch=visible_widget();
	reasons_table->show();
      }

    // We always set the package anyway in case something changed,
    // but only scroll to the top in this case:
    if(!have_pkg || pkg != lastPkg || ver != lastVer)
      {
	lastPkg=pkg;
	lastVer=ver;

	description->move_to_top();
	reasons->move_to_top();
      }
    // If we are autoswitched and unbroken, switch back.
    else if(!hasBreakage && autoswitch.valid())
      {
	autoswitch->show();
	autoswitch=NULL;
      }

    hadBreakage=hasBreakage;

    // Important: we have to set have_pkg according to whether
    // pkg.end() is true, because if it is, then we might be closing
    // the cache, and then we might not recognize that the package is
    // null when we open the cache again.
    have_pkg = !pkg.end();
  }

  /** Cycles the multiplex, taking autoswitch behavior into account. */
  void cycle()
  {
    cw::widget_ref tmpref(this);

    if(autoswitch.valid() && autoswitch!=visible_widget())
      {
	autoswitch->show();
	autoswitch=NULL;
      }
    else
      {
	autoswitch=NULL;
	cycle_forward();
      }
  }

  /** Re-updates the package views, given that the package state may
   *  have changed.
   */
  void reset_package()
  {
    if(have_pkg)
      set_package(lastPkg, lastVer);
  }

  /** Set the description directly, without reference to a package.
   *  Used when no package is selected.
   *
   *  \param s the new description
   */
  void set_description(const std::wstring &s)
  {
    cw::widget_ref tmpref(this);

    if(s!=lastDesc)
      {
	lastDesc=s;

	description->set_fragment(make_desc_fragment(s));
	reasons->set_fragment(cw::sequence_fragment(make_desc_fragment(s),
						    cw::newline_fragment(),
						    nopackage(),
						    NULL));
	why->set_no_package();

	description->move_to_top();
	reasons->move_to_top();
      }
  }
};

typedef cw::util::ref_ptr<info_area_multiplex> info_area_multiplex_ref;

namespace
{
  bool view_is_active(cw::widget_ref view)
  {
    cw::util::ref_ptr<cw::container> owner = view->get_owner();

    while(owner.valid())
      {
	if(owner->get_active_widget() != view)
	  return false;

	view = owner;
	owner = view->get_owner();
      }

    return true;
  }

  bool do_info_multiplex_cycle_information_active(cw::widget &valveBare)
  {
    cw::widget_ref valve(&valveBare);
    return view_is_active(valve);
  }

  bool do_info_multiplex_cycle_information(cw::widget &valveBare,
					   info_area_multiplex &multiplexBare)
  {
    cw::widget_ref valve(&valveBare);
    info_area_multiplex_ref multiplex(&multiplexBare);

    if(!view_is_active(valve))
      return false;
    else
      {
	multiplex->cycle();
	return true;
      }
  }

  void do_update_info_area_show_tabs(cw::widgets::multiplex &mBare)
  {
    cw::widgets::multiplex_ref m(&mBare);

    m->set_show_tabs(aptcfg->FindB(PACKAGE "::UI::InfoAreaTabs", false));
  }
}

cw::widget_ref make_package_view(list<package_view_item> &format,
				const cw::widget_ref &mainwidget,
				menu_redirect *menu_handler,
				pkg_signal *sig, desc_signal *desc_sig,
				bool show_reason_first)
{
  bool found_mainwidget=false;

  cw::table_ref rval=cw::table::create();

  eassert(mainwidget.valid());

  for(list<package_view_item>::iterator i=format.begin();
      i!=format.end();
      i++)
    {
      switch(i->type)
	{
	case PACKAGE_VIEW_MAINWIDGET:
	  if(found_mainwidget)
	    _error->Error(_("make_package_view: error in arguments -- two main widgets??"));
	  else
	    i->widget=mainwidget;
	  break;
	case PACKAGE_VIEW_STATIC:
	  if(!i->columns)
	    _error->Error(_("make_package_view: error in arguments -- bad column list for static item"));
	  else
	    {
	      pkg_handling_label_ref l=pkg_handling_label::create(i->columns);
	      i->widget=l;

	      if(sig)
		sig->connect(sigc::mem_fun(*l.unsafe_get_ref(), &pkg_handling_label::do_columnify));

	      if(!i->columns_cfg.empty())
		aptcfg->connect(i->columns_cfg,
				sigc::bind(sigc::ptr_fun(do_set_column_format),
					   i->columns_cfg,
					   i->columns_cfg_default,
					   l.weak_ref()));
	    }
	  break;
	case PACKAGE_VIEW_DESCRIPTION:
	  {
	    hier_editor_ref e=hier_editor::create();
	    pkg_description_widget_ref w=pkg_description_widget::create();
	    pkg_why_widget_ref why = pkg_why_widget::create();
	    cw::text_layout_ref l=cw::text_layout::create();

	    cw::table_ref wt=cw::table::create();
	    cw::table_ref lt=cw::table::create();
	    cw::table_ref why_table = cw::table::create();
	    info_area_multiplex_ref m=info_area_multiplex::create(e,
								  w, wt,
								  why, why_table,
								  l, lt);
	    cw::scrollbar_ref ws=cw::scrollbar::create(cw::scrollbar::VERTICAL);
	    cw::scrollbar_ref ls=cw::scrollbar::create(cw::scrollbar::VERTICAL);
	    cw::scrollbar_ref why_scrollbar = cw::scrollbar::create(cw::scrollbar::VERTICAL);

	    w->location_changed.connect(sigc::mem_fun(*ws.unsafe_get_ref(), &cw::scrollbar::set_slider));
	    l->location_changed.connect(sigc::mem_fun(*ls.unsafe_get_ref(), &cw::scrollbar::set_slider));
	    why->location_changed.connect(sigc::mem_fun(*why_scrollbar.unsafe_get_ref(), &cw::scrollbar::set_slider));

	    wt->add_widget_opts(w, 0, 0, 1, 1, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK);
	    wt->add_widget_opts(ws, 0, 1, 1, 1, cw::table::ALIGN_RIGHT, cw::table::ALIGN_CENTER | cw::table::FILL);

	    lt->add_widget_opts(l, 0, 0, 1, 1, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK);
	    lt->add_widget_opts(ls, 0, 1, 1, 1, cw::table::ALIGN_RIGHT, cw::table::EXPAND | cw::table::ALIGN_CENTER | cw::table::FILL);

	    why_table->add_widget_opts(why, 0, 0, 1, 1, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK);
	    why_table->add_widget_opts(why_scrollbar, 0, 1, 1, 1, cw::table::ALIGN_RIGHT, cw::table::EXPAND | cw::table::ALIGN_CENTER | cw::table::FILL);

	    // HACK: speaks for itself
	    cw::tree_ref thetree=mainwidget.dyn_downcast<cw::tree>();

	    i->widget=m;

	    // Set up a null reason to start with.  (the package
	    // signal is only called if an actual package is
	    // highlighted)
	    l->set_fragment(nopackage());

	    if(sig)
	      sig->connect(sigc::mem_fun(*m.unsafe_get_ref(), &info_area_multiplex::set_package));

	    if(desc_sig)
	      desc_sig->connect(sigc::mem_fun(*m.unsafe_get_ref(), &info_area_multiplex::set_description));

	    mainwidget->connect_key("DescriptionDown", &cw::config::global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::line_down));
	    mainwidget->connect_key("DescriptionUp", &cw::config::global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::line_up));
	    mainwidget->connect_key("DescriptionCycle", &cw::config::global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::cycle));
	    mainwidget->connect_key("EditHier", &cw::config::global_bindings,
				    sigc::mem_fun(*e.unsafe_get_ref(),
						  &cw::widget::show));
	    mainwidget->connect_key("EditHier", &cw::config::global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &cw::widget::show));
	    mainwidget->connect_key("EditHier", &cw::config::global_bindings,
				    sigc::bind(sigc::mem_fun(*rval.unsafe_get_ref(), &cw::table::focus_widget_bare),
					       m.weak_ref()));

	    package_cycle_information_enabled.connect(sigc::bind(sigc::ptr_fun(do_info_multiplex_cycle_information_active), rval.weak_ref()));
	    package_cycle_information.connect(sigc::bind(sigc::ptr_fun(do_info_multiplex_cycle_information), rval.weak_ref(), m.weak_ref()));

	    e->hidden_sig.connect(sigc::bind(sigc::mem_fun(*rval.unsafe_get_ref(), &cw::table::focus_widget_bare),
					     mainwidget.weak_ref()));

	    if(thetree.valid())
	      e->commit_changes.connect(sigc::mem_fun(*thetree.unsafe_get_ref(), &cw::tree::line_down));

	    m->add_widget(e, W_("Hierarchy Editor"));

	    m->add_widget(wt, W_("Description"));
	    wt->show_all();

	    m->add_widget(lt, W_("Related Dependencies"));
	    lt->show_all();

	    m->add_widget(why_table, W_("Why Installed"));
	    why_table->show_all();

	    aptcfg->connect(PACKAGE "::UI::InfoAreaTabs",
			    sigc::bind(sigc::ptr_fun(&do_update_info_area_show_tabs),
				       m.weak_ref()));
	    m->set_show_tabs(aptcfg->FindB(PACKAGE "::UI::InfoAreaTabs", false));

	    if(show_reason_first)
	      lt->show();
	    else
	      wt->show();

	    // FIXME: this is a grotesque hack.
	    if(mainwidget->get_visible())
	      rval->focus_widget(mainwidget);
	  }
	  break;
	default:
	  _error->Error(_("make_package_view: bad argument!"));
	  break;
	}

      if(i->widget.valid())
	{
	  // If we have a main widget or a description widget, the
	  // size of the widget in question will be ignored.
	  int xopts=i->xopts, yopts=i->yopts;

	  if(i->type == PACKAGE_VIEW_MAINWIDGET ||
	     i->type == PACKAGE_VIEW_DESCRIPTION)
	    {
	      xopts|=cw::table::IGNORE_SIZE_REQUEST;
	      yopts|=cw::table::IGNORE_SIZE_REQUEST;
	    }

	  rval->add_widget_opts(i->widget, i->row, i->col, i->h, i->w,
				xopts, yopts);

	  i->widget->set_bg_style(i->st);

	  if(i->popupdownkey.size()>0)
	    rval->connect_key(i->popupdownkey,
			      &cw::config::global_bindings,
			      sigc::mem_fun(*i->widget.unsafe_get_ref(),
					    &cw::widget::toggle_visible));

	  if(i->visible)
	    i->widget->show();
	}
    }

  // Slow, but the list is (hopefully) << 10 elements or so.
  for(list<package_view_item>::iterator i=format.begin();
      i!=format.end();
      i++)
    if(i->popupdownlinked.size()>0)
      for(list<package_view_item>::iterator j=format.begin();
	  j!=format.end();
	  j++)
	{
	  if(!strcasecmp(j->name.c_str(), i->popupdownlinked.c_str()))
	    {
	      // Having to make two connections is annoying.
	      j->widget->shown_sig.connect(sigc::mem_fun(*i->widget.unsafe_get_ref(), &cw::widget::show));
	      j->widget->hidden_sig.connect(sigc::mem_fun(*i->widget.unsafe_get_ref(), &cw::widget::hide));
	      break;
	    }
	}

  if(!mainwidget.valid())
    _error->Error(_("make_package_view: no main widget found"));

  if(menu_handler)
    create_menu_bindings(menu_handler, rval);

  return rval;
}
