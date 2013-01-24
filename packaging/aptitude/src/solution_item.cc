// solution_item.cc
//
//   Copyright (C) 2005, 2007, 2009 Daniel Burrows
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

#include "solution_item.h"


#include "aptitude.h"
#include "pkg_info_screen.h"
#include "solution_fragment.h"
#include "ui.h"


#include <apt-pkg/pkgrecords.h>

#include <generic/apt/resolver_manager.h>

#include <generic/util/util.h>

#include <cwidget/config/keybindings.h>
#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/widgets/tree.h>


using namespace std;
namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}


action_type analyze_action(const aptitude_universe::version &ver)
{
  pkgCache::PkgIterator pkg=ver.get_pkg();
  pkgCache::VerIterator curver=pkg.CurrentVer();
  pkgCache::VerIterator newver=ver.get_ver();

  if(curver.end() ||
     pkg->CurrentState == pkgCache::State::ConfigFiles)
    {
      if(newver.end())
	return action_keep;
      else
	return action_install;
    }
  else if(newver.end())
    return action_remove;
  else if(newver == curver)
    return action_keep;
  else
    {
      int cmp=_system->VS->CmpVersion(curver.VerStr(),
				      newver.VerStr());

      // The versions shouldn't be equal -- otherwise
      // something is majorly wrong.
      // eassert(cmp!=0);
      //
      // The above is not true: consider, eg, the case of a
      // locally compiled package and a standard package.

      /** \todo indicate "sidegrades" separately? */
      if(cmp <= 0)
	return action_upgrade;
      else
	return action_downgrade;
    }
}

/** \return a description of what will happen if the given version
 *  is installed.
 */
static cw::fragment *action_description(const aptitude_resolver_version &ver)
{
  pkgCache::PkgIterator pkg = ver.get_pkg();

  switch(analyze_action(ver))
    {
    case action_remove:
      return cw::fragf(_("Remove %F [%s (%s)]"),
		   cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)),
		   pkg.CurrentVer().VerStr(),
		   archives_text(pkg.CurrentVer()).c_str());
      break;

    case action_install:
      return cw::fragf(_("Install %F [%s (%s)]"),
		   cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)),
		   ver.get_ver().VerStr(),
		   archives_text(ver.get_ver()).c_str());
      break;

    case action_keep:
      if(ver.get_ver().end())
	return cw::fragf(_("Cancel the installation of %F"),
		     cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)));
      else if(ver.get_package().current_version().get_ver().end())
	return cw::fragf(_("Cancel the removal of %F"),
		     cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)));
      else
	return cw::fragf(_("Keep %F at version %s (%s)"),
		     cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)),
		     ver.get_ver().VerStr(),
		     archives_text(ver.get_ver()).c_str());

      break;

    case action_upgrade:
      return cw::fragf(_("Upgrade %F [%s (%s) -> %s (%s)]"),
		   cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)),
		   pkg.CurrentVer().VerStr(),
		   archives_text(pkg.CurrentVer()).c_str(),
		   ver.get_ver().VerStr(), archives_text(ver.get_ver()).c_str());
      break;


    case action_downgrade:
      return cw::fragf(_("Downgrade %F [%s (%s) -> %s (%s)]"),
		   cw::text_fragment(pkg.FullName(true), cw::style_attrs_on(A_BOLD)),
		   pkg.CurrentVer().VerStr(), archives_text(pkg.CurrentVer()).c_str(),
		   ver.get_ver().VerStr(), archives_text(ver.get_ver()).c_str());
      break;
    default:
      // Impossible.
      abort();
    }

  abort();
}


const wchar_t *solution_item::tag()
{
  return L"";
}

const wchar_t *solution_item::label()
{
  return L"";
}

cw::style solution_item::get_normal_style()
{
  if(is_rejected())
    return cw::get_style("SolutionActionRejected");
  else if(is_mandatory())
    return cw::get_style("SolutionActionApproved");
  else
    return cw::style();
}

bool solution_item::dispatch_key(const cw::config::key &k, cw::tree *owner)
{
  if(cw::config::global_bindings.key_matches(k, "SolutionActionReject"))
    {
      toggle_rejected();
      owner->line_down();
    }
  else if(cw::config::global_bindings.key_matches(k, "SolutionActionApprove"))
    {
      toggle_mandated();
      owner->line_down();
    }
  else
    return cw::treeitem::dispatch_key(k, owner);

  return true;
}


//////////////////////////// Menu Redirections //////////////////////////

bool solution_item::resolver_toggle_approved()
{
  toggle_mandated();
  return true;
}

bool solution_item::resolver_toggle_approved_enabled()
{
  return true;
}

bool solution_item::resolver_toggle_rejected()
{
  toggle_rejected();
  return true;
}

bool solution_item::resolver_toggle_rejected_enabled()
{
  return true;
}


/////////////////////////////////////////////////////////////////////////

bool solution_act_item::is_rejected()
{
  eassert(resman->resolver_exists());

  return resman->is_rejected(ver);
}

bool solution_act_item::is_mandatory()
{
  eassert(resman->resolver_exists());

  return resman->is_mandatory(ver);
}

void solution_act_item::reject()
{
  resman->reject_version(ver);
}

void solution_act_item::unreject()
{
  resman->unreject_version(ver);
}

void solution_act_item::mandate()
{
  resman->mandate_version(ver);
}

void solution_act_item::unmandate()
{
  resman->unmandate_version(ver);
}

void solution_act_item::do_highlighted_changed(bool highlighted)
{
  if(highlighted)
    {
      if(apt_cache_file == NULL)
	{
	  set_short_description(cw::fragf(""));
	  set_active_dep(aptitude_resolver_dep());
	  return;
	}

      pkgCache::VerIterator real_ver = ver.get_ver();

      if(real_ver.end())
	real_ver = ver.get_package().current_version().get_ver();

      if(real_ver.end())
	real_ver = ver.get_pkg().VersionList();

      if(real_ver.end() || real_ver.FileList().end() ||
	 apt_package_records == NULL)
	set_short_description(cw::fragf(""));
      else
	set_short_description(cw::text_fragment(get_short_description(real_ver, apt_package_records)));

      set_active_dep(d);
    }
  else
    {
      set_short_description(cw::fragf(""));

      set_active_dep(aptitude_resolver_dep());
    }
}

void solution_act_item::show_target_info()
{
  pkgCache::VerIterator real_ver = ver.get_ver();
  pkgCache::PkgIterator pkg = ver.get_pkg();

  if(real_ver.end())
    real_ver = ver.get_package().current_version().get_ver();

  if(real_ver.end())
    real_ver = pkg.VersionList();

  show_info_screen(pkg, real_ver);
}

bool solution_act_item::dispatch_key(const cw::config::key &k, cw::tree *owner)
{
  if(cw::config::global_bindings.key_matches(k, "InfoScreen"))
    {
      show_target_info();
      return true;
    }
  else
    return solution_item::dispatch_key(k, owner);
}

void solution_act_item::paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
{
  unsigned int basex = hierarchical ? 2*get_depth() : 0;
  unsigned int width = win->getmaxx();

  unsigned int x = 0;

  win->move(y, 0);

  if(x < width)
    {
      if(is_rejected())
	win->addch('R'); // For "reject"
      else if(is_mandatory())
	win->addch('A'); // For "accept"
      else
	win->addch(' ');
      ++x;
    }

  if(x < width)
    {
      win->addch(' ');
      ++x;
    }

  while(x < width && x < basex)
    {
      win->addch(' ');
      ++x;
    }

  if(x < width)
    {
      win->addch('-');
      ++x;
    }

  if(x < width)
    {
      win->addch('>');
      ++x;
    }

  if(x < width)
    {
      win->addch(' ');
      ++x;
    }

  cw::fragment *f = clipbox(action_description(ver));
  cw::fragment_contents c = f->layout(width-x, width-x, st);
  delete f;

  eassert(c.size() < 2);
  if(c.size() > 0)
    {
      const cw::fragment_line &l = c.front();

      cw::fragment_line::const_iterator loc = l.begin();
      while(loc != l.end() && x < width)
	{
	  win->attrset(loc->attrs);
	  win->add_wch(loc->ch);
	  x += wcwidth(loc->ch);
	  ++loc;
	}
    }

  win->apply_style(st);

  while(x < width)
    {
      win->addch(' ');
      ++x;
    }
}

bool solution_act_item::view_target_enabled()
{
  return true;
}

bool solution_act_item::view_target()
{
  show_target_info();
  return true;
}




void solution_act_item_bare::paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
{
  unsigned int basex = hierarchical ? 2*get_depth() : 0;
  unsigned int width = win->getmaxx();

  unsigned int x = 0;

  win->move(y, 0);

  if(x < width)
    {
      if(is_rejected())
	win->addch('R'); // For "reject"
      else if(is_mandatory())
	win->addch('A'); // For "accept"
      else
	win->addch(' ');
      ++x;
    }

  if(x < width)
    {
      win->addch(' ');
      ++x;
    }

  while(x < width && x < basex)
    {
      win->addch(' ');
      ++x;
    }

  win->apply_style(st+cw::style_attrs_on(A_BOLD));

  aptitude_universe::version ver = get_ver();

  const char *name = ver.get_pkg().FullName(true).c_str();
  while(x < width && *name)
    {
      win->addch(*name);
      ++name;
      ++x;
    }

  // Ensure that at least one space separates the two columns.
  if(x < width)
    {
      win->addch(' ');
      ++x;
    }

  win->apply_style(st);
  string righttext;

  pkgCache::VerIterator currver = ver.get_pkg().CurrentVer();

  if(currver.end() || ver.get_ver().end() || currver == ver.get_ver())
    {
      pkgCache::VerIterator dispv = currver;

      if(dispv.end())
	dispv = ver.get_ver();

      if(dispv.end())
	righttext = "[UNINST]";
      else
	righttext = ssprintf("[%s (%s)]",
			     dispv.VerStr(), archives_text(dispv).c_str());
    }
  else
    {
      righttext = "[";

      if(currver.end())
	righttext += "UNINST";
      else
	{
	  righttext += currver.VerStr();
	  righttext += " ";
	  righttext += archives_text(currver);
	}

      righttext += " -> ";

      if(ver.get_ver().end())
	righttext += "UNINST";
      else
	{
	  righttext += ver.get_ver().VerStr();
	  righttext += " ";
	  righttext += archives_text(ver.get_ver());
	}

      righttext += "]";
    }

  unsigned int startx;
  if(x+righttext.size() >= width)
    startx = x;
  else
    startx = width-righttext.size();
  while(x < startx)
    {
      win->addch(' ');
      ++x;
    }

  unsigned int rightloc = 0;
  while(x < width && rightloc < righttext.size())
    {
      win->addch(righttext[rightloc]);
      ++rightloc;
      ++x;
    }
}



bool solution_unresolved_item::is_rejected()
{
  return resman->is_hardened(d);
}

bool solution_unresolved_item::is_mandatory()
{
  return resman->is_approved_broken(d);
}

void solution_unresolved_item::do_highlighted_changed(bool highlighted)
{
  if(highlighted)
    {
      if(apt_cache_file == NULL)
	set_active_dep(aptitude_resolver_dep());
      else
	set_active_dep(d);
    }
  else
    {
      set_active_dep(aptitude_resolver_dep());
    }
}

void solution_unresolved_item::reject()
{
  resman->harden_dep(d);
}

void solution_unresolved_item::unreject()
{
  resman->unharden_dep(d);
}

void solution_unresolved_item::mandate()
{
  resman->approve_broken_dep(d);
}

void solution_unresolved_item::unmandate()
{
  resman->unapprove_broken_dep(d);
}

void solution_unresolved_item::paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
{
  unsigned int basex = hierarchical ? 2*get_depth() : 0;
  unsigned int width = win->getmaxx();

  unsigned int x = 0;

  win->move(y, 0);

  if(x < width)
    {
      if(is_rejected())
	win->addch('R'); // For "reject"
      else if(is_mandatory())
	win->addch('A'); // For "accept"
      else
	win->addch(' ');
      ++x;
    }

  if(x < width)
    {
      win->addch(' ');
      ++x;
    }

  while(x < width && x < basex)
    {
      win->addch(' ');
      ++x;
    }

  wstring text;

  if(!fully_explained)
    text = swsprintf(W_("%s recommends %s").c_str(),
		     d.get_dep().ParentPkg().FullName(true).c_str(),
		     dep_targets(d.get_dep()).c_str());
  else
    text = swsprintf(W_("-> Leave the dependency \"%s recommends %s\" unresolved.").c_str(),
		     d.get_dep().ParentPkg().FullName(true).c_str(),
		     dep_targets(d.get_dep()).c_str());

  wstring::const_iterator loc = text.begin();

  while(loc != text.end() && x < width)
    {
      win->add_wch(*loc);
      x += wcwidth(*loc);
      ++loc;
    }

  while(x < width)
    {
      win->addch(' ');
      ++x;
    }
}
