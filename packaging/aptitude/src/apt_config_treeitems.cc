// apt_config_treeitems.cc      -*-c++-*-
//
// Copyright (C) 2007-2008 Daniel Burrows
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

#include "apt_config_treeitems.h"

#include "ui.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <generic/util/util.h>

#include <cwidget/config/keybindings.h>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/widgets/widget.h>
#include <cwidget/widgets/layout_item.h>
#include <cwidget/widgets/treeitem.h>
#include <cwidget/widgets/subtree.h>
#include <cwidget/widgets/tree.h>

#include <map>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

namespace aptitude
{
  namespace ui
  {
    namespace config
    {
      namespace
      {
	cw::fragment *drophardwrapbox(cw::fragment *header, const std::string &contents)
	{
	  return cw::dropbox(header, cw::hardwrapbox(cw::text_fragment(contents)));
	}

	cw::fragment *drophardwrapbox(cw::fragment *header, const std::wstring &contents)
	{
	  return cw::dropbox(header, cw::hardwrapbox(cw::text_fragment(contents)));
	}
      }

      class boolean_config_treeitem : public config_treeitem
      {
	const std::string  item;
	const std::wstring description;
	const std::string  long_description;
	const bool         dflt;

	void value_changed()
	{
	  cw::toplevel::update();
	  description_changed();
	}
      public:
	boolean_config_treeitem(const std::string &_item,
				const std::wstring &_description,
				const std::string &_long_description,
				bool _dflt)
	  : item(_item),
	    description(_description),
	    long_description(_long_description),
	    dflt(_dflt)
	{
	  aptcfg->connect(item, sigc::mem_fun(*this, &boolean_config_treeitem::value_changed));
	}

	cw::fragment *get_long_description() const
	{
	  bool value = aptcfg->FindB(item, dflt);

	  std::vector<cw::fragment *> fragments;
          fragments.push_back(drophardwrapbox(cw::fragf(_("%BOption:%b  ")),
                                              item));
          fragments.push_back(drophardwrapbox(cw::fragf(_("%BDefault:%b ")),
                                              dflt  ? _("True") : _("False")));
          fragments.push_back(drophardwrapbox(cw::fragf(_("%BValue:%b   ")),
                                              value ? _("True") : _("False")));

	  fragments.push_back(cw::newline_fragment());
	  fragments.push_back(wrapbox(cw::fragf(long_description.c_str())));

	  return cw::sequence_fragment(fragments);
	}

	void toggle()
	{
	  const bool old_value = aptcfg->FindB(item, dflt);
	  const bool new_value = !old_value;
	  aptcfg->Set(item, new_value ? "true" : "false");

	  apt_dumpcfg(PACKAGE);
	}

	bool dispatch_key(const cw::config::key &k, cw::tree *owner)
	{
	  if(cw::config::global_bindings.key_matches(k, "Confirm") ||
	     cw::config::global_bindings.key_matches(k, "PushButton"))
	    toggle();
	  else
	    return config_treeitem::dispatch_key(k, owner);

	  return true;
	}

	void dispatch_mouse(short id, int x, mmask_t bstate, cw::tree *owner)
	{
	  if((bstate & BUTTON1_CLICKED) != 0 &&
	     x >= 2 * get_depth() &&
	     x <  2 * get_depth() + 3)
	    toggle();
	}

	void paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
	{
	  bool checked = aptcfg->FindB(item, dflt);
	  const char *box = checked ? "[X]" : "[ ]";

	  cw::treeitem::paint(win, y, hierarchical,
			     swsprintf(L"%s %ls", box, description.c_str()));
	}

	const wchar_t *tag()
	{
	  return description.c_str();
	}

	const wchar_t *label()
	{
	  return tag();
	}
      };

      cw::treeitem *make_boolean_item(const std::wstring &description,
				     const std::string  &long_description,
				     const std::string  &item,
				     const bool          dflt)
      {
	return new boolean_config_treeitem(item,
					   description,
					   long_description,
					   dflt);
      }

      namespace
      {
	// This is just the line containing the text of the item, not the
	// description that precedes it.
	class string_config_treeitem : public config_treeitem
	{
	  const std::string  item;
	  const std::wstring witem;
	  const std::wstring description;
	  const std::string  long_description;
	  const std::string  dflt;

	  void value_changed()
	  {
	    cw::toplevel::update();
	    description_changed();
	  }

	public:
	  string_config_treeitem(const std::string  &_item,
				 const std::wstring &_description,
				 const std::string  &_long_description,
				 const std::string  &_dflt)
	    : item(_item),
	      witem(cw::util::transcode(_item)),
	      description(_description),
	      long_description(_long_description),
	      dflt(_dflt)
	  {
	    aptcfg->connect(item, sigc::mem_fun(*this, &string_config_treeitem::value_changed));
	  }

	  cw::fragment *get_long_description() const
	  {
	    const std::string value = aptcfg->Find(item, dflt.c_str());

	    std::vector<cw::fragment *> fragments;


	    fragments.push_back(drophardwrapbox(cw::fragf(_("%BOption:%b  ")),
						item));
	    fragments.push_back(drophardwrapbox(cw::fragf(_("%BDefault:%b ")),
						dflt));
	    fragments.push_back(drophardwrapbox(cw::fragf(_("%BValue:%b   ")),
						value));


	    fragments.push_back(cw::newline_fragment());
	    fragments.push_back(wrapbox(cw::fragf(long_description.c_str())));

	    return cw::sequence_fragment(fragments);
	  }

	  void paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
	  {
	    win->move(y, 0);
	    int x = 0;
	    const int maxx = win->getmaxx();

	    std::wstring value = cw::util::transcode(aptcfg->Find(item, dflt.c_str()));
	    std::wstring::const_iterator valueIt = value.begin();
	    while(x < maxx)
	      {
		if(x < get_depth())
		  {
		    win->addch(' ');
		    ++x;
		  }
		else
		  {
		    if(valueIt == value.end())
		      {
			win->addch(' ');
			++x;
		      }
		    else
		      {
			win->add_wch(*valueIt);
			x += wcwidth(*valueIt);
			++valueIt;
		      }
		  }
	      }

	    if(valueIt != value.end())
	      {
		win->move(y, maxx - 5);
		win->addnstr(" ... ", 5);
	      }
	  }

	  const wchar_t *tag()
	  {
	    return witem.c_str();
	  }

	  const wchar_t *label()
	  {
	    return witem.c_str();
	  }

	  void set_text(const std::wstring &text)
	  {
	    aptcfg->Set(item, cw::util::transcode(text));
	    apt_dumpcfg(PACKAGE);
	  }

	  void edit()
	  {
	    prompt_string(ssprintf(_("Editing \"%ls\": "), description.c_str()),
			  aptcfg->Find(item, dflt.c_str()),
			  cw::util::arg(sigc::mem_fun(this, &string_config_treeitem::set_text)),
			  NULL,
			  NULL,
			  NULL);
	  }

	  bool dispatch_key(const cw::config::key &k, cw::tree *owner)
	  {
	    if(cw::config::global_bindings.key_matches(k, "Confirm"))
	      edit();
	    else
	      return config_treeitem::dispatch_key(k, owner);

	    return true;
	  }
	};

	class dummy_subtree : public cw::subtree<config_treeitem>
	{
	  const std::wstring description;
	public:
	  dummy_subtree(const std::wstring &_description)
	    : cw::subtree<config_treeitem>(true),
	      description(_description)
	  {
	    set_selectable(false);
	  }

	  void paint(cw::tree *win, int y,
		     bool hierarchical, const cw::style &)
	  {
	    cw::subtree<config_treeitem>::paint(win, y, hierarchical, description);
	  }

	  const wchar_t *tag()
	  {
	    return L"";
	  }

	  const wchar_t *label()
	  {
	    return L"";
	  }
	};
      }

      cw::treeitem *make_string_item(const std::wstring &description,
				    const std::string  &long_description,
				    const std::string  &item,
				    const std::string  &dflt)
      {
	dummy_subtree *rval = new dummy_subtree(description);
	rval->add_child(new string_config_treeitem(item,
						   description,
						   long_description,
						   dflt));
	return rval;
      }

      class radio_config_treeitem : public config_treeitem
      {
	const std::string  item;
	const std::wstring description;
	const std::string  long_description;
	const std::map<std::string, radio_choice> choices;
	const std::string  my_choice;
	const std::string  dflt;

	void value_changed()
	{
	  cw::toplevel::update();
	  description_changed();
	}
      public:
	radio_config_treeitem(const std::string &_item,
			      const std::wstring &_description,
			      const std::string &_long_description,
			      const std::map<std::string, radio_choice> &_choices,
			      const std::string &_my_choice,
			      const std::string &_dflt)
	  : item(_item),
	    description(_description),
	    long_description(_long_description),
	    choices(_choices),
	    my_choice(_my_choice),
	    dflt(_dflt)
	{
	  aptcfg->connect(item, sigc::mem_fun(*this, &radio_config_treeitem::value_changed));
	}

	cw::fragment *get_long_description() const
	{
	  const std::string value = aptcfg->Find(item, dflt.c_str());
	  const std::map<std::string, radio_choice>::const_iterator value_found =
	    choices.find(value);
	  const std::map<std::string, radio_choice>::const_iterator dflt_found =
	    choices.find(dflt);
	  const std::map<std::string, radio_choice>::const_iterator my_choice_found =
	    choices.find(my_choice);

	  eassert(my_choice_found != choices.end());

	  const std::wstring value_desc =
	    value_found == choices.end()
	    ? cw::util::transcode(value)
	    : W_(value_found->second.get_untranslated_description().c_str());
	  const std::wstring dflt_desc =
	    dflt_found == choices.end()
	    ? cw::util::transcode(dflt)
	    : W_(dflt_found->second.get_untranslated_description().c_str());

	  std::vector<cw::fragment *> fragments;
	  fragments.push_back(drophardwrapbox(cw::fragf(_("%BOption:%b  ")),
					      item));
	  fragments.push_back(drophardwrapbox(cw::fragf(_("%BDefault:%b ")),
					      dflt_desc));
	  fragments.push_back(drophardwrapbox(cw::fragf(_("%BValue:%b   ")),
					      value_desc));

	  fragments.push_back(cw::newline_fragment());
	  fragments.push_back(drophardwrapbox(cw::fragf(_("%BChoice:%b  ")),
					      _(my_choice_found->second.get_untranslated_description().c_str())));
	  fragments.push_back(wrapbox(cw::text_fragment(_(my_choice_found->second.get_untranslated_long_description().c_str()))));

	  fragments.push_back(cw::newline_fragment());
	  fragments.push_back(wrapbox(cw::fragf(long_description.c_str())));

	  return cw::sequence_fragment(fragments);
	}

	void choose()
	{
	  aptcfg->Set(item, my_choice);

	  apt_dumpcfg(PACKAGE);
	}

	bool dispatch_key(const cw::config::key &k, cw::tree *owner)
	{
	  if(cw::config::global_bindings.key_matches(k, "Confirm") ||
	     cw::config::global_bindings.key_matches(k, "PushButton"))
	    choose();
	  else
	    return config_treeitem::dispatch_key(k, owner);

	  return true;
	}

	void dispatch_mouse(short id, int x, mmask_t bstate, cw::tree *owner)
	{
	  if((bstate & BUTTON1_CLICKED) != 0 &&
	     x >= 2 * get_depth() &&
	     x <  2 * get_depth() + 3)
	    choose();
	}

	void paint(cw::tree *win, int y, bool hierarchical, const cw::style &st)
	{
	  const std::map<std::string, radio_choice>::const_iterator my_choice_found =
	    choices.find(my_choice);

	  eassert(my_choice_found != choices.end());

	  bool selected = aptcfg->Find(item, dflt.c_str()) == my_choice;
	  const char *box = selected ? "(*)" : "( )";

	  cw::treeitem::paint(win, y, hierarchical,
			      swsprintf(L"%s %s", box, _(my_choice_found->second.get_untranslated_description().c_str())));
	}

	const wchar_t *tag()
	{
	  return description.c_str();
	}

	const wchar_t *label()
	{
	  return tag();
	}
      };

      cw::treeitem *make_radio_item(const std::wstring &description,
				    const std::string  &long_description,
				    const std::string  &item,
				    const std::vector<radio_choice> &choices,
				    const std::string  &dflt)
      {
	dummy_subtree *rval = new dummy_subtree(description);
	std::map<std::string, radio_choice> choices_map;
	for(std::vector<radio_choice>::const_iterator it = choices.begin();
	    it != choices.end(); ++it)
	    choices_map[it->get_value()] = *it;

	for(std::vector<radio_choice>::const_iterator it = choices.begin();
	    it != choices.end(); ++it)
	    rval->add_child(new radio_config_treeitem(item,
						      description,
						      long_description,
						      choices_map,
						      it->get_value(),
						      dflt));

	return rval;
      }
    }
  }
}
