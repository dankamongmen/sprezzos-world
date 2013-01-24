// apt_config_widgets.cc
//
//   Copyright 2000, 2001, 2005 Daniel Burrows
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

#include "apt_config_widgets.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <cwidget/config/colors.h>
#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/widgets/size_box.h>

using namespace std;
namespace cw = cwidget;

namespace cwidget
{
  using namespace widgets;
}

apt_bool_widget::apt_bool_widget(const wstring &_label,
				 const string &_item, bool _default)
  :item(_item), my_default(_default),
   cb(cw::checkbutton::create(cw::flowbox(cw::text_fragment(_label)),
			      aptcfg->FindB(_item, _default)))
{
}

apt_bool_widget::apt_bool_widget(const string &_label,
				 const string &_item, bool _default)
  :item(_item), my_default(_default),
   cb(cw::checkbutton::create(cw::flowbox(cw::text_fragment(_label)), aptcfg->FindB(_item, _default)))
{
}

void apt_bool_widget::commit()
{
  // Setting an option causes it to be saved.
  if(aptcfg->ExistsUser(item) ||
     aptcfg->FindB(item, my_default) != cb->get_checked())
    aptcfg->Set(item, cb->get_checked()?"true":"false");
}

apt_string_widget::apt_string_widget(const string &_item,
				     const string &_default)
  :item(_item), my_default(_default),
   el(cw::editline::create("", aptcfg->Find(_item, _default.c_str()))),
   w(cw::size_box::create(cw::size(5, 1), el))
{
}

void apt_string_widget::commit()
{
  string text = cw::util::transcode(el->get_text());

  if(aptcfg->ExistsUser(item) ||
     aptcfg->Find(item, my_default.c_str())!=text)
    aptcfg->Set(item, text);
}

apt_radio_widget::apt_radio_widget(const string &_item,
				   const vector<string> &_choices,
				   const string &_default)
  :item(_item), choices(_choices), my_default(_default)
{
}

void apt_radio_widget::commit()
{
  string choice;

  if(rg.selection_valid())
    {
      choice=choices[rg.get_selected()];

      if(aptcfg->ExistsUser(item) ||
	 aptcfg->Find(item, my_default.c_str())!=choice)
	aptcfg->Set(item, choice);
    }
}
