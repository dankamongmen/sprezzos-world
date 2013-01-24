// load_config.cc
//
//  Copyright 2000,2001 Daniel Burrows

#include "aptitude.h"

#include <cwidget/config/keybindings.h>
#include <cwidget/style.h>

#include <apt-pkg/configuration.h>
#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <cwidget/generic/util/transcode.h>

#include <ctype.h>

using namespace std;

namespace cw = cwidget;

static int parse_color(const string &s)
{
  const char * const cs=s.c_str();

  if(!strcasecmp(cs, "black"))
    return COLOR_BLACK;
  else if(!strcasecmp(cs, "red"))
    return COLOR_RED;
  else if(!strcasecmp(cs, "green"))
    return COLOR_GREEN;
  else if(!strcasecmp(cs, "yellow"))
    return COLOR_YELLOW;
  else if(!strcasecmp(cs, "blue"))
    return COLOR_BLUE;
  else if(!strcasecmp(cs, "magenta"))
    return COLOR_MAGENTA;
  else if(!strcasecmp(cs, "cyan"))
    return COLOR_CYAN;
  else if(!strcasecmp(cs, "white"))
    return COLOR_WHITE;
  else if(!strcasecmp(cs, "default"))
    return -1;
  else
    {
      _error->Error(_("Unrecognized color name \"%s\""), cs);
      return -1;
    }
}

static attr_t parse_attr(const char *s, size_t len)
{
  if(!strncasecmp(s, "standout", len))
    return A_STANDOUT;
  else if(!strncasecmp(s, "underline", len))
    return A_UNDERLINE;
  else if(!strncasecmp(s, "reverse", len))
    return A_REVERSE;
  else if(!strncasecmp(s, "blink", len))
    return A_BLINK;
  else if(!strncasecmp(s, "dim", len))
    return A_DIM;
  else if(!strncasecmp(s, "bold", len))
    return A_BOLD;
  else if(!strncasecmp(s, "protect", len))
    return A_PROTECT;
  else if(!strncasecmp(s, "invisible", len))
    return A_INVIS;
  else if(!strncasecmp(s, "altcharset", len))
    return A_ALTCHARSET;
  else
    {
      _error->Error(_("Unrecognized attribute name \"%s\""), s);
      return 0;
    }
}

static attr_t parse_attrs(const string &s)
{
  attr_t rval=0;

  const char * const cs=s.c_str();

  string::size_type start=0;
  while(start<s.size())
    {
      while(start < s.size() && s[start] == ',')
	++start;

      string::size_type len=0;
      while(start+len < s.size() && s[start+len] != ',')
	++len;

      rval|=parse_attr(cs+start, len);
      start+=len;
    }

  return rval;
}

void load_styles(std::string group, bool use_theme)
{
  Configuration::Item const *cfg_grp=aptcfg->get_cfg(use_theme)->Tree(group.c_str());

  if(!cfg_grp)
    return;

  for(Configuration::Item const *i=cfg_grp->Child; i; i=i->Next)
    {
      if(!i->Value.empty())
	_error->Error(_("Invalid entry in style definition group: \"%s\""), i->Tag.c_str());
      else if(i->Tag.empty())
	_error->Error(_("Invalid tagless entry in style definition group: \"%s\""), i->Value.c_str());
      else
	{
	  cw::style curr;

	  for(Configuration::Item const *j=i->Child; j; j=j->Next)
	    {
	      if(!strcasecmp(j->Tag.c_str(), "fg"))
		{
		  int c = parse_color(j->Value);

		  if(c == -1)
		    _error->Error(_("The default color may only be used as a background."));
		  else
		    curr.set_fg(c);
		}
	      else if(!strcasecmp(j->Tag.c_str(), "bg"))
		curr.set_bg(parse_color(j->Value));
	      else
		{
		  void (cw::style::*f)(attr_t)=NULL;
		  if(!strcasecmp(j->Tag.c_str(), "set"))
		    f=&cw::style::attrs_on;
		  else if(!strcasecmp(j->Tag.c_str(), "clear"))
		    f=&cw::style::attrs_off;
		  else if(!strcasecmp(j->Tag.c_str(), "flip"))
		    f=&cw::style::attrs_flip;

		  if(!f)
		    _error->Error(_("Unknown style attribute %s"),
				  j->Tag.c_str());
		  else
		    (curr.*f)(parse_attrs(j->Value));
		}
	    }

	  set_style(i->Tag, curr);
	}
    }
}

void load_bindings(std::string group, cw::config::keybindings *toload, bool use_theme)
{
  Configuration::Item const *cfg_grp=aptcfg->get_cfg(use_theme)->Tree(group.c_str());

  if(!cfg_grp)
    return;

  for(Configuration::Item const *i=cfg_grp->Child; i; i=i->Next)
    if(!i->Child)
      {
	if(i->Value.empty())
	  _error->Error(_("Invalid entry in keybinding group: \"%s\""), i->Tag.c_str());
	else if(i->Tag.empty())
	  _error->Error(_("Invalid entry in keybinding group: \"%s\""), i->Value.c_str());
	else
	  {
	    std::string::size_type split=0,newsplit;
	    cw::config::keybinding newbinding;
	    do
	      {
		newsplit=i->Value.find(',',split);
		string currval(i->Value, split, newsplit-split);

		cw::config::key k = cw::config::parse_key(cw::util::transcode(currval));

		if(k.ch!=(wint_t) ERR)
		  newbinding.push_back(k);
		else
		  _error->Error(_("Ignoring invalid keybinding \"%s\" -> \"%s\""), i->Tag.c_str(), currval.c_str());
		split=newsplit+1;
	      } while(newsplit!=string::npos);
	    cw::config::global_bindings.set(i->Tag, newbinding);
	  }
      }
}
