// load_pkgview.cc

#include "load_pkgview.h"

#include "aptitude.h"

#include "pkg_item.h"
#include "pkg_columnizer.h"
#include "ui.h"

#include <generic/apt/config_signal.h>
#include <generic/apt/apt.h>

#include <cwidget/generic/util/transcode.h>
#include <cwidget/widgets/table.h>

#include <apt-pkg/error.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

std::list<package_view_item> *load_pkgview(std::string cfggroup)
{
  std::list<package_view_item> *rval=new std::list<package_view_item>;

  for(const Configuration::Item *Curr=aptcfg->Tree(cfggroup.c_str())->Child;
      Curr; Curr=Curr->Next)
    {
      package_view_item tmp;
      Configuration subtree(Curr);

      tmp.widget=NULL;

      tmp.name=Curr->Tag;

      if(!strcasecmp(Curr->Value.c_str(), "mainwidget"))
	{
	  // FIXME: delete the widget here?
	  tmp.type=PACKAGE_VIEW_MAINWIDGET;
	  tmp.columns=NULL;
	}
      else if(!strcasecmp(Curr->Value.c_str(), "description"))
	{
	  tmp.type=PACKAGE_VIEW_DESCRIPTION;
	  tmp.columns=NULL;
	  if(subtree.Exists("PopUpDownKey"))
	    tmp.popupdownkey=subtree.Find("PopUpDownKey");

	  if(subtree.Exists("PopUpdownLinked"))
	    tmp.popupdownlinked=subtree.Find("PopUpdownLinked");
	}
      else if(!strcasecmp(Curr->Value.c_str(), "static"))
	{
	  // FIXME: deleting rval will leak column definitions.
	  tmp.type=PACKAGE_VIEW_STATIC;

	  // Use columnscfg as a reference to a config item if it exists;
	  // otherwise, use columns as a configuration string.
	  if(!subtree.Exists("Columns") && !subtree.Exists("ColumnsCfg"))
	    {
	      _error->Error(_("Couldn't parse layout: No column format specified for static item"));
	      delete rval;
	      return NULL;
	    }

	  // The actual column format.
	  std::string colinf;

	  if(subtree.Exists("ColumnsCfg"))
	    {
	      tmp.columns_cfg_default="";
	      tmp.columns_cfg=subtree.Find("ColumnsCfg");

	      if(tmp.columns_cfg=="HEADER")
		{
		  tmp.columns_cfg=PACKAGE "::UI::Package-Header-Format";
		  tmp.columns_cfg_default=default_pkgheaderdisplay;
		}
	      else if(tmp.columns_cfg=="STATUS")
		{
		  tmp.columns_cfg=PACKAGE "::UI::Package-Status-Format";
		  tmp.columns_cfg_default=default_pkgstatusdisplay;
		}

	      colinf=aptcfg->Find(tmp.columns_cfg,
				  tmp.columns_cfg_default.c_str());
	    }
	  else
	    colinf=subtree.Find("Columns");

	  std::wstring wcolinf;
	  if(!cw::util::transcode(colinf.c_str(), wcolinf))
	    {
	      _error->Error(_("Couldn't parse layout: encoding error in column descriptor"));
	      delete rval;
	      return NULL;
	    }
	  tmp.columns=parse_columns(wcolinf,
				    pkg_item::pkg_columnizer::parse_column_type,
				    pkg_item::pkg_columnizer::defaults);
	  if(!tmp.columns)
	    {
	      delete rval;
	      return NULL;
	    }

	  if(subtree.Exists("PopUpdownKey"))
	    tmp.popupdownkey=subtree.Find("PopUpDownKey");

	  if(subtree.Exists("PopUpdownLinked"))
	    tmp.popupdownlinked=subtree.Find("PopUpdownLinked");
	}
      else
	{
	  _error->Error(_("Couldn't parse layout: unknown view item type \"%s\""), Curr->Value.c_str());
	  delete rval;
	  return NULL;
	}

      if(!subtree.Exists("Row"))
	{
	  _error->Error(_("Couldn't parse layout: no row number specified"));
	  delete rval;
	  return NULL;
	}
      tmp.row=subtree.FindI("Row");

      if(!subtree.Exists("Column"))
	{
	  _error->Error(_("Couldn't parse layout: no row number specified"));
	  delete rval;
	  return NULL;
	}
      tmp.col=subtree.FindI("Column");

      if(!subtree.Exists("Width"))
	{
	  _error->Error(_("Couldn't parse layout: no width specified"));
	  delete rval;
	  return NULL;
	}
      tmp.w=subtree.FindI("Width");

      if(!subtree.Exists("Height"))
	{
	  _error->Error(_("Couldn't parse layout: no height specified"));
	  delete rval;
	  return NULL;
	}
      tmp.h=subtree.FindI("Height");

      tmp.xopts=0;
      tmp.yopts=0;

      if(subtree.FindB("ColExpand", false))
	tmp.xopts |= cw::table::EXPAND;
      if(subtree.FindB("RowExpand", false))
	tmp.yopts |= cw::table::EXPAND;

      if(subtree.FindB("ColShrink", false))
	tmp.xopts |= cw::table::SHRINK;
      if(subtree.FindB("RowShrink", false))
	tmp.yopts |= cw::table::SHRINK;

      if(subtree.Exists("ColAlign"))
	{
	  std::string s=subtree.Find("ColAlign");

	  if(!strcasecmp(s.c_str(), "left"))
	    tmp.xopts|=cw::table::ALIGN_LEFT;
	  else if(!strcasecmp(s.c_str(), "right"))
	    tmp.xopts|=cw::table::ALIGN_RIGHT;
	  else if(!strcasecmp(s.c_str(), "center"))
	    tmp.xopts|=cw::table::ALIGN_CENTER;
	  else
	    {
	      _error->Error(_("Unknown alignment type '%s'"), s.c_str());
	      delete rval;
	      return NULL;
	    }
	}

      if(subtree.Exists("RowAlign"))
	{
	  std::string s=subtree.Find("RowAlign");

	  if(!strcasecmp(s.c_str(), "top"))
	    tmp.yopts|=cw::table::ALIGN_LEFT;
	  else if(!strcasecmp(s.c_str(), "bottom"))
	    tmp.yopts|=cw::table::ALIGN_RIGHT;
	  else if(!strcasecmp(s.c_str(), "center"))
	    tmp.yopts|=cw::table::ALIGN_CENTER;
	  else
	    {
	      _error->Error(_("Unknown alignment type '%s'"), s.c_str());
	      delete rval;
	      return NULL;
	    }
	}

      if(subtree.Exists("Style"))
	{
	  tmp.st=cw::get_style(subtree.Find("Style"));
	}

      tmp.visible=subtree.FindB("Visible", true);

      rval->push_back(tmp);
    }

  return rval;
}
