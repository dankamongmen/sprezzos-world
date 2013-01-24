// pkg_columnizer.cc
//
//  The pkg_columnizer class.
//
//  Copyright 1999-2005, 2007-2008, 2010 Daniel Burrows
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

#include "aptitude.h"

#include "pkg_columnizer.h"
#include "solution_fragment.h" // For archives_text.

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/tasks.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/transcode.h>

#include <unistd.h>

namespace cw = cwidget;

cw::config::column_definition_list *pkg_item::pkg_columnizer::columns=NULL;
const char *pkg_item::pkg_columnizer::default_pkgdisplay="%c%a%M%S %p %Z %v %V";

// NOTE: the default widths here will be overridden by the initialization
//      code.
cw::config::column_type_defaults pkg_item::pkg_columnizer::defaults[pkg_columnizer::numtypes] = {
  {30, true, true},     // name
  {8, false, false},    // installed_size
  {8, false, false},    // debsize
  {1, false, false},    // stateflag
  {1, false, false},    // actionflag
  {40, true, true},     // description
  {14, false, false},   // currver
  {14, false, false},   // candver
  {11, false, false},   // longstate
  {10, false, false},   // longaction
  {35, true, true},     // maintainer
  {9, false, true},     // priority
  {3, false, true},     // shortpriority
  {10, false, true},    // section
  {2, false, false},    // revdepcount
  {1, false, false},    // autoset
  {1, false, false},    // tagged
  {10, true, true},     // archive
  {9, false, false},    // sizechange
  {strlen(PACKAGE), false, false},  // progname
  {strlen(VERSION), false, false},  // progver
  {12, false, true},    // brokencount
  {30, false, true},    // diskusage
  {17, false, true},    // downloadsize
  {4, false, false},    // pin_priority
  {15, false, true},    // hostname
  {1, false, false}     // trust_state
};

// Default widths for:
// name, installed_size, debsize, stateflag, actionflag, description, currver,
// candver, longstate, longaction, maintainer, priority, section, revdepcount,
// brokencount, diskusage, downloadsize.
//
// You can't set default widths for the program name and version here (those
// strings aren't affected by translation, for one thing)
const char *default_widths = N_("30 8 8 1 1 40 14 14 11 10 35 9 10 2 1 1 10 9 12 30 17 15");

const char *pkg_item::pkg_columnizer::column_names[pkg_columnizer::numtypes]=
  {N_("Package"),
   N_("InstSz"),
   N_("DebSz"),
   N_("State"),
   N_("Action"),
   N_("Description"),
   N_("InstVer"),
   N_("CandVer"),
   N_("LongState"),
   N_("LongAction"),
   N_("Maintainer"),
   N_("Priority"),
   N_("Section"),
   N_("RC"),
   N_("Auto"),
   N_("Tag"),

   // These don't make sense with headers, but whatever:
   N_("ProgName"),
   N_("ProgVer"),
   N_("#Broken"),
   N_("DiskUsage"),
   N_("DownloadSize")
  };

cw::column_disposition pkg_item::pkg_columnizer::setup_column(int type)
{
  return setup_column(pkg, visible_ver, basex, type);
}

cw::column_disposition pkg_item::pkg_columnizer::setup_column(const pkgCache::PkgIterator &pkg,
							      const pkgCache::VerIterator &visible_ver,
							      int basex,
							      int type)
{
  switch(type)
    {
    case name:
      if(!pkg.end())
	return cw::column_disposition(pkg.FullName(true), basex);
      else
	return cw::column_disposition("", 0);

      break;
    case installed_size:
      if(!visible_ver.end())
	{
	  if(visible_ver->InstalledSize>0)
	    return cw::column_disposition(SizeToStr(visible_ver->InstalledSize)+'B', 0);
	  else
	    return cw::column_disposition(_("<N/A>"), 0);
	}
      else
	return cw::column_disposition("", 0);	

      break;
    case debsize:
      if(!visible_ver.end())
	{
	  if(visible_ver->Size>0)
	    return cw::column_disposition(SizeToStr(visible_ver->Size)+'B', 0);
	  else
	    return cw::column_disposition(_("<N/A>"), 0);
	}
      else
	return cw::column_disposition("", 0);
      break;
    case sizechange:
      if(pkg.end())
	return cw::column_disposition("", 0);
      else
	{
	  pkgCache::VerIterator inst_ver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

	  // TODO: move this to a generic file (common with cmdline.cc)
	  int dsize=(inst_ver.end()?0:inst_ver->InstalledSize)
	    -(pkg.CurrentVer().end()?0:pkg.CurrentVer()->InstalledSize);

	  if(dsize==0)
	    return cw::column_disposition("", 0);
	  else if(dsize>0)
	    return cw::column_disposition("+"+SizeToStr(dsize)+"B", 0);
	  else
	    return cw::column_disposition("-"+SizeToStr(dsize)+"B", 0);
	}
      break;
    case currver:
      if(pkg.end())
	return cw::column_disposition("", 0);
      else if(!pkg.CurrentVer().end())
	return cw::column_disposition(pkg.CurrentVer().VerStr(), 0);
      else
	return cw::column_disposition(_("<none>"), 0);

      break;
    case candver:
      if(!pkg.end())
	{
	  pkgCache::VerIterator cand_ver=(*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);

	  if(!cand_ver.end() && !pkg_obsolete(pkg))
	    return cw::column_disposition(cand_ver.VerStr(), 0);
	  else
	    return cw::column_disposition(_("<none>"), 0);
	}
      else
	return cw::column_disposition("", 0);

      break;
    case stateflag:
      if(pkg.end())
	return cw::column_disposition("", 0);
      else if(pkg.VersionList().end())
	return cw::column_disposition("v", 0);
      else if((*apt_cache_file)[pkg].NowBroken())
	return cw::column_disposition("B", 0);
      else
	switch(pkg->CurrentState)
	  {
	  case pkgCache::State::NotInstalled:
	    return cw::column_disposition("p", 0);
	    // Assume it's purged if we're in this state.  Is this correct?
	    // If it's removed but config-files are present it should be
	    // in the ConfigFiles state.
	  case pkgCache::State::UnPacked:
	    return cw::column_disposition("u", 0);
	  case pkgCache::State::HalfConfigured:
	    return cw::column_disposition("C", 0);
	  case pkgCache::State::HalfInstalled:
	    return cw::column_disposition("H", 0);
	  case pkgCache::State::ConfigFiles:
	    return cw::column_disposition("c", 0);
	  case pkgCache::State::Installed:
	    return cw::column_disposition("i", 0);
#ifdef APT_HAS_TRIGGERS
	  case pkgCache::State::TriggersAwaited:
	    return cw::column_disposition("W", 0);
	  case pkgCache::State::TriggersPending:
	    return cw::column_disposition("T", 0);
#endif
	  default:
	    return cw::column_disposition("E", 0);
	  }

      break;
    case longstate:
      if(pkg.end())
	return cw::column_disposition("", 0);
      else if(pkg.VersionList().end())
	return cw::column_disposition(_("virtual"), 0);
      else if((*apt_cache_file)[pkg].NowBroken())
	return cw::column_disposition("B", 0);
      else
	switch(pkg->CurrentState)
	  {
	  case pkgCache::State::NotInstalled:
	    return cw::column_disposition(_("purged"), 0);
	    // Assume it's purged if we're in this state.  Is this correct?
	    // If it's removed but config-files are present it should be
	    // in the ConfigFiles state.
	  case pkgCache::State::UnPacked:
	    return cw::column_disposition(_("unpacked"), 0);
	  case pkgCache::State::HalfConfigured:
	    return cw::column_disposition(_("half-config"), 0);
	  case pkgCache::State::HalfInstalled:
	    return cw::column_disposition(_("half-install"), 0);
	  case pkgCache::State::ConfigFiles:
	    return cw::column_disposition(_("config-files"), 0);
	  case pkgCache::State::Installed:
	    return cw::column_disposition(_("installed"), 0);
#ifdef APT_HAS_TRIGGERS
	  case pkgCache::State::TriggersAwaited:
	    return cw::column_disposition(_("triggers-awaited"), 0);
	  case pkgCache::State::TriggersPending:
	    return cw::column_disposition(_("triggers-pending"), 0);
#endif
	  default:
	    return cw::column_disposition(_("ERROR"), 0);
	  }

      break;
    case actionflag:
      {
	if(pkg.end())
	  return cw::column_disposition("", 0);

	aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
	aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
	pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

	if(state.Status!=2 &&
	   (*apt_cache_file)->get_ext_state(pkg).selection_state==pkgCache::State::Hold &&
	   !state.InstBroken())
	  return cw::column_disposition("h", 0);
	else if(state.Upgradable() && !pkg.CurrentVer().end() &&
		!candver.end() && candver.VerStr() == estate.forbidver)
	  return cw::column_disposition("F", 0);
	else if(state.Delete())
	  return cw::column_disposition((state.iFlags&pkgDepCache::Purge)?"p":"d", 0);
	else if(state.InstBroken())
	  return cw::column_disposition("B", 0);
	else if(state.NewInstall())
	  return cw::column_disposition("i", 0);
	else if(state.iFlags&pkgDepCache::ReInstall)
	  return cw::column_disposition("r", 0);
	else if(state.Upgrade())
	  return cw::column_disposition("u", 0);
	else if(pkg.CurrentVer().end())
	  return cw::column_disposition(" ", 0);
	else
	  return cw::column_disposition(" ", 0);
      }

      break;
    case longaction:
      {
	if(pkg.end())
	  return cw::column_disposition("", 0);

	aptitudeDepCache::StateCache state=(*apt_cache_file)[pkg];
	aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
	pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

	if(state.Status!=2 && (state.Held() || estate.selection_state==pkgCache::State::Hold) && !state.InstBroken())
	  return cw::column_disposition(_("hold"), 0);
	else if(state.Upgradable() && !pkg.CurrentVer().end() &&
		!candver.end() && candver.VerStr() == estate.forbidver)
	  return cw::column_disposition(_("forbidden upgrade"), 0);
	else if(state.Delete())
	  return cw::column_disposition((state.iFlags&pkgDepCache::Purge)?_("purge"):_("delete"), 0);
	else if(state.InstBroken())
	  return cw::column_disposition(_("broken"), 0);
	else if(state.NewInstall())
	  return cw::column_disposition(_("install"), 0);
	else if(state.iFlags&pkgDepCache::ReInstall)
	  return cw::column_disposition(_("reinstall"), 0);
	else if(state.Upgrade())
	  return cw::column_disposition(_("upgrade"), 0);
	else if(pkg.CurrentVer().end())
	  return cw::column_disposition(_("none"), 0);
	else
	  return cw::column_disposition(_("none"), 0);
      }

      break;
    case description:
      return cw::column_disposition(get_short_description(visible_ver,
						      apt_package_records), 0);

      break;
    case maintainer:
      if(!visible_ver.end() &&
	 !visible_ver.FileList().end() &&
	 apt_package_records)
	return cw::column_disposition(apt_package_records->Lookup(visible_ver.FileList()).Maintainer(), 0);
      else
	return cw::column_disposition("", 0);

      break;
    case priority:
      if(!visible_ver.end() &&
	 const_cast<pkgCache::VerIterator &>(visible_ver).PriorityType() &&
	 const_cast<pkgCache::VerIterator &>(visible_ver).PriorityType()[0])
	return cw::column_disposition(const_cast<pkgCache::VerIterator &>(visible_ver).PriorityType(), 0);
      else
	return cw::column_disposition(_("Unknown"), 0);

      break;
    case shortpriority:
      if(!visible_ver.end())
	switch(visible_ver->Priority)
	  {
	  case pkgCache::State::Important:
	    return cw::column_disposition(_("Imp"), 0);
	  case pkgCache::State::Required:
	    return cw::column_disposition(_("Req"), 0);
	  case pkgCache::State::Standard:
	    return cw::column_disposition(_("Std"), 0);
	  case pkgCache::State::Optional:
	    return cw::column_disposition(_("Opt"), 0);
	  case pkgCache::State::Extra:
	    return cw::column_disposition(_("Xtr"), 0);
	  default:
	    return cw::column_disposition(_("ERR"), 0);
	  }
      else
	return cw::column_disposition(_("Unknown"), 0);

      break;
    case section:
      if(!visible_ver.end() && visible_ver.Section())
	return cw::column_disposition(visible_ver.Section(), 0);
      else
	return cw::column_disposition(_("Unknown"), 0);

      break;
    case progname:
      return cw::column_disposition(PACKAGE, 0);
    case progver:
      return cw::column_disposition(VERSION, 0);
    case brokencount:
      if(apt_cache_file && (*apt_cache_file)->BrokenCount()>0)
	{
	  char buf[512];

	  snprintf(buf, 512, _("#Broken: %ld"),
		   (*apt_cache_file)->BrokenCount());
	  return cw::column_disposition(buf, 0);
	}
      else
	return cw::column_disposition("", 0);

      break;
    case diskusage:
      {
	char buf[256];
	if(apt_cache_file && (*apt_cache_file)->UsrSize()>0)
	  {
	    // Be translator-friendly -- breaking messages up like that is not
	    // so good..
	    snprintf(buf, 256, _("Will use %sB of disk space"), SizeToStr((*apt_cache_file)->UsrSize()).c_str());
	    return cw::column_disposition(buf, 0);
	  }
	else if(apt_cache_file &&
		(*apt_cache_file)->UsrSize()<0)
	  {
	    // Be translator-friendly -- breaking messages up like that is not
	    // so good..
	    snprintf(buf, 256, _("Will free %sB of disk space"), SizeToStr(-(*apt_cache_file)->UsrSize()).c_str());
	    return cw::column_disposition(buf, 0);
	  }
	else
	  return cw::column_disposition("", 0);
      }

      break;
    case downloadsize:
      {
	if(apt_cache_file && (*apt_cache_file)->DebSize()!=0)
	  {
	    char buf[256];
	    snprintf(buf, 256,
		     _("DL Size: %sB"), SizeToStr((*apt_cache_file)->DebSize()).c_str());
	    return cw::column_disposition(buf, 0);
	  }
	else
	  return cw::column_disposition("", 0);
      }

      break;
    case pin_priority:
      {
	if(apt_cache_file && !pkg.end())
	  {
	    char buf[256];

	    pkgPolicy *policy=dynamic_cast<pkgPolicy *>(&(*apt_cache_file)->GetPolicy());

	    if(!policy)
	      return cw::column_disposition("", 0);

	    // Not quite sure what this indicates
	    signed short priority=policy->GetPriority(pkg);

	    if(priority==0)
	      return cw::column_disposition("", 0);

	    snprintf(buf, 256, "%d", priority);

	    return cw::column_disposition(buf, 0);
	  }
	else
	  return cw::column_disposition("", 0);
      }
    case autoset:
      // Display the "auto" flag UNLESS the package has been removed
      // or purged already and is not presently being installed.
      if((!pkg.CurrentVer().end() ||
	  (*apt_cache_file)[pkg].Install()) &&
	 ((*apt_cache_file)[pkg].Flags & pkgCache::Flag::Auto))
	return cw::column_disposition("A", 0);
      else
	return cw::column_disposition("", 0);

      break;
    case tagged:
      if((*apt_cache_file)->get_ext_state(pkg).tagged)
	return cw::column_disposition("*", 0);
      else
	return cw::column_disposition("", 0);

      break;

    case archive:
      if(!visible_ver.end())
	{
	  string buf = archives_text(visible_ver, true, ",");
	  return cw::column_disposition(buf,0);
	}
      else
	return cw::column_disposition("", 0);

    case hostname:
      {
	char buf[256];
	buf[255]=0;
	if(gethostname(buf, 255)!=0)
	  // Panic
	  // ForTranslators: Hostname
	  return cw::column_disposition(_("HN too long"), 0);
	else
	  return cw::column_disposition(buf, 0);
      }

      break;
    case revdepcount:
      {
	int count=0;
	char buf[100];
	if(!visible_ver.end())
	  {
	    for(pkgCache::DepIterator D=pkg.RevDependsList(); !D.end(); D++)
	      if(D.IsCritical() &&
		 !is_conflict(D->Type) &&
		 D.ParentVer()==D.ParentPkg().CurrentVer() &&
		 // That test is CORRECT; we want to see if the version
		 // providing the dependency is correct.
		 // (I'm writing this note because I just looked at the test
		 //  and couldn't remember what it did despite writing it
		 //  5 minutes ago. Maybe I should have my head examined :) )
		 _system->VS->CheckDep(visible_ver.VerStr(), D->CompareOp, D.TargetVer()))
		count++;
	  }

	for(pkgCache::PrvIterator i=pkg.ProvidesList(); !i.end(); i++)
	  for(pkgCache::DepIterator D=i.ParentPkg().RevDependsList(); !D.end(); D++)
	    {
	      if(D.IsCritical() &&
		 !is_conflict(D->Type) &&
		 D.ParentVer()==D.ParentPkg().CurrentVer() &&
		 _system->VS->CheckDep(i.ProvideVersion(), D->CompareOp, D.TargetVer()))
		count++;
	    }
	snprintf(buf, 100, "%i", count);
	return cw::column_disposition(buf, 0);
      }
      break;
    case trust_state:
      {
	if(visible_ver.end())
	  return cw::column_disposition(" ", 0);
	else if(package_trusted(visible_ver))
	  return cw::column_disposition(" ", 0);
	else
	  return cw::column_disposition("U", 0);
      }
    default:
      return cw::column_disposition(_("ERROR"), 0);
    }
}

int pkg_item::pkg_columnizer::parse_column_type(char id)
{
  switch(id)
    {
    case 'p':
      return name;
    case 'I':
      return installed_size;
    case 'D':
      return debsize;
    case 'c':
      // 'c' for CURRENTSTATE
      return stateflag;
    case 'a':
      return actionflag;
    case 'd':
      return description;
    case 'v':
      return currver;
    case 'V':
      return candver;
    case 'm':
      return maintainer;
    case 'C':
      return longstate;
    case 'A':
      return longaction;
    case 'P':
      return priority;
    case 'R':
      return shortpriority;
    case 'S':
      return trust_state;
    case 's':
      return section;
    case 'r':
      return revdepcount;
    case 'M': // autoMatic
      return autoset;
    case 'T':
      return tagged;
    case 't': // like apt-get -t
      return archive;

    case 'B':
      return brokencount;
    case 'N':
      return progname;
    case 'n':
      return progver;
    case 'u':
      return diskusage;
    case 'o':
      return downloadsize;
    case 'H':
      return hostname;
    case 'Z':
      return sizechange;
    case 'i':
      return pin_priority;
    default:
      return -1;
    }
}

class pkg_item::pkg_columnizer::pkg_genheaders:public column_generator
{
protected:
  cw::column_disposition setup_column(int type)
  {
    return cw::column_disposition(_(pkg_columnizer::column_names[type]), 0);
  }
public:
  pkg_genheaders(const cw::config::column_definition_list &_columns)
    : column_generator(_columns) {}
};

void pkg_item::pkg_columnizer::init_formatting()
{
  sscanf(_(default_widths),
	 "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
	 &defaults[name].width,
	 &defaults[installed_size].width,
	 &defaults[debsize].width,
	 &defaults[stateflag].width,
	 &defaults[actionflag].width,
	 &defaults[description].width,
	 &defaults[currver].width,
	 &defaults[candver].width,
	 &defaults[longstate].width,
	 &defaults[longaction].width,
	 &defaults[maintainer].width,
	 &defaults[priority].width,
	 &defaults[section].width,
	 &defaults[revdepcount].width,
	 &defaults[autoset].width,
	 &defaults[tagged].width,
	 &defaults[archive].width,
	 &defaults[sizechange].width,
	 &defaults[brokencount].width,
	 &defaults[diskusage].width,
	 &defaults[downloadsize].width,
	 &defaults[hostname].width);
}

void pkg_item::pkg_columnizer::setup_columns(bool force_update)
{
  // If we haven't been initialized yet, set up the translated formatting
  // stuff.
  if(!columns)
    init_formatting();

  if(force_update)
    delete columns;
  if(force_update || !columns)
    {
      std::wstring cfg;

      if(!cw::util::transcode(aptcfg->Find(PACKAGE "::UI::Package-Display-Format",
				 default_pkgdisplay).c_str(),
		    cfg))
	_error->Errno("iconv", _("Unable to transcode package display format after \"%ls\""), cfg.c_str());
      else
	columns=parse_columns(cfg,
			      pkg_columnizer::parse_column_type,
			      pkg_columnizer::defaults);
      if(!columns)
	{
	  cfg.clear();
	  if(!cw::util::transcode(default_pkgdisplay, cfg))
	    _error->Errno("iconv", _("Unable to transcode package display format after \"%ls\""), cfg.c_str());
	  else
	    columns=parse_columns(cfg,
				  pkg_columnizer::parse_column_type,
				  pkg_columnizer::defaults);
	  if(!columns)
	    {
	      _error->Warning(_("Internal error: Default column string is unparsable"));
	      const cw::config::column_definition
		col(cw::config::column_definition::COLUMN_GENERATED,
		    pkg_columnizer::name,
		    pkg_columnizer::defaults[pkg_columnizer::name].width,
		    true, true, false);
	      columns->push_back(col);
	    }
	}
    }
}

std::wstring pkg_item::pkg_columnizer::format_column_names(unsigned int width)
{
  setup_columns();

  cw::config::empty_column_parameters p;
  return pkg_genheaders(*columns).layout_columns(width, p);
}
