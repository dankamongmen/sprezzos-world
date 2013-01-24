// download_list.cc
//
//   Copyright (C) 2001-2005, 2008 Daniel Burrows
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

#include "aptitude.h"
#include "download_list.h"
#include "ui.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>

#include <generic/util/util.h>

#include <apt-pkg/acquire-worker.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/strutl.h>

#include <cwidget/fragment.h>
#include <cwidget/toplevel.h>
#include <cwidget/dialogs.h>
#include <cwidget/config/colors.h>
#include <cwidget/config/keybindings.h>
#include <cwidget/generic/util/transcode.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/functors/ptr_fun.h>

#include <algorithm>

using namespace std;

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

/** \brief The number of seconds after which the spinner will spin, if
 *  we are just displaying a spinner instead of a real progress
 *  indicator.
 */
const int download_list_spinner_interval = 1;

download_list::workerinf::workerinf(const wstring &_msg, unsigned long long _current, unsigned long long _total)
  :msg(_msg), current(_current), total(_total)
{
}

download_list::workerinf::workerinf(const string &_msg, unsigned long long _current, unsigned long long _total)
  :msg(cw::util::transcode(_msg)), current(_current), total(_total)
{
}

// Unfortunately the cancel_slot is necessary so we know we've cancelled..
static cw::widget_ref download_summary(const download_list_ref &l,
				       bool failed,
				       bool already_cancelled,
				       const sigc::slot0<void> &cancel_slot,
				       unsigned long long FetchedBytes,
				       unsigned long long ElapsedTime,
				       unsigned long long CurrentCPS)
{
  vector<cw::fragment *> fragments;

  fragments.push_back(cw::fragf(_("Downloaded %sB in %s (%sB/s)."),
				SizeToStr(FetchedBytes).c_str(), TimeToStr(ElapsedTime).c_str(),
				SizeToStr(CurrentCPS).c_str()));

  if(failed)
    // TODO: list the stuff that failed?
    fragments.push_back(cw::fragf(_("%n%nSome files were not downloaded successfully.")));

  cw::fragment *f = wrapbox(cw::sequence_fragment(fragments));

  cw::widget_ref rval;
  if(already_cancelled || !cancel_slot)
    rval=cw::dialogs::ok(f, NULL);
  else
    rval=cw::dialogs::yesno(f,
			    NULL,
			    W_("Continue"),
			    cw::util::arg(cancel_slot),
			    W_("Cancel"),
			    cw::style_attrs_flip(A_REVERSE));

  rval->connect_key("PrevPage",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::pageup));
  rval->connect_key("Up",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::lineup));
  rval->connect_key("NextPage",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::pagedown));
  rval->connect_key("Down",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::linedown));
  rval->connect_key("Begin",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::skip_to_top));
  rval->connect_key("End",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::skip_to_bottom));
  rval->connect_key("Left",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::shift_left));
  rval->connect_key("Right",
		    &cw::config::global_bindings,
		    sigc::mem_fun(*l.unsafe_get_ref(), &download_list::shift_right));

  rval->show_all();

  return rval;
}
				 

download_list::download_list(bool _display_messages, bool _display_cumulative_progress)
  :start(0), sticky_end(true), was_cancelled(false), failed(false),
   display_messages(_display_messages),

   display_cumulative_progress(_display_cumulative_progress), startx(0),

   TotalItems(0), CurrentItems(0),
   CurrentCPS(0), TotalBytes(0), CurrentBytes(0),
   start_time(time(NULL))
{
  do_layout.connect(sigc::mem_fun(*this, &download_list::layout_me));
}

void download_list::cancel()
{
  was_cancelled=true;

  cancelled();
}

void download_list::destroy()
{
  cancel();

  cw::widget::destroy();
}

void download_list::paint(const cw::style &st)
{
  cw::widget_ref tmpref(this);

  int y=0;
  unsigned int where=start;
  int width,height;
  const cw::style progress_style = st + cw::get_style("DownloadProgress");
  getmaxyx(height, width);

  // Display the completed items
  while(y<height-1 && where<msgs.size())
    {
      const cw::style msg_style = st + msgs[where].second;
      const wstring disp(msgs[where].first,
			 min<wstring::size_type>(startx, msgs[where].first.size()));

      show_string_as_progbar(0, y, disp,
			     msg_style, msg_style,
			     width, width);

      y++;
      where++;
    }

  // Display the currently active workers
  where=0;
  while(y<height-1 && where<workers.size())
    {
      int width1;

      if(workers[where].total!=0)
	width1=(int) ((((double) width)*(double) workers[where].current)/(double) workers[where].total);
      else
	width1=0;

      if(width1>width)
	width1=width;

      wstring disp(workers[where].msg,
		   min<wstring::size_type>(startx, workers[where].msg.size()));

      show_string_as_progbar(0, y, disp,
			     progress_style, st,
			     width1, width);

      y++;
      where++;
    }

  // status line.
  wstring output=L"";

  output += W_("Total Progress: ");

  int barsize=0;

  if((TotalBytes+TotalItems)>0)
    {
      string progress_string;

      if(display_cumulative_progress)
	{
	  unsigned long long ETA = 0;

	  if(CurrentCPS > 0)
	    ETA = (TotalBytes - CurrentBytes) / CurrentCPS;

	  double fraction_complete =
            ((double) (CurrentBytes + CurrentItems)) / ((double) (TotalBytes + TotalItems));

	  // Clamp to [0,1]
	  if(fraction_complete > 1)
	    fraction_complete = 1;
	  if(fraction_complete < 0)
	    fraction_complete = 0;

	  barsize = int(width * fraction_complete);

	  apply_style(cw::get_style("Status"));

	  if(CurrentCPS > 0)
	    progress_string = ssprintf(_(" [ %i%% ] (%sB/s, %s remaining)"), int(100.0 * fraction_complete), SizeToStr(CurrentCPS).c_str(), TimeToStr(ETA).c_str());
	  else if(CurrentBytes>0 || CurrentItems>0)
	    progress_string = ssprintf(_(" [ %i%% ] (stalled)"), int(100.0 * fraction_complete));
	  else
	    progress_string = ssprintf(_(" [ %i%% ]"), int(100.0 * fraction_complete));
	}
      else
	{
	  const int num_spinner_stages = 4;
	  const int stages_passed = (time(NULL) - start_time) / download_list_spinner_interval;
	  const int current_stage = stages_passed % num_spinner_stages;

	  std::string spinner = "#";
	  switch(current_stage)
	    {
	    case 0: spinner = "|";  break;
	    case 1: spinner = "/";  break;
	    case 2: spinner = "-";  break;
	    case 3: spinner = "\\"; break;
	    }

	  barsize = 0;
	  output = W_("Downloading... ");
	  progress_string = spinner;
	}


      output += cw::util::transcode(progress_string);
    }

  show_string_as_progbar(0,
			 height-1,
			 output,
			 progress_style,
			 cw::get_style("Status"),
			 barsize,
			 width);
}

void download_list::update_workers(pkgAcquire *Owner)
{
  cw::widget_ref tmpref(this);

  int width,height;
  getmaxyx(height, width);

  workers.erase(workers.begin(), workers.end());

  // FIXME: do "something" if there are no active workers
  pkgAcquire::Worker *serf=Owner->WorkersBegin();
  while(serf)
    {
      if(!serf->CurrentItem)
	{
	  if(!serf->Status.empty())
	    {
	      workers.push_back(workerinf(serf->Status, 0, 1));
	      if(get_visible())
		cw::toplevel::queuelayout();
	    }
	}
      else
	{
	  pkgAcquire::ItemDesc *item=serf->CurrentItem;
	  string status =
	    (item->Owner->Status == pkgAcquire::Item::StatFetching)
	      ? item->ShortDesc
	    : item->Description + ": ";
	  wstring output = cw::util::transcode(status);

	  char intbuf[50]; // Waay more than enough.

	  sprintf(intbuf,
		  " [ %sB/%sB ]",
		  SizeToStr(serf->CurrentSize).c_str(),
		  SizeToStr(serf->TotalSize).c_str());

	  output += cw::util::transcode(intbuf);

	  workers.push_back(workerinf(output,
				      serf->CurrentSize,
				      serf->TotalSize));
	  if(get_visible())
	    cw::toplevel::queuelayout();
	}

      serf=Owner->WorkerStep(serf);
    }

  sync_top();
}

void download_list::MediaChange(string media, string drive,
				download_signal_log &manager,
				const sigc::slot1<void, bool> &k)
{
  cw::widget_ref tmpref(this);

  cw::fragment *f = wrapbox(cw::fragf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
				      media.c_str(), drive.c_str()));

  cw::widget_ref w=cw::dialogs::yesno(f,
				      cw::util::arg(sigc::bind(k, true)),
				      W_("Continue"),
				      cw::util::arg(sigc::bind(k, false)),
				      W_("Abort"),
				      cw::get_style("MediaChange"));
  w->show_all();

  popup_widget(w);
}

void download_list::IMSHit(pkgAcquire::ItemDesc &itmdesc,
			   download_signal_log &manager)
{
  cw::widget_ref tmpref(this);

  if(display_messages)
    {
      msgs.push_back(msg(cw::util::transcode(itmdesc.Description + " " + _("[Hit]")),
			 cw::get_style("DownloadHit")));

      sync_top();

      if(get_visible())
	cw::toplevel::queuelayout();
    }
}

void download_list::Fetch(pkgAcquire::ItemDesc &itmdesc,
			  download_signal_log &manager)
{
}

void download_list::Done(pkgAcquire::ItemDesc &itmdesc,
			 download_signal_log &manager)
{
  cw::widget_ref tmpref(this);

  if(display_messages)
    {
      msgs.push_back(msg(cw::util::transcode(itmdesc.Description + " " + _("[Downloaded]")),
			 cw::get_style("DownloadProgress")));

      sync_top();

      if(get_visible())
	cw::toplevel::queuelayout();
    }
}

void download_list::Fail(pkgAcquire::ItemDesc &itmdesc,
			 download_signal_log &manager)
{
  cw::widget_ref tmpref(this);

  if(display_messages)
    {
      if(itmdesc.Owner->Status==pkgAcquire::Item::StatIdle)
	return;

      // ???
      if(itmdesc.Owner->Status==pkgAcquire::Item::StatDone)
	msgs.push_back(msg(cw::util::transcode(itmdesc.Description + " " + _("[IGNORED]")),
			   cw::get_style("DownloadHit")));
      else
	{
	  failed=true;

	  msgs.push_back(msg(cw::util::transcode(itmdesc.Description + " " + _("[ERROR]")),
			     cw::get_style("Error")));
	  msgs.push_back(msg(cw::util::transcode(" " + itmdesc.Owner->ErrorText),
			     cw::get_style("Error")));
	}

      sync_top();

      if(get_visible())
	cw::toplevel::queuelayout();
    }
}

void download_list::Start(download_signal_log &manager)
{
  cw::widget_ref tmpref(this);

  // Delete stuff from previous runs (eg, for multiple CDs)
  workers.erase(workers.begin(), workers.end());
  msgs.erase(msgs.begin(), msgs.end());
}

void download_list::Stop(download_signal_log &manager, const sigc::slot0<void> &k)
{
  cw::widget_ref tmpref(this);

  string s=aptcfg->Find(PACKAGE "::UI::Pause-After-Download", "OnlyIfError");

  bool show_summary=false;

  if(s == "OnlyIfError")
    show_summary=failed;
  else if(StringToBool(s, 0))
    show_summary=true;

  if(show_summary)
    {
      cw::widget_ref summary = download_summary(this,
						failed,
						was_cancelled,
						cancelled.make_slot(),
						manager.get_fetched_bytes(),
						manager.get_elapsed_time(),
						manager.get_currentCPS());

      summary->destroyed.connect(k);

      popup_widget(summary);
    }
  else
    k();
}

void download_list::Complete(download_signal_log &manager)
{
  // Destroy ourselves.
  cw::widget::destroy();
}

void download_list::Pulse(pkgAcquire *Owner, download_signal_log &manager,
			  const sigc::slot1<void, bool> &k)
{
  cw::widget_ref tmpref(this);

  TotalBytes=manager.get_total_bytes();
  TotalItems=manager.get_total_items();
  CurrentBytes=manager.get_current_bytes();
  CurrentItems=manager.get_current_items();
  CurrentCPS=manager.get_currentCPS();

  update_workers(Owner);

  if(get_visible())
    cw::toplevel::queuelayout(); // Force an update

  if(was_cancelled)
    k(false);
  else
    k(true);
}

int download_list::width_request()
{
  return 40;
}

int download_list::height_request(int w)
{
  return msgs.size()+workers.size()+1;
}

bool download_list::handle_key(const cw::config::key &k)
{
  cw::widget_ref tmpref(this);

  if(cw::config::global_bindings.key_matches(k, "NextPage"))
    pagedown();
  if(cw::config::global_bindings.key_matches(k, "Down"))
    linedown();
  else if(cw::config::global_bindings.key_matches(k, "PrevPage"))
    pageup();
  else if(cw::config::global_bindings.key_matches(k, "Up"))
    lineup();
  else if(cw::config::global_bindings.key_matches(k, "Begin"))
    skip_to_top();
  else if(cw::config::global_bindings.key_matches(k, "End"))
    skip_to_bottom();
  else if(cw::config::global_bindings.key_matches(k, "Left"))
    shift_left();
  else if(cw::config::global_bindings.key_matches(k, "Right"))
    shift_right();
  else
    return cw::widget::handle_key(k);

  return true;
}

void download_list::pageup()
{
  if(start>(unsigned) getmaxy())
    {
      start-=getmaxy()-1;

      cw::toplevel::update();
    }
  else
    {
      start=0;

      cw::toplevel::update();
    }

  sticky_end=false;
}

void download_list::lineup()
{
  if(start>0)
    {
      --start;

      cw::toplevel::update();
    }

  sticky_end=false;
}

void download_list::pagedown()
{
  if(start<msgs.size()+workers.size()-getmaxy())
    {
      start+=getmaxy();
      cw::toplevel::update();
    }
  else
    sticky_end=true;
}

void download_list::linedown()
{
  if(start<msgs.size()+workers.size()-getmaxy())
    {
      ++start;
      cw::toplevel::update();
    }
  else
    sticky_end=true;
}

void download_list::skip_to_bottom()
{
  if(start+getmaxy()<msgs.size()+workers.size())
    start=msgs.size()+workers.size()-getmaxy();

  sticky_end=true;

  cw::toplevel::update();
}

void download_list::skip_to_top()
{
  start=0;
  sticky_end=false;

  cw::toplevel::update();
}

void download_list::shift_left()
{
  if(startx>0)
    {
      startx-=8;
      cw::toplevel::update();
    }
}

void download_list::shift_right()
{
  wstring::size_type maxx=0;

  if(display_messages)
    for(vector<wstring>::size_type n=0; n<msgs.size(); ++n)
      maxx=max<int>(maxx, msgs[n].first.size());

  for(vector<wstring>::size_type n=0; n<workers.size(); ++n)
    maxx=max<int>(maxx, workers[n].msg.size());

  if(startx+8<maxx)
    {
      startx+=8;
      cw::toplevel::update();
    }
}

void download_list::layout_me()
{
  sync_top();
}

void download_list::sync_top()
{
  if(getmaxy()==0)
    return;

  if(sticky_end)
    {
      unsigned int optimal_start=(msgs.size()+workers.size()+1);

      if(optimal_start>=(unsigned) getmaxy())
	{
	  optimal_start-=getmaxy();

	  if(start<optimal_start)
	    start=optimal_start;
	}
    }
}
