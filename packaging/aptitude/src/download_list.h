// download_list.h  (this is -*-c++-*-)
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
//

#ifndef DOWNLOAD_LIST_H
#define DOWNLOAD_LIST_H

#include <vector>
#include <string>

#include <apt-pkg/acquire.h>

#include <cwidget/generic/util/slotarg.h>

#include <cwidget/widgets/widget.h>

#include <time.h>

/** \brief A download manager
 *
 * 
 *  A better download manager.  (..finally..)
 * 
 *  \file download_list.h
 */

class download_signal_log;

class download_list:public cwidget::widgets::widget
{
  // Caches the last-seen info about a worker.
  struct workerinf
  {
    std::wstring msg;
    unsigned long long current, total;

    workerinf(const std::wstring &_msg, unsigned long long _current, unsigned long long _total);
    workerinf(const std::string &_msg, unsigned long long _current, unsigned long long _total);
  };

  typedef std::pair<std::wstring, cwidget::style> msg;

  // Contains strings paired with attributes.
  typedef std::vector<msg> msglist;

  // Contains strings paired with the current and total sizes associated
  // with that worker (updated in Pulse)
  typedef std::vector<workerinf> workerlist;

  msglist msgs;
  workerlist workers;

  // Where in the list we are (can be >msgs.size() -- that would mean we're
  // viewing the currently progressing downloads)
  unsigned int start;

  // Whether we should stick to the bottom of the screen (set to false
  // when the user scrolls up, set back to true when the user scrolls
  // back down)
  bool sticky_end;

  // Set to true if the user asks to cancel the download
  bool was_cancelled;

  // Set to true if an item failed.
  bool failed;

  // Will we display things other than the current workers?
  bool display_messages;

  /** Used to hack around the utter braindeadness of apt's download
   *  status reporting.  If this is \b true, we assume that there is a
   *  meaningful estimate of the total download size.  Otherwise, we
   *  calculate our own estimate by assuming that each file takes an
   *  equal percentage of the time; in addition, the time indicator is
   *  hidden (since its main effect in this case is to trigger bug
   *  reports).
   */
  bool display_cumulative_progress;

  // The starting offset to display messages with.
  std::string::size_type startx;

  // We have to mirror the download_signal_log's information, since
  // we're only synchronous with it in the log callbacks.
  unsigned long TotalItems, CurrentItems;
  unsigned long long CurrentCPS, TotalBytes, CurrentBytes;

  /** \brief When the download started, as returned by time(2).
   *
   *  Used to display a spinner if a real progress percentage can't be
   *  computed because we're in a list update.
   */
  time_t start_time;

  // Updates the set of displayed progress bars.
  void update_workers(pkgAcquire *Owner);

  // Syncs the current location in the list which is displayed
  void sync_top();

  // Cancels a download
  void cancel();

  void layout_me();
protected:
  void paint(const cwidget::style &st);
  bool handle_key(const cwidget::config::key &k);

  download_list(bool _display_messages = true,
		bool _display_cumulative_progress = true);
public:
  static cwidget::util::ref_ptr<download_list> create(bool display_messages = true,
						      bool display_cumulative_progress = true)
  {
    cwidget::util::ref_ptr<download_list> rval(new download_list(display_messages, display_cumulative_progress));
    rval->decref();
    return rval;
  }

  void MediaChange(std::string media, std::string drive,
		   download_signal_log &manager,
		   const sigc::slot1<void, bool> &k);
  void IMSHit(pkgAcquire::ItemDesc &itmdesc, download_signal_log &manager);
  void Fetch(pkgAcquire::ItemDesc &itmdesc, download_signal_log &manager);
  void Done(pkgAcquire::ItemDesc &itmdesc, download_signal_log &manager);
  void Fail(pkgAcquire::ItemDesc &itmdesc, download_signal_log &manager);
  void Pulse(pkgAcquire *Owner, download_signal_log &manager,
	     const sigc::slot1<void, bool> &k);
  void Start(download_signal_log &manager);
  void Stop(download_signal_log &manager, const sigc::slot0<void> &k);
  void Complete(download_signal_log &manager);

  int width_request();
  int height_request(int w);

  bool get_cursorvisible() {return false;}
  cwidget::widgets::point get_cursorloc() { return cwidget::widgets::point(0,0); }
  bool focus_me() {return true;}

  sigc::signal<void> cancelled;

  // FIXME: overriding this is a terrible hack.  A cancel() hook should be
  // used instead.
  void destroy();

  // Used to move around in the list (can be called manually to
  // simulate these actions)
  void pageup();
  void pagedown();
  void lineup();
  void linedown();
  void skip_to_top();
  void skip_to_bottom();
  void shift_left();
  void shift_right();
};

typedef cwidget::util::ref_ptr<download_list> download_list_ref;

#endif
