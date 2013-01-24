// ui.cc
//
//   Copyright 2000-2009 Daniel Burrows <dburrows@debian.org>
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
//  Various UI glue-code and routines.

#include "ui.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/adaptors/retype_return.h>
#include <sigc++/functors/ptr_fun.h>
#include <sigc++/functors/mem_fun.h>

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

#include <apt-pkg/acquire.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/error.h>
#include <apt-pkg/packagemanager.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <fstream>
#include <sstream>
#include <utility>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "aptitude.h"

#include "apt_config_widgets.h"
#include "apt_options.h"
#include "broken_indicator.h"
#include "defaults.h"
#include "load_config.h"
#include "load_grouppolicy.h"
#include "load_pkgview.h"
#include "solution_dialog.h"
#include "solution_fragment.h"
#include "solution_screen.h"

#include <cwidget/curses++.h>
#include <cwidget/dialogs.h>
#include <cwidget/fragment.h>
#include <cwidget/toplevel.h>
#include <cwidget/widgets/button.h>
#include <cwidget/widgets/center.h>
#include <cwidget/widgets/editline.h>
#include <cwidget/widgets/frame.h>
#include <cwidget/widgets/label.h>
#include <cwidget/widgets/menu.h>
#include <cwidget/widgets/menubar.h>
#include <cwidget/widgets/multiplex.h>
#include <cwidget/widgets/pager.h>
#include <cwidget/widgets/scrollbar.h>
#include <cwidget/widgets/stacked.h>
#include <cwidget/widgets/statuschoice.h>
#include <cwidget/widgets/table.h>
#include <cwidget/widgets/text_layout.h>
#include <cwidget/widgets/togglebutton.h>
#include <cwidget/widgets/transient.h>
#include <cwidget/widgets/tree.h>

#include <mine/cmine.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>
#include <generic/apt/download_update_manager.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <generic/util/temp.h>
#include <generic/util/util.h>

#include "dep_item.h"
#include "download_list.h"
#include "menu_tree.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_info_screen.h"
#include "pkg_tree.h"
#include "pkg_ver_item.h"
#include "pkg_view.h"
#include "safe_slot_event.h"
#include "ui_download_manager.h"
#include "progress.h"

using namespace std;
namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

typedef generic_solution<aptitude_universe> aptitude_solution;

static cw::menubar_ref main_menu;
static cw::menu_ref views_menu;

static cw::stacked_ref main_stacked;

// Hmm, is this table the best idea?..
static cw::table_ref main_table;

static cw::multiplex_ref main_multiplex;
static cw::multiplex_ref main_status_multiplex;

// I think it's better to only have a single preview screen at once.  (note to
// self: data-segment stuff initializes to 0 already..)
static pkg_tree_ref active_preview_tree;
static cw::widget_ref active_preview;

// True if a download or package-list update is proceeding.  This hopefully will
// avoid the nasty possibility of collisions between them.
// FIXME: uses implicit locking -- if multithreading happens, should use a mutex
//       instead.
static bool active_download;

// While a status-widget download progress thingy is active, this will be
// set to it.
cw::widget_ref active_status_download;

sigc::signal0<void> file_quit;

sigc::signal0<bool, cw::util::accumulate_or> undo_undo;
sigc::signal0<bool, cw::util::accumulate_or> undo_undo_enabled;

sigc::signal0<void> package_states_changed;

sigc::signal0<bool, cw::util::accumulate_or> package_menu_enabled;
sigc::signal0<bool, cw::util::accumulate_or> package_forbid_enabled;
sigc::signal0<bool, cw::util::accumulate_or> package_information_enabled;
sigc::signal0<bool, cw::util::accumulate_or> package_cycle_information_enabled;
sigc::signal0<bool, cw::util::accumulate_or> package_changelog_enabled;

sigc::signal0<bool, cw::util::accumulate_or> find_search_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_search_back_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_research_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_repeat_search_back_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_limit_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_cancel_limit_enabled;
sigc::signal0<bool, cw::util::accumulate_or> find_broken_enabled;

sigc::signal0<bool, cw::util::accumulate_or> package_install;
sigc::signal0<bool, cw::util::accumulate_or> package_remove;
sigc::signal0<bool, cw::util::accumulate_or> package_purge;
sigc::signal0<bool, cw::util::accumulate_or> package_hold;
sigc::signal0<bool, cw::util::accumulate_or> package_keep;
sigc::signal0<bool, cw::util::accumulate_or> package_mark_auto;
sigc::signal0<bool, cw::util::accumulate_or> package_unmark_auto;
sigc::signal0<bool, cw::util::accumulate_or> package_forbid;
sigc::signal0<bool, cw::util::accumulate_or> package_information;
sigc::signal0<bool, cw::util::accumulate_or> package_cycle_information;
sigc::signal0<bool, cw::util::accumulate_or> package_changelog;

sigc::signal0<bool, cw::util::accumulate_or> resolver_toggle_rejected;
sigc::signal0<bool, cw::util::accumulate_or> resolver_toggle_rejected_enabled;
sigc::signal0<bool, cw::util::accumulate_or> resolver_toggle_approved;
sigc::signal0<bool, cw::util::accumulate_or> resolver_toggle_approved_enabled;
sigc::signal0<bool, cw::util::accumulate_or> resolver_view_target;
sigc::signal0<bool, cw::util::accumulate_or> resolver_view_target_enabled;

sigc::signal0<bool, cw::util::accumulate_or> find_search;
sigc::signal0<bool, cw::util::accumulate_or> find_search_back;
sigc::signal0<bool, cw::util::accumulate_or> find_research;
sigc::signal0<bool, cw::util::accumulate_or> find_repeat_search_back;
sigc::signal0<bool, cw::util::accumulate_or> find_limit;
sigc::signal0<bool, cw::util::accumulate_or> find_cancel_limit;
sigc::signal0<bool, cw::util::accumulate_or> find_broken;

sigc::signal1<void, bool> install_finished;
sigc::signal1<void, bool> update_finished;

const char *default_pkgstatusdisplay="%d";
const char *default_pkgheaderdisplay="%N %n #%B %u %o";
const char *default_grpstr="task,status,section(subdirs,passthrough),section(topdir)";
// ForTranslators: This string is a confirmation message, which users
// (especially CJK users) should be able to input without input
// methods.  Please include nothing but ASCII characters.
const char *confirm_delete_essential_str=N_("Yes, I am aware this is a very bad idea");


void ui_start_download(bool hide_preview)
{
  active_download = true;

  if(apt_cache_file != NULL)
    (*apt_cache_file)->set_read_only(true);

  if(hide_preview && active_preview.valid())
    active_preview->destroy();
}

void ui_stop_download()
{
  active_download = false;

  if(apt_cache_file != NULL)
    (*apt_cache_file)->set_read_only(false);
}

static cw::fragment *apt_error_fragment()
{
  vector<cw::fragment *> frags;

  if(_error->empty())
    frags.push_back(cw::text_fragment(_("Er, there aren't any errors, this shouldn't have happened..")));
  else while(!_error->empty())
    {
      string currerr, tag;
      bool iserr=_error->PopMessage(currerr);
      if(iserr)
	tag=_("E:");
      else
	tag=_("W:");

      frags.push_back(indentbox(0, 3,
				wrapbox(cw::fragf("%B%s%b %s",
					      tag.c_str(),
					      currerr.c_str()))));
    }

  return cw::sequence_fragment(frags);
}

// Handles "search" dialogs for pagers
static void pager_search(cw::pager &p)
{
  prompt_string(W_("Search for: "),
		p.get_last_search(),
		cw::util::arg(sigc::mem_fun(p, &cw::pager::search_for)),
		NULL,
		NULL,
		NULL);
}

// similar
static void pager_repeat_search(cw::pager &p)
{
  p.search_for(L"");
}

static void pager_repeat_search_back(cw::pager &p)
{
  p.search_back_for(L"");
}

static cw::widget_ref make_error_dialog(const cw::text_layout_ref &layout)
{
  cw::table_ref t=cw::table::create();

  cw::scrollbar_ref s=cw::scrollbar::create(cw::scrollbar::VERTICAL);

  t->add_widget(layout, 0, 0, 1, 1, true, true);
  t->add_widget_opts(s, 0, 1, 1, 1,
		     cw::table::ALIGN_RIGHT,
		     cw::table::ALIGN_CENTER | cw::table::FILL);

  layout->location_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &cw::scrollbar::set_slider));
  s->scrollbar_interaction.connect(sigc::mem_fun(layout.unsafe_get_ref(), &cw::text_layout::scroll));

  return cw::dialogs::ok(t, NULL, W_("Ok"), cw::get_style("Error"));
}

// blah, I hate C++
static void do_null_ptr(cw::text_layout_ref *p)
{
  *p=NULL;
}

void check_apt_errors()
{
  if(_error->empty())
    return;

  static cw::text_layout_ref error_dialog_layout = NULL;

  if(!error_dialog_layout.valid())
    {
      error_dialog_layout = cw::text_layout::create(apt_error_fragment());
      error_dialog_layout->destroyed.connect(sigc::bind(sigc::ptr_fun(do_null_ptr), &error_dialog_layout));

      main_stacked->add_visible_widget(make_error_dialog(error_dialog_layout),
				       true);
    }
  else
    error_dialog_layout->append_fragment(apt_error_fragment());
}

static void read_only_permissions_table_destroyed(apt_config_widget &w)
{
  w.commit();
  apt_dumpcfg(PACKAGE);

  delete &w;
}

static bool do_read_only_permission()
{
  if(active_download)
    return false;
  else
    {
      (*apt_cache_file)->set_read_only(false);


      if(!aptcfg->FindB(PACKAGE "::Suppress-Read-Only-Warning", false))
	{
	  cw::table_ref t(cw::table::create());

	  cw::fragment *f = wrapbox(cw::text_fragment(_("WARNING: the package cache is opened in read-only mode!  This change and all subsequent changes will not be saved unless you stop all other running apt-based programs and select \"Become root\" from the Actions menu.")));

	  t->add_widget_opts(cw::text_layout::create(f),
			     0, 0, 1, 1, cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			     cw::table::EXPAND | cw::table::FILL);

	  apt_bool_widget *w = new apt_bool_widget(_("Never display this message again."),
						   PACKAGE "::Suppress-Read-Only-Warning", false);

	  // HACK:
	  t->add_widget_opts(cw::label::create(""), 1, 0, 1, 1,
			     cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			     0);

	  t->add_widget_opts(w->cb, 2, 0, 1, 1,
			     cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			     cw::table::EXPAND | cw::table::FILL);

	  // HACK:
	  t->add_widget_opts(cw::label::create(""), 3, 0, 1, 1,
			     cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			     0);

	  cw::button_ref ok(cw::button::create(_("Ok")));

	  t->add_widget_opts(ok, 4, 0, 1, 1,
			     cw::table::EXPAND | cw::table::SHRINK,
			     cw::table::EXPAND);

	  t->show_all();

	  t->focus_widget(ok);

	  cw::frame_ref frame = cw::frame::create(t);
	  frame->set_bg_style(cw::style_attrs_flip(A_REVERSE));

	  cw::center_ref c = cw::center::create(frame);

	  ok->pressed.connect(sigc::mem_fun(c.unsafe_get_ref(), &cw::widget::destroy));
	  c->destroyed.connect(sigc::bind(sigc::ptr_fun(&read_only_permissions_table_destroyed), sigc::ref(*w)));

	  popup_widget(c);
	}

      return true;
    }
}

static void do_read_only_fail()
{
  eassert(active_download);

  show_message(_("You may not modify the state of any package while a download is underway."));
}

static void do_connect_read_only_callbacks()
{
  if(apt_cache_file != NULL)
    {
      (*apt_cache_file)->read_only_permission.connect(sigc::ptr_fun(&do_read_only_permission));
      (*apt_cache_file)->read_only_fail.connect(sigc::ptr_fun(&do_read_only_fail));
    }
}

// Runs a sub-aptitude with the same selections that the user
// has currently made, but as root.
//
// Because tagfiles don't work on FIFOs, and su closes all open fds,
// this is a lot hairier than it should be.
//
// This writes the current status to a file in a designated temporary
// directory, then loads it in a su'd instance.  A FIFO is used to
// make the reader block until the writer is done writing.  Not
// foolproof but the user would have to go to a lot of pointless
// trouble to screw it up.
//
// Note that the deletion of the temporary files is safe, since this
// routine blocks until the sub-process exits.

static void do_su_to_root(string args)
{
  if(getuid()==0)
    {
      show_message(_("You already are root!"));
      return;
    }

  std::string root_command = aptcfg->Find(PACKAGE "::Get-Root-Command",
					  "su:/bin/su");

  if(root_command == "su")
    root_command = "su:/bin/su";
  else if(root_command == "sudo")
    root_command = "sudo:/usr/bin/sudo";

  std::string::size_type splitloc = root_command.find(':');
  if(splitloc == std::string::npos)
    {
      _error->Error(_("Invalid Get-Root-Command; it should start with su: or sudo:"));
      return;
    }

  std::string protocol(root_command, 0, splitloc);
  if(protocol != "su" && protocol != "sudo")
    {
      _error->Error(_("Invalid Get-Root-Command; it should start with su: or sudo:, not %s:"), protocol.c_str());
      return;
    }
  std::string root_program(root_command, splitloc + 1);

  temp::name statusname("pkgstates");
  temp::name fifoname("control");

  if(mkfifo(fifoname.get_name().c_str(), 0600) != 0)
    {
      _error->Errno("do_su_to_root",
		    "Couldn't create temporary FIFO");
      return;
    }

  // Shut curses down.  This is done before we fork because otherwise
  // we'll end up with curses apparently being initialized in the
  // child and with mutexes locked (despite the fact that no thread
  // holds them), and get horribly confused, especially if we try to
  // invoke exit(3).
  //
  // An alternative is to invoke _exit(2) instead, but it seems
  // cleaner to suspend curses right here.
  cw::toplevel::suspend();

  int pid=fork();

  if(pid==-1)
    {
      _error->Error(_("Unable to fork: %s"), strerror(errno));
      cw::toplevel::resume();
    }
  else if(pid==0) // I'm a child!
    {
      // Read one byte from the FIFO for synchronization
      char tmp;
      int fd = open(fifoname.get_name().c_str(), O_RDONLY);
      if(read(fd, &tmp, 1) < 0)
	{
	  std::string errmsg = ssprintf("aptitude: failed to synchronize with parent process");
	  perror(errmsg.c_str());
	  exit(1);
	}
      close(fd);

      // It's ok to use argv0 to generate the command,
      // since the user has to authenticate themselves as root (ie, if
      // they're going to do something evil that way they'd have su'd
      // directly)
      //
      // What happens if tmpdir has spaces in it?  Can I get more
      // control over how the su-to-root function is called?
      if(protocol == "su")
	{
	  std::ostringstream cmdbuf;
	  cmdbuf << argv0 << " --no-gui -S "
		 << statusname.get_name() << " "
		 << args;
	  execl(root_program.c_str(), root_program.c_str(), "-c", cmdbuf.str().c_str(), NULL);

	  exit(1);
	}
      else if(protocol == "sudo")
	{
	  std::vector<std::string> cmdlist;
	  // Split whitespace in the input command.
	  std::string command = root_program + " " + argv0 + " --no-gui -S " + statusname.get_name() + " " + args;
	  std::string::const_iterator it = command.begin();
	  while(it != command.end() && isspace(*it))
	    ++it;

	  while(it != command.end())
	    {
	      std::string tmp;
	      while(it != command.end() && !isspace(*it))
		{
		  tmp += *it;
		  ++it;
		}

	      cmdlist.push_back(tmp);

	      while(it != command.end() && isspace(*it))
		++it;
	    }

	  const char **real_cmd = new const char*[cmdlist.size() + 1];
	  for(std::string::size_type i = 0; i < cmdlist.size(); ++i)
	    real_cmd[i] = cmdlist[i].c_str();
	  real_cmd[cmdlist.size()] = NULL;

	  execv(real_cmd[0], const_cast<char* const*>(real_cmd));

	  std::string errmsg = ssprintf("aptitude: do_su_to_root: exec \"%s\" failed", root_program.c_str());
	  perror(errmsg.c_str());

	  delete[] real_cmd;

	  // Eek, something bad happened.
	  exit(1);
	}
      else
	exit(1); // Should never happen -- see the test above.
    }
  else
    {
      int status;
      OpProgress foo; // Need a generic non-outputting progress bar

      // Save the selection list.  Check first if it's NULL to handle the
      // case of a closed cache.
      if(apt_cache_file != NULL)
	(*apt_cache_file)->save_selection_list(foo, statusname.get_name().c_str());

      // Ok, wake the other process up.
      char tmp=0;
      int fd=open(fifoname.get_name().c_str(), O_WRONLY);
      if(write(fd, &tmp, 1) < 0)
	{
	  // If we can't synchronize with it, we'd better kill it.
	  std::string errmsg = ssprintf("aptitude: failed to synchronize with child process");
	  perror(errmsg.c_str());
	  kill(pid, SIGTERM);
	}
      close(fd);

      // Wait for a while so we don't accidentally daemonize ourselves.
      while(waitpid(pid, &status, 0)!=pid)
	;

      if(!WIFEXITED(status) || WEXITSTATUS(status))
	{
	  _error->Error("%s",
			_("Subprocess exited with an error -- did you type your password correctly?"));
	  cw::toplevel::resume();
	  check_apt_errors();
	  // We have to clear these out or the cache won't reload properly (?)

	  progress_ref p = gen_progress_bar();
	  apt_reload_cache(p->get_progress().unsafe_get_ref(), true, statusname.get_name().c_str());
	  p->destroy();
	}
      else
	{
	  // Clear out our references to these objects so they get
	  // removed, although the shutdown routine should also do
	  // that.
	  statusname = temp::name();
	  fifoname   = temp::name();

	  exit(0);
	}
    }
}

static bool su_to_root_enabled()
{
  return getuid()!=0;
}

static void update_menubar_autohide()
{
  main_menu->set_always_visible(main_multiplex->num_children()==0 ||
				!aptcfg->FindB(PACKAGE "::UI::Menubar-Autohide",
					       false));
}

cw::widget_ref reload_message;
static void do_show_reload_message()
{
  if(!reload_message.valid())
    {
      cw::widget_ref w = cw::frame::create(cw::label::create(_("Loading cache")));
      reload_message  = cw::center::create(w);
      reload_message->show_all();
      popup_widget(reload_message);

      cw::toplevel::tryupdate();
    }
}

static void do_hide_reload_message()
{
  if(reload_message.valid())
    {
      reload_message->destroy();
      reload_message = NULL;
      cw::toplevel::tryupdate();
    }
}

/** \brief If this is \b true, there's a "really quit aptitude?"
 *  prompt displayed.
 *
 *  The sole purpose of this variable is to prevent the dialog
 *  box that asks about quitting from showing up multiple times.
 */
static bool really_quit_active = false;

static void do_really_quit_answer(bool should_i_quit)
{
  really_quit_active = false;

  if(should_i_quit)
    file_quit();
}

static void do_quit()
{
  if(aptcfg->FindB(PACKAGE "::UI::Prompt-On-Exit", true))
    {
      if(!really_quit_active)
	{
	  really_quit_active = true;
	  prompt_yesno(_("Really quit Aptitude?"), false,
		       cw::util::arg(sigc::bind(ptr_fun(do_really_quit_answer), true)),
		       cw::util::arg(sigc::bind(ptr_fun(do_really_quit_answer), false)));
	}
    }
  else
    file_quit();
}

static void do_destroy_visible()
{
  if(aptcfg->FindB(PACKAGE "::UI::Exit-On-Last-Close", true) &&
     main_multiplex->num_children()<=1)
    do_quit();
  else
    {
      cw::widget_ref w=main_multiplex->visible_widget();
      if(w.valid())
	w->destroy();

      // If all the screens are destroyed, we make the menu visible (so the
      // user knows that something is still alive :) )
      update_menubar_autohide();
    }
}

static bool view_next_prev_enabled()
{
  return main_multiplex->num_visible()>1;
}

static bool any_view_visible()
{
  return main_multiplex->visible_widget().valid();
}

// These are necessary because main_multiplex isn't created until after
// the slot in the static initializer..
// (creating the menu info at a later point would solve this problem)
static void do_view_next()
{
  main_multiplex->cycle_forward();
}

static void do_view_prev()
{
  main_multiplex->cycle_backward();
}

static void do_show_options_tree()
{
  cw::widget_ref w = aptitude::ui::config::make_options_tree();
  add_main_widget(w,
		  _("Preferences"),
		  _("Change the behavior of aptitude"),
		  _("Preferences"));
  w->show_all();
}

#if 0
static void do_show_ui_options_dlg()
{
  cw::widget_ref w = make_ui_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}

static void do_show_misc_options_dlg()
{
  cw::widget_ref w=make_misc_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}

static void do_show_dependency_options_dlg()
{
  cw::widget_ref w=make_dependency_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}
#endif

static void really_do_revert_options()
{
  apt_revertoptions();
  apt_dumpcfg(PACKAGE);
}

static void do_revert_options()
{
  prompt_yesno(_("Really discard your personal settings and reload the defaults?"),
	       false,
	       cw::util::arg(sigc::ptr_fun(really_do_revert_options)),
	       NULL);
}

static cw::widget_ref make_default_view(const menu_tree_ref &mainwidget,
				       pkg_signal *sig,
				       desc_signal *desc_sig,
				       bool allow_visible_desc=true,
				       bool show_reason_first=false)
{
  if(aptcfg->Exists(PACKAGE "::UI::Default-Package-View"))
    {
      list<package_view_item> *format=load_pkgview(PACKAGE "::UI::Default-Package-View");

      if(format)
	{
	  // The unsafe_get_ref is to convert mainwidget to be a
	  // menu_redirect pointer.
	  cw::widget_ref rval=make_package_view(*format, mainwidget,
					       mainwidget.unsafe_get_ref(), sig,
					       desc_sig, show_reason_first);
	  delete format;

	  if(rval.valid())
	    return rval;
	}
    }

  list<package_view_item> basic_format;

  // FIXME: do the config lookup inside the package-view code?
  basic_format.push_back(package_view_item("static1",
					   parse_columns(cw::util::transcode(aptcfg->Find(PACKAGE "::UI::Package-Header-Format", default_pkgheaderdisplay)),
							 pkg_item::pkg_columnizer::parse_column_type,
							 pkg_item::pkg_columnizer::defaults),
					   PACKAGE "::UI::Package-Header-Format",
					   0, 0, 1, 1,
					   cw::table::ALIGN_CENTER | cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::table::ALIGN_CENTER,
					   cw::get_style("Header"),
					   "",
					   "",
					   true));

  basic_format.push_back(package_view_item("main", 1, 0, 1, 1,
					   cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::style(),
					   true));

  basic_format.push_back(package_view_item("static2",
					   parse_columns(cw::util::transcode(aptcfg->Find(PACKAGE "::UI::Package-Status-Format", default_pkgstatusdisplay)),
							 pkg_item::pkg_columnizer::parse_column_type,
							 pkg_item::pkg_columnizer::defaults),
					   PACKAGE "::UI::Package-Status-Format",
					   2, 0, 1, 1,
					   cw::table::ALIGN_CENTER | cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::table::ALIGN_CENTER,
					   cw::get_style("Status"),
					   "", "",
					   true));

  basic_format.push_back(package_view_item("desc", 3, 0, 1, 1,
					   cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
					   cw::style(),
					   "ShowHideDescription", "",
					   allow_visible_desc && aptcfg->FindB(PACKAGE "::UI::Description-Visible-By-Default", true)));

  return make_package_view(basic_format, mainwidget,
			   mainwidget.unsafe_get_ref(),
			   sig, desc_sig,
			   show_reason_first);
}

void do_new_package_view(OpProgress &progress)
{
  pkg_grouppolicy_factory *grp=NULL;
  std::string grpstr="";

  if(aptcfg->Exists(PACKAGE "::UI::Default-Grouping"))
    {
      grpstr=aptcfg->Find(PACKAGE "::UI::Default-Grouping");
      grp=parse_grouppolicy(grpstr);
    }

  if(!grp) // Fallback
    {
      grpstr=default_grpstr;
      grp=parse_grouppolicy(grpstr);

      if(!grp)
	// Eek!  The default grouping failed to parse.  Fall all the
	// way back.
	grp=new pkg_grouppolicy_task_factory(new pkg_grouppolicy_status_factory(new pkg_grouppolicy_section_factory(pkg_grouppolicy_section_factory::split_subdirs,true,new pkg_grouppolicy_section_factory(pkg_grouppolicy_section_factory::split_topdir,false,new pkg_grouppolicy_end_factory()))));
    }

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));

  tree->build_tree(progress);
}

// For signal connections.
static void do_new_package_view_with_new_bar()
{
  progress_ref p = gen_progress_bar();
  do_new_package_view(*p->get_progress().unsafe_get_ref());
  p->destroy();
}

static void do_new_recommendations_view()
{
  progress_ref p = gen_progress_bar();

  pkg_grouppolicy_factory *grp = new pkg_grouppolicy_end_factory();
  std::string grpstr="section(subdirs, passthrough)";

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp,
				     L"!~v!~i~RBrecommends:~i");

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal,
				    true,
				    true),
		  _("Recommended Packages"),
		  _("View packages that it is recommended that you install"),
		  _("Recommendations"));

  tree->build_tree(*p->get_progress().unsafe_get_ref());
  p->destroy();
}

void do_new_flat_view(OpProgress &progress)
{
  pkg_grouppolicy_factory *grp = new pkg_grouppolicy_end_factory;
  pkg_tree_ref tree = pkg_tree::create("", grp);
  tree->set_limit(cw::util::transcode("!~v"));

  add_main_widget(make_default_view(tree,
                                    &tree->selected_signal,
                                    &tree->selected_desc_signal),
                  _("Packages"),
                  _("View available packages and choose actions to perform"),
                  _("Packages"));

  tree->build_tree(progress);
}

// For signal connections.
static void do_new_flat_view_with_new_bar()
{
  progress_ref p = gen_progress_bar();
  do_new_flat_view(*p->get_progress().unsafe_get_ref());
  p->destroy();
}

static void do_new_tag_view_with_new_bar()
{
  progress_ref p = gen_progress_bar();

  pkg_grouppolicy_factory *grp = NULL;
  string grpstr = "tag";
  grp = parse_grouppolicy(grpstr);

  pkg_tree_ref tree = pkg_tree::create(grpstr.c_str(), grp);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));

  tree->build_tree(*p->get_progress().unsafe_get_ref());
  p->destroy();
}

void do_new_hier_view(OpProgress &progress)
{
  pkg_grouppolicy_factory *grp=NULL;
  std::string grpstr="";

  grpstr="hier";
  grp=parse_grouppolicy(grpstr);

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp);
  tree->set_limit(cw::util::transcode("!~v"));
  //tree->set_hierarchical(false);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));
  tree->build_tree(progress);
}

// For signal connections.
static void do_new_hier_view_with_new_bar()
{
  progress_ref p=gen_progress_bar();
  do_new_hier_view(*p->get_progress().unsafe_get_ref());
  p->destroy();
}

cw::widget_ref make_info_screen(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver)
{
  pkg_info_screen_ref w = pkg_info_screen::create(pkg, ver);
  cw::widget_ref rval    = make_default_view(w, w->get_sig(), w->get_desc_sig(), false);
  w->repeat_signal(); // force the status line in the view to update
  return rval;
}

void show_info_screen(const pkgCache::PkgIterator &pkg,
		      const pkgCache::VerIterator &ver)
{
  cw::widget_ref w = make_info_screen(pkg, ver);
  const string name = pkg.FullName(true);
  const string menulabel =
    ssprintf(_("Information about %s"), name.c_str());
  const string tablabel =
    ssprintf(_("%s info"), name.c_str());
  insert_main_widget(w, menulabel, "", tablabel);
}

cw::widget_ref make_dep_screen(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      bool reverse)
{
  pkg_dep_screen_ref w = pkg_dep_screen::create(pkg, ver, reverse);
  cw::widget_ref rval   = make_default_view(w, w->get_sig(), w->get_desc_sig(), true);
  w->repeat_signal(); // force the status line in the view to update
  return rval;
}

void show_dep_screen(const pkgCache::PkgIterator &pkg,
		     const pkgCache::VerIterator &ver,
		     bool reverse)
{
  cw::widget_ref w = make_dep_screen(pkg, ver, reverse);
  const string name = pkg.FullName(true);
  const string menulabel =
    ssprintf(reverse ? _("Packages depending on %s")
		     : _("Dependencies of %s"),
	     name.c_str());
  const string tablabel =
    ssprintf(reverse ? _("%s reverse deps")
		     : _("%s deps"),
	     name.c_str());
  insert_main_widget(w, menulabel, "", tablabel);
}

cw::widget_ref make_ver_screen(const pkgCache::PkgIterator &pkg)
{
  pkg_ver_screen_ref w = pkg_ver_screen::create(pkg);
  cw::widget_ref rval   = make_default_view(w, w->get_sig(), w->get_desc_sig(), true);
  w->repeat_signal();
  return rval;
}

void show_ver_screen(const pkgCache::PkgIterator &pkg)
{
  cw::widget_ref w = make_ver_screen(pkg);
  const string name = pkg.FullName(true);
  const string menulabel =
    ssprintf(_("Available versions of %s"), name.c_str());
  const string tablabel =
    ssprintf(_("%s versions"), name.c_str());
  insert_main_widget(w, menulabel, "", tablabel);
}

static void do_help_about()
{
  cw::fragment *f = cw::fragf(_("Aptitude %s%n%nCopyright 2000-2008 Daniel Burrows.%n"
				"%n"
				"aptitude comes with %BABSOLUTELY NO WARRANTY%b; for details see 'license' in the Help menu.  This is free software, and you are welcome to redistribute it under certain conditions; see 'license' for details."), VERSION);

  cw::widget_ref w=cw::dialogs::ok(wrapbox(f));
  w->show_all();

  popup_widget(w);
}

/** Set up a new top-level file-viewing widget with a scrollbar. */
static cw::widget_ref setup_fileview(const std::string &filename,
				    const char *encoding,
				    const std::string &menudesc,
				    const std::string &longmenudesc,
				    const std::string &tabdesc)
{

  cw::table_ref t      = cw::table::create();
  cw::scrollbar_ref s  = cw::scrollbar::create(cw::scrollbar::VERTICAL);
  cw::file_pager_ref p = cw::file_pager::create(filename, encoding);

  p->line_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &cw::scrollbar::set_slider));
  s->scrollbar_interaction.connect(sigc::mem_fun(p.unsafe_get_ref(), &cw::pager::scroll_page));
  p->scroll_top(); // Force a scrollbar update.

  p->connect_key("Search", &cw::config::global_bindings,
		 sigc::bind(sigc::ptr_fun(&pager_search), p.weak_ref()));
  p->connect_key("ReSearch", &cw::config::global_bindings,
		 sigc::bind(sigc::ptr_fun(&pager_repeat_search), p.weak_ref()));
  p->connect_key("RepeatSearchBack", &cw::config::global_bindings,
		 sigc::bind(sigc::ptr_fun(&pager_repeat_search_back), p.weak_ref()));

  t->add_widget_opts(p, 0, 0, 1, 1,
		     cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
		     cw::table::EXPAND | cw::table::FILL);
  t->add_widget_opts(s, 0, 1, 1, 1,
		     0, cw::table::EXPAND | cw::table::FILL);

  s->show();
  p->show();

  add_main_widget(t, menudesc, longmenudesc, tabdesc);
  return t;
}

static void do_help_license()
{
  setup_fileview(HELPDIR "/COPYING",
		 NULL,
		 _("License"),
		 _("View the terms under which you may copy and distribute aptitude"),
		 _("License"));
}

static void do_help_help()
{
  static cw::widget_ref fileview_widget;

  if(fileview_widget.valid())
    {
      fileview_widget->show();
      return;
    }

  // ForTranslators: You can translate help.txt and set the filename here.
  std::string filename = ssprintf(HELPDIR "/%s", P_("Localized file|help.txt"));

  const char *encoding = P_("Encoding of help.txt|UTF-8");

  // Deal with missing localized docs.
  if(access(filename.c_str(), R_OK) != 0)
    {
      filename = HELPDIR "/help.txt";
      encoding = "UTF-8";
    }

  fileview_widget = setup_fileview(filename,
				   encoding,
				   _("Online Help"),
				   _("View a brief introduction to aptitude"),
				   _("Help"));
  fileview_widget->destroyed.connect(sigc::mem_fun(fileview_widget,
						   &cw::widget_ref::clear));
}

static void do_help_readme()
{
  // Look up the translation of README.
  // ForTranslators: You can translate README and set the filename here.
  std::string readme_file = ssprintf(HELPDIR "/%s", P_("Localized file|README"));
  const char *encoding    = P_("Encoding of README|ISO_8859-1");

  // Deal with missing localized docs.
  if(access(readme_file.c_str(), R_OK)!=0)
    {
      readme_file = HELPDIR "/README";
      encoding    = "ISO_8859-1";
    }

  setup_fileview(readme_file,
		 encoding,
		 _("User's Manual"),
		 _("Read the full user's manual of aptitude"),
		 _("Manual"));
}

static void do_help_faq()
{
  setup_fileview(HELPDIR "/FAQ",
		 NULL,
		 _("FAQ"),
		 _("View a list of frequently asked questions"),
		 _("FAQ"));
}

// news isn't translated since it's just a changelog.
static void do_help_news()
{
  setup_fileview(HELPDIR "/NEWS",
		 NULL,
		 _("News"),
		 ssprintf(_("View the important changes made in each version of %s"), PACKAGE),
		 _("News"));
}

static void do_kill_old_tmp(const std::string old_tmpdir)
{
  if(!aptitude::util::recursive_remdir(old_tmpdir))
    show_message(ssprintf(_("Unable to remove the old temporary directory; you should remove %s by hand."), old_tmpdir.c_str()));
}

static void cancel_kill_old_tmp(const std::string old_tmpdir)
{
  show_message(ssprintf(_("Will not remove %s; you should examine the files in it and remove them by hand."), old_tmpdir.c_str()));
  aptcfg->Set(PACKAGE "::Ignore-Old-Tmp", "true");
  apt_dumpcfg(PACKAGE);
}

static void maybe_show_old_tmpdir_message()
{
  std::string tmpdir_path = get_homedir() + "/.aptitude/.tmp";

  if(aptcfg->FindB(PACKAGE "::Ignore-Old-Tmp", false))
    {
      // Watch for the reappearance of this directory.
      if(access(tmpdir_path.c_str(), F_OK) != 0)
	{
	  aptcfg->Set(PACKAGE "::Ignore-Old-Tmp", "false");
	  apt_dumpcfg(PACKAGE);
	}

      return;
    }

  // Remove it silently if it's empty.
  if(rmdir(tmpdir_path.c_str()) == 0)
    return;

  if(access(tmpdir_path.c_str(), F_OK) == 0)
    prompt_yesno_popup(wrapbox(cw::fragf(_("It appears that a previous version of aptitude left files behind in %s.  These files are probably useless and safe to delete.%n%nDo you want to remove this directory and all its contents?  If you select \"No\", you will not see this message again."), tmpdir_path.c_str())),
		       false,
		       cw::util::arg(sigc::bind(sigc::ptr_fun(do_kill_old_tmp), tmpdir_path)),
		       cw::util::arg(sigc::bind(sigc::ptr_fun(cancel_kill_old_tmp), tmpdir_path)));
}

// There are some circular references because of the code to test
// for consistency before doing a package run; the last routine called before
// the program starts downloading will verify that the selections are
// consistent and call its predecessors if they are not.
//
// (er, can I disentangle this by rearranging the routines?  I think maybe I
//  can to some degree)

namespace
{
  void run_dpkg_with_cwidget_suspended(sigc::slot1<pkgPackageManager::OrderResult, int> f,
				       sigc::slot1<void, pkgPackageManager::OrderResult> k)
  {
    cw::toplevel::suspend();
    pkgPackageManager::OrderResult rval = f(-1);
    
    if(rval != pkgPackageManager::Incomplete)
      {
	cerr << _("Press Return to continue.") << endl;
	int c = getchar();

	while(c != '\n'  && c != EOF)
	  c = getchar();
      }

    // libapt-pkg likes to stomp on SIGINT and SIGQUIT.  Restore them
    // here in the simplest possible way.
    cw::toplevel::install_sighandlers();

    cw::toplevel::resume();

    k(rval);
    return;
  }
}

namespace
{
  // Note that this is only safe if it's OK to copy the thunk in a
  // background thread (i.e., it won't be invalidated by an object being
  // destroyed in another thread).  In the special cases where we use
  // this it should be all right.
  void do_post_thunk(const safe_slot0<void> &thunk)
  {
    cw::toplevel::post_event(new aptitude::safe_slot_event(thunk));
  }

  progress_with_destructor make_progress_bar()
  {
    progress_ref rval = gen_progress_bar();
    return std::make_pair(rval->get_progress(),
			  sigc::mem_fun(*rval.unsafe_get_ref(),
					&progress::destroy));
  }
}

void install_or_remove_packages()
{
  boost::shared_ptr<download_install_manager> m =
    boost::make_shared<download_install_manager>(false,
                                                 true,
                                                 sigc::ptr_fun(&run_dpkg_with_cwidget_suspended));

  m->post_forget_new_hook.connect(package_states_changed.make_slot());

  std::pair<download_signal_log *, download_list_ref>
    download_log_pair = gen_download_progress(false, false,
					      _("Downloading packages"),
					      _("View the progress of the package download"),
					      _("Package Download"));

  ui_download_manager *uim = new ui_download_manager(m,
						     download_log_pair.first,
						     download_log_pair.second,
						     sigc::ptr_fun(&make_progress_bar),
						     &do_post_thunk);

  download_log_pair.second->cancelled.connect(sigc::mem_fun(*uim, &ui_download_manager::aborted));

  uim->download_starts.connect(sigc::bind(sigc::ptr_fun(&ui_start_download), true));
  uim->download_stops.connect(sigc::ptr_fun(&ui_stop_download));

  uim->download_complete.connect(install_finished.make_slot());
  uim->start();
}

/** Make sure that no trust violations are about to be committed.  If
 *  any are, warn the user and give them a chance to fix the
 *  situation.
 *
 *  Should this warning be displayed when a package is selected instead?
 */
static void check_package_trust()
{
  vector<pkgCache::VerIterator> untrusted;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

      if(state.Install())
	{
	  pkgCache::VerIterator curr=pkg.CurrentVer();
	  pkgCache::VerIterator cand=state.InstVerIter(*apt_cache_file);

	  if((curr.end() || package_trusted(curr)) &&
	     !package_trusted(cand))
	    untrusted.push_back(cand);
	}
    }

  if(!untrusted.empty())
    {
      vector<cw::fragment *> frags;

      frags.push_back(wrapbox(cw::fragf(_("%BWARNING%b: untrusted versions of the following packages will be installed!%n%n"
				      "Untrusted packages could %Bcompromise your system's security%b.  "
				      "You should only proceed with the installation if you are certain that this is what you want to do.%n%n"), "Error")));

      for(vector<pkgCache::VerIterator>::const_iterator i=untrusted.begin();
	  i!=untrusted.end(); ++i)
	frags.push_back(clipbox(cw::fragf(_("  %S*%N %s [version %s]%n"),
				      "Bullet",
					  i->ParentPkg().FullName(true).c_str(), i->VerStr())));

      main_stacked->add_visible_widget(cw::dialogs::yesno(cw::sequence_fragment(frags),
						       cw::util::arg(sigc::ptr_fun(install_or_remove_packages)),
						       W_("Really Continue"),
						       NULL,
						       W_("Abort Installation"),
						       cw::get_style("TrustWarning"),
						       true,
						       false),
				       true);
    }
  else
    install_or_remove_packages();
}

namespace
{
  void actually_do_package_run();
}

static void reset_preview()
{
  active_preview=NULL;
  active_preview_tree=NULL;
}

// One word: ewwwwwwww.  I REALLY need to get my act together on the
// view-customization stuff.
static void do_show_preview()
{
  if(!active_preview_tree.valid())
    {
      eassert(!active_preview.valid());

      pkg_grouppolicy_factory *grp=NULL;
      std::string grpstr;

      if(aptcfg->Exists(PACKAGE "::UI::Default-Preview-Grouping"))
	{
	  grpstr=aptcfg->Find(PACKAGE "::UI::Default-Preview-Grouping");
	  grp=parse_grouppolicy(grpstr);
	}

      //if(!grp && aptcfg->Exists(PACKAGE "::UI::Default-Grouping"))
	//{
	//  grpstr=aptcfg->Find(PACKAGE "::UI::Default-Grouping");
	//  grp=parse_grouppolicy(grpstr);
	//}

      if(!grp)
	{
	  grpstr="action";
	  grp=new pkg_grouppolicy_mode_factory(new pkg_grouppolicy_end_factory);
	}

      const std::wstring limitstr =
        cw::util::transcode(aptcfg->Find(PACKAGE "::UI::Preview-Limit", ""));
      active_preview_tree=pkg_tree::create(grpstr.c_str(), grp, limitstr.c_str());

      active_preview=make_default_view(active_preview_tree,
				       &active_preview_tree->selected_signal,
				       &active_preview_tree->selected_desc_signal,
				       true,
				       true);

      active_preview->destroyed.connect(sigc::ptr_fun(reset_preview));
      active_preview->connect_key("DoInstallRun",
				  &cw::config::global_bindings,
				  sigc::ptr_fun(actually_do_package_run));
      add_main_widget(active_preview, _("Preview of package installation"),
		      _("View and/or adjust the actions that will be performed"),
		      _("Preview"));

      progress_ref p=gen_progress_bar();
      active_preview_tree->build_tree(*p->get_progress().unsafe_get_ref());
      p->destroy();
    }
  else
    {
      eassert(active_preview.valid());
      active_preview->show();
    }
}

static void do_keep_all()
{
  if(apt_cache_file == NULL)
    return;

  auto_ptr<undo_group> undo(new apt_undo_group);

  aptitudeDepCache::action_group group(*apt_cache_file, undo.get());

  for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
      !i.end(); ++i)
    (*apt_cache_file)->mark_keep(i, false, false, undo.get());

  if(!undo.get()->empty())
    apt_undos->add_item(undo.release());

  package_states_changed();
}

static void fixer_dialog_done()
{
  if(active_preview_tree.valid())
    active_preview_tree->build_tree();
  do_package_run_or_show_preview();
}

static void install_fixer_dialog()
{
  cw::widget_ref w=make_solution_dialog();
  w->destroyed.connect(sigc::ptr_fun(fixer_dialog_done));
  popup_widget(w, true);
}

// FIXME: blocks.
static void auto_fix_broken()
{
  undo_group *undo=new apt_undo_group;

  try
    {
      eassert(resman != NULL);
      eassert(resman->resolver_exists());

      aptitude_solution sol = resman->get_solution(resman->get_selected_solution(), 0);

      (*apt_cache_file)->apply_solution(sol, undo);

      cw::widget_ref d = cw::dialogs::ok(cw::fragf("%s%n%n%F",
					   _("Some packages were broken and have been fixed:"),
					   solution_fragment(sol)),
				     NULL);

      popup_widget(d, true);
    }
  catch(NoMoreSolutions)
    {
      show_message(_("No solution to these dependency problems exists!"),
		   NULL,
		   cw::get_style("Error"));
    }
  catch(NoMoreTime)
    {
      show_message(cw::fragf(_("Ran out of time while trying to resolve dependencies (press \"%s\" to try harder)"),
			 cw::config::global_bindings.readable_keyname("NextSolution").c_str()),
		   NULL,
		   cw::get_style("Error"));
    }

  if(!undo->empty())
    apt_undos->add_item(undo);
  else
    delete undo;

  if(active_preview_tree.valid())
    active_preview_tree->build_tree();
}


//  Huge FIXME: the preview interacts badly with the menu.  This can be solved
// in a couple ways, including having the preview be a popup dialog -- the best
// thing IMO, though, would be to somehow allow particular widgets to override
// the meaning of global commands.  This needs a little thought, though.  (fake
// keys?  BLEACH)
static void actually_do_package_run_post_essential_check()
{
  if(apt_cache_file)
    {
      if(!active_download)
	{
	  // whatever we call will chain to the next appropriate
	  // routine.
	  if((*apt_cache_file)->BrokenCount()>0)
	    {
	      if(_config->FindB(PACKAGE "::Auto-Fix-Broken", true))
		{
		  auto_fix_broken();
		  do_show_preview();
		}
	      else
		install_fixer_dialog();

	      return;
	    }

	  if(getuid()==0  || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
	    check_package_trust();
	  else
	    {
	      popup_widget(cw::dialogs::yesno(wrapbox(cw::text_fragment(_("Installing/removing packages requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
					   cw::util::arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "-i")),
					   W_("Become root"),
					   cw::util::arg(sigc::ptr_fun(&check_package_trust)),
					   W_("Don't become root"),
					   cw::get_style("Error")));
	    }
	}
      else
	show_message(_("A package-list update or install run is already taking place."), NULL, cw::get_style("Error"));
    }
}

namespace
{
  void actually_do_package_run_finish_delete_essential(const std::wstring &response)
  {
    if(response == cw::util::transcode(_(confirm_delete_essential_str)) ||
       response == cw::util::transcode(confirm_delete_essential_str))
      actually_do_package_run_post_essential_check();
  }

  void actually_do_package_run()
  {
    if(apt_cache_file)
      {
	// Failsafe check to ensure that we aren't deleting any
	// Essential packages.
	//
	// \todo We should remember which ones the user already
	// confirmed and not ask twice.
	std::vector<pkgCache::PkgIterator> deleted_essential, broken_essential;

	for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
	    !pkg.end(); ++pkg)
	  {
	    pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];

	    if((pkg->Flags & pkgCache::Flag::Essential) &&
	       state.Status != 2)
	      {
		if(state.Delete())
		  deleted_essential.push_back(pkg);
		if(state.InstBroken())
		  broken_essential.push_back(pkg);
	      }
	  }

	if(deleted_essential.empty() && broken_essential.empty())
	  actually_do_package_run_post_essential_check();
	else
	  {
	    // We reuse the command line's strings here so that
	    // translators don't need to add new translations.
	    std::vector<cw::fragment *> fragments;
	    if(!deleted_essential.empty())
	      {
		fragments.push_back(wrapbox(cw::text_fragment(_("The following ESSENTIAL packages will be REMOVED!\n"))));

		for(std::vector<pkgCache::PkgIterator>::const_iterator it =
		      deleted_essential.begin(); it != deleted_essential.end(); ++it)
		  {
		    fragments.push_back(cw::fragf("  %S*%N %s%n",
						  "Bullet",
						  it->FullName(true).c_str()));
		  }

		fragments.push_back(cw::newline_fragment());
	      }

	    if(!broken_essential.empty())
	      {
		fragments.push_back(cw::text_fragment(_("The following ESSENTIAL packages will be BROKEN by this action:\n")));

		for(std::vector<pkgCache::PkgIterator>::const_iterator it =
		      broken_essential.begin(); it != broken_essential.end(); ++it)
		  {
		    fragments.push_back(cw::fragf("  %S*%N %s%n",
						  "Bullet",
						  it->FullName(true).c_str()));
		  }

		fragments.push_back(cw::newline_fragment());
	      }

	    fragments.push_back(wrapbox(cw::text_fragment(_("WARNING: Performing this action will probably cause your system to break!\n         Do NOT continue unless you know EXACTLY what you are doing!\n"))));
	    fragments.push_back(wrapbox(cw::fragf(_("To continue, type the phrase \"%s\":\n"), confirm_delete_essential_str)));

	    cw::widget_ref w = cw::dialogs::string(cw::sequence_fragment(fragments),
						   L"",
						   cw::util::arg(sigc::ptr_fun(&actually_do_package_run_finish_delete_essential)),
						   NULL,
						   NULL,
						   NULL,
						   cw::style_attrs_flip(A_REVERSE));
	    w->show_all();
	    popup_widget(w);
	  }
      }
  }
}

void do_package_run_or_show_preview()
{
  // UI: check that something will actually be done first.
  bool some_action_happening=false;
  bool some_non_simple_keep_happening=false;
  for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
    {
      pkg_action_state state = find_pkg_state(i, *apt_cache_file);

      if(state!=pkg_unchanged)
	{
	  some_action_happening=true;

	  if(!(state==pkg_hold && !(*apt_cache_file)->is_held(i)))
	    some_non_simple_keep_happening=true;
	}

      if(some_action_happening && some_non_simple_keep_happening)
	break;
    }

  if(!some_action_happening)
    {
      show_message(_("No packages are scheduled to be installed, removed, or upgraded."));
      return;
    }
  else if(!some_non_simple_keep_happening &&
	  !aptcfg->FindB(PACKAGE "::Allow-Null-Upgrade", false))
    {
      show_message(_("No packages will be installed, removed or upgraded.  "
		     "Some packages could be upgraded, but you have not chosen to upgrade them.  "
		     "Type \"U\" to prepare an upgrade."));
      return;
    }

  if(apt_cache_file)
    {
      if(!active_preview.valid() || !active_preview->get_visible())
	{
	  if(aptcfg->FindB(PACKAGE "::Display-Planned-Action", true))
	    do_show_preview();
	  else
	    actually_do_package_run();
	}
      else
	{
	  active_preview_tree->build_tree();
	  // We need to rebuild the tree since this is called after a
	  // broken-fixing operation.  This feels like a hack, though..
	  active_preview->show();
	}
    }
}

static bool can_start_download()
{
  return !active_download;
}

static bool can_start_download_and_install()
{
  return !active_download && apt_cache_file != NULL;
}

void do_package_run()
{
  if(apt_cache_file)
    {
      if(active_preview_tree.valid() && active_preview_tree->get_visible())
	actually_do_package_run();
      else if((*apt_cache_file)->BrokenCount()>0)
	{
	  if(aptcfg->FindB(PACKAGE "::Auto-Fix-Broken", true))
	    {
	      auto_fix_broken();
	      do_package_run_or_show_preview();
	    }
	  else
	    install_fixer_dialog();
	}
      else
	do_package_run_or_show_preview();
    }
}

static void lists_autoclean_msg(boost::weak_ptr<download_update_manager> m_weak)
{
  boost::shared_ptr<download_update_manager> m(m_weak);

  if(m.get() == NULL)
    return;

  cw::widget_ref msg = cw::center::create(cw::frame::create(cw::label::create(_("Deleting obsolete downloaded files"))));
  m->post_autoclean_hook.connect(sigc::mem_fun(msg.unsafe_get_ref(),
					       &cw::widget::destroy));

  popup_widget(msg);
  cw::toplevel::tryupdate();
}

void really_do_update_lists()
{
  boost::shared_ptr<download_update_manager> m = boost::make_shared<download_update_manager>();
  m->pre_autoclean_hook.connect(sigc::bind(sigc::ptr_fun(lists_autoclean_msg),
					   boost::weak_ptr<download_update_manager>(m)));
  m->post_forget_new_hook.connect(package_states_changed.make_slot());

  std::pair<download_signal_log *, download_list_ref>
    download_log_pair = gen_download_progress(false, true,
					      _("Updating package lists"),
					      _("View the progress of the package list update"),
					      _("List Update"));

  ui_download_manager *uim = new ui_download_manager(m,
						     download_log_pair.first,
						     download_log_pair.second,
						     sigc::ptr_fun(&make_progress_bar),
						     &do_post_thunk);

  download_log_pair.second->cancelled.connect(sigc::mem_fun(*uim, &ui_download_manager::aborted));

  uim->download_starts.connect(sigc::bind(sigc::ptr_fun(&ui_start_download), false));
  uim->download_stops.connect(sigc::ptr_fun(&ui_stop_download));

  uim->download_complete.connect(update_finished.make_slot());
  uim->start();
}

void do_update_lists()
{
  if(!active_download)
    {
      if(getuid()==0 || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
	really_do_update_lists();
      else
	{
	  popup_widget(cw::dialogs::yesno(wrapbox(cw::text_fragment(_("Updating the package lists requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
				       cw::util::arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "-u")),
				       W_("Become root"),
				       cw::util::arg(sigc::ptr_fun(&really_do_update_lists)),
				       W_("Don't become root"),
				       cw::get_style("Error")));
	}
    }
  else
    show_message(_("A package-list update or install run is already taking place."), NULL, cw::get_style("Error"));
}

static void do_sweep()
{
  add_main_widget(cmine::create(), _("Minesweeper"), _("Waste time trying to find mines"), _("Minesweeper"));
}

static void really_do_clean()
{
  if(active_download)
    // Erk!  That's weird!
    _error->Error(_("Cleaning while a download is in progress is not allowed"));
  else
    {
      // Lock the archive directory
      FileFd lock;
      if (_config->FindB("Debug::NoLocking",false) == false)
        {
          lock.Fd(GetLock(_config->FindDir("Dir::Cache::archives") + "lock"));
          if (_error->PendingError() == true)
            {
              _error->Error(_("Unable to lock the download directory"));
              return;
            }
        }

      cw::widget_ref msg=cw::center::create(cw::frame::create(cw::label::create(_("Deleting downloaded files"))));
      msg->show_all();
      popup_widget(msg);
      cw::toplevel::tryupdate();

      if(aptcfg)
	{
	  pkgAcquire fetcher;
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives"));
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives")+"partial/");
	}

      msg->destroy();

      show_message(_("Downloaded package files have been deleted"));
    }
}

void do_clean()
{
  if(getuid()==0 || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
    really_do_clean();
  else
    {
	  popup_widget(cw::dialogs::yesno(wrapbox(cw::text_fragment(_("Cleaning the package cache requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
				       cw::util::arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "--clean-on-startup")),
				       W_("Become root"),
				       cw::util::arg(sigc::ptr_fun(&really_do_update_lists)),
				       W_("Don't become root"),
				       cw::get_style("Error")));
    }
}

// Ok, this is, uh, weird.  Erase has to be overridden to at least
// call unlink()
//
// Should I list what I'm doing like apt-get does?
class my_cleaner:public pkgArchiveCleaner
{
  long total_size;
protected:
  virtual void Erase(const char *file,
		     string pkg,
		     string ver,
		     struct stat &stat)
  {
    if(unlink(file)==0)
      total_size+=stat.st_size;
  }
public:
  my_cleaner();
  long get_total_size() {return total_size;}
};

// g++ bug?  If I include this implementation above in the class
// def'n, it never gets called!
my_cleaner::my_cleaner()
  :total_size(0)
{
}

static bool do_autoclean_enabled()
{
  return apt_cache_file != NULL;
}

static void really_do_autoclean()
{
  if(apt_cache_file == NULL)
    _error->Error(_("The apt cache file is not available; cannot auto-clean."));
  else if(active_download)
    // Erk!  That's weird!
    _error->Error(_("Cleaning while a download is in progress is not allowed"));
  else
    {
      // Lock the archive directory
      FileFd lock;
      if (_config->FindB("Debug::NoLocking",false) == false)
        {
          lock.Fd(GetLock(_config->FindDir("Dir::Cache::archives") + "lock"));
          if (_error->PendingError() == true)
            {
              _error->Error(_("Unable to lock the download directory"));
              return;
            }
        }

      cw::widget_ref msg=cw::center::create(cw::frame::create(cw::label::create(_("Deleting obsolete downloaded files"))));
      msg->show_all();
      popup_widget(msg);
      cw::toplevel::tryupdate();

      long cleaned_size=0;

      if(aptcfg)
	{
	  my_cleaner cleaner;

	  cleaner.Go(aptcfg->FindDir("Dir::Cache::archives"), *apt_cache_file);
	  cleaner.Go(aptcfg->FindDir("Dir::Cache::archives")+"partial/",
		     *apt_cache_file);

	  cleaned_size=cleaner.get_total_size();
	}

      msg->destroy();

      show_message(ssprintf(_("Obsolete downloaded package files have been deleted, freeing %sB of disk space."),
			    SizeToStr(cleaned_size).c_str()));
    }
}

void do_autoclean()
{
  if(getuid()==0 || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
    really_do_autoclean();
  else
    {
	  popup_widget(cw::dialogs::yesno(wrapbox(cw::text_fragment(_("Deleting obsolete files requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
				       cw::util::arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "--autoclean-on-startup")),
				       W_("Become root"),
				       cw::util::arg(sigc::ptr_fun(&really_do_update_lists)),
				       W_("Don't become root"),
				       cw::get_style("Error")));
    }
}

static bool do_mark_upgradable_enabled()
{
  return apt_cache_file != NULL;
}

static void do_mark_upgradable()
{
  if(apt_cache_file)
    {
      undo_group *undo=new apt_undo_group;

      (*apt_cache_file)->mark_all_upgradable(true, true, undo);

      if(!undo->empty())
	apt_undos->add_item(undo);
      else
	delete undo;

      package_states_changed();
    }
}

static bool forget_new_enabled()
{
  if(!apt_cache_file)
    return false;
  else
    return (*apt_cache_file)->get_new_package_count()>0;
}

void do_forget_new()
{
  if(apt_cache_file)
    {
      undoable *undo=NULL;
      (*apt_cache_file)->forget_new(&undo);
      if(undo)
	apt_undos->add_item(undo);

      package_states_changed();
    }
}

#ifdef WITH_RELOAD_CACHE
static void do_reload_cache()
{
  progress_ref p = gen_progress_bar();
  apt_reload_cache(p->get_progress().unsafe_get_ref(), true);
  p->destroy();
}
#endif

static void start_solution_calculation();

class interactive_continuation : public resolver_manager::background_continuation
{
  /** The manager associated with this continuation; usually resman. */
  resolver_manager *manager;

  /** Indicate that a new solution is available by invoking the
   *  selection_changed signal.
   */
  class success_event : public cw::toplevel::event
  {
    resolver_manager *manager;
  public:
    success_event(resolver_manager *_manager)
      :manager(_manager)
    {
    }

    void dispatch()
    {
      manager->state_changed();
    }
  };

  class no_more_solutions_event : public cw::toplevel::event
  {
    resolver_manager *manager;
  public:
    no_more_solutions_event(resolver_manager *_manager)
      :manager(_manager)
    {
    }

    void dispatch()
    {
      resolver_manager::state st = manager->state_snapshot();

      if(st.selected_solution == st.generated_solutions)
	manager->select_previous_solution();
      show_message(_("No more solutions."));

      manager->state_changed();
    }
  };

  class solution_search_aborted_event : public cw::toplevel::event
  {
    resolver_manager *manager;
    std::string msg;
  public:
    solution_search_aborted_event(resolver_manager *_manager,
				  std::string _msg)
      : manager(_manager), msg(_msg)
    {
    }

    void dispatch()
    {
      resolver_manager::state st = manager->state_snapshot();

      if(st.selected_solution == st.generated_solutions)
	manager->select_previous_solution();
      show_message(ssprintf("Fatal error in dependency resolver.  You can continue searching, but some solutions might be impossible to generate.\n\n%s",
			    msg.c_str()));

      manager->state_changed();
    }
  };
public:
  interactive_continuation(resolver_manager *_manager)
    :manager(_manager)
  {
  }

  void success(const aptitude_solution &sol)
  {
    cw::toplevel::post_event(new success_event(manager));
  }

  void no_more_solutions()
  {
    cw::toplevel::post_event(new no_more_solutions_event(manager));
  }

  void no_more_time()
  {
    start_solution_calculation();
  }

  void interrupted()
  {
  }

  void aborted(const std::string &errmsg)
  {
    cw::toplevel::post_event(new solution_search_aborted_event(manager, errmsg));
  }
};

void cwidget_resolver_trampoline(const sigc::slot<void> &f)
{
  f();
}

// If the current solution pointer is past the end of the solution
// list, queue up a calculation for it in the background thread.
static void start_solution_calculation()
{
  resman->maybe_start_solution_calculation(boost::make_shared<interactive_continuation>(resman),
					   &cwidget_resolver_trampoline);
}

static void do_connect_resolver_callback()
{
  resman->state_changed.connect(sigc::ptr_fun(&start_solution_calculation));
  // We may have missed a signal before making the connection:
  start_solution_calculation();
  resman->state_changed.connect(sigc::ptr_fun(&cw::toplevel::update));
}

static bool do_next_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return
    state.selected_solution < state.generated_solutions &&
    !(state.selected_solution + 1 == state.generated_solutions &&
      state.solutions_exhausted);
}

void do_next_solution()
{
  if(!do_next_solution_enabled())
    beep();
  else
    {
      // If an error was encountered, pressing "next solution"
      // skips it.
      resman->discard_error_information();
      resman->select_next_solution();
    }
}

static bool do_previous_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.selected_solution > 0;
}

void do_previous_solution()
{
  if(!do_previous_solution_enabled())
    beep();
  else
    resman->select_previous_solution();
}

static bool do_first_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.resolver_exists && state.selected_solution > 0;
}

static void do_first_solution()
{
  if(!do_first_solution_enabled())
    beep();
  else
    resman->select_solution(0);
}

static bool do_last_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.resolver_exists && state.selected_solution + 1 < state.generated_solutions;
}

static void do_last_solution()
{
  if(resman == NULL)
    {
      beep();
      return;
    }

  resolver_manager::state state = resman->state_snapshot();

  if(!(state.resolver_exists && state.selected_solution + 1 < state.generated_solutions))
    beep();
  else
    resman->select_solution(state.generated_solutions - 1);
}

static bool do_apply_solution_enabled_from_state(const resolver_manager::state &state)
{
  return
    state.resolver_exists &&
    state.selected_solution >= 0 &&
    state.selected_solution < state.generated_solutions;
}

static bool do_apply_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();
  return do_apply_solution_enabled_from_state(state);
}

void do_apply_solution()
{
  if(!apt_cache_file)
    return;

  resolver_manager::state state = resman->state_snapshot();

  if(!do_apply_solution_enabled_from_state(state))
    {
      beep();
      return;
    }
  else
    {
      undo_group *undo=new apt_undo_group;
      try
	{
	  (*apt_cache_file)->apply_solution(resman->get_solution(state.selected_solution, aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000)),
					    undo);
	}
      catch(NoMoreSolutions)
	{
	  show_message(_("Unable to find a solution to apply."),
		       NULL,
		       cw::get_style("Error"));
	}
      catch(NoMoreTime)
	{
	  show_message(_("Ran out of time while trying to find a solution."),
		       NULL,
		       cw::get_style("Error"));
	}

      if(!undo->empty())
	{
	  apt_undos->add_item(undo);
	  package_states_changed();
	}
      else
	delete undo;
    }
}

namespace
{
  bool do_reject_break_holds_enabled()
  {
    return resman != NULL && resman->resolver_exists();
  }

  void do_reject_break_holds()
  {
    if(!do_reject_break_holds_enabled())
      beep();
    else
      resman->reject_break_holds();
  }
}

static void do_nullify_solver(cw::widget_ref *solver)
{
  *solver = NULL;
}

static bool do_examine_solution_enabled()
{
  return resman != NULL && resman->resolver_exists() &&
    aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000) > 0;
}

void do_examine_solution()
{
  if(!do_examine_solution_enabled())
    {
      beep();
      return;
    }

  static cw::widget_ref solver;

  if(solver.valid())
    solver->show();
  else
    {
      solver = make_solution_screen();
      solver->destroyed.connect(sigc::bind(sigc::ptr_fun(&do_nullify_solver),
					   &solver));

      add_main_widget(solver, _("Resolve Dependencies"),
		      _("Search for solutions to unsatisfied dependencies"),
		      _("Resolve Dependencies"));
    }
}

static void handle_dump_resolver_response(const wstring &s)
{
  if(resman != NULL && resman->resolver_exists())
    {
      ofstream out(cw::util::transcode(s).c_str());

      if(!out)
	_error->Errno("dump_resolver", _("Unable to open %ls"), s.c_str());
      else
	{
	  resman->dump(out);

	  if(!out)
	    _error->Errno("dump_resolver", _("Error while dumping resolver state"));
	}
    }
}

static void do_dump_resolver()
{
  static cw::editline::history_list history;

  if(resman != NULL && resman->resolver_exists())
    prompt_string(_("File to write resolver state to: "),
		  "",
		  cw::util::arg(sigc::ptr_fun(handle_dump_resolver_response)),
		  NULL,
		  NULL,
		  &history);
}

// NOTE ON TRANSLATIONS!
//
//   Implicitly translating stuff in the widget set would be ugly.  Everything
// would need to make sure its input to the widget set was able to survive
// translation.
//
//   Using N_ here and translating when we need to display the description
// would be ugly.  Everything would need to make sure its input to the UI would
// be able to handle translation.
//
//   What I do is ugly, but it doesn't force other bits of the program to handle
// input to us with velvet gloves, or otherwise break stuff.  So these structures
// just contain a char *.  I can modify the char *.  In particular, I can mark
// these for translation, then walk through them and munge the pointers to point
// to the translated version.  "EWWWWW!" I hear you say, and you're right, but
// the alternatives are worse.

cw::menu_info actions_menu[]={
  //{cw::menu_info::MENU_ITEM, N_("Test ^Errors"), NULL,
  //N_("Generate an APT error for testing purposes"),
  //SigC::bind(SigC::slot(&silly_test_error), "This is a test error item.")},

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Install/remove packages"), "DoInstallRun",
	       N_("Perform all pending installs and removals"), sigc::ptr_fun(do_package_run), sigc::ptr_fun(can_start_download_and_install)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Update package list"), "UpdatePackageList",
	       N_("Check for new versions of packages"), sigc::ptr_fun(do_update_lists), sigc::ptr_fun(can_start_download)),

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Mark Up^gradable"), "MarkUpgradable",
	       N_("Mark all upgradable packages which are not held for upgrade"),
	       sigc::ptr_fun(do_mark_upgradable), sigc::ptr_fun(do_mark_upgradable_enabled)),

  // FIXME: this is a bad name for the menu item.
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Forget new packages"), "ForgetNewPackages",
	       N_("Forget which packages are \"new\""),
	       sigc::ptr_fun(do_forget_new), sigc::ptr_fun(forget_new_enabled)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Canc^el pending actions"), NULL,
	       N_("Cancel all pending installations, removals, holds, and upgrades."),
	       sigc::ptr_fun(do_keep_all)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Clean package cache"), NULL,
	       N_("Delete package files which were previously downloaded"),
	       sigc::ptr_fun(do_clean)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Clean ^obsolete files"), NULL,
	       N_("Delete package files which can no longer be downloaded"),
	       sigc::ptr_fun(do_autoclean), sigc::ptr_fun(do_autoclean_enabled)),

  cw::menu_info::MENU_SEPARATOR,

#ifdef WITH_RELOAD_CACHE
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Reload package cache"), NULL,
	       N_("Reload the package cache"),
	       sigc::ptr_fun(do_reload_cache)),
#endif

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Play Minesweeper"), NULL,
	       N_("Waste time trying to find mines"), sigc::ptr_fun(do_sweep)),

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Become root"), NULL,
	       N_("Restart the program as root; your settings will be preserved"), sigc::bind(sigc::ptr_fun(do_su_to_root), ""), sigc::ptr_fun(su_to_root_enabled)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Quit"), "QuitProgram",
	       N_("Exit the program"), sigc::ptr_fun(do_quit)),

  cw::menu_info::MENU_END
};

cw::menu_info undo_menu[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Undo"), "Undo",
	       N_("Undo the last package operation or group of operations"),
	       sigc::hide_return(undo_undo.make_slot()),
	       undo_undo_enabled.make_slot()),

  cw::menu_info::MENU_END
};

cw::menu_info package_menu[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Install"), "Install",
	       N_("Flag the currently selected package for installation or upgrade"),
	       sigc::hide_return(package_install.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Remove"), "Remove",
	       N_("Flag the currently selected package for removal"),
	       sigc::hide_return(package_remove.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Purge"), "Purge",
	       N_("Flag the currently selected package and its configuration files for removal"),
	       sigc::hide_return(package_purge.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Keep"), "Keep",
	       N_("Cancel any action on the selected package"),
	       sigc::hide_return(package_keep.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Hold"), "Hold",
	       N_("Cancel any action on the selected package, and protect it from future upgrades"),
	       sigc::hide_return(package_hold.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Mark ^Auto"), "SetAuto",
	       N_("Mark the selected package as having been automatically installed; it will automatically be removed if no other packages depend on it"),
	       sigc::hide_return(package_mark_auto.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Mark ^Manual"), "ClearAuto",
	       N_("Mark the selected package as having been manually installed; it will not be removed unless you manually remove it"),
	       sigc::hide_return(package_unmark_auto.make_slot()),
	       package_menu_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Forbid Version"), "ForbidUpgrade",
	       N_("Forbid the candidate version of the selected package from being installed; newer versions of the package will be installed as usual"),
	       sigc::hide_return(package_forbid.make_slot()),
	       package_forbid_enabled.make_slot()),
  cw::menu_info::MENU_SEPARATOR,
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("I^nformation"), "InfoScreen",
	       N_("Display more information about the selected package"),
	       sigc::hide_return(package_information.make_slot()),
	       package_information_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("C^ycle Package Information"), "DescriptionCycle",
		N_("Cycle through the modes of the package information area: it can show the package's long description, a summary of its dependency status, or an analysis of why the package is required."),
		sigc::hide_return(package_cycle_information.make_slot()),
		package_cycle_information_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Changelog"), "Changelog",
	       N_("Display the Debian changelog of the selected package"),
	       sigc::hide_return(package_changelog.make_slot()),
	       package_changelog_enabled.make_slot()),
  cw::menu_info::MENU_END
};

cw::menu_info resolver_menu[] = {
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Examine Solution"),
	       "ExamineSolution", N_("Examine the currently selected solution to the dependency problems."),
	       sigc::ptr_fun(do_examine_solution),
	       sigc::ptr_fun(do_examine_solution_enabled)),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Apply ^Solution"),
	       "ApplySolution", N_("Perform the actions contained in the currently selected solution."),
	       sigc::ptr_fun(do_apply_solution),
	       sigc::ptr_fun(do_apply_solution_enabled)),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Next Solution"),
	       "NextSolution", N_("Select the next solution to the dependency problems."),
	       sigc::ptr_fun(do_next_solution),
	       sigc::ptr_fun(do_next_solution_enabled)),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Previous Solution"),
	       "PrevSolution", N_("Select the previous solution to the dependency problems."),
	       sigc::ptr_fun(do_previous_solution),
	       sigc::ptr_fun(do_previous_solution_enabled)),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^First Solution"),
	       "FirstSolution", N_("Select the first solution to the dependency problems."),
	       sigc::ptr_fun(do_first_solution),
	       sigc::ptr_fun(do_first_solution_enabled)),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Last Solution"),
	       "LastSolution", N_("Select the last solution to the dependency problems that has been generated so far."),
	       sigc::ptr_fun(do_last_solution),
	       sigc::ptr_fun(do_last_solution_enabled)),

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Toggle ^Rejected"),
	       "SolutionActionReject", N_("Toggle whether the currently selected action is rejected."),
	       sigc::hide_return(resolver_toggle_rejected.make_slot()),
	       resolver_toggle_rejected_enabled.make_slot()),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Toggle ^Approved"),
	       "SolutionActionApprove", N_("Toggle whether the currently selected action is approved."),
	       sigc::hide_return(resolver_toggle_approved.make_slot()),
	       resolver_toggle_approved_enabled.make_slot()),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^View Target"),
	       "InfoScreen", N_("View the package which will be affected by the selected action"),
	       sigc::hide_return(resolver_view_target.make_slot()),
	       resolver_view_target_enabled.make_slot()),

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Reject Breaking ^Holds"),
		"RejectBreakHolds",
		N_("Reject all actions that would change the state of held packages or install forbidden versions"),
		sigc::ptr_fun(&do_reject_break_holds),
		sigc::ptr_fun(&do_reject_break_holds_enabled)),

  cw::menu_info::MENU_END
};

cw::menu_info search_menu[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Find"), "Search",
	       N_("Search forwards"),
	       sigc::hide_return(find_search.make_slot()),
	       find_search_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Find Backwards"), "SearchBack",
	       N_("Search backwards"),
	       sigc::hide_return(find_search_back.make_slot()),
	       find_search_back_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Find ^Again"), "ReSearch",
	       N_("Repeat the last search"),
	       sigc::hide_return(find_research.make_slot()),
	       find_research_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Find Again ^Backwards"), "RepeatSearchBack",
	       N_("Repeat the last search in the opposite direction"),
	       sigc::hide_return(find_repeat_search_back.make_slot()),
	       find_repeat_search_back_enabled.make_slot()),
  cw::menu_info::MENU_SEPARATOR,
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Limit Display"),
	       "ChangePkgTreeLimit", N_("Apply a filter to the package list"),
	       sigc::hide_return(find_limit.make_slot()),
	       find_limit_enabled.make_slot()),
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Un-Limit Display"),
	       NULL, N_("Remove the filter from the package list"),
	       sigc::hide_return(find_cancel_limit.make_slot()),
	       find_cancel_limit_enabled.make_slot()),
  cw::menu_info::MENU_SEPARATOR,
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Find ^Broken"),
	       "SearchBroken", N_("Find the next package with unsatisfied dependencies"),
	       sigc::hide_return(find_broken.make_slot()),
	       find_broken_enabled.make_slot()),
  cw::menu_info::MENU_END
};

cw::menu_info options_menu[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Preferences"), NULL,
	       N_("Change the behavior of aptitude"),
	       sigc::ptr_fun(do_show_options_tree)),

#if 0
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^UI options"), NULL,
	       N_("Change the settings which affect the user interface"),
	       sigc::ptr_fun(do_show_ui_options_dlg)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Dependency handling"), NULL,
	       N_("Change the settings which affect how package dependencies are handled"),
	       sigc::ptr_fun(do_show_dependency_options_dlg)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Miscellaneous"), NULL,
	       N_("Change miscellaneous program settings"),
	       sigc::ptr_fun(do_show_misc_options_dlg)),
#endif

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Revert options"), NULL,
	       N_("Reset all settings to the system defaults"),
	       sigc::ptr_fun(do_revert_options)),

  //{cw::menu_info::MENU_ITEM, N_("^Save options"), NULL,
  // N_("Save current settings for future sessions"), cw::util::arg(bind(sigc::ptr_fun(apt_dumpcfg)),
  //							 PACKAGE)},

  cw::menu_info::MENU_END
};

cw::menu_info views_menu_info[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Next"), "CycleNext",
	       N_("View next display"), sigc::ptr_fun(do_view_next),
	       sigc::ptr_fun(view_next_prev_enabled)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Prev"), "CyclePrev",
	       N_("View previous display"), sigc::ptr_fun(do_view_prev),
	       sigc::ptr_fun(view_next_prev_enabled)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Close"), "Quit",
	       N_("Close this display"), sigc::ptr_fun(do_destroy_visible),
	       sigc::ptr_fun(any_view_visible)),

  cw::menu_info::MENU_SEPARATOR,

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("New Package ^View"), NULL,
	       N_("Create a new default package view"),
	       sigc::ptr_fun(do_new_package_view_with_new_bar)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("Audit ^Recommendations"), NULL,
	       N_("View packages which it is recommended that you install, but which are not currently installed."),
	       sigc::ptr_fun(do_new_recommendations_view)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("New ^Flat Package List"), NULL,
	       N_("View all the packages on the system in a single uncategorized list"),
	       sigc::ptr_fun(do_new_flat_view_with_new_bar)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("New ^Debtags Browser"),
	       NULL,
	       N_("Browse packages using Debtags data"),
	       sigc::ptr_fun(do_new_tag_view_with_new_bar)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("New Categorical ^Browser"),
	       NULL,
	       N_("Browse packages by category"),
	       sigc::ptr_fun(do_new_hier_view_with_new_bar)),

  cw::menu_info::MENU_SEPARATOR,
  cw::menu_info::MENU_END
};

cw::menu_info help_menu_info[]={
  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^About"), NULL,
	       N_("View information about this program"),
	       sigc::ptr_fun(do_help_about)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^Help"), "Help",
	       N_("View the on-line help"), sigc::ptr_fun(do_help_help)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("User's ^Manual"), NULL,
	       N_("View the detailed program manual"),
	       sigc::ptr_fun(do_help_readme)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^FAQ"), NULL,
	       N_("View a list of frequently asked questions"),
	       sigc::ptr_fun(do_help_faq)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^News"), NULL,
		N_("View the important changes made in each version of " PACKAGE),
	       sigc::ptr_fun(do_help_news)),

  cw::menu_info(cw::menu_info::MENU_ITEM, N_("^License"), NULL,
	       N_("View the terms under which you may copy and distribute aptitude"),
	       sigc::ptr_fun(do_help_license)),

  cw::menu_info::MENU_END
};

// This is responsible for converting a particular menu-info thingy.
static void munge_menu(cw::menu_info *menu)
{
  while(menu->item_type!=cw::menu_info::MENU_END)
    {
      if(menu->item_description)
	menu->item_description=_(menu->item_description);

      menu->item_name=_(menu->item_name);

      ++menu;
    }
}

static void do_show_menu_description(cw::menu_item *item, cw::label &label)
{
  if(item && item->get_description().size()>0)
    {
      label.show();
      label.set_text(wrapbox(cw::text_fragment(item->get_description())));
    }
  else
    {
      label.hide();
      label.set_text("");
    }
}

// I want discardable signal arguments!
static void do_setup_columns()
{
  pkg_item::pkg_columnizer::setup_columns(true);
}

static void load_options(string base, bool usetheme)
{
  load_styles(base+"::Styles", usetheme);
  load_bindings(base+"::Keybindings", &cw::config::global_bindings, usetheme);
  load_bindings(base+"::Keybindings::EditLine", cw::editline::bindings, usetheme);
  load_bindings(base+"::Keybindings::Menu", cw::menu::bindings, usetheme);
  load_bindings(base+"::Keybindings::Menubar", cw::menubar::bindings, usetheme);
  load_bindings(base+"::Keybindings::Minesweeper", cmine::bindings, usetheme);
  load_bindings(base+"::Keybindings::MinibufChoice", cw::statuschoice::bindings, usetheme);
  load_bindings(base+"::Keybindings::Pager", cw::pager::bindings, usetheme);
  load_bindings(base+"::KeyBindings::PkgNode", pkg_tree_node::bindings, usetheme);
  load_bindings(base+"::Keybindings::PkgTree", pkg_tree::bindings, usetheme);
  load_bindings(base+"::Keybindings::Table", cw::table::bindings, usetheme);
  load_bindings(base+"::Keybindings::TextLayout", cw::text_layout::bindings, usetheme);
  load_bindings(base+"::Keybindings::Tree", cw::tree::bindings, usetheme);
}

static cw::menu_ref add_menu(cw::menu_info *info, const std::string &name,
			    const cw::label_ref &menu_description)
{
  munge_menu(info);

  cw::menu_ref menu=cw::menu::create(0, 0, 0, info);

  main_menu->append_item(cw::util::transcode(name), menu);

  menu->item_highlighted.connect(sigc::bind(sigc::ptr_fun(do_show_menu_description),
					    menu_description.weak_ref()));

  return menu;
}

static void do_update_show_tabs(cw::multiplex &mp)
{
  mp.set_show_tabs(aptcfg->FindB(PACKAGE "::UI::ViewTabs", true));
}

// argh
class help_bar:public cw::label
{
protected:
  help_bar(const wstring &txt, const cw::style &st):cw::label(txt, st)
  {
    set_visibility();
  }
public:
  static
  cw::util::ref_ptr<help_bar> create(const wstring &txt, const cw::style &st)
  {
    cw::util::ref_ptr<help_bar> rval(new help_bar(txt, st));
    rval->decref();
    return rval;
  }

  inline void set_visibility()
  {
    set_visible(aptcfg->FindB(PACKAGE "::UI::HelpBar", true));
  }
};
typedef cw::util::ref_ptr<help_bar> help_bar_ref;

/** \brief A list of global connections that must be disconnected when the UI exits. */
std::deque<sigc::connection> global_connections;

void ui_init()
{
  cw::toplevel::init();
  init_defaults();

  // The basic behavior of the package state signal is to update the
  // display.
  global_connections.push_back(package_states_changed.connect(sigc::ptr_fun(cw::toplevel::update)));

  global_connections.push_back(consume_errors.connect(sigc::ptr_fun(check_apt_errors)));

  if(aptcfg->Exists(PACKAGE "::Theme"))
    load_options(PACKAGE "::Themes::"+string(aptcfg->Find(PACKAGE "::Theme"))+"::"+PACKAGE "::UI", true);

  load_options(PACKAGE "::UI", false);

  cw::label_ref menu_description=cw::label::create("");

  main_menu=cw::menubar::create(!aptcfg->FindB(PACKAGE "::UI::Menubar-Autohide", false));

  aptcfg->connect(string(PACKAGE "::UI::Menubar-Autohide"),
		  sigc::ptr_fun(update_menubar_autohide));
  aptcfg->connect(string(PACKAGE "::UI::Package-Display-Format"),
		  sigc::ptr_fun(do_setup_columns));

  global_connections.push_back(cache_closed.connect(sigc::ptr_fun(do_show_reload_message)));
  global_connections.push_back(cache_reloaded.connect(sigc::ptr_fun(do_hide_reload_message)));
  global_connections.push_back(cache_reloaded.connect(sigc::ptr_fun(do_connect_resolver_callback)));
  if(apt_cache_file)
    {
      do_connect_resolver_callback();
      start_solution_calculation();
    }
  global_connections.push_back(cache_reload_failed.connect(sigc::ptr_fun(do_hide_reload_message)));

  global_connections.push_back(cache_reloaded.connect(sigc::ptr_fun(do_connect_read_only_callbacks)));
  if(apt_cache_file)
    do_connect_read_only_callbacks();

  add_menu(actions_menu, _("Actions"), menu_description);
  add_menu(undo_menu, _("Undo"), menu_description);
  add_menu(package_menu, _("Package"), menu_description);
  add_menu(resolver_menu, _("Resolver"), menu_description);
  add_menu(search_menu, _("Search"), menu_description);
  add_menu(options_menu, _("Options"), menu_description);
  views_menu=add_menu(views_menu_info, _("Views"), menu_description);
  add_menu(help_menu_info, _("Help"), menu_description);

  main_stacked=cw::stacked::create();
  main_menu->set_subwidget(main_stacked);
  main_stacked->show();

  main_stacked->connect_key_post("QuitProgram", &cw::config::global_bindings, sigc::ptr_fun(do_quit));
  main_stacked->connect_key_post("Quit", &cw::config::global_bindings, sigc::ptr_fun(do_destroy_visible));
  main_stacked->connect_key_post("CycleNext",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_view_next));
  main_stacked->connect_key_post("CyclePrev",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_view_prev));
  main_stacked->connect_key_post("DoInstallRun",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_package_run));
  main_stacked->connect_key_post("UpdatePackageList",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_update_lists));
  main_stacked->connect_key_post("MarkUpgradable",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_mark_upgradable));
  main_stacked->connect_key_post("ForgetNewPackages",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_forget_new));
  main_stacked->connect_key_post("Help",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_help_help));
  main_stacked->connect_key_post("Undo",
				 &cw::config::global_bindings,
				 sigc::hide_return(undo_undo.make_slot()));
  main_stacked->connect_key_post("NextSolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_next_solution));
  main_stacked->connect_key_post("PrevSolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_previous_solution));
  main_stacked->connect_key_post("FirstSolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_first_solution));
  main_stacked->connect_key_post("LastSolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_last_solution));
  main_stacked->connect_key_post("ApplySolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_apply_solution));
  main_stacked->connect_key_post("ExamineSolution",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_examine_solution));
  main_stacked->connect_key_post("DumpResolver",
				 &cw::config::global_bindings,
				 sigc::ptr_fun(do_dump_resolver));

  main_table=cw::table::create();
  main_stacked->add_widget(main_table);
  main_table->show();

  // FIXME: highlight the keys.
  wstring menu_key=cw::config::global_bindings.readable_keyname("ToggleMenuActive"),
    help_key=cw::config::global_bindings.readable_keyname("Help"),
    quit_key=cw::config::global_bindings.readable_keyname("Quit"),
    update_key=cw::config::global_bindings.readable_keyname("UpdatePackageList"),
    install_key=cw::config::global_bindings.readable_keyname("DoInstallRun");

  wstring helptext = swsprintf(W_("%ls: Menu  %ls: Help  %ls: Quit  %ls: Update  %ls: Download/Install/Remove Pkgs").c_str(),
			menu_key.c_str(),
			help_key.c_str(),
			quit_key.c_str(),
			update_key.c_str(),
			install_key.c_str());

  help_bar_ref help_label(help_bar::create(helptext, cw::get_style("Header")));
  main_table->add_widget_opts(help_label, 0, 0, 1, 1,
			      cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			      cw::table::ALIGN_CENTER);
  aptcfg->connect(string(PACKAGE "::UI::HelpBar"),
		  sigc::mem_fun(help_label.unsafe_get_ref(), &help_bar::set_visibility));

  main_multiplex=cw::multiplex::create(aptcfg->FindB(PACKAGE "::UI::ViewTabs", true));
  aptcfg->connect(string(PACKAGE "::UI::ViewTabs"),
		  sigc::bind(sigc::ptr_fun(&do_update_show_tabs), main_multiplex.weak_ref()));
  main_table->add_widget_opts(main_multiplex, 1, 0, 1, 1,
			      cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			      cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK);
  main_multiplex->show();

  cw::widget_ref b=make_broken_indicator();
  main_table->add_widget_opts(b, 2, 0, 1, 1,
			      cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			      cw::table::ALIGN_CENTER);

  main_status_multiplex=cw::multiplex::create();
  main_status_multiplex->set_bg_style(cw::get_style("Status"));
  main_table->add_widget_opts(main_status_multiplex, 3, 0, 1, 1,
			      cw::table::EXPAND | cw::table::FILL | cw::table::SHRINK,
			      cw::table::ALIGN_CENTER);
  main_status_multiplex->show();

  main_status_multiplex->add_widget(menu_description);

  main_menu->show();

  cw::toplevel::settoplevel(main_menu);

  check_apt_errors();
  cw::toplevel::main_hook.connect(sigc::ptr_fun(check_apt_errors));

  help_label->set_visibility();

  update_menubar_autohide();

  // Force parsing of the column stuff.
  // FIXME: put this in load_options() and kill all other references
  //       to setup_columns?
  pkg_item::pkg_columnizer::setup_columns();

  // Make sure the broken indicator doesn't annoyingly pop up for a
  // moment. (hack?)
  //
  // Note that it *should* be visible if we enter this code from the
  // command-line (i.e., with the cache already loaded).  More hack?
  if(resman != NULL && resman->resolver_exists())
    b->show();
  else
    b->hide();

  maybe_show_old_tmpdir_message();
}

struct clear_global_connections_on_exit
{
  clear_global_connections_on_exit()
  {
  }

  ~clear_global_connections_on_exit()
  {
    for(std::deque<sigc::connection>::iterator it = global_connections.begin();
	it != global_connections.end(); ++it)
      it->disconnect();
    global_connections.clear();
  }
};

void ui_main()
{
  clear_global_connections_on_exit clearer;

  cw::toplevel::mainloop();

  if(apt_cache_file &&
     (aptitudeDepCache *) (*apt_cache_file) &&
     apt_cache_file->is_locked())
    {
      progress_ref p=gen_progress_bar();
      (*apt_cache_file)->save_selection_list(*p->get_progress().unsafe_get_ref());
      p->destroy();
    }

  cw::toplevel::shutdown();
}

void popup_widget(const cw::widget_ref &w, bool do_show_all)
{
  main_stacked->add_widget(w);

  if(do_show_all)
    w->show_all();
  else
    w->show();
}

static void setup_main_widget(const cw::widget_ref &w, const std::wstring &menuref,
			      const std::wstring &menudesc)
{
  cw::menu_item *menuentry=new cw::menu_item(menuref, "", menudesc);

  // FIXME: if w is removed from the multiplexer but not destroyed, this may
  //       break.  Fix for now: Don't Do That Then!
  w->destroyed.connect(sigc::bind(sigc::mem_fun(views_menu.unsafe_get_ref(), &cw::menu::remove_item), menuentry));
  menuentry->selected.connect(sigc::mem_fun(w.unsafe_get_ref(), &cw::widget::show));

  views_menu->append_item(menuentry);
}

// Handles the case where the last view is destroyed directly (other than
// through do_destroy_visible); for instance, when a download completes.
static void main_widget_destroyed()
{
  if(aptcfg->FindB(PACKAGE "::UI::Exit-On-Last-Close", true) &&
     main_multiplex->num_children()==0)
    // Don't prompt -- if the last view is destroyed, assume it was by
    // the user's request.
    file_quit();
}

void add_main_widget(const cw::widget_ref &w, const std::wstring &menuref,
		     const std::wstring &menudesc,
		     const std::wstring &tabdesc)
{
  setup_main_widget(w, menuref, menudesc);
  main_multiplex->add_widget(w, tabdesc);
  w->show();
  w->destroyed.connect(sigc::ptr_fun(main_widget_destroyed));

  update_menubar_autohide();
}

void add_main_widget(const cw::widget_ref &w, const std::string &menuref,
		     const std::string &menudesc,
		     const std::string &tabdesc)
{
  add_main_widget(w, cw::util::transcode(menuref), cw::util::transcode(menudesc),
		  cw::util::transcode(tabdesc));
}

void insert_main_widget(const cw::widget_ref &w, const std::wstring &menuref,
			const std::wstring &menudesc,
			const std::wstring &tabdesc)
{
  setup_main_widget(w, menuref, menudesc);
  main_multiplex->add_widget_after(w, main_multiplex->visible_widget(), tabdesc);
  w->show();

  update_menubar_autohide();
}

void insert_main_widget(const cw::widget_ref &w, const std::string &menuref,
			const std::string &menudesc,
			const std::string &tabdesc)
{
  insert_main_widget(w, cw::util::transcode(menuref),
		     cw::util::transcode(menudesc), cw::util::transcode(tabdesc));
}

cw::widget_ref active_main_widget()
{
  return main_multiplex->visible_widget();
}

progress_ref gen_progress_bar()
{
  progress_ref rval = progress::create();

  main_status_multiplex->add_visible_widget(rval, true);

  return rval;
}

cw::fragment *wrapbox(cw::fragment *contents)
{
  if(aptcfg->FindB(PACKAGE "::UI::Fill-Text", false))
    return fillbox(contents);
  else
    return flowbox(contents);
}

static void reset_status_download()
{
  active_status_download=NULL;
}

std::pair<download_signal_log *,
	  download_list_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const wstring &title,
		      const wstring &longtitle,
		      const wstring &tablabel)
{
  download_signal_log *m=new download_signal_log;
  download_list_ref w=NULL;

  if(force_noninvasive ||
     aptcfg->FindB(PACKAGE "::UI::Minibuf-Download-Bar", false))
    {
      w = download_list::create(false, !list_update);
      main_status_multiplex->add_visible_widget(w, true);
      active_status_download=w;
      w->destroyed.connect(sigc::ptr_fun(&reset_status_download));
    }
  else
    {
      w = download_list::create(true, !list_update);
      add_main_widget(w, title, longtitle, tablabel);
    }

  m->MediaChange_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
					   &download_list::MediaChange));
  m->IMSHit_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				      &download_list::IMSHit));

  m->Fetch_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Fetch));

  m->Done_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Done));

  m->Fail_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Fail));

  m->Pulse_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Pulse));

  m->Start_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Start));

  m->Stop_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Stop));

  m->Complete_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
					&download_list::Complete));

  return std::make_pair(m, w);
}

static void do_prompt_string(const wstring &s,
			     cw::editline &e,
			     sigc::slot0<void> realslot)
{
  e.add_to_history(s);
  realslot();
}

std::pair<download_signal_log *, download_list_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const string &title,
		      const string &longtitle,
		      const string &tablabel)
{
  return gen_download_progress(force_noninvasive,
			       list_update,
			       cw::util::transcode(title),
			       cw::util::transcode(longtitle),
			       cw::util::transcode(tablabel));
}

void prompt_string(const std::wstring &prompt,
		   const std::wstring &text,
		   cw::util::slotarg<sigc::slot1<void, wstring> > slot,
		   cw::util::slotarg<sigc::slot0<void> > cancel_slot,
		   cw::util::slotarg<sigc::slot1<void, wstring> > changed_slot,
		   cw::editline::history_list *history)
{
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      cw::editline_ref e=cw::editline::create(prompt, text, history);
      e->set_allow_wrap(true);
      e->set_clear_on_first_edit(true);
      if(slot)
	e->entered.connect(*slot);

      e->entered.connect(sigc::bind(sigc::ptr_fun(do_prompt_string),
				    e.weak_ref(),
				    sigc::mem_fun(e.unsafe_get_ref(), &cw::widget::destroy)));
      if(changed_slot)
	e->text_changed.connect(*changed_slot);

      e->connect_key("Cancel",
		     &cw::config::global_bindings,
		     sigc::mem_fun(e.unsafe_get_ref(), &cw::widget::destroy));

      if(cancel_slot)
	e->connect_key("Cancel",
		       &cw::config::global_bindings,
		       *cancel_slot);

      main_status_multiplex->add_visible_widget(e, true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(cw::dialogs::string(prompt, text,
						      slot, cancel_slot,
						      changed_slot,
						      history),
				     true);
}

void prompt_string(const std::string &prompt,
		   const std::string &text,
		   cw::util::slotarg<sigc::slot1<void, wstring> > slot,
		   cw::util::slotarg<sigc::slot0<void> > cancel_slot,
		   cw::util::slotarg<sigc::slot1<void, wstring> > changed_slot,
		   cw::editline::history_list *history)
{
  prompt_string(cw::util::transcode(prompt), cw::util::transcode(text),
		slot, cancel_slot, changed_slot, history);
}

static void do_prompt_yesno(int cval,
			    bool deflt,
			    cw::util::slot0arg yesslot,
			    cw::util::slot0arg noslot)
{
  bool rval;

  if(deflt)
    rval=!cval;
  else
    rval=cval;

  if(rval)
    {
      if(yesslot)
	(*yesslot)();
    }
  else
    {
      if(noslot)
	(*noslot)();
    }
}

void prompt_yesno(const std::wstring &prompt,
		  bool deflt,
		  cw::util::slot0arg yesslot,
		  cw::util::slot0arg noslot)
{
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      string yesstring, nostring;

      yesstring+=_("yes_key")[0];
      nostring+=_("no_key")[0];
      string yesnostring=deflt?yesstring+nostring:nostring+yesstring;

      cw::statuschoice_ref c=cw::statuschoice::create(prompt, cw::util::transcode(yesnostring));
      c->chosen.connect(sigc::bind(sigc::ptr_fun(&do_prompt_yesno),
				   deflt,
				   yesslot,
				   noslot));

      main_status_multiplex->add_visible_widget(c, true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(cw::dialogs::yesno(prompt,
						     yesslot,
						     noslot,
						     deflt),
				     true);
}

void prompt_yesno_popup(cw::fragment *prompt,
			bool deflt,
			cw::util::slot0arg yesslot,
			cw::util::slot0arg noslot)
{
  main_stacked->add_visible_widget(cw::dialogs::yesno(prompt,
						   yesslot,
						   noslot,
						   false,
						   deflt),
				   true);
}

void prompt_yesno(const std::string &prompt,
		  bool deflt,
		  cw::util::slot0arg yesslot,
		  cw::util::slot0arg noslot)
{
  return prompt_yesno(cw::util::transcode(prompt), deflt, yesslot, noslot);
}

class self_destructing_layout : public cw::text_layout
{
protected:
  self_destructing_layout() : cw::text_layout()
  {
  }

  self_destructing_layout(cw::fragment *f) : cw::text_layout(f)
  {
  }

public:
  bool handle_key(const cw::config::key &k)
  {
    if(!cw::text_layout::focus_me() ||
       !cw::text_layout::handle_key(k))
      destroy();

    return true;
  }

  /** \brief Unlike cw::text_layouts, self-destructing widgets
   *  can always grab the focus.
   */
  bool focus_me()
  {
    return true;
  }

  static cw::util::ref_ptr<self_destructing_layout> create()
  {
    cw::util::ref_ptr<self_destructing_layout> rval(new self_destructing_layout());
    rval->decref();
    return rval;
  }

  static cw::util::ref_ptr<self_destructing_layout> create(cw::fragment *f)
  {
    cw::util::ref_ptr<self_destructing_layout> rval(new self_destructing_layout(f));
    rval->decref();
    return rval;
  }
};

void show_message(cw::fragment *msg,
		  cw::util::slot0arg okslot,
		  const cw::style &st)
{
  msg=wrapbox(msg);
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      cw::text_layout_ref l = self_destructing_layout::create(msg);
      l->set_bg_style(cw::get_style("Status")+st);
      if(okslot)
	l->destroyed.connect(*okslot);

      main_status_multiplex->add_visible_widget(cw::transient::create(l), true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(cw::dialogs::ok(msg, okslot, st), true);
}

void show_message(const std::string &msg,
		  cw::util::slot0arg okslot,
		  const cw::style &st)
{
  show_message(cw::text_fragment(msg), okslot, st);
}

void show_message(const std::wstring &msg,
		  cw::util::slot0arg okslot,
		  const cw::style &st)
{
  show_message(cw::text_fragment(msg), okslot, st);
}
