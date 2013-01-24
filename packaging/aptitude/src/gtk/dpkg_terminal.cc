// dpkg_terminal.cc
//
//  Copyright 2008-2011 Daniel Burrows
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

#include "dpkg_terminal.h"

#include <vte/vte.h>
#include <vte/reaper.h>
#include <glib-object.h>

#include <generic/util/temp.h>

#include <cwidget/generic/util/ssprintf.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h> // Fox UNIX-domain sockets.

#include <termios.h>

#include <apt-pkg/error.h>

#include <aptitude.h>
#include <loggers.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include "gui.h"

namespace cw = cwidget;

using logging::LoggerPtr;
using aptitude::Loggers;

namespace gui
{
  namespace
  {
    // In some theoretical sense, it might be worthwhile to create a
    // thread to monitor the dpkg socket.  In practical terms, though,
    // I expect that it'll be perfectly acceptable to handle dpkg
    // status messages from the main loop.
    //
    // The main reason for this class is that we need to store the
    // dpkg status fd parser somewhere.  It also makes me feel a
    // little better about copying slots around.
    //
    // This is a self-deleting class: it kills itself when it hits
    // EOF on the socket.
    class dpkg_socket_data_processor : public sigc::trackable
    {
      int fd;
      safe_slot1<void, aptitude::apt::dpkg_status_message> report_message;
      aptitude::apt::dpkg_status_parser parser;
      logging::LoggerPtr logger;

      static void delete_socket_data_processor(logging::LoggerPtr logger,
					       dpkg_socket_data_processor *processor)
      {
	LOG_TRACE(logger, "Deleting dpkg socket data processor " << processor << ".");
	delete processor;
      }

    public:
      dpkg_socket_data_processor(int _fd,
				 const safe_slot1<void, aptitude::apt::dpkg_status_message> &_report_message)
	: fd(_fd), report_message(_report_message),
	  logger(Loggers::getAptitudeDpkgStatusPipe())
      {
      }

      bool process_data_from_dpkg_socket(Glib::IOCondition condition)
      {
	LOG_TRACE(logger, "Got dpkg socket event: condition " << condition);

	bool rval = false;

	if(condition & Glib::IO_IN)
	  {
	    LOG_TRACE(logger, "Reading data from the dpkg socket.");
	    char buf[1024];
	    const int buf_len = sizeof(buf);
	    int amt;
	    bool read_anything = false;
	    do
	      {
		amt = recv(fd, buf, buf_len, MSG_DONTWAIT);

		if(amt < 0)
		  {
		    int errnum = errno;
		    std::string err(cw::util::sstrerror(errnum));
		    LOG_FATAL(logger, "Error reading from the dpkg socket: " << err);
		    break;
		  }

		// TODO: I should escape all the socket data.
		LOG_DEBUG(logger, "Read data from the dpkg socket: \"" << std::string(buf, amt) << "\".");

		parser.process_input(buf, amt);

		if(aptcfg->FindB("Debug::Aptitude::Dpkg-Status-Fd", false))
		  write(1, buf, amt);

		while(parser.has_pending_message())
		  {
		    aptitude::apt::dpkg_status_message
		      msg(parser.pop_message());
		    LOG_TRACE(logger, "Parsed dpkg message: " << msg << ".");
		    report_message.get_slot()(msg);
		  }

		if(amt > 0)
		  read_anything = true;
	      }
	    while(amt > 0);

	    if(read_anything)
	      LOG_TRACE(logger, "No data received from the dpkg socket, assuming the process exited.");
	    else
	      LOG_TRACE(logger, "Done reading data from the dpkg socket.");

	    rval = read_anything;
	  }
	else if( (condition & Glib::IO_NVAL) ||
		 (condition & Glib::IO_ERR) ||
		 (condition & Glib::IO_HUP) )
	  {
	    LOG_TRACE(logger, "The socket seems to have closed, exiting.");
	    rval = false;
	  }
	else
	  {
	    LOG_ERROR(logger, "Unexpected IO condition " << condition);
	    _error->Warning("Unexpected IO condition %d", condition);
	    rval = false;
	  }

	if(!rval)
	  // We could probably just "delete this" right here, but I feel
	  // a little nervous about deleting something that's actively
	  // involved in signal delivery.
	  {
	    LOG_DEBUG(logger, "Scheduling this socket data processor (" << this << ") for deletion.");
	    sigc::slot0<void> delete_this =
	      sigc::bind(sigc::ptr_fun(&dpkg_socket_data_processor::delete_socket_data_processor),
			 logger, this);
	    post_event(make_safe_slot(delete_this));
	  }

	return rval;
      }
    };

    // Closure structure to connect up the child-exited signal.
    struct child_exited_info
    {
      // The PID of the child to monitor.
      pid_t pid;

      // The continuation of the child.
      safe_slot1<void, pkgPackageManager::OrderResult> k;

      LoggerPtr logger;

      gulong handler_id;

      child_exited_info(pid_t _pid,
			const safe_slot1<void, pkgPackageManager::OrderResult> &_k)
	: pid(_pid),
	  k(_k),
	  logger(Loggers::getAptitudeDpkgTerminal())
      {
      }
    };

    void destroy_child_exited_info(gpointer data,
				   GClosure *closure)
    {
      LOG_TRACE(((child_exited_info *)data)->logger,
		"Destroying the child exited signal associated with " << data << ".");
      delete (child_exited_info *)data;
    }

    void handle_dpkg_result(VteReaper *vtereaper,
			    gint pid,
			    gint status,
			    gpointer user_data)
    {
      child_exited_info *info = (child_exited_info *)user_data;
      LOG_DEBUG(info->logger,
		"PID " << pid << " exited, status " << status << " (" << user_data << ").");

      if(info->pid != pid)
	{
	  LOG_TRACE(info->logger,
		    "Wrong PID, not announcing that dpkg finished.");
	  return;
	}

      pkgPackageManager::OrderResult result = pkgPackageManager::Failed;
      if(WIFEXITED(status))
	result = (pkgPackageManager::OrderResult) WEXITSTATUS(status);


      LOG_TRACE(info->logger,
		"dpkg result is " << result << ".");

      info->k.get_slot()(pkgPackageManager::Failed);

      g_signal_handler_disconnect(vte_reaper_get(),
				  info->handler_id);
    }

    // Connect to the child-exited signal so we know when dpkg exits.
    // We can't use a sigc++ connection because there's no C++ wrapper
    // for VTE, and just using an old-school GTK+ binding is easier
    // than writing a C++ wrapper.
    void connect_dpkg_result(pid_t pid,
			     safe_slot1<void, pkgPackageManager::OrderResult> k)
    {
      child_exited_info *info = new child_exited_info(pid, k);

      LOG_TRACE(info->logger, "Setting up callback to listen for the exit status of process "
		<< pid << " (" << info << ")");

      // We use implicit locking here (plus the fact that we are
      // running in the foreground thread) to know that the signal
      // won't be triggered before handler_id is set.
      info->handler_id =
	g_signal_connect_data(vte_reaper_get(),
			      "child-exited",
			      G_CALLBACK(&handle_dpkg_result),
			      info,
			      &destroy_child_exited_info,
			      (GConnectFlags)0);
    }
  }

  void DpkgTerminal::reset_inactivity_timeout()
  {
    if(!subprocess_complete)
      {
	inactivity_interval = aptcfg->FindI(PACKAGE "::Dpkg-Inactivity-Interval", 120);
	if(inactivity_interval > 0)
	  {
	    LOG_TRACE(Loggers::getAptitudeDpkgTerminalInactivity(),
		      "Resetting the dpkg inactivity timeout to " << inactivity_interval << " seconds.");
	    inactivity_interval_expired_connection.disconnect();
	    inactivity_interval_expired_connection =
	      Glib::signal_timeout().connect_seconds(sigc::mem_fun(*this, &DpkgTerminal::subprocess_timeout_handler),
						     inactivity_interval);
	  }
	else
	  LOG_TRACE(Loggers::getAptitudeDpkgTerminalInactivity(),
		    "Not resetting the dpkg inactivity timeout: the timeout interval " << inactivity_interval << " is less than or equal to zero.");
      }
    else
      LOG_TRACE(Loggers::getAptitudeDpkgTerminalInactivity(),
		"Not resetting the dpkg inactivity timeout: dpkg already terminated.");
  }

  bool DpkgTerminal::subprocess_timeout_handler()
  {
    LOG_TRACE(Loggers::getAptitudeDpkgTerminalInactivity(),
	      "Subprocess has been inactive for " << inactivity_interval << " seconds.");

    if(!subprocess_complete)
      subprocess_running_changed(false);
    else
	LOG_WARN(Loggers::getAptitudeDpkgTerminalInactivity(),
		 "Suppressing inactivity warning: the subprocess is already complete.");

    return false;
  }

  void DpkgTerminal::handle_status_message(aptitude::apt::dpkg_status_message msg)
  {
    subprocess_running_changed(true);
    reset_inactivity_timeout();
    status_message(msg);
  }

  void DpkgTerminal::handle_dpkg_finished(pkgPackageManager::OrderResult result)
  {
    LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
	      "dpkg completed (status " << result << ")");
    inactivity_interval_expired_connection.disconnect();
    subprocess_complete = true;
    finished(result);
  }

  DpkgTerminal::DpkgTerminal()
    : sent_finished_signal(false),
      subprocess_complete(false),
      logger(Loggers::getAptitudeDpkgTerminal())
  {
    LOG_TRACE(logger, "Creating the dpkg terminal manager.");

    GtkWidget *vte = vte_terminal_new();
    terminal = (Glib::wrap(vte, false));

    aptcfg->connect(PACKAGE "::Dpkg-Inactivity-Interval",
		    sigc::mem_fun(*this, &DpkgTerminal::reset_inactivity_timeout));
    reset_inactivity_timeout();
  }

  DpkgTerminal::~DpkgTerminal()
  {
    LOG_TRACE(logger, "Destroying the dpkg terminal manager (" << this << ").");
    delete terminal;
  }

  /** \brief Exception related to the temporary socket class. */
  class TemporarySocketFail : public cwidget::util::Exception
  {
    const std::string msg;
  public:
    TemporarySocketFail(const std::string &_msg) : msg(_msg)
    {
    }

    std::string errmsg() const
    {
      return msg;
    }
  };

  int open_unix_socket()
  {
    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if(sock == -1)
      {
	int errnum = errno;
	std::string err = cw::util::sstrerror(errnum);
	std::string msg = cw::util::ssprintf(_("%s: Unable to create a Unix-domain socket: %s"),
					     "open_unix_socket",
					     err.c_str());
	LOG_FATAL(Loggers::getAptitudeDpkgTerminal(), msg);
	throw TemporarySocketFail(msg);
      }

    return sock;
  }

  /** \brief Represents the read end of a pair of Unix-domain sockets. */
  class temporary_listen_socket
  {
    temp::name name;

    std::auto_ptr<FileFd> listen_sock;
    struct sockaddr_un addr;

  public:
    temporary_listen_socket(const temp::name &_name,
			    int backlog)
      : name(_name)
    {
      using namespace cwidget::util;
      int fd = open_unix_socket();

      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Listening on the temporary socket \"" << name.get_name() << "\".");

      listen_sock = std::auto_ptr<FileFd>(new FileFd(fd));

      const size_t max_socket_name = sizeof(addr.sun_path);

      if(name.get_name().size() > max_socket_name)
	{
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(), "Internal error: the temporary socket name \"" << name.get_name() << " is too long!");
	  throw TemporarySocketFail(ssprintf(_("Internal error: the temporary socket name \"%s\" is too long!"),
					     name.get_name().c_str()));
	}

      addr.sun_family = AF_UNIX;
      strncpy(addr.sun_path, name.get_name().c_str(), max_socket_name);

      if(bind(listen_sock->Fd(), (struct sockaddr *)&addr, sizeof(addr)) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to bind to the temporary socket: " << err);
	  throw TemporarySocketFail(ssprintf("%s: Unable to bind to the temporary socket: %s",
					     __PRETTY_FUNCTION__,
					     err.c_str()));
	}

      if(listen(listen_sock->Fd(), backlog) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to listen on the temporary socket: " << err);
	  throw TemporarySocketFail(ssprintf("%s: Unable to listen on the temporary socket: %s",
					     __PRETTY_FUNCTION__,
					     err.c_str()));
	}
    }

    int accept() const
    {
      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Accepting a connection to \"" << name.get_name() << "\"");
      int result = ::accept(listen_sock->Fd(), NULL, NULL);
      if(result == -1)
	{
	  int errnum = errno;
	  std::string errmsg = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to accept a connection: " << errmsg);
	  throw TemporarySocketFail(cw::util::ssprintf(_("%s: Unable to accept a connection: %s"),
						       __PRETTY_FUNCTION__,
						       errmsg.c_str()));
	}

      LOG_DEBUG(Loggers::getAptitudeDpkgTerminal(),
		"Accepted connection to the temporary socket \"" << name.get_name() << "\" as fd " << result << ".");

      return result;
    }

    const sockaddr_un &get_addr() const { return addr; }
  };


  class temporary_client_socket
  {
    temp::name name;

    struct sockaddr_un addr;

    std::auto_ptr<FileFd> fd;

  public:
    temporary_client_socket(const temp::name &_name)
      : name(_name)
    {
      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Connecting to the temporary socket \"" << name.get_name() << "\".");

      const size_t max_socket_name = sizeof(addr.sun_path);

      if(name.get_name().size() > max_socket_name)
	{
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    "Internal error: the temporary socket name \""
		    << name.get_name() << "\" is too long!");
	  throw TemporarySocketFail(cw::util::ssprintf(_("Internal error: the temporary socket name \"%s\" is too long!"),
						       name.get_name().c_str()));
	}

      addr.sun_family = AF_UNIX;
      strncpy(addr.sun_path, name.get_name().c_str(), max_socket_name);

      fd = std::auto_ptr<FileFd>(new FileFd(open_unix_socket()));


      if(connect(fd->Fd(), (struct sockaddr *)&addr, sizeof(addr)) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": Unable to bind to the temporary socket: " << err);
	  throw TemporarySocketFail(cw::util::ssprintf("%s: Unable to bind to the temporary socket: %s",
						       __PRETTY_FUNCTION__,
						       err.c_str()));
	}

      LOG_DEBUG(Loggers::getAptitudeDpkgTerminal(),
		"Connected to the temporary socket \"" << name.get_name() << "\" as fd " << fd->Fd() << ".");
    }

    int get_fd() const { return fd->Fd(); }
  };

  void DpkgTerminal::child_process(const temp::name &dpkg_socket_name,
				   const safe_slot1<pkgPackageManager::OrderResult, int> &f)
  {
    // The child process.  It passes status information to the
    // parent process and uses its *return code* to indicate the
    // success / failure state.

    LOG_INFO(logger, "Child process starting.");

    // We have to do this before tcsetattr(), because tcsetattr()
    // requires the terminal (that's the parent process) to respond,
    // but that process is blocked in accept() until we open the side
    // channels.
    //
    // This is all somewhat fragile; I wish Gnome bug #320128 was
    // fixed so that I could just make an internal pipe. :-(
    LOG_TRACE(logger, "Opening sockets to parent process.");

    std::auto_ptr<temporary_client_socket> dpkg_sock;
    try
      {
	dpkg_sock = std::auto_ptr<temporary_client_socket>(new temporary_client_socket(dpkg_socket_name));
      }
    catch(TemporarySocketFail &ex)
      {
	LOG_FATAL(logger, "Unable to open sockets: " << ex.errmsg());
	_error->Error("%s", ex.errmsg().c_str());
	_error->DumpErrors();
	_exit(pkgPackageManager::Failed);
      }

    char timebuf[512] = "";

    const time_t start_time = time(0);
    struct tm start_local_time;
    localtime_r(&start_time, &start_local_time);
    if(strftime(timebuf, sizeof(timebuf), "%c", &start_local_time) == 0)
      strcpy(timebuf, "ERR");
    printf(_("[%s] dpkg process starting...\n"), timebuf);

    _error->DumpErrors();

    // To handle job control (so that we can detect when the child
    // needs terminal access), we fork into a "shell process" and a
    // "dpkg process".  The "dpkg process" actually runs dpkg; its
    // exit status indicates whether it was successful.  Both
    // processes set the dpkg process up as the head of a new process
    // group (the reason for this is to ensure that it ends up in that
    // group before either process starts to do stuff with it).
    //
    // The dpkg process is started in the background; it is moved into
    // the foreground or the background based on the commands received
    // from the parent (GUI) process.

    pkgPackageManager::OrderResult result = f.get_slot()(dpkg_sock->get_fd());

    LOG_TRACE(logger, "Subprocess finished, result is " << result << ".");

    // Make sure errors appear somewhere (we really ought to push
    // them down the FIFO).
    _error->DumpErrors();

    const time_t end_time = time(0);
    struct tm end_local_time;
    localtime_r(&end_time, &end_local_time);
    if(strftime(timebuf, sizeof(timebuf), "%c", &end_local_time) == 0)
      strcpy(timebuf, "ERR");

    switch(result)
      {
      case pkgPackageManager::Completed:
	printf(_("[%s] dpkg process complete.\n"), timebuf);
	break;
      case pkgPackageManager::Failed:
	printf(_("[%s] dpkg process failed.\n"), timebuf);
	break;
      case pkgPackageManager::Incomplete:
	printf(_("[%s] dpkg process complete; there are more packages left to process.\n"),
	       timebuf);
	break;
      default:
	printf("[%s] dpkg process complete; internal error: bad result code (%d).\n",
	       timebuf, result);
	break;
      }

    _exit(result);
  }

  void DpkgTerminal::run(const safe_slot1<pkgPackageManager::OrderResult, int> &f)
  {
    // Create a temporary UNIX-domain socket to pass status
    // information to the parent.
    temp::name dpkg_socket_name("commsocket");

    // To avoid races, we bind the receive end of the socket first and
    // start accepting connections.
    std::auto_ptr<temporary_listen_socket> listen_sock;

    try
      {
        // TODO: now that I control the fork() call, I can probably
        // use pipes instead of a socket.
	listen_sock = std::auto_ptr<temporary_listen_socket>(new temporary_listen_socket(dpkg_socket_name, 1));
      }
    catch(TemporarySocketFail &ex)
      {
	LOG_FATAL(logger, "Failed to create a listen socket for the temporary control socket: " << ex.errmsg());
	_error->Error("%s", ex.errmsg().c_str());
	finished(pkgPackageManager::Failed);
	return;
      }

    GtkWidget *vte = terminal->gobj();
    GError *error = NULL;
    VtePty *pty = vte_terminal_pty_new(VTE_TERMINAL(vte),
                                       static_cast<VtePtyFlags>(VTE_PTY_NO_LASTLOG | VTE_PTY_NO_UTMP | VTE_PTY_NO_WTMP),
                                       &error);

    if(error != NULL)
      {
        // Couldn't create a pty ... what to do now?  Can't run dpkg
        // without a pty, so abort.
        LOG_ERROR(logger, "Unable to create a pty: " << error->message);
        _error->Error("Unable to create a pty: %s", error->message);

        if(pty != NULL)
          g_free(pty);

        return;
      }

    pid_t pid = fork();

    if(pid == 0)
      {
        vte_pty_child_setup(pty);
        child_process(dpkg_socket_name, f);
      }
    else
      {
	LOG_INFO(logger, "The shell process is PID " << pid << ".");

	int dpkg_sock = -1;

	try
	  {
	    dpkg_sock = listen_sock->accept();
	  }
	catch(TemporarySocketFail &ex)
	  {
	    LOG_ERROR(logger, "Failed to establish control sockets: " << ex.errmsg());
	    _error->Error("%s", ex.errmsg().c_str());
	    // Not a fatal error -- try to install anyway.
	  }

	// Catch status output from the install process.
	sigc::slot1<void, aptitude::apt::dpkg_status_message>
	  report_message_slot(sigc::mem_fun(*this, &DpkgTerminal::handle_status_message));
	dpkg_socket_data_processor *data_processor = new dpkg_socket_data_processor(dpkg_sock, make_safe_slot(report_message_slot));
	if(dpkg_sock != -1)
	  Glib::signal_io().connect(sigc::mem_fun(*data_processor, &dpkg_socket_data_processor::process_data_from_dpkg_socket),
				    dpkg_sock,
				    Glib::IO_IN | Glib::IO_ERR | Glib::IO_HUP | Glib::IO_NVAL);

	// The parent process.  Here we just wait for the reaper to
	// tell us that the child finished, then return the result.
	// We use implicit locking here to avoid a race condition that
	// could occur: we know that the reaper won't fire before we
	// can connect to it because its signal executions go through
	// the main loop, and this function call is blocking the main
	// loop.
	sigc::slot1<void, pkgPackageManager::OrderResult> finished_slot =
	  sigc::mem_fun(*this, &DpkgTerminal::handle_dpkg_finished);
	connect_dpkg_result(pid, make_safe_slot(finished_slot));

	vte_reaper_add_child(pid);
      }
  }

  void DpkgTerminal::inject_yes()
  {
    LOG_TRACE(Loggers::getAptitudeDpkgTerminal(), "Sending 'y' to the dpkg process.");

    VteTerminal *vte = VTE_TERMINAL(terminal->gobj());

    vte_terminal_feed_child(vte, "y\n", 2);
  }

  void DpkgTerminal::inject_no()
  {
    LOG_TRACE(Loggers::getAptitudeDpkgTerminal(), "Sending 'n' to the dpkg process.");

    VteTerminal *vte = VTE_TERMINAL(terminal->gobj());

    vte_terminal_feed_child(vte, "n\n", 2);
  }
}
