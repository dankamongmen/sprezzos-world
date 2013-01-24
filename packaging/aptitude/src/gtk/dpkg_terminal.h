// -*-c++-*-

// dpkg_terminal.h
//
//  Copyright 2008-2010 Daniel Burrows
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

#ifndef DPKG_TERMINAL_H
#define DPKG_TERMINAL_H

#include <gtkmm.h>

#include <apt-pkg/packagemanager.h>

#include <generic/apt/parse_dpkg_status.h>
#include <generic/util/safe_slot.h>
#include <generic/util/temp.h>

#include <loggers.h>

struct sockaddr_un;

/** \brief Support for creating a GUI terminal in which dpkg can be
 *  invoked.
 *
 *  \file dpkg_terminal.h
 */

namespace gui
{
  /** \brief This object manages setting up and destroying the dpkg
   *  terminal.
   */
  class DpkgTerminal : public sigc::trackable
  {
    Gtk::Widget *terminal;
    bool sent_finished_signal;

    // Used to suppress activity timeouts after dpkg finishes.
    bool subprocess_complete;

    logging::LoggerPtr logger;

    /** \brief How long (in seconds) we wait between messages from the
     *  install process before we claim that it's waiting for input.
     *
     *  Option Aptitude::Dpkg-Inactivity-Interval.
     *
     *  If 0, we never claim the install process is waiting for input.
     */
    int inactivity_interval;

    /** \brief The connection that will wake the main thread up to
     *  announce that the dpkg terminal is waiting for user input.
     */
    sigc::connection inactivity_interval_expired_connection;

    /** \brief Disconnect any pending activity-threshold connection,
     *  then recreate the timeout.
     */
    void reset_inactivity_timeout();

    /** \brief Invoked when the subprocess is waiting for input
     *  (according to the activity timeout).
     */
    bool subprocess_timeout_handler();

    /** \brief Reset the activity timeout and forward the given
     *  message to the status message signal.
     */
    void handle_status_message(aptitude::apt::dpkg_status_message msg);

    /** \brief Invoked when the subprocess terminates.
     *
     *  Flips the 
     */
    void handle_dpkg_finished(pkgPackageManager::OrderResult result);

    // Forbid copying.
    DpkgTerminal(const DpkgTerminal &);

    /** \brief The code that initializes the dpkg invocation in the
     *  terminal.
     *
     *  \param dpkg_socket_name
     *              The socket on which to report the child's dpkg status.
     *  \param f    A callback to invoke after dpkg finishes running.
     */
    void child_process(const temp::name &dpkg_socket_name,
		       const safe_slot1<pkgPackageManager::OrderResult, int> &f);
  public:
    /** \brief Initialize a new terminal. */
    DpkgTerminal();

    ~DpkgTerminal();

    /** \brief Return a wrapper around the terminal object.
     *
     *  The wrapper is owned by this DpkgTerminal.
     */

    Gtk::Widget *get_widget() const { return terminal; }
    /** \brief A signal that triggers in the main thread when dpkg
     *	finishes running.
     */
    sigc::signal1<void, pkgPackageManager::OrderResult> finished; 

    /** \brief A signal that triggers in the main thread when a
     *  message is received on the dpkg status pipe.
     */
    sigc::signal1<void, aptitude::apt::dpkg_status_message> status_message;

    /** \brief Emitted when the dpkg process appears to have stopped
     *  running or continued.
     *
     *  The argument states whether the process is currently running:
     *  "true" if it is running, and "false" if it is suspended.  This
     *  is a heuristic based on whether the subprocess has generated
     *  output, and it may be wrong in both directions.  It might be
     *  generated several times in a row with the same argument.
     */
    sigc::signal<void, bool> subprocess_running_changed;

    /** \brief Start running dpkg in the encapsulated terminal.
     *
     *  This must be invoked from a foreground thread.
     *
     *  \param f The function that actually invokes dpkg.
     */
    void run(const safe_slot1<pkgPackageManager::OrderResult, int> &f);

    // It would be somewhat nicer to just pass around slots to say
    // "yes" or "no" (so only something that read a
    // conffile-replacement message would have a token that could
    // invoke these).

    /** \brief Send "yes" in reply to a "replace this conffile?"
     *  message.
     */
    void inject_yes();

    /** \brief Send "no" in reply to a "replace this conffile?" message.
     */
    void inject_no();
  };
}

#endif
