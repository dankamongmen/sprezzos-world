/** \file qt_main.cc */
//
// Copyright (C) 2010 Daniel Burrows
// Copyright 2008-2009 Obey Arthur Liu
// Copyright (C) 2010 Piotr Galiszewski
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Local includes
#include "qt_main.h"

#include "windows/main_window.h"

#include <generic/apt/apt.h>

#include <loggers.h>

// System includes
#include <apt-pkg/error.h>

#include <QtGui/QApplication>

#include <signal.h>

using logging::LoggerPtr;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      void qt_consume_errors()
      {
        LoggerPtr logger = Loggers::getAptitudeQtInit();

        while(!_error->empty())
          {
            std::string msg;
            if(_error->PopMessage(msg))
              LOG_ERROR(logger, "E: " << msg);
            else
              LOG_WARN(logger, "W: " << msg);
          }
      }

      bool main(int argc, char **argv)
      {
        LoggerPtr logger = Loggers::getAptitudeQtInit();

        LOG_TRACE(logger, "Qt frontend starting.");

	// Don't crash if a subprocess breaks a pipe.
	signal(SIGPIPE, SIG_IGN);

        // TODO: this should be connected to something that properly
        // saves the errors and displays them in the UI.
        consume_errors.connect(sigc::ptr_fun(&qt_consume_errors));

        LOG_TRACE(logger, "Creating QApplication");

	QApplication app(argc,argv);

        LOG_TRACE(logger, "Displaying main window");

	main_window *main = new main_window;
	main->show();

        LOG_INFO(logger, "Entering main loop");

	app.exec();

        LOG_TRACE(logger, "Main loop complete");

	return true;
      }
    }
  }
}
