/** \file logging.h */    // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
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

#ifndef APTITUDE_UTIL_LOGGING_H
#define APTITUDE_UTIL_LOGGING_H

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

#include <sigc++/connection.h>
#include <sigc++/slot.h>

#include <sstream>

#include <limits.h> // For INT_MIN

namespace aptitude
{
  namespace util
  {
    /** \brief A lightweight, no-frills logging library.
     *
     *  aptitude implements its own logging instead of using an
     *  external library for a couple reasons.  We don't need the full
     *  power of a real logging library, since we only use this for
     *  debugging purposes, and real logging libraries have proven to
     *  be flaky and poorly maintained.  In particular log4cxx was
     *  responsible for many crashes due to aptitude breaking
     *  undocumented assumptions that the library made.
     */
    namespace logging
    {
      /** \brief The importance of a log message.
       */
      enum log_level
        {
          TRACE_LEVEL = 0,
          DEBUG_LEVEL = 1,
          INFO_LEVEL = 2,
          WARN_LEVEL = 3,
          ERROR_LEVEL = 4,
          FATAL_LEVEL = 5,
          /** \brief The OFF level is special: loggers set to this
           *  level never display any messages.
           */
          OFF_LEVEL = INT_MIN
        };

      /** \brief Get a string corresponding to a log level.
       *
       *  The level must be a member of the enum log_level.
       */
      const char *describe_log_level(log_level l);

      class Logger;
      typedef boost::shared_ptr<Logger> LoggerPtr;

      class LoggingSystem;

      /** \brief A logger is used to log messages in a particular
       *  category in conjunction with the LOG_*() macros below.
       */
      class Logger
      {
        // The "effective" level of this logger.  Only messages at or
        // above this level will be logged.  Defaults to ERROR.
        //
        // This is provided as a concrete member in the base class so
        // that "is logging enabled?" tests can be inlined.
        log_level effectiveLevel;

        class Impl;

        Logger(log_level _effectiveLevel);
        Logger(const Logger &);

        // Logger is really just an interface to the hidden
        // LoggingSystem singleton.  For organizational reasons and to
        // handle threading better, all the "real" code in this module
        // lives over there.  Note that this means there can't be any
        // other implementations of Logger; the virtual methods here
        // are just to hide the implementation, not to provide
        // polymorphism.
        friend class LoggingSystem;

      public:
        virtual ~Logger();

        /** \brief Return \b true if messages logged at the given
         *  level will appear.
         *
         *  Useful because this lets us suppress the (potentially
         *  expensive) generation of log messages if they're disabled.
         */
        bool isEnabledFor(log_level l) const
        {
          return
            effectiveLevel != OFF_LEVEL &&
            l >= effectiveLevel;
        }

        /** \brief Retrieve the effective log level of this logger. */
        log_level getEffectiveLevel() const { return effectiveLevel; }

        /** \brief Unconditionally log a message to this logger.
         *
         *  This logs even if the logger is not enabled.
         */
        virtual void log(const char *sourceFilename,
                         int sourceLineNumber,
                         log_level logLevel,
                         const std::string &msg) = 0;

        /** \brief Return the name of this logger's category. */
        virtual const std::string &getCategory() const = 0;

        /** \brief Modify the level that is set for this logger.
         *
         *  Any descendants of this logger will be automatically
         *  updated if necessary.
         *
         *  \warning This function is not thread-safe.  This is a
         *  concession I made for the sake of a simple implementation.
         *  Normally, setLevel will be invoked during initialization,
         *  before any background threads are created.
         *
         *  \param value The new level to configure, or an empty
         *  boost::optional value to clear the configuration of this
         *  logger.
         */
        virtual void setLevel(const boost::optional<log_level> &value) = 0;

        /** \brief Register a slot that is to be invoked when a
         *  message should be logged.
         *
         *  This is one of the places where I compromise efficiency
         *  (maybe) to get a simple implementation and a simple
         *  interface.
         *
         *  The parameters to the slot are:
         *
         *   1. The name of the source file in which the log message
         *      is located.
         *   2. The line number on which the log message is located.
         *   3. The log level of the log message.
         *   4. The logger that generated the message.
         *   5. The log message.
         *
         *  Note that this is the *only* way that messages are logged,
         *  in contrast to many other logging frameworks.
         *
         *  This function is thread-safe (as long as it's safe to copy
         *  the slot, anyway).
         */
        virtual sigc::connection
        connect_message_logged(const sigc::slot<
                                 void,
                                 const char *,
                                 int,
                                 log_level,
                                 LoggerPtr,
                                 std::string> &slot) = 0;

        /** \brief Retrieve a logger for the given category, creating
         * it if necessary.
         *
         *  A category consists of one or more nonempty components
         *  separated by ".", such as "aptitude.util.logging", with
         *  the exception that the root logger is indicated by the
         *  empty string ("").
         *
         *  This function is thread-safe.
         */
        static LoggerPtr getLogger(const std::string &category);
      };

#define LOG_LEVEL(level, logger, msg)                                   \
      do                                                                \
        {                                                               \
          ::aptitude::util::logging::log_level __aptitude_util_logging_level = (level); \
          ::aptitude::util::logging::LoggerPtr __aptitude_util_logging_logger = (logger); \
          if(__aptitude_util_logging_logger->isEnabledFor(__aptitude_util_logging_level)) \
            {                                                           \
              std::ostringstream __aptitude_util_logging_stream;        \
              __aptitude_util_logging_stream << msg;                    \
              (__aptitude_util_logging_logger)->log(__FILE__,           \
                                                    __LINE__,           \
                                                    __aptitude_util_logging_level, \
                                                    __aptitude_util_logging_stream.str()); \
            }                                                           \
        } while(0)                                                      \

#define LOG_TRACE(logger, msg) LOG_LEVEL(::aptitude::util::logging::TRACE_LEVEL, logger, msg)
#define LOG_DEBUG(logger, msg) LOG_LEVEL(::aptitude::util::logging::DEBUG_LEVEL, logger, msg)
#define LOG_INFO(logger, msg) LOG_LEVEL(::aptitude::util::logging::INFO_LEVEL, logger, msg)
#define LOG_WARN(logger, msg) LOG_LEVEL(::aptitude::util::logging::WARN_LEVEL, logger, msg)
#define LOG_ERROR(logger, msg) LOG_LEVEL(::aptitude::util::logging::ERROR_LEVEL, logger, msg)
#define LOG_FATAL(logger, msg) LOG_LEVEL(::aptitude::util::logging::FATAL_LEVEL, logger, msg)

      /** \brief Represents the entire logging system.
       *
       *  This class is exposed primarily for use with tests: to test
       *  the logging system itself, you really need to be able to
       *  create a local one that no-one else will mess with.
       *
       *  That said, this is a representation of an entire logging
       *  system.  It keeps track of which loggers are available and
       *  provides a routine for creating them that's equivalent to
       *  Logger::getLogger.
       */
      class LoggingSystem
      {
        class Impl;
        friend class Logger;

        LoggingSystem();
        LoggingSystem(const LoggingSystem &);

        friend boost::shared_ptr<LoggingSystem> createLoggingSystem();

      public:
        virtual ~LoggingSystem();

        /** \brief Retrieve a logger for the given category.
         *
         *  The returned logger is the only logger for that category
         *  within this logging system, but will be distinct from
         *  loggers created in any other logging system (including the
         *  global one managed by Logger::getLogger).
         */
        virtual LoggerPtr getLogger(const std::string &category) = 0;
      };

      /** \brief Create a new, local logging system. */
      boost::shared_ptr<LoggingSystem> createLoggingSystem();
    }
  }
}

#endif // APTITUDE_UTIL_LOGGING_H
