/** \file logging.cc */


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

#include "logging.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/weak_ptr.hpp>

#include <cwidget/generic/threads/threads.h>

#include <iostream>

using boost::enable_shared_from_this;
using boost::make_shared;
using boost::multi_index_container;
using boost::multi_index::const_mem_fun;
using boost::multi_index::hashed_non_unique;
using boost::multi_index::hashed_unique;
using boost::multi_index::indexed_by;
using boost::multi_index::tag;
using boost::optional;
using boost::shared_ptr;
using boost::weak_ptr;
using cwidget::threads::mutex;

namespace aptitude
{
  namespace util
  {
    namespace logging
    {
      const char *describe_log_level(log_level l)
      {
        switch(l)
          {
          case TRACE_LEVEL: return "TRACE";
          case DEBUG_LEVEL: return "DEBUG";
          case INFO_LEVEL: return "INFO";
          case WARN_LEVEL: return "WARN";
          case ERROR_LEVEL: return "ERROR";
          default: return "???";
          }
      }

      class Logger::Impl : public Logger,
                           public enable_shared_from_this<Logger::Impl>
      {
        // If set, the level that has been configured for this
        // logger; otherwise, indicates that no level is configured
        // (the logger will inherit its level from its parent).
        optional<log_level> configuredLevel;

        const std::string category;

        const shared_ptr<Logger::Impl> parent;

        // We carry a weak pointer to the parent logging system, so
        // that we can call back into it even if it's not the global
        // instance.
        const weak_ptr<LoggingSystem::Impl> loggingSystemWeak;

        sigc::signal<void, const char *, int, log_level, LoggerPtr, std::string>
        signal_message_logged;

        static log_level getDefaultLevel() { return ERROR_LEVEL; }

        friend class LoggingSystem;

      public:
        Impl(const std::string &_category,
             const shared_ptr<Logger::Impl> &_parent,
             const shared_ptr<LoggingSystem::Impl> &_loggingSystem);

        void log(const char *sourceFilename,
                 int sourceLineNumber,
                 log_level logLevel,
                 const std::string &msg);

        const std::string &getCategory() const;
        const shared_ptr<Logger::Impl> &getParent() const;

        void setLevel(const optional<log_level> &value);

        sigc::connection
        connect_message_logged(const sigc::slot<
                                 void,
                                 const char *,
                                 int,
                                 log_level,
                                 LoggerPtr,
                                 std::string> &slot);
      };

      Logger::Impl::Impl(const std::string &_category,
                         const shared_ptr<Logger::Impl> &_parent,
                         const shared_ptr<LoggingSystem::Impl> &_loggingSystem)
        : Logger(_parent.get() == NULL
                 ? getDefaultLevel()
                 : _parent->effectiveLevel),
          category(_category),
          parent(_parent),
          loggingSystemWeak(_loggingSystem)
      {
      }

      void Logger::Impl::log(const char *sourceFilename,
                             int sourceLineNumber,
                             log_level logLevel,
                             const std::string &msg)
      {
        // \note shared_from_this() could be costly; it would be more
        // efficient to receive the logger as a parameter, but that
        // would clutter the interface.


        // We emit this log message at each level of the hierarchy,
        // but the logger passed along always refers to where the
        // message started.
        const boost::shared_ptr<Impl> self = shared_from_this();
        boost::shared_ptr<Impl> logger = self;
        while(logger.get() != NULL)
          {
            logger->signal_message_logged(sourceFilename,
                                          sourceLineNumber,
                                          logLevel,
                                          self,
                                          msg);

            logger = logger->parent;
          }
      }

      const std::string &Logger::Impl::getCategory() const
      {
        return category;
      }

      const shared_ptr<Logger::Impl> &Logger::Impl::getParent() const
      {
        return parent;
      }

      LoggingSystem::LoggingSystem()
      {
      }

      LoggingSystem::~LoggingSystem()
      {
      }

      /** \brief Wrapper for the state of the logging system.
       *
       *  A singleton class is used to better encapsulate the state
       *  and to handle lifetime issues properly.
       */
      class LoggingSystem::Impl
        : public LoggingSystem,
          public enable_shared_from_this<LoggingSystem::Impl>
      {
        class by_category_tag;
        class by_parent_tag;

        typedef multi_index_container<
          shared_ptr<Logger::Impl>,
          indexed_by<
            hashed_unique<tag<by_category_tag>,
                          const_mem_fun<Logger::Impl,
                                        const std::string &,
                                        &Logger::Impl::getCategory> >,
            hashed_non_unique<tag<by_parent_tag>,
                              const_mem_fun<Logger::Impl,
                                            const shared_ptr<Logger::Impl> &,
                                            &Logger::Impl::getParent> > > >
        logger_table;

        typedef logger_table::index<by_category_tag>::type by_category_index;
        typedef logger_table::index<by_parent_tag>::type by_parent_index;

        logger_table loggers;

        // Protects access to the table of loggers.  Although
        // setLevel() is not thread-safe, the other public routines
        // are, and some of them need to access the table.
        mutex loggers_mutex;

        typedef by_parent_index::const_iterator child_iterator;

        /** \brief Find the immediate children of the given logger. */
        std::pair<child_iterator, child_iterator>
        find_children(const shared_ptr<Logger::Impl> &parent)
        {
          return loggers.get<by_parent_tag>().equal_range(parent);
        }

        /** \brief Set the effective level of the given logger and all
         *  its descendants, stopping at proper descendants that don't
         *  have a configured level.
         *
         *  \param logger  The starting logger.  This logger's effective
         *                 level is always modified, even if it has a
         *                 configured level.
         *
         *  \param effectiveLevel   The new effective level of logger
         *                          and of its descendants.
         */
        void recursiveSetEffectiveLevel(const shared_ptr<Logger::Impl> &logger,
                                        log_level effectiveLevel)
        {
          logger->effectiveLevel = effectiveLevel;
          std::pair<child_iterator, child_iterator> children =
            find_children(logger);

          for(child_iterator it = children.first; it != children.second; ++it)
            {
              const boost::shared_ptr<Logger::Impl> child = *it;

              if(!child->configuredLevel)
                recursiveSetEffectiveLevel(child, effectiveLevel);
            }
        }

        Impl(const Impl &);

        // Like getLogger, but
        //  1. Returns a Logger::Impl instead of a Logger;
        //  2. Doesn't lock the mutex (so it can be used recursively
        //     from within other routines).
        shared_ptr<Logger::Impl> doGetLogger(const std::string &category);

      public:
        Impl()
        {
        }

        void setLevel(const shared_ptr<Logger::Impl> &category,
                      const optional<log_level> &level);

        LoggerPtr getLogger(const std::string &category);

        void log(const char *sourceFilename,
                 int sourceLineNumber,
                 log_level logLevel,
                 const std::string &msg);

        /** \brief Get the implicit global logging system. */
        static Impl &get()
        {
          // We deliberately leak this to avoid unpleasant surprises.
          // Otherwise, the system would be destroyed when global
          // destructors ran, which has all sorts of nasty
          // possibilities.
          //
          // Needs to be a shared_ptr so enable_shared_from_this()
          // works (which allows loggers to take weak references to
          // the logging system).
          static shared_ptr<Impl> *system =
            new shared_ptr<Impl>(make_shared<Impl>());

          return **system;
        }

        /** \brief Create a new logging system. */
        static shared_ptr<Impl> create()
        {
          return make_shared<Impl>();
        }
      };

      void LoggingSystem::Impl::setLevel(const shared_ptr<Logger::Impl> &category,
                                         const optional<log_level> &level)
      {
        mutex::lock l(loggers_mutex);

        // Update the category itself.
        category->configuredLevel = level;

        // If the configured level is now blank, we need to propagate
        // the effective level from our parent (which is assumed to
        // already be correct).  If we have no parent, we use Logger's
        // default level.
        log_level newEffectiveLevel;

        if(!level)
          {
            const shared_ptr<Logger::Impl> parent = category->getParent();
            if(parent.get() == NULL)
              newEffectiveLevel = Logger::Impl::getDefaultLevel();
            else
              newEffectiveLevel = parent->effectiveLevel;
          }
        else
          newEffectiveLevel = *level;

        recursiveSetEffectiveLevel(category, newEffectiveLevel);
      }

      LoggerPtr LoggingSystem::Impl::getLogger(const std::string &category)
      {
        mutex::lock l(loggers_mutex);

        return doGetLogger(category);
      }

      shared_ptr<Logger::Impl>
      LoggingSystem::Impl::doGetLogger(const std::string &category)
      {
        by_category_index &by_category = loggers.get<by_category_tag>();

        // First, try to find it directly.
        {
          by_category_index::const_iterator found =
            by_category.find(category);

          if(found != by_category.end())
            return *found;
        }

        shared_ptr<Logger::Impl> parent;
        // If they aren't asking for the root logger, find or
        // instantiate the parent.
        if(!category.empty())
          {
            // TODO: make "find the parent category name" a separate
            // static method.
            const std::string::size_type last_dot = category.rfind('.');
            std::string parent_category_name;
            if(last_dot != std::string::npos)
              parent_category_name.assign(category, 0, last_dot);

            parent = doGetLogger(parent_category_name);
          }

        shared_ptr<Logger::Impl> rval =
          make_shared<Logger::Impl>(category, parent, shared_from_this());

        loggers.insert(rval);

        return rval;
      }

      void Logger::Impl::setLevel(const optional<log_level> &value)
      {
        boost::shared_ptr<LoggingSystem::Impl> loggingSystem =
          loggingSystemWeak.lock();

        if(loggingSystem.get() != NULL)
          loggingSystem->setLevel(shared_from_this(), value);
      }

      sigc::connection
      Logger::Impl::connect_message_logged(const sigc::slot<
                                             void,
                                             const char *,
                                             int,
                                             log_level,
                                             LoggerPtr,
                                             std::string> &slot)
      {
        return signal_message_logged.connect(slot);
      }


      Logger::Logger(log_level _effectiveLevel)
        : effectiveLevel(_effectiveLevel)
      {
      }

      Logger::~Logger()
      {
      }

      LoggerPtr Logger::getLogger(const std::string &category)
      {
        return LoggingSystem::Impl::get().getLogger(category);
      }

      boost::shared_ptr<LoggingSystem> createLoggingSystem()
      {
        return LoggingSystem::Impl::create();
      }
    }
  }
}
