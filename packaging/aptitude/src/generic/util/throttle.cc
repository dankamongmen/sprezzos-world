/** \file throttle.cc */   // -*-c++-*-

#include "throttle.h"

// Local includes:
#include <loggers.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/optional.hpp>

#include <generic/util/util.h>

#include <errno.h>
#include <sys/time.h>

using aptitude::Loggers;
using boost::make_shared;
using boost::optional;
using boost::shared_ptr;
using logging::LoggerPtr;

namespace aptitude
{
  namespace util
  {
    namespace
    {
      class throttle_impl : public throttle
      {
        boost::optional<struct timeval> last_update;

        logging::LoggerPtr logger;

        // Used to ensure that we only warn once about gettimeofday()
        // failing.
        bool wrote_time_error;

        static const double update_interval = 0.7;

        void write_time_error(int errnum);

      public:
        throttle_impl();

        /** \return \b true if the timer has expired. */
        bool update_required();

        /** \brief Reset the timer that controls when the display is
         *  updated.
         */
        void reset_timer();
      };

      const double throttle_impl::update_interval;

      void throttle_impl::write_time_error(int errnum)
      {
        if(!wrote_time_error)
          {
            LOG_ERROR(logger,
                      "gettimeofday() failed: " <<
                      sstrerror(errnum));
            wrote_time_error = true;
          }
      }

      throttle_impl::throttle_impl()
        : logger(Loggers::getAptitudeCmdlineThrottle()),
          wrote_time_error(false)
      {
      }

      bool throttle_impl::update_required()
      {
        if(!last_update)
          return true;
        else
          {
            // Time checking code shamelessly stolen from apt, since
            // we know theirs works.
            struct timeval now;
            if(gettimeofday(&now, 0) != 0)
              {
                write_time_error(errno);
                return false;
              }
            else
              {
                const struct timeval &last_update_time = *last_update;
                double diff =
                  now.tv_sec - last_update_time.tv_sec +
                  (now.tv_usec - last_update_time.tv_usec)/1000000.0;

                bool rval = diff >= update_interval;

                return rval;
              }
          }
      }

      void throttle_impl::reset_timer()
      {
        LOG_TRACE(logger, "Resetting the update timer.");

        struct timeval now;
        if(gettimeofday(&now, 0) != 0)
          write_time_error(errno);
        else
          last_update = now;
      }
    }

    throttle::~throttle()
    {
    }

    shared_ptr<throttle> create_throttle()
    {
      return make_shared<throttle_impl>();
    }
  }
}
