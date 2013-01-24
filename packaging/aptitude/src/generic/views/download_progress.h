/** \file download_progress.h */    // -*-c++-*-

// Copyright (C) 2010-2011 Daniel Burrows
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

#ifndef APTITUDE_GENERIC_VIEWS_DOWNLOAD_PROGRESS_H
#define APTITUDE_GENERIC_VIEWS_DOWNLOAD_PROGRESS_H

// System includes:
#include <boost/optional.hpp>
#include <boost/variant.hpp>

#include <sigc++/slot.h>

#include <string>
#include <vector>

namespace aptitude
{
  namespace views
  {
    /** \brief Interface for objects that can display the progress of
     *  a download.
     *
     *  Operates at a higher level and more cleanly than AcqProgress.
     *  See src/generic/controllers/acquire_download_progress.h for a
     *  bridge between the two worlds.
     */
    class download_progress
    {
    public:
      virtual ~download_progress();

      /** \brief Represents the download progress of a single file. */
      class file_progress
      {
        unsigned long long current_size;
        unsigned long long total_size;

        bool complete;
        std::string description;
        boost::optional<unsigned long> id;
        std::string mode;

      public:
        file_progress(unsigned long long _current_size,
                      unsigned long long _total_size,
                      bool _complete,
                      const std::string &_description,
                      const boost::optional<unsigned long> &_id,
                      const std::string &_mode)
          : current_size(_current_size),
            total_size(_total_size),

            complete(_complete),
            description(_description),
            id(_id),
            mode(_mode)
        {
        }

        /** \brief Get the number of bytes that have been downloaded. */
        unsigned long long get_current_size() const { return current_size; }

        /** \brief Get the total size of the file. */
        unsigned long long get_total_size() const { return total_size; }

        /** \return \b true if the file has been successfully fetched
         *  according to the download backend.
         */
        bool get_complete() const { return complete; }

        /** \brief Get a brief description of this file. */
        const std::string &get_description() const { return description; }

        /** \brief Get an integer that identifies this item.
         *
         *  May be unset if the item doesn't have an ID yet.
         */
        const boost::optional<unsigned long> &get_id() const { return id; }

        /** \brief Retrieve the current mode string for the file.
         *
         *  If there is no mode, this will be an empty string.
         */
        const std::string &get_mode() const { return mode; }

        bool operator==(const file_progress &other) const;
        bool operator!=(const file_progress &other) const
        {
          return ! (*this == other);
        }
      };

      /** \brief Represents the current progress of a download. */
      class status
      {
      public:
        typedef boost::variant<file_progress, std::string> worker_status;

      private:
        const unsigned long long download_rate;
        const std::vector<worker_status> active_downloads;
        const double fraction_complete;
        const unsigned long long time_remaining;

      public:
        status(const unsigned long long _download_rate,
               const std::vector<worker_status> &_active_downloads,
               const double _fraction_complete,
               const unsigned long long _time_remaining)
          : download_rate(_download_rate),
            active_downloads(_active_downloads),
            fraction_complete(_fraction_complete),
            time_remaining(_time_remaining)
        {
        }

        /** \brief Get the current download speed in bytes per second. */
        unsigned long long get_download_rate() const { return download_rate; }

        /** \brief Get the currently active download processes. */
        const std::vector<worker_status> &get_active_downloads() const
        {
          return active_downloads;
        }

        /** \brief Get the proportional completion of the download (scale
         *  of 0 to 1).
         */
        double get_fraction_complete() const { return fraction_complete; }

        /** \brief Get the estimated number of seconds until the download
         *  completes.
         */
        unsigned long long get_time_remaining() const { return time_remaining; }

        bool operator==(const status &other) const;
        bool operator!=(const status &other) const
        {
          return ! (*this == other);
        }
      };

      /** \brief Update the download progress indicator.
       *
       *  \param current_status    The current status of the download.
       *
       *  \return \b true to continue the download; \b false to abort it.
       */
      virtual bool update_progress(const status &current_status) = 0;

      /** \brief Invoked when a file is starting to be downloaded.
       *
       *  \param description    A brief description of the file.
       *  \param id             An integer identifying this file, or
       *                        unset if it hasn't been assigned yet.
       *  \param file_size      The size of the file; invalid if the
       *                        file size isn't known.
       */
      virtual void file_started(const std::string &description,
                                const boost::optional<unsigned long> &id,
                                const boost::optional<unsigned long long> &file_size) = 0;

      /** \brief Invoked when a file isn't even started because it was
       *  already downloaded.
       *
       *  \param description    A brief description of the file.
       *  \param id             An integer identifying this file, or
       *                        unset if it hasn't been assigned yet.
       *  \param file_size      The size of the file; invalid if the
       *                        file size isn't known.
       */

      virtual void file_already_downloaded(const std::string &description,
                                           const boost::optional<unsigned long> &id,
                                           const boost::optional<unsigned long long> &file_size) = 0;

      /** \brief Invoked when a file fails to download.
       *
       *  \param ignored        True if the file was successfully fetched
       *                        anyway.
       *
       *                        I"m not sure what this means, but it
       *                        matters to existing download UIs.
       *  \param error          A textual description of the error.
       *  \param description    A brief description of the file.
       *  \param id             An integer identifying this file, or
       *                        unset if it hasn't been assigned yet.
       */
      virtual void error(bool ignored,
                         const std::string &error,
                         const std::string &description,
                         const boost::optional<unsigned long> &id) = 0;

      /** \brief Invoked when something is done being downloaded.
       *
       *  \param description    A brief description of the file.
       *  \param id             An integer identifying this file, or
       *                        unset if it hasn't been assigned yet.
       */
      virtual void file_finished(const std::string &description,
                                 const boost::optional<unsigned long> &id) = 0;

      /** \brief Invoked when each stage of the download is complete.
       *
       *  The whole download process might not be done; for instance,
       *  we might need to change to a new CD.  complete() is invoked
       *  after the entire download finishes.
       *
       *  \param fetched_bytes  The number of bytes that were downloaded.
       *
       *  \param elapsed_time   How long (in seconds) the download lasted.
       *
       *  \param latest_download_rate  The final estimated download rate.
       *
       * \todo Should the parameters be incorporated into a status
       * snapshot so that can be used instead?
       */
      virtual void done(unsigned long long fetched_bytes,
                        unsigned long long elapsed_time,
                        unsigned long long latest_download_rate) = 0;

      /** \brief Invoked when the install media should be replaced.
       *
       *  \param media          The label of the media to insert.
       *  \param drive          The name of the drive in which the media
       *                        should be placed.
       *  \param k              A continuation that must be invoked when
       *                        the user has said whether it's OK to
       *                        continue (possibly after media_change has
       *                        returned).  Pass \b true to continue the
       *                        installation, or \b false to abort it.
       */
      virtual void media_change(const std::string &media,
                                const std::string &drive,
                                const sigc::slot1<void, bool> &k) = 0;

      /** \brief Invoked when the whole download finishes.
       *
       *  \param fetched_bytes  The number of bytes that were downloaded.
       *
       *  \param elapsed_time   How long (in seconds) the download lasted.
       *
       *  \param latest_download_rate  The final estimated download rate.
       */
      virtual void complete(unsigned long long fetched_bytes,
                            unsigned long long elapsed_time,
                            unsigned long long latest_download_rate) = 0;
    };

    std::ostream &operator<<(std::ostream &out,
                             const download_progress::file_progress &progress);

    std::ostream &operator<<(std::ostream &out,
                             const download_progress::status &status);
  }
}

#endif // APTITUDE_GENERIC_VIEWS_DOWNLOAD_PROGRESS_H
