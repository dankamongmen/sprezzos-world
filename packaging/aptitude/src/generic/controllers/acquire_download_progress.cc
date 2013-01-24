/** \file acquire_download_progress.cc */

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

// Local includes:
#include "acquire_download_progress.h"

#include <generic/apt/download_signal_log.h>
#include <generic/views/download_progress.h>

// System includes:
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/acquire-worker.h>

#include <boost/make_shared.hpp>

#include <sigc++/bind.h>

using boost::make_shared;
using boost::shared_ptr;

namespace aptitude
{
  namespace controllers
  {
    namespace
    {
      template<typename T>
      boost::optional<T> if_not_zero(const T &t)
      {
        if(t == 0)
          return boost::optional<T>();
        else
          return boost::optional<T>(t);
      }
    }

    class acquire_download_progress::impl : public acquire_download_progress,
                                            public sigc::trackable
    {
      friend shared_ptr<impl>
      make_shared<impl>();

      void media_change(const std::string &media,
                        const std::string &drive,
                        download_signal_log &manager,
                        const sigc::slot1<void, bool> &k);

      void ims_hit(pkgAcquire::ItemDesc &item, download_signal_log &manager);
      void fetch(pkgAcquire::ItemDesc &item, download_signal_log &manager);
      void done(pkgAcquire::ItemDesc &item, download_signal_log &manager);
      void fail(pkgAcquire::ItemDesc &item, download_signal_log &manager);
      void start(download_signal_log &manager);
      void stop(download_signal_log &manager, const sigc::slot0<void> &k);
      void complete(download_signal_log &manager);

      void pulse(pkgAcquire *owner, download_signal_log &manager,
                 const sigc::slot1<void, bool> &k);


      // Keeps track of the next ID to assign to a download item.
      unsigned long id;

      shared_ptr<views::download_progress> view;

      shared_ptr<views::download_progress::status>
      get_current_status(pkgAcquire *owner, download_signal_log &manager);

    public:
      impl(download_signal_log *log,
           const shared_ptr<views::download_progress> &_view);
    };

    acquire_download_progress::impl::impl(download_signal_log *log,
                                          const shared_ptr<views::download_progress> &_view)
      : id(1),
        view(_view)
    {
      log->MediaChange_sig.connect(sigc::mem_fun(*this, &impl::media_change));
      log->IMSHit_sig.connect(sigc::mem_fun(*this, &impl::ims_hit));
      log->Fetch_sig.connect(sigc::mem_fun(*this, &impl::fetch));
      log->Done_sig.connect(sigc::mem_fun(*this, &impl::done));
      log->Fail_sig.connect(sigc::mem_fun(*this, &impl::fail));
      log->Pulse_sig.connect(sigc::mem_fun(*this, &impl::pulse));
      log->Start_sig.connect(sigc::mem_fun(*this, &impl::start));
      log->Stop_sig.connect(sigc::mem_fun(*this, &impl::stop));
      log->Complete_sig.connect(sigc::mem_fun(*this, &impl::complete));
    }

    namespace
    {
      // Ensure that the download progress gets an update after a
      // successful media change, so that we have the same behavior as
      // the old download progress object.
      void do_media_change_result(bool result,
                                  download_signal_log &manager,
                                  const sigc::slot1<void, bool> &k)
      {
        if(result)
          manager.set_update(true);

        k(result);
      }
    }

    void acquire_download_progress::impl::media_change(const std::string &media,
                                                       const std::string &drive,
                                                       download_signal_log &manager,
                                                       const sigc::slot1<void, bool> &k)
    {
      view->media_change(media,
                         drive,
                         sigc::bind(sigc::ptr_fun(&do_media_change_result),
                                    manager,
                                    k));
    }

    shared_ptr<views::download_progress::status>
    acquire_download_progress::impl::get_current_status(pkgAcquire *owner,
                                                        download_signal_log &manager)
    {
      typedef views::download_progress::file_progress file_progress;
      typedef views::download_progress::status status;
      typedef status::worker_status worker_status;

      std::vector<worker_status> active_downloads;

      // Read the workers:
      for(pkgAcquire::Worker *it = owner->WorkersBegin(); it != 0;
          it = owner->WorkerStep(it))
        {
          if(it->CurrentItem == NULL)
            {
              if(!it->Status.empty())
                active_downloads.push_back(it->Status);
            }
          else
            {
              file_progress
                current_file(it->CurrentSize,
                             it->TotalSize,
                             it->CurrentItem->Owner->Complete,
                             it->CurrentItem->ShortDesc,
                             if_not_zero(it->CurrentItem->Owner->ID),
                             it->CurrentItem->Owner->Mode == NULL
                               ? ""
                               : it->CurrentItem->Owner->Mode);

              active_downloads.push_back(current_file);
            }
        }

      const unsigned long long download_rate = manager.get_currentCPS();

      const unsigned long long current_bytes = manager.get_current_bytes();
      const unsigned long long total_bytes = manager.get_total_bytes();
      const unsigned long current_items = manager.get_current_items();
      const unsigned long total_items = manager.get_total_items();

      const double fraction_complete =
        ((double) (current_bytes + current_items)) / ((double) (total_bytes + total_items));

      const unsigned long long time_remaining =
        download_rate == 0
          ? 0
          : ((unsigned long long) (total_bytes - current_bytes)) / download_rate;

      return boost::make_shared<status>(manager.get_currentCPS(),
                                        active_downloads,
                                        fraction_complete,
                                        time_remaining);
    }

    void acquire_download_progress::impl::ims_hit(pkgAcquire::ItemDesc &item,
                                                  download_signal_log &manager)
    {
      view->file_already_downloaded(item.Description,
                                    if_not_zero(item.Owner->ID),
                                    if_not_zero(item.Owner->FileSize));

      manager.set_update(true);
    }

    void acquire_download_progress::impl::fetch(pkgAcquire::ItemDesc &item,
                                                download_signal_log &manager)
    {
      if(item.Owner->Complete)
        return;

      item.Owner->ID = id;
      ++id;

      view->file_started(item.Description,
                         if_not_zero(item.Owner->ID),
                         if_not_zero(item.Owner->FileSize));

      manager.set_update(true);
    }

    void acquire_download_progress::impl::done(pkgAcquire::ItemDesc &item, download_signal_log &manager)
    {
      view->file_finished(item.Description,
                          if_not_zero(item.Owner->ID));

      manager.set_update(true);
    }

    void acquire_download_progress::impl::fail(pkgAcquire::ItemDesc &item,
                                               download_signal_log &manager)
    {
      // Ignore certain kinds of transient failures (bad code)
      if (item.Owner->Status == pkgAcquire::Item::StatIdle)
        return;

      view->error(item.Owner->Status == pkgAcquire::Item::StatDone,
                  item.Owner->ErrorText,
                  item.Description,
                  if_not_zero(item.Owner->ID));

      manager.set_update(true);
    }

    void acquire_download_progress::impl::start(download_signal_log &manager)
    {
      id = 1;
    }

    void acquire_download_progress::impl::stop(download_signal_log &manager, const sigc::slot0<void> &k)
    {
      view->done(manager.get_fetched_bytes(),
                 manager.get_elapsed_time(),
                 manager.get_currentCPS());

      k();
    }

    void acquire_download_progress::impl::complete(download_signal_log &manager)
    {
      view->complete(manager.get_fetched_bytes(),
                     manager.get_elapsed_time(),
                     manager.get_currentCPS());
    }

    void acquire_download_progress::impl::pulse(pkgAcquire *owner,
                                                download_signal_log &manager,
                                                const sigc::slot1<void, bool> &k)
    {
      const shared_ptr<views::download_progress::status> status =
        get_current_status(owner, manager);

      manager.set_update(false);

      k(view->update_progress(*status));
    }

    acquire_download_progress::acquire_download_progress()
    {
    }

    acquire_download_progress::~acquire_download_progress()
    {
    }

    shared_ptr<acquire_download_progress>
    create_acquire_download_progress(download_signal_log *log,
                                     const shared_ptr<views::download_progress> &view)
    {
      return make_shared<acquire_download_progress::impl>(log, view);
    }
  }
}

