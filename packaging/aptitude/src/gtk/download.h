// -*-c++-*-

// download.h
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef DOWNLOAD_H_
#define DOWNLOAD_H_

#undef OK
#include <gtkmm.h>

#include <map>

#include <apt-pkg/acquire.h>

#include <gtk/gui.h>
#include <gtk/tab.h>
#include <generic/apt/download_signal_log.h>
#include <generic/util/refcounted_base.h>

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
  class DownloadTab;

  class DownloadColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<Glib::ustring> URI;
      Gtk::TreeModelColumn<Glib::ustring> Status;
      Gtk::TreeModelColumn<int> ProgressPerc;
      Gtk::TreeModelColumn<bool> ProgressVisible;
      Gtk::TreeModelColumn<Glib::ustring> Description;
      Gtk::TreeModelColumn<Glib::ustring> ShortDesc;

      DownloadColumns();
  };

  /** \brief Uses a GTK+ tree model to display the status of an
   *  Acquire download.
   *
   *  This is a heap-allocated, reference-counted structure so that
   *  situations where the status object doesn't belong to any
   *  particular scope can be handled cleanly and safely.
   */
  class download_list_model : public aptitude::util::refcounted_base_threadsafe
  {
  private:
    DownloadColumns download_columns;
    Glib::RefPtr<Gtk::ListStore> download_store;
    bool failed; // True if an item failed to download.

    Gtk::TreeModel::iterator last_not_worker_iter;

    std::map<pkgAcquire::Worker *, Gtk::TreeModel::iterator> worker_map;
    void update_workers(pkgAcquire *Owner);
    void finish_item(const pkgAcquire::ItemDesc &Itm, string status);

    // Noncopyable.
    download_list_model(const download_list_model &);

    download_list_model();

  public:
    static cwidget::util::ref_ptr<download_list_model> create()
    {
      return new download_list_model;
    }

    /** \brief Convenience routine to connect the signals
     *  from log to appropriate slots in this object.
     */
    void connect(download_signal_log *log);

    void IMSHit(pkgAcquire::ItemDesc &itemdesc, download_signal_log &manager);
    void Fetch(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
    void Done(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
    void Fail(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
    void Pulse(pkgAcquire *Owner, download_signal_log &manager,
	       const sigc::slot1<void, bool> &k);
    void Start(download_signal_log &manager);
    void Stop(download_signal_log &manager);

    const DownloadColumns &get_columns() const { return download_columns; }
    Glib::RefPtr<Gtk::TreeModel> get_model() const { return download_store; }
  };

  class DownloadTab : public Tab
  {
    private:
      Gtk::TreeViewColumn * URI;
      Gtk::TreeViewColumn * Status;
      Gtk::TreeViewColumn * ProgressPerc;
      Gtk::TreeViewColumn * ShortDesc;
      Gtk::TreeViewColumn * Description;

      Gtk::TreeView * treeview;

      template <class ColumnType>
      int append_column(Glib::ustring title,
          Gtk::TreeViewColumn * treeview_column,
          const Gtk::TreeModelColumn<ColumnType>& model_column,
          int size);

    public:
      DownloadTab(const Glib::ustring &label,
		  const cwidget::util::ref_ptr<download_list_model> &status);

      Gtk::TreeView * get_treeview() { return treeview; };
  };

  class Notification;

  /** \brief Create a notification object that displays the current
   *  download status.
   *
   *  The notification lets the user cancel the download, by clicking
   *  the "close" button, or view its status in a new tab.  When the
   *  download is complete, the notification is destroyed.
   *
   *  \param title                 A description of the download.
   *                               For instance, "Downloading packages"
   *
   *  \param progress_mode         What to show with the main progress bar.
   *
   *  \param download_list_model   The GUI status object that
   *                               this notification will track.
   *
   *  \param log                   The go-between to deliver download
   *                               status notifications from the
   *                               background thread.  This pointer
   *                               is owned by the returned notification.
   */
  Notification *make_download_notification(const std::string &title,
					   download_progress_mode progress_mode,
					   const cwidget::util::ref_ptr<download_list_model> &status,
					   download_signal_log *log);
}

#endif /* DOWNLOAD_H_ */
