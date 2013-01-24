// download.cc
//
//  Copyright 1999-2010 Daniel Burrows
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

#include "download.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/util/util.h>

#include <apt-pkg/strutl.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/acquire-worker.h>

#include "gui.h"
#include "notify.h"

namespace cw = cwidget;

namespace gui
{
  void download_list_model::update_workers(pkgAcquire *Owner)
  {
    // Remove any workers that don't appear in the owner's list of
    // active workers.  First compute the set of workers that we
    // "remember" from before, then remove any workers that are
    // active, and the ones that are left over will be the inactive
    // ones.
    std::set<pkgAcquire::Worker *> inactive_workers;
    for(std::map<pkgAcquire::Worker *, Gtk::TreeModel::iterator>::const_iterator it =
	  worker_map.begin(); it != worker_map.end(); ++it)
      inactive_workers.insert(it->first);

    for(pkgAcquire::Worker *serf = Owner->WorkersBegin();
	serf != NULL; serf = Owner->WorkerStep(serf))
    {
      inactive_workers.erase(serf);

      std::map<pkgAcquire::Worker *, Gtk::TreeModel::iterator>::iterator
	found = worker_map.find(serf);

      if (serf->CurrentItem)
	{
	  pkgAcquire::ItemDesc *Item = serf->CurrentItem;

	  Gtk::TreeModel::iterator iter;
	  if(found == worker_map.end())
	    {
	      iter = download_store->append();
	      worker_map[serf] = iter;
	    }
	  else
	    iter = found->second;

	  Gtk::TreeModel::Row row = *iter;
	  row[download_columns.URI] = Item->URI;
	  row[download_columns.ShortDesc] = Item->ShortDesc;
	  row[download_columns.Description] = Item->Description;
	  row[download_columns.ProgressVisible] = true;

	  // TODO: We should use convertPercent from progress.cc
	  if (serf->TotalSize != 0)
	    row[download_columns.ProgressPerc] = 100 * serf->CurrentSize / serf->TotalSize;
	  else
	    row[download_columns.ProgressPerc] = 0;
	  row[download_columns.Status] = serf->Status;
	}
      else
	{
	  if(found != worker_map.end())
	    {
	      download_store->erase(found->second);
	      worker_map.erase(found);
	    }
	}
    }

    for(std::set<pkgAcquire::Worker *>::const_iterator it =
	  inactive_workers.begin();
	it != inactive_workers.end(); ++it)
      {
	std::map<pkgAcquire::Worker *, Gtk::TreeModel::iterator>::iterator
	  found = worker_map.find(*it);

	if(found == worker_map.end())
	  std::cerr << "Weirdness: a worker that should be in the map isn't." << std::endl;
	else
	  {
	    download_store->erase(found->second);
	    worker_map.erase(found);
	  }
      }
  }

  void download_list_model::finish_item(const pkgAcquire::ItemDesc &Itm, std::string status)
  {
    if(last_not_worker_iter)
      last_not_worker_iter = download_store->insert_after(last_not_worker_iter);
    else
      last_not_worker_iter = download_store->prepend();

    Gtk::TreeModel::iterator item_iter = last_not_worker_iter;

    Gtk::TreeModel::Row row = *item_iter;

    row[download_columns.URI] = Itm.URI;
    row[download_columns.ShortDesc] = Itm.ShortDesc;
    row[download_columns.Description] = Itm.Description;
    row[download_columns.ProgressVisible] = false;
    row[download_columns.ProgressPerc] = 100;
    row[download_columns.Status] = status;
  }

  download_list_model::download_list_model()
    : download_columns(),
      download_store(Gtk::ListStore::create(download_columns)),
      failed(false)
  {
  }

  void download_list_model::connect(download_signal_log *log)
  {
    log->IMSHit_sig.connect(sigc::mem_fun(*this, &download_list_model::IMSHit));
    log->Fetch_sig.connect(sigc::mem_fun(*this, &download_list_model::Fetch));
    log->Done_sig.connect(sigc::mem_fun(*this, &download_list_model::Done));
    log->Fail_sig.connect(sigc::mem_fun(*this, &download_list_model::Fail));
    log->Pulse_sig.connect(sigc::mem_fun(*this, &download_list_model::Pulse));
    log->Start_sig.connect(sigc::mem_fun(*this, &download_list_model::Start));
  }

  void download_list_model::Start(download_signal_log &manager)
  {
  }

  void download_list_model::Stop(download_signal_log &manager)
  {
    for(std::map<pkgAcquire::Worker *, Gtk::TreeModel::iterator>::iterator it =
	  worker_map.begin(); it != worker_map.end(); ++it)
      download_store->erase(it->second);
    worker_map.clear();
  }

  void download_list_model::Pulse(pkgAcquire *Owner,
				  download_signal_log &manager,
				  const sigc::slot1<void, bool> &k)
  {
    update_workers(Owner);
  }

  void download_list_model::Fail(pkgAcquire::ItemDesc &Itm,
				 download_signal_log &log)
  {
    switch(Itm.Owner->Status)
      {
      case pkgAcquire::Item::StatIdle:
	break;

      case pkgAcquire::Item::StatDone:
	finish_item(Itm, _("Ignored"));
	break;

      default:
	// \todo Display an error icon and shade the row.
	failed = true;
	finish_item(Itm, _("Failed"));
	break;
      }
  }

  void download_list_model::IMSHit(pkgAcquire::ItemDesc &Itm,
				   download_signal_log &manager)
  {
    finish_item(Itm, _("Already downloaded"));
  }

  void download_list_model::Fetch(pkgAcquire::ItemDesc &Itm,
				  download_signal_log &manager)
  {
  }

  void download_list_model::Done(pkgAcquire::ItemDesc &Itm,
				 download_signal_log &manager)
  {
    finish_item(Itm, _("Done"));
  }

  DownloadColumns::DownloadColumns()
  {
    add(URI);
    add(Status);
    add(ProgressPerc);
    add(ProgressVisible);
    add(ShortDesc);
    add(Description);
  }

  template <class ColumnType>
  int DownloadTab::append_column(Glib::ustring title,
      Gtk::TreeViewColumn * treeview_column,
      const Gtk::TreeModelColumn<ColumnType>& model_column,
      int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
    treeview_column->set_sort_column(model_column);
    return treeview->append_column(*treeview_column);
  }

  DownloadTab::DownloadTab(const Glib::ustring &label,
			   const cw::util::ref_ptr<download_list_model> &status)
    : Tab(Download, label,
          Gnome::Glade::Xml::create(glade_main_file, "main_download_scrolledwindow"),
          "main_download_scrolledwindow")
  {
    get_xml()->get_widget("main_download_treeview", treeview);

    const Glib::RefPtr<Gtk::TreeModel> download_store = status->get_model();
    const DownloadColumns &download_columns = status->get_columns();

    treeview->set_model(download_store);
    void (Gtk::TreeView::*scroll_to_row)(const Gtk::TreeModel::Path &)
      = &Gtk::TreeView::scroll_to_row;
    download_store->signal_row_inserted().connect(sigc::hide(sigc::mem_fun(*treeview, scroll_to_row)));

    append_column(Glib::ustring(_("Description")), Description, download_columns.Description, 400);

    {
      // Custom renderer to show a percentage progress bar
      Gtk::CellRendererProgress* progress_renderer = Gtk::manage(new Gtk::CellRendererProgress);
      ProgressPerc = manage(new Gtk::TreeViewColumn(_("Progress"), *progress_renderer));
      ProgressPerc->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
      ProgressPerc->set_fixed_width(100);
      ProgressPerc->add_attribute(progress_renderer->property_value(), download_columns.ProgressPerc);
      ProgressPerc->add_attribute(progress_renderer->property_visible(), download_columns.ProgressVisible);
      treeview->append_column(*ProgressPerc);
    }

    append_column(Glib::ustring(_("Status")), Status, download_columns.Status, 150);

    {
      int shortDescIdx = append_column(Glib::ustring(_("Short Description")), ShortDesc, download_columns.ShortDesc, 100);
      Gtk::TreeViewColumn *col = treeview->get_column(shortDescIdx - 1);
      if(col == NULL)
	std::cerr << "Internal error: the column I just added to the download view is missing." << std::endl;
      else
	col->set_visible(false);
    }

    append_column(Glib::ustring(_("URI")), URI, download_columns.URI, 250);

    get_widget()->show();
  }

  namespace
  {
    // This is the main class that's responsible for managing a GUI
    // download.
    class DownloadNotification : public Notification
    {
      cw::util::ref_ptr<download_list_model> status;
      DownloadTab *tab;
      Gtk::ProgressBar *progress;
      const std::string title;
      download_progress_mode progress_mode;
      bool finished;
      bool cancel_ok;
      bool cancelled;
      bool success;
      Gtk::MessageDialog *close_ok_prompt;

      void tab_destroyed()
      {
	tab = NULL;
      }

      void finish_prompt_cancel_ok(int response)
      {
	if(response == Gtk::RESPONSE_YES)
	  {
	    cancel_ok = true;
	    close();
	  }

	if(close_ok_prompt != NULL)
	  close_ok_prompt->hide();
      }

      bool prompt_cancel_ok()
      {
	if(!finished && !cancel_ok)
	  {
	    if(close_ok_prompt == NULL)
	      {
		// \todo We should find the real ultimate parent window of
		// the notification instead of assuming it's pMainWindow.
		close_ok_prompt = new Gtk::MessageDialog(*pMainWindow, _("The download is still in progress.  Should it be canceled?"),
							 false, Gtk::MESSAGE_QUESTION,
							 Gtk::BUTTONS_YES_NO, true);
		close_ok_prompt->signal_response().connect(sigc::mem_fun(*this, &DownloadNotification::finish_prompt_cancel_ok));
	      }

	    close_ok_prompt->show();
	    return false;
	  }
	else
	  return true;
      }

      void cancel()
      {
	cancelled = true;
      }

      void view_details()
      {
	if(tab != NULL)
	  tab->get_widget()->show();
	else
	  {
	    tab = new DownloadTab(title, status);
	    tab->closed.connect(sigc::mem_fun(*this, &DownloadNotification::tab_destroyed));
	    tab_add(tab);
	  }
      }

      void set_progress_text(const std::string &text)
      {
	std::string progress_text;

	if(text.empty())
	  progress_text = title;
	else
	  progress_text = title + ": " + text;

	progress->set_text(progress_text);
	progress->set_tooltip_text(progress_text);
      }

      static void finishMediaChange(int response_id,
				    sigc::slot1<void, bool> k)
      {
	k(response_id == Gtk::RESPONSE_OK);
      }

      void do_pulse()
      {
	progress->pulse();
      }

    public:
      DownloadNotification(const std::string &_title,
			   download_progress_mode _progress_mode,
			   const cw::util::ref_ptr<download_list_model> &_status)
	: Notification(true),
	  status(_status),
	  tab(NULL),
	  progress(new Gtk::ProgressBar),
	  title(_title),
	  progress_mode(_progress_mode),
	  finished(false),
	  cancel_ok(false),
	  cancelled(false),
	  success(true),
	  close_ok_prompt(NULL)
      {
	progress->set_text(title);
	progress->set_pulse_step(0.1);
	progress->set_ellipsize(Pango::ELLIPSIZE_END);
	progress->show();
	prepend_widget(progress);

	Gtk::Button *view_details_button = new Gtk::Button(_("View Details"));
	view_details_button->signal_clicked().connect(sigc::mem_fun(*this, &DownloadNotification::view_details));
	view_details_button->show();
	add_button(view_details_button);

	closing.connect(sigc::mem_fun(*this, &DownloadNotification::prompt_cancel_ok));
	closed.connect(sigc::mem_fun(*this, &DownloadNotification::cancel));

	if(progress_mode == download_progress_pulse)
	  Glib::signal_timeout().connect(sigc::bind_return(sigc::mem_fun(*this, &DownloadNotification::do_pulse),
							   true),
					 100);

	finalize();
	show();
      }

      ~DownloadNotification()
      {
	delete close_ok_prompt;
      }

      void connect(download_signal_log *log)
      {
	log->MediaChange_sig.connect(sigc::mem_fun(*this,
						   &DownloadNotification::MediaChange));

	log->Pulse_sig.connect(sigc::mem_fun(*this,
					     &DownloadNotification::Pulse));

	log->Stop_sig.connect(sigc::mem_fun(*this,
					    &DownloadNotification::Stop));
      }

      void Fail(pkgAcquire::ItemDesc &Itm,
		download_signal_log &log)
      {
	success = false;
      }

      void MediaChange(string media, string drive,
		       download_signal_log &manager,
		       const sigc::slot1<void, bool> &k)
      {
	const Glib::ustring msg = _("Change media");
	Gtk::Dialog dialog(msg, *pMainWindow, true, true);
	Gtk::Label label(ssprintf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
				  media.c_str(), drive.c_str()));
	dialog.get_vbox()->pack_end(label, true, true, 0);
	dialog.add_button(_("Continue"), Gtk::RESPONSE_OK);
	dialog.add_button(_("Abort"), Gtk::RESPONSE_CANCEL);
	dialog.signal_response().connect(sigc::bind(sigc::ptr_fun(&finishMediaChange), k));
	dialog.show();
      }

      void Pulse(pkgAcquire *Owner,
		 download_signal_log &manager,
		 const sigc::slot1<void, bool> &k)
      {
	switch(progress_mode)
	  {
	  case download_progress_pulse:
	    {
	      std::string rate_string;
	      if(manager.get_current_bytes() > 0 || manager.get_current_items() > 0)
		rate_string = ssprintf(_("%s: %sB/s"),
				       title.c_str(),
				       SizeToStr(manager.get_currentCPS()).c_str());
	      else
		rate_string = title + "...";

	      progress->set_text(rate_string);
	    }
	    break;

	  case download_progress_item_count:
	    {
	      double fraction = 0;
	      if(manager.get_total_items() != 0)
		fraction = ((double)manager.get_current_items()) / ((double)manager.get_total_items());

	      progress->set_fraction(fraction);

	      std::string progress_string;
	      if(manager.get_currentCPS() > 0)
		progress_string = ssprintf(_("%s: %lu/%lu, %sB/s"),
					   title.c_str(),
					   manager.get_current_items(),
					   manager.get_total_items(),
					   SizeToStr(manager.get_currentCPS()).c_str());
	      else if(manager.get_current_bytes() > 0)
		progress_string = ssprintf(_("%s: %lu/%lu, stalled"),
					   title.c_str(),
					   manager.get_current_items(),
					   manager.get_total_items());
	      else
		progress_string = title + "...";

	      progress->set_text(progress_string);
	    }
	    break;

	  case download_progress_size:
	    {
	      double fraction = 0;
	      if(manager.get_total_items() != 0)
		fraction = ((double)(manager.get_current_bytes() + manager.get_current_items())) / ((double)(manager.get_total_bytes() + manager.get_total_items()));

	      progress->set_fraction(fraction);

	      std::string progress_string;
	      if(manager.get_currentCPS() > 0)
		progress_string = ssprintf(_("%s: %sB of %sB at %sB/s, %s remaining"),
					   title.c_str(),
					   SizeToStr(manager.get_current_bytes()).c_str(),
					   SizeToStr(manager.get_total_bytes()).c_str(),
					   SizeToStr(manager.get_currentCPS()).c_str(),
					   TimeToStr(((manager.get_total_bytes() - manager.get_current_bytes()) / manager.get_currentCPS())).c_str());
	      else if(manager.get_current_bytes() > 0 || manager.get_current_items() > 0)
		progress_string = ssprintf(_("%s: %sB of %sB, stalled"),
					   title.c_str(),
					   SizeToStr(manager.get_current_bytes()).c_str(),
					   SizeToStr(manager.get_total_bytes()).c_str());
	      else
		progress_string = title + "...";

	      progress->set_text(progress_string);
	    }
	    break;
	  }

	k(!cancelled);
      }

      void Stop(download_signal_log &manager, const sigc::slot0<void> &k)
      {
	// \todo Maybe use some other condition here?
        if(!success)
          // \todo The "error" color is copied around; it should
          // be a constant somewhere.
          set_color(Gdk::Color("#FFE0E0"));

        progress->hide();
        Glib::RefPtr<Gtk::TextBuffer> buffer(Gtk::TextBuffer::create());

        if(success)
          buffer->set_text(title + ": " + _("Completed"));
        else
          buffer->set_text(title + ": " + _("Completed with errors"));
        set_buffer(buffer);

	finished = true;

	k();
      }
    };
  }

  Notification *make_download_notification(const std::string &title,
					   download_progress_mode progress_mode,
					   const cw::util::ref_ptr<download_list_model> &status,
					   download_signal_log *log)
  {
    DownloadNotification *rval = new DownloadNotification(title, progress_mode, status);
    rval->connect(log);
    return rval;
  }
}
