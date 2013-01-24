// -*-c++-*-

// pkgview.h
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

#ifndef PKGVIEW_H_
#define PKGVIEW_H_

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt.h>
#include <generic/apt/matching/pattern.h>
#include <generic/util/refcounted_base.h>

#include <gtk/entityview.h>

#include <cwidget/generic/util/ref_ptr.h>

#include "gui.h" // For entity_state_info.

namespace cwidget
{
  namespace threads
  {
    class thread;
  }
}

class refcounted_progress;

namespace gui
{
  class PkgEntity : public Entity
  {
    private:
      entity_state_info current_state_columns();
      entity_state_info selected_package_state_columns();
      string selected_package_state_color();
      pkgCache::PkgIterator pkg;

    public:
      PkgEntity(const pkgCache::PkgIterator &_pkg) : pkg(_pkg) { }

      void activated(const Gtk::TreeModel::Path &path,
		     const Gtk::TreeViewColumn *column,
		     const EntityView *view);

      /** \brief Fill in the contents of a tree-model row for the given
       *  package/version pair.
       *
       *  \param row                 The row to fill in; any existing values
       *                             will be overwritten.
       *  \param pkg                 The package to display in this row.
       *  \param ver                 The version to display in this row.
       *  \param version_specific    The row is version specific (influences
       *                             coloring and selected status display)
       */
      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row);
      void add_packages(std::set<pkgCache::PkgIterator> &packages);
      void add_actions(std::set<PackagesAction> &actions);
      void dispatch_action(PackagesAction action, bool first_pass);

      pkgCache::PkgIterator get_pkg() { return pkg; }
      /** \brief Get the version, if any, that should be displayed. */
      pkgCache::VerIterator get_ver() const;

      /** \brief Get the version, if any, that would be displayed for the given package. */
      static pkgCache::VerIterator get_ver(const pkgCache::PkgIterator &pkg);
  };

  /** \brief Interface for generating tree-views.
   *
   *  A tree-view generator takes each package that appears in the
   *  current package view and places it into an encapsulated
   *  Gtk::TreeModel.
   */
  class PkgTreeModelGenerator
  {
  public:
    // FIXME: Hack while finding a nonblocking thread join.
    bool finished;
    PkgTreeModelGenerator()
    {
      finished = false;
    }
    virtual ~PkgTreeModelGenerator();

    /** \brief Add the given package and version to this tree view.
     *
     *  \param pkg  The package to add.
     */
    virtual void add(const pkgCache::PkgIterator &pkg) = 0;

    /** \brief Perform actions that need to be taken after adding all
     *  the packages.
     *
     *  For instance, this typically sorts the entire view.
     */
    virtual void finish() = 0;

    /** \brief Retrieve the model associated with this generator.
     *
     *  The model will be filled in as add_package() is invoked.
     *  Normally you should only use the model once it is entirely
     *  filled in (to avoid unnecessary screen updates).
     *
     *  \return  The model built by this generator.
     */
    virtual Glib::RefPtr<Gtk::TreeModel> get_model() = 0;
  };

  /** \brief Base class for views that display a subset of the packages
   *  found in the cache.
   *
   *  \todo The name is lame.
   */
  class PkgViewBase : public EntityView
  {
    cwidget::util::ref_ptr<aptitude::matching::pattern> limit;
    sigc::slot1<PkgTreeModelGenerator *, const EntityColumns *> generatorK;

    /** \brief Discards the contents of the list and stops building
     *  it.
     */
    void do_cache_closed();

    class background_build_store : public sigc::trackable
    {
      /** \brief Used to cancel the background build thread on a
       *  best-effort basis.
       *
       *  Canceling the thread isn't always possible, and it isn't
       *  strictly necessary; it just saves some CPU time to do so.
       */
      class cancel_flag : public aptitude::util::refcounted_base_threadsafe
      {
	cancel_flag()
	  : canceled(false)
	{
	}

	// TODO: is it any better to have a mutex around this?  Seems
	// like that might just introduce tons of locks for no good
	// reason, esp. since this is just a best-effort cancel.  (of
	// course, we could check every 1000 iterations or so...)
	bool canceled;

      public:
	static cwidget::util::ref_ptr<cancel_flag> create()
	{
	  return new cancel_flag;
	}

	bool is_canceled() const
	{
	  return canceled;
	}

	void cancel()
	{
	  canceled = true;
	}
      };

      class build_thread;

      /** \brief The callback for the current build thread.
       *
       *  When the build is canceled we disconnect this slot, so that it
       *  has no effect if it's posted to the mean thread.
       */
      safe_slot1<void, Glib::RefPtr<Gtk::TreeModel> > builder_callback;

      /** \brief Like builder_callback, but for the current progress. */
      safe_slot2<void, int, int> builder_progress_callback;

      /** \brief The cancel flag for the current build thread. */
      cwidget::util::ref_ptr<cancel_flag> builder_cancel;

      /** \brief The set of build threads that are active at this
       *  moment.
       *
       *  These are used when cancel_now() is invoked; they are
       *  maintained in the main thread.
       */
      std::set<cwidget::threads::thread *> active_threads;

      /** \brief The current build thread, if any.
       *
       *  This is set by the main thread when the builder is started,
       *  and nulled when builder_complete() or builder_cancel() is
       *  invoked.  (note, however, that it is not deleted until it's
       *  removed from active_threads, which happens either when it
       *  exits or when it is stopped by cancel_now().
       */
      cwidget::threads::thread *builder;

      /** \brief If the given thread exists in active_threads, delete
       *  it and remove it from that set.
       *
       *  Invoked when a thread exits normally, and when it's stopped
       *  by cancel_now().
       */
      void thread_stopped(cwidget::threads::thread *t);

      /** \brief Tells us how to create a new progres bar for
       *  displaying the build progress.
       */
      sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > builder_progress_k;

      /** \brief Holds a reference to the progress bar of the current
       *  builder, if any.
       *
       *  This is set and cleared in the same cases as builder.
       */
      cwidget::util::ref_ptr<refcounted_progress> builder_progress;

      /** \brief The connection that pulses the main progress bar, if
       *  any.
       */
      sigc::connection pulse_connection;

      bool pulse_progress();

      void progress(int current, int total);

      /** \brief Invoked in the main thread when the store is done being
       *  built in the background.
       *
       *  Not invoked if the rebuild is canceled.
       */
      void rebuild_store_finished(Glib::RefPtr<Gtk::TreeModel> model);

    public:
      background_build_store(const sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > &_builder_progress_k);
      ~background_build_store();

      /** \brief Start rebuilding the store.
       *
       *  Cancels any existing rebuild as a side effect.
       */
      void start(const sigc::slot1<PkgTreeModelGenerator *, const EntityColumns *> &generatorK,
		 const EntityColumns *columns,
		 const cwidget::util::ref_ptr<aptitude::matching::pattern> &limit);

      /** \brief Stop the background list builder.
       *
       *  This actually just disconnects it (so that invoking its slot
       *  has no effect) and then asks it to cancel; it doesn't wait for
       *  the thread to actually stop.
       */
      void cancel();

      /** \brief Stop all outstanding build threads, and wait for them
       *  to finish.
       *
       *  This should be invoked, for instance, when the cache is
       *  being closed.
       */
      void cancel_now();

      /** \brief Signal indicating that the store has been rebuilt.
       */
      sigc::signal<void, Glib::RefPtr<Gtk::TreeModel> > store_rebuilt;
    };

    /** \brief Invoked when the background builder is finished
     *  rebuilding the store.
     *
     *  This attaches the model to the tree-view and signals to
     *  clients that it's ready.
     */
    void store_rebuilt(const Glib::RefPtr<Gtk::TreeModel> &model);

    background_build_store background_builder;

  public:
    sigc::signal<void> store_reloading;
    sigc::signal<void> store_reloaded;
    /** \brief Create a new PkgViewBase.
     *
     *  \param _generatorK   A function that takes the columns of this PkgView and
     *                       returns a PkgTreeModelGenerator object, which is used
     *                       to build the contents of the list displayed in this view.
     *  \param refGlade      The Glade XML tree from which to load this PkgView's contents.
     *  \param gladename     The name of the package view's TreeView in the Glade XML tree.
     *  \param parent_title  A string used to identify this view when the "edit columns"
     *                       dialog box is popped up.
     *  \param limit         The initial limit (or the empty string to ask the user to search).
     */
    PkgViewBase(const sigc::slot1<PkgTreeModelGenerator *, const EntityColumns *> _generatorK,
		const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
		const Glib::ustring &gladename,
		const Glib::ustring &parent_title,
		const Glib::ustring &limit,
		const sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > &build_progress_k);

    virtual ~PkgViewBase();

    /** \brief Rebuild the store using the currently set limit. */
    virtual void rebuild_store();

    /** \brief Change this view's limit to the given pattern. */
    void set_limit(const cwidget::util::ref_ptr<aptitude::matching::pattern> &limit);
  };

  /** \brief A view that displays an unorganized list of packages. */
  class PkgView : public PkgViewBase
  {
  public:
    class Generator : public PkgTreeModelGenerator
    {
      Glib::RefPtr<Gtk::ListStore> store;
      // We need this to set up the sorting at the end.
      const EntityColumns *columns;
    public:
      Generator(const EntityColumns *columns);
      static Generator *create(const EntityColumns *columns);

      void add(const pkgCache::PkgIterator &pkg);
      void finish();
      Glib::RefPtr<Gtk::TreeModel> get_model();
    };

    /** \brief Create a new PkgView.
     *
     *  \param refGlade      The Glade XML tree from which to load this PkgView's contents.
     *  \param gladename     The name of the package view's TreeView in the Glade XML tree.
     *  \param parent_title  A string used to identify this view when the "edit columns"
     *                       dialog box is popped up.
     *  \param limit         The initial limit (or the empty string to ask the user to search).
     */
    PkgView(const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
	    const Glib::ustring &gladename,
	    const Glib::ustring &parent_title,
	    const Glib::ustring &limit,
	    const sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > &build_progress_k);
  };
}

#endif /* PKGVIEW_H_ */
