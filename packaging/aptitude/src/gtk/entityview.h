// -*-c++-*-

// entityview.h
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

#ifndef ENTITYVIEW_H_
#define ENTITYVIEW_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>
#include <generic/util/refcounted_base.h>

#include <cwidget/generic/util/ref_ptr.h>

#include "gui.h" // For PackagesAction.

namespace gui
{
  class EntityView;
  class EntityColumns;

  class Entity : public aptitude::util::refcounted_base_threadsafe
  {
    public:
      virtual ~Entity();

      /** \brief Fill in the contents of a tree-model row for this entity.
       *
       *  The entity should be placed in the 
       *
       *  \param row                 The row to fill in; any existing values
       *                             will be overwritten.
       *  \param pkg                 The package to display in this row.
       *  \param ver                 The version to display in this row.
       *  \param version_specific    The row is version specific (influences
       *                             coloring and selected status display)
       */
      virtual void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row) = 0;

      /** \brief Invoked when the row is double-clicked. */
      virtual void activated(const Gtk::TreeModel::Path &path,
			     const Gtk::TreeViewColumn *column,
			     const EntityView *view) = 0;

      /** \brief Retrieve the set of packages upon which this row depends.
       *
       *  This is the set of packages that should trigger a redraw of this
       *  row when they change.
       *
       *  If this becomes a bottleneck, consider switching to vector or to
       *  returning a single package.
       */
      virtual void add_packages(std::set<pkgCache::PkgIterator> &packages) = 0;

      virtual void add_actions(std::set<PackagesAction> &actions) = 0;

      /** \brief Apply an action to this entity.
       *
       *  \param action       The action to apply.
       *  \param first_pass   True if this is the first pass of dispatches.
       *                      Actions are dispatched to each entity twice
       *                      so that the autoinst code produces better
       *                      results: it works a lot better if you flag
       *                      all the packages being modified before you
       *                      engage it.
       */
      virtual void dispatch_action(PackagesAction action, bool first_pass) = 0;
  };
  typedef cwidget::util::ref_ptr<Entity> EntityRef;

  /** \brief An entity that is responsible for a header row. */
  class HeaderEntity : public Entity
  {
    Glib::ustring text;
  public:
    HeaderEntity(const Glib::ustring &_text) : text(_text) { }

    void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row);
    void add_packages(std::set<pkgCache::PkgIterator> &packages);
    void activated(const Gtk::TreeModel::Path &path,
		   const Gtk::TreeViewColumn *column,
		   const EntityView *view);
    void add_actions(std::set<PackagesAction> &actions);
    void dispatch_action(PackagesAction action, bool first_pass);

    void set_text(const Glib::ustring &_text) { text = _text; }
  };

  // This is a base class. Views can add model columns by deriving it.
  class EntityColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<EntityRef> EntObject;
      Gtk::TreeModelColumn<bool> BgSet;
      Gtk::TreeModelColumn<Glib::ustring> BgColor;
      Gtk::TreeModelColumn<Glib::ustring> CurrentStatusIcon;
      Gtk::TreeModelColumn<Glib::ustring> SelectedStatusIcon;
      /** \brief The markup that's displayed in the "name" column. */
      Gtk::TreeModelColumn<Glib::ustring> NameMarkup;
      /** \brief The markup that's displayed in the "version" column. */
      Gtk::TreeModelColumn<Glib::ustring> VersionMarkup;
      /** \brief The markup that's displayed in the "archive" column. */
      Gtk::TreeModelColumn<Glib::ustring> ArchiveMarkup;
      /** \brief The string that the "name" column should be sorted on. */
      Gtk::TreeModelColumn<Glib::ustring> Name;
      /** \brief The string that the "version" column should be sorted on. */
      Gtk::TreeModelColumn<Glib::ustring> Version;
      /** \brief The string that the "archive" column should be sorted on. */
      Gtk::TreeModelColumn<Glib::ustring> Archive;
      /** \brief The text used to display the name column's "description" tooltip.
       *
       *  \todo I'm tired, so this is just a proof-of-concept; it
       *  should be actual markup, but that requires more work to do
       *  right (read: a description renderer that outputs markup
       *  instead of writing to a TextBuffer).
       */
      Gtk::TreeModelColumn<Glib::ustring> Description;
      /** \brief The markup used to display the status column's "description" tooltip.
       *
       *  \todo  We should build a window showing both icons and the description
       *  of each.
       */
      Gtk::TreeModelColumn<Glib::ustring> StatusDescriptionMarkup;

      /** \brief Indicates whether the entity is automatically installed. */
      Gtk::TreeModelColumn<bool> AutomaticallyInstalled;

      /** \brief Text to display as the tooltip for the automatically installed column. */
      Gtk::TreeModelColumn<Glib::ustring> AutomaticallyInstalledTooltip;

      /** \brief Indicates whether to display the AutomaticallyInstalled renderer. */
      Gtk::TreeModelColumn<bool> AutomaticallyInstalledVisible;

      EntityColumns();
  };

  class EntityTreeView : public Gtk::TreeView
  {
    public:
      EntityTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      bool on_button_press_event(GdkEventButton* event);
      void on_cursor_changed();
      sigc::signal<void, GdkEventButton*> signal_context_menu;
      sigc::signal<void> signal_selection;
      sigc::signal<void> signal_selection_change;
  };

  class EntityView : public aptitude::util::refcounted_base_threadsafe
  {
    private:
      /** \brief Used to let the user control which columns are visible. */
      class EditColumnsDialog;

      EntityTreeView * tree;
      EntityColumns cols;
      EditColumnsDialog *visible_columns_dialog;
      Glib::ustring visible_columns_dialog_parent_title;

      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> revstore;
      void init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                Glib::ustring gladename);

      void on_cache_closed();
      void on_cache_reloaded();

      Gtk::TreeViewColumn * EditColumns;
      Gtk::TreeViewColumn * Status;
      Gtk::TreeViewColumn * AutomaticallyInstalled;
      Gtk::TreeViewColumn * Name;
      Gtk::TreeViewColumn * Version;
      Gtk::TreeViewColumn * Archive;

      /** \brief Sets up generic column properties that don't have to do
       *  with creating the renderer.
       */
      void setup_column_properties(Gtk::TreeViewColumn *treeview_column, int size);

      /** \brief Creates a column with a default renderer. */
      template<class ColumnType>
      int append_column(const Glib::ustring &title, Gtk::TreeViewColumn *&treeview_column,
			Gtk::TreeModelColumn<ColumnType> &model_column, int size);

      /** \brief Creates a column that uses the given model column as
       *  Pango markup.
       */
      int append_markup_column(const Glib::ustring &title, Gtk::TreeViewColumn *&treeview_column,
			       Gtk::TreeModelColumn<Glib::ustring> &model_column, int size);

      /** \brief A support function used to customize sort order in
       *  trees that are sorted by version.
       */
      int compare_rows_by_version(const Gtk::TreeModel::iterator &row1,
				  const Gtk::TreeModel::iterator &row2);

      void context_menu_handler(GdkEventButton * event);

      /** \brief Enforces constraints on column order. */
      bool column_drop_handler(Gtk::TreeView *self, Gtk::TreeViewColumn *column,
			       Gtk::TreeViewColumn *prev_column,
			       Gtk::TreeViewColumn *next_column);
      void row_activated_handler(const Gtk::TreeModel::Path &, Gtk::TreeViewColumn*);

    public:
      /** \brief Construct a new packages view.
       *
       *  \param refGlade    The XML tree containing
       *                     the widgets for this view.
       *  \param gladename   The Glade name of the widget.
       *
       *  \param parent_title  A string by which we should refer to the
       *                       parent widget or location of this view.
       *                       Mainly used to generate the "edit columns"
       *                       dialog box.
       */
      EntityView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                 Glib::ustring gladename,
		 const Glib::ustring &parent_title);
      virtual ~EntityView();

      // TODO: perhaps rebuild_store() should be a virtual function
      // invoked when the global cache is reopened.  This would amount
      // to a design decision that EntityViews are always dependent on
      // the cache state, which I think will always be the case, but
      // I'd like to wait on doing this.  Also, handling reloads
      // properly could involve tab-level coordination anyway.  --
      // dburrows 2008-08-05

      /** \brief Display a dialog that allows the user to modify which
       *  columns are visible in this view.
       */
      void show_edit_columns_dialog();

      /** \brief Set the parent tab title used to display the column editor.
       *
       *  \param parent_title  The title of the parent tab,
       *                       used to inform the user which
       *                       tab is being modified.
       */
      void edit_columns_dialog_parent_title_changed(const Glib::ustring &parent_title);

      void refresh_view(const std::set<pkgCache::PkgIterator> *changed_packages);
      EntityTreeView * get_treeview() const { return tree; };
      const EntityColumns * get_columns() const { return &cols; };
      Gtk::TreeViewColumn *get_status_column() const { return Status; }
      Gtk::TreeViewColumn *get_name_column() const { return Name; }
      Gtk::TreeViewColumn *get_version_column() const { return Version; }
      Gtk::TreeViewColumn *get_archive_column() const { return Archive; }
      Gtk::TreeViewColumn *get_automatically_installed_column() const { return AutomaticallyInstalled; }
      Glib::RefPtr<Gtk::TreeModel> get_model() const { return get_treeview()->get_model(); };
      const std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() const { return &revstore; };
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() { return &revstore; };
      /** \brief Attach a new model to this view.
       *
       *  \param model  The new model.
       *
       *  In addition to invoking set_model() on the tree-view, this
       *  function builds reverse pointers into the model that are
       *  used to refresh it after package states change, and also
       *  sets up column-specific sorting functions (for instance,
       *  sorting versions according to Debian policy).
       */
      void set_model(const Glib::RefPtr<Gtk::TreeModel> &model);


      /** \brief Return the Package menu actions that are currently
       *  allowed on the selection.
       */
      std::set<PackagesAction> get_package_menu_actions() const;

      /** \brief Apply the given action to all the currently selected
       *  packages.
       */
      void apply_action_to_selected(PackagesAction action);

      /** \brief A signal indicating that the allowed package actions
       *  have changed.
       */
      sigc::signal0<void> package_menu_actions_changed;
  };

}

#endif /* ENTITYVIEW_H_ */
