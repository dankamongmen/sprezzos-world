// entityview.cc
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

#include "entityview.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt_undo_group.h>

#include <gtk/gui.h>
#include <gtk/info.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/ssprintf.h>

#include "treeview_cell_tooltips.h"

namespace gui
{
  Entity::~Entity()
  {
  }

  void HeaderEntity::fill_row(const EntityColumns *cols, Gtk::TreeModel::Row &row)
  {
    row[cols->EntObject] = this;

    row[cols->BgColor] = "light yellow"; // Say we want blue header..
    row[cols->BgSet] = true; // We do want to put color in there, yes.

    row[cols->CurrentStatusIcon] = ""; // dummy
    row[cols->SelectedStatusIcon] = ""; // dummy

    // This is the content of the header
    // TODO: Maybe we should delegate the markup to the caller
    row[cols->NameMarkup] = "<span size=\"large\">" + Glib::Markup::escape_text(text) + "</span>";

    row[cols->VersionMarkup] = ""; // dummy

    row[cols->Name] = text;
    row[cols->Version] = "";
    row[cols->Archive] = "";
  }

  void HeaderEntity::activated(const Gtk::TreeModel::Path &path,
			       const Gtk::TreeViewColumn *column,
			       const EntityView *view)
  {
  }

  void HeaderEntity::add_packages(std::set<pkgCache::PkgIterator> &packages)
  {
  }

  void HeaderEntity::add_actions(std::set<PackagesAction> &actions)
  {
  }

  void HeaderEntity::dispatch_action(PackagesAction action, bool first_pass)
  {
  }

  EntityColumns::EntityColumns()
  {
    add(EntObject);
    add(BgSet);
    add(BgColor);
    add(CurrentStatusIcon);
    add(SelectedStatusIcon);
    add(NameMarkup);
    add(VersionMarkup);
    add(ArchiveMarkup);
    add(Name);
    add(Version);
    add(Archive);
    add(Description);
    add(StatusDescriptionMarkup);
    add(AutomaticallyInstalled);
    add(AutomaticallyInstalledTooltip);
    add(AutomaticallyInstalledVisible);
  }

  EntityTreeView::EntityTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)
  : Gtk::TreeView(cobject)
  {
    ;;
  }

  bool EntityTreeView::on_button_press_event(GdkEventButton* event)
  {
    bool return_value = true;

    if ((event->type == GDK_BUTTON_PRESS) && (event->button == 3))
    {
      Gtk::TreeModel::Path path;
      Gtk::TreeViewColumn *column;
      int cell_x;
      int cell_y;

      if(get_path_at_pos(round(event->x), round(event->y),
                         path, column,
                         cell_x, cell_y))
        {
          // If this row isn't already selected, change the selection
          // to just it.
          Glib::RefPtr<Gtk::TreeView::Selection> selection = get_selection();

          // We could try letting the user expand the selection by
          // holding a shift key down.  I decided not to because I
          // couldn't figure out an obviously right semantics for it,
          // so I figured behaving the same way all the time was the
          // best shot at following the Principle of Least Surprise.
          if(!selection->is_selected(path))
            {
              selection->unselect_all();
              selection->select(path);
            }

          signal_context_menu(event);
        }
    }
    else if ((event->type == GDK_BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the row to be selected by the right-click:
      return_value = Gtk::TreeView::on_button_press_event(event);
      signal_selection();
    }
    else if ((event->type == GDK_2BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the regular signals to be emitted:
      return_value = Gtk::TreeView::on_button_press_event(event);
    }
    return return_value;
  }

  void EntityTreeView::on_cursor_changed()
  {
    // TODO: we might plan to do some more elaborate filtering
    //       based on which tab is being active. If not, we may
    //       as well revert to a simple signal instead of this
    //       virtual function.
    signal_selection_change();
  }

  /** \brief A popup dialog that allows the user to change which of
   *  the columns in a TreeView are currently visible.
   *
   * \todo Maybe handle reordering here too, like Nautilus does.
   *
   * \todo Support applying the "visible columns" settings to all
   * active views.
   *
   * \todo Support checking whether the current selections are equal
   * to the configured default, and change the default if not.  (also
   * save settings when we do?)
   *
   * \todo This should be lifted into its own .h/.cc files.
   */
  class EntityView::EditColumnsDialog : public Gtk::Dialog
  {
    static const Glib::Quark description_property;
    static const Glib::Quark edit_name_property;
    static const Glib::Quark hidden_property;

    static void ustring_destroy_notify(gpointer data)
    {
      Glib::ustring *str =
	static_cast<Glib::ustring *>(data);

      delete str;
    }

  public:
    /** \brief Set the description of a column.
     *
     *  The description will be displayed in the "edit columns" dialog
     *  box.
     *
     *  \param col            The column to modify.
     *  \param description    The new description of the column.
     */
    static void set_description(Gtk::TreeViewColumn *col,
				const Glib::ustring &description)
    {
      col->set_data(description_property, new Glib::ustring(description),
		    &ustring_destroy_notify);
    }

    /** \brief Retrieve the description of a column that was set by
     *  set_description().
     *
     *  \param col  The column whose description should be retrieved.
     *
     *  \return The description of col, if one was set using
     *  set_description(), and the empty string otherwise.
     */
    static Glib::ustring get_description(Gtk::TreeViewColumn *col)
    {
      gpointer rval = col->get_data(description_property);

      if(rval == NULL)
	return Glib::ustring();
      else
	return *static_cast<Glib::ustring *>(rval);
    }


    /** \brief Set the edit-name of a column.
     *
     *  The name will be displayed in the "edit columns" dialog box,
     *  overriding the name of the column that is displayed in its
     *  header.
     *
     *  \param col       The column to modify.
     *  \param edit_name The new edit-name of the column.
     */
    static void set_edit_name(Gtk::TreeViewColumn *col,
			      const Glib::ustring &edit_name)
    {
      col->set_data(edit_name_property, new Glib::ustring(edit_name),
		    &ustring_destroy_notify);
    }

    /** \brief Retrieve the edit-name of a column that was set by
     * set_edit_name().
     *
     *  \param col  The column whose edit-name should be retrieved.
     *
     *  \return The edit-name of col, if one was set using
     *  set_edit_name(), and the empty string otherwise.
     */
    static Glib::ustring get_edit_name(Gtk::TreeViewColumn *col)
    {
      gpointer rval = col->get_data(edit_name_property);

      if(rval == NULL)
	return Glib::ustring();
      else
	return *static_cast<Glib::ustring *>(rval);
    }


    /** \brief Do not add the column to the editor.
     *
     *  The column will not be displayed in the "edit columns" dialog box.
     *
     *  \param col            The column to modify.
     *  \param hidden         true to hide the column from the columns editor, false otherwise.
     */
    static void set_hidden(Gtk::TreeViewColumn *col,
                           bool hidden)
    {
      col->set_data(hidden_property, reinterpret_cast<gpointer>(hidden));
    }

    /** \brief Retrieve the hiding of a column that was set by set_hidden(),
     *  and false otherwise.
     *
     *  \param col  The column whose description should be retrieved.
     *
     *  \return The hiding of col, if one was set using
     *  set_hidden(), and false otherwise.
     */
    static bool get_hidden(Gtk::TreeViewColumn *col)
    {
      gpointer rval = col->get_data(hidden_property);

      if(rval == NULL)
	return false;
      else
	return reinterpret_cast<glong>(rval);
    }

    class ModelColumns : public Gtk::TreeModel::ColumnRecord
    {
    public:
      Gtk::TreeModelColumn<Glib::ustring> name;
      Gtk::TreeModelColumn<Glib::ustring> description;
      Gtk::TreeModelColumn<bool> visible;
      Gtk::TreeModelColumn<Gtk::TreeViewColumn *> column;

      ModelColumns()
      {
	add(name);
	add(description);
	add(visible);
	add(column);
      }
    };

    Gtk::Label *header_label;
    Gtk::TreeView *main_treeview;
    ModelColumns model_columns;
    Glib::RefPtr<Gtk::ListStore> main_treeview_model;

    // Set the column's visibility from the tree model entry's visibility.
    void toggle_visible(const Glib::ustring &path) const
    {
      Gtk::TreeModel::iterator iter(main_treeview->get_model()->get_iter(path));
      if(iter)
	{
	  const Gtk::TreeRow &r(*iter);
	  // The check box hasn't been changed yet, so its current
	  // state is the old value.  When we update the actual
	  // column's visibility, a callback will modify the row's
	  // "visible" flag.
	  const bool old_visible = r[model_columns.visible];
	  const bool new_visible = !old_visible;
	  Gtk::TreeViewColumn * const col = r[model_columns.column];

	  col->property_visible() = new_visible;
	}
    }

    // Set the tree model entry from the column's visibility.
    void update_visible(const Gtk::TreeViewColumn *col, const Gtk::TreeModel::iterator iter) const
    {
      if(iter)
	{
	  Gtk::TreeRow r(*iter);
	  r[model_columns.visible] = col->get_visible();
	}
    }

    void handle_response(int response_id)
    {
      switch(response_id)
	{
	case Gtk::RESPONSE_CLOSE:
	  closed();
	  break;

	default:
	  // Should we complain that something weird happened?
	  break;
	}
    }

  public:
    EditColumnsDialog(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)
      : Gtk::Dialog(cobject),
	header_label(NULL),
	main_treeview(NULL),
	model_columns(),
	main_treeview_model(Gtk::ListStore::create(model_columns))
    {
      refGlade->get_widget("edit_package_columns_label", header_label);
      refGlade->get_widget("edit_package_columns_treeview", main_treeview);

      if(header_label == NULL ||
	 main_treeview == NULL)
	return;

      main_treeview_model->set_sort_column(model_columns.name, Gtk::SORT_ASCENDING);
      main_treeview->set_model(main_treeview_model);

      const int name_idx = main_treeview->append_column(_("Name"), model_columns.name) - 1;
      main_treeview->get_column(name_idx)->set_sort_column(model_columns.name);

      {
	Gtk::TreeViewColumn * const visible_column = manage(new Gtk::TreeViewColumn(_("Visible?")));
	Gtk::CellRendererToggle * const toggle_renderer = manage(new Gtk::CellRendererToggle);
	toggle_renderer->signal_toggled().connect(sigc::mem_fun(*this, &EditColumnsDialog::toggle_visible));
	toggle_renderer->property_activatable() = true;
	toggle_renderer->property_visible() = true;

	visible_column->pack_end(*toggle_renderer);
	visible_column->add_attribute(toggle_renderer->property_active(), model_columns.visible);
	visible_column->set_sort_column(model_columns.visible);
	main_treeview->append_column(*visible_column);
      }

      const int description_idx = main_treeview->append_column(_("Description"), model_columns.description) - 1;
      main_treeview->get_column(description_idx)->set_sort_column(model_columns.description);

      signal_response().connect(sigc::mem_fun(*this, &EditColumnsDialog::handle_response));
    }

    /** \brief Update the "header" label in this window using the
     *  given string as the name of the tab being modified.
     *
     *  \param s   A brief description of the parent tab, such as
     *             "Packages: wesnoth" or "Dashboard".
     */
    void set_parent_title(const std::string &s)
    {
      header_label->set_text(cwidget::util::ssprintf(_("Columns of \"%s\":"), s.c_str()));
      set_title(cwidget::util::ssprintf(_("Editing the columns of \"%s\""), s.c_str()));
    }

    /** \brief Add a tree-view column to the list of columns being
     * edited.
     *
     *  If the column has an empty edit name, this routine does
     *  nothing.
     */
    void add_column_to_list(Gtk::TreeViewColumn * col) const
    {
      if (get_hidden(col))
        return;

      Glib::ustring name = get_edit_name(col);
      if(name.empty())
	name = col->get_title();

      if(name.empty())
	return;

      Gtk::TreeModel::iterator iter(main_treeview_model->append());
      Gtk::TreeRow r(*iter);

      Glib::ustring path(main_treeview_model->get_path(iter).to_string());

      r[model_columns.name] = name;
      r[model_columns.description] = get_description(col);
      r[model_columns.visible] = col->property_visible();
      r[model_columns.column] = col;

      // Whenever the column is hidden or shown, toggle the
      // corresponding entry in the tree.  NB: this relies on the
      // fact that iterators are stable in ListStore!
      col->property_visible().signal_changed()
	.connect(sigc::bind(sigc::mem_fun(*this, &EditColumnsDialog::update_visible),
			    col, iter));
    }

    /** \brief Add all the columns of the given tree-view to the list of columns being edited. */
    void add_columns_to_list(Gtk::TreeView * treeview) const
    {
      Glib::ListHandle<Gtk::TreeViewColumn *> columns(treeview->get_columns());
      for(Glib::ListHandle<Gtk::TreeViewColumn *>::const_iterator it = columns.begin();
	  it != columns.end(); ++it)
	{
	  add_column_to_list(*it);
	}
    }

    sigc::signal0<void> closed;
  };

  const Glib::Quark EntityView::EditColumnsDialog::description_property("aptitude-visible-columns-editor-column-description-property");
  const Glib::Quark EntityView::EditColumnsDialog::edit_name_property("aptitude-visible-columns-editor-column-edit-name-property");
  const Glib::Quark EntityView::EditColumnsDialog::hidden_property("aptitude-visible-columns-editor-column-hidden-property");

  // \todo Perhaps "Edit Columns..." should be available without going
  // through the menu, so it's useful in tabs that have more than one
  // package list.  For instance, we could add an extra,
  // always-visible column with a title of "..." that pops up the
  // dialog when its header is clicked.
  void EntityView::show_edit_columns_dialog()
  {
    if(visible_columns_dialog == NULL)
      {
	Glib::RefPtr<Gnome::Glade::Xml> glade_xml =
	  Gnome::Glade::Xml::create(glade_main_file, "edit_package_columns_dialog");
	glade_xml->get_widget_derived("edit_package_columns_dialog", visible_columns_dialog);
	if(visible_columns_dialog)
	  {
	    Gtk::Container *toplevel_container = tree->get_toplevel();
	    Gtk::Window *toplevel_window = dynamic_cast<Gtk::Window *>(toplevel_container);
	    if(toplevel_window == NULL)
	      // Should never happen, but make sure we have
	      // *something* set as the transient parent.
	      toplevel_window = pMainWindow;
	    visible_columns_dialog->set_transient_for(*toplevel_window);

	    visible_columns_dialog->closed.connect(sigc::mem_fun(*visible_columns_dialog,
								 &Gtk::Widget::hide));
	    visible_columns_dialog->add_columns_to_list(tree);
	    visible_columns_dialog->set_parent_title(visible_columns_dialog_parent_title);
	    visible_columns_dialog->set_accept_focus(true);
	  }
      }

    if(visible_columns_dialog == NULL)
      _error->Error("Unable to create the edit columns dialog.");
    else
      visible_columns_dialog->show();
  }

  void EntityView::edit_columns_dialog_parent_title_changed(const Glib::ustring &parent_title)
  {
    visible_columns_dialog_parent_title = parent_title;
    if(visible_columns_dialog != NULL)
      visible_columns_dialog->set_parent_title(parent_title);
  }

  void EntityView::init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                        Glib::ustring gladename)
  {
    refGlade->get_widget_derived(gladename, tree);

    tree->signal_context_menu.connect(sigc::mem_fun(*this, &EntityView::context_menu_handler));
    tree->signal_row_activated().connect(sigc::mem_fun(*this, &EntityView::row_activated_handler));
    tree->signal_selection_change.connect(package_menu_actions_changed.make_slot());
    tree->set_column_drag_function(sigc::mem_fun(*this, &EntityView::column_drop_handler));
    if(apt_cache_file != NULL)
      {
	(*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView::refresh_view));
	(*apt_cache_file)->package_states_changed.connect(sigc::hide(package_menu_actions_changed.make_slot()));
      }
    cache_closed.connect(sigc::mem_fun(*this, &EntityView::on_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &EntityView::on_cache_reloaded));

    {
      EditColumns = manage(new Gtk::TreeViewColumn("..."));
      EditColumns->signal_clicked().connect(sigc::mem_fun(*this, &EntityView::show_edit_columns_dialog));
      EditColumns->set_clickable(true);
      // Don't let the properties of this column be edited.
      EditColumnsDialog::set_hidden(EditColumns, true);
      tree->append_column(*EditColumns);
    }

    {
      // \todo should the selected status icon have a dropdown menu?
      // And how best to achieve that?
      Gtk::CellRendererPixbuf *current_status_icon_renderer = manage(new Gtk::CellRendererPixbuf);
      Gtk::CellRendererPixbuf *selected_status_icon_renderer = manage(new Gtk::CellRendererPixbuf);
      Status = manage(new Gtk::TreeViewColumn("", *current_status_icon_renderer));
      Status->pack_end(*selected_status_icon_renderer);

      Status->add_attribute(current_status_icon_renderer->property_stock_id(),
				   cols.CurrentStatusIcon);

      Status->add_attribute(selected_status_icon_renderer->property_stock_id(),
				   cols.SelectedStatusIcon);
      EditColumnsDialog::set_edit_name(Status, _("Status"));
      EditColumnsDialog::set_description(Status, _("Icons showing the current and future status of this package."));

      setup_column_properties(Status, -1);
      // Needs to be GROW_ONLY because otherwise it gets clipped in
      // the preview display.
      tree->append_column(*Status);
    }
    set_markup_tooltip(tree, Status, cols.StatusDescriptionMarkup);

    {
      Gtk::CellRendererToggle *automatically_installed_renderer = manage(new Gtk::CellRendererToggle);
      automatically_installed_renderer->property_activatable() = false;
      AutomaticallyInstalled = manage(new Gtk::TreeViewColumn(_("Auto"), *automatically_installed_renderer));
      AutomaticallyInstalled->add_attribute(automatically_installed_renderer->property_active(),
					    cols.AutomaticallyInstalled);
      AutomaticallyInstalled->add_attribute(automatically_installed_renderer->property_visible(),
					    cols.AutomaticallyInstalledVisible);
      setup_column_properties(AutomaticallyInstalled, 48);
      EditColumnsDialog::set_description(AutomaticallyInstalled, _("Whether the package is automatically installed."));
      tree->append_column(*AutomaticallyInstalled);
      set_text_tooltip(tree, AutomaticallyInstalled, cols.AutomaticallyInstalledTooltip);
    }

    append_markup_column(Glib::ustring(_("Name")), Name, cols.NameMarkup, 350);
    EditColumnsDialog::set_description(Name, _("The name and description of the package."));
    set_text_tooltip(tree, Name, cols.Description);
    {
      Gtk::CellRenderer *renderer = tree->get_column_cell_renderer(tree->get_columns().size() - 1);
      if(renderer == NULL)
        std::cerr << "Why don't I have a renderer when I just added one?" << std::endl;
      else
        {
          Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
          if(renderer_text == NULL)
            std::cerr << "Why don't I have a text renderer when I just added one?" << std::endl;
          else
            renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
        }
    }
    append_markup_column(Glib::ustring(_("Version")), Version, cols.VersionMarkup, 80);
    EditColumnsDialog::set_description(Version, _("The version number of the package."));
    {
      Gtk::CellRenderer *renderer = tree->get_column_cell_renderer(tree->get_columns().size() - 1);
      if(renderer == NULL)
        std::cerr << "Why don't I have a renderer when I just added one?" << std::endl;
      else
        {
          Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
          if(renderer_text == NULL)
            std::cerr << "Why don't I have a text renderer when I just added one?" << std::endl;
          else
            renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
        }
    }
    append_markup_column(Glib::ustring(_("Archive")), Archive, cols.ArchiveMarkup, 80);
    EditColumnsDialog::set_description(Archive, _("The package archives that contain this version."));
    {
      Gtk::CellRenderer *renderer = tree->get_column_cell_renderer(tree->get_columns().size() - 1);
      if(renderer == NULL)
        std::cerr << "Why don't I have a renderer when I just added one?" << std::endl;
      else
        {
          Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
          if(renderer_text == NULL)
            std::cerr << "Why don't I have a text renderer when I just added one?" << std::endl;
          else
            renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
        }
    }

    tree->set_search_column(cols.Name);

    // TODO: There should be a way to do this in Glade maybe.
    tree->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  void EntityView::on_cache_closed()
  {
    // TODO: throw away all the model rows here?
  }

  void EntityView::on_cache_reloaded()
  {
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView::refresh_view));
    // TODO: we should rebuild the display, but we can't do that
    // without more information about what we were displaying.  Maybe
    // we should just rely on the tab to trigger a rebuild.
  }

  void EntityView::setup_column_properties(Gtk::TreeViewColumn *treeview_column,
                                             int size)
  {
    Glib::ListHandle<Gtk::CellRenderer *> renderers = treeview_column->get_cell_renderers();
    for(Glib::ListHandle<Gtk::CellRenderer *>::const_iterator it  =
	  renderers.begin(); it != renderers.end(); ++it)
      {
	treeview_column->add_attribute((*it)->property_cell_background_set(), cols.BgSet);
	treeview_column->add_attribute((*it)->property_cell_background(), cols.BgColor);
      }
    if (size < 0)
      treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_AUTOSIZE);
    else
      {
        treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
        treeview_column->set_fixed_width(size);
        treeview_column->set_resizable(true);
      }
    treeview_column->set_reorderable(true);
  }

  template <class ColumnType>
  int EntityView::append_column(const Glib::ustring &title,
                                  Gtk::TreeViewColumn *&treeview_column,
                                  Gtk::TreeModelColumn<ColumnType> &model_column,
                                  int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    setup_column_properties(treeview_column, size);
    treeview_column->set_sort_column(model_column);
    return tree->append_column(*treeview_column);
  }

  int EntityView::append_markup_column(const Glib::ustring &title,
				       Gtk::TreeViewColumn *&treeview_column,
				       Gtk::TreeModelColumn<Glib::ustring> &model_column,
				       int size)
  {
    Gtk::CellRendererText *renderer = manage(new Gtk::CellRendererText);
    treeview_column = manage(new Gtk::TreeViewColumn(title, *renderer));
    treeview_column->add_attribute(renderer->property_markup(),
                                   model_column);
    // TODO: this will work for now, but ideally we would have a
    // second, un-marked-up column that we use to sort.
    treeview_column->set_sort_column(model_column);
    setup_column_properties(treeview_column, size);
    return tree->append_column(*treeview_column);
  }

  int EntityView::compare_rows_by_version(const Gtk::TreeModel::iterator &row1,
					  const Gtk::TreeModel::iterator &row2)
  {
    Glib::ustring version1 = (*row1)[cols.Version];
    Glib::ustring version2 = (*row2)[cols.Version];

    // Defensiveness: apt returns <, =, or > 0, but glibmm says we
    // should return exactly -1, 0, or 1.
    int apt_compare_result = _system->VS->CmpVersion(version1, version2);

    if(apt_compare_result < 0)
      return -1;
    else if(apt_compare_result > 0)
      return 1;
    else
      return 0;
  }

  void EntityView::refresh_view(const std::set<pkgCache::PkgIterator> *changed_packages)
  {
    for(std::set<pkgCache::PkgIterator>::iterator pkg = changed_packages->begin(); pkg != changed_packages->end(); pkg++)
      {
        std::pair<std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator,
        std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator> reverse_range =
	  get_reverse_store()->equal_range(*pkg);

        for (std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator reverse_iter =
          reverse_range.first;
        reverse_iter != reverse_range.second; reverse_iter++)
          {
            Gtk::TreeModel::iterator iter = reverse_iter->second;
            Gtk::TreeModel::Row row = *iter;
	    cwidget::util::ref_ptr<Entity> ent = row[get_columns()->EntObject];
	    ent->fill_row(get_columns(), row);
          }
      }
  }

  void EntityView::apply_action_to_selected(PackagesAction action)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = get_treeview()->get_selection();
    if(refSelection)
    {
      Gtk::TreeSelection::ListHandle_Path path_list = refSelection->get_selected_rows();
      std::list<Gtk::TreeModel::iterator> iter_list;
      for (Gtk::TreeSelection::ListHandle_Path::iterator path = path_list.begin();
        path != path_list.end(); path++)
      {
        iter_list.push_back(model->get_iter(*path));
      }

      std::auto_ptr<undo_group> undo(new undo_group);
      {
	aptitudeDepCache::action_group group(*apt_cache_file, undo.get());
	for(int pass = 0; pass < 2; ++pass)
	  {
	    for(std::list<Gtk::TreeModel::iterator>::const_iterator iter =
		  iter_list.begin(); iter != iter_list.end(); ++iter)
	      {
		cwidget::util::ref_ptr<Entity> ent = (**iter)[cols.EntObject];
		ent->dispatch_action(action, pass == 0);
	      }
	  }
      }
      if(!undo.get()->empty())
	apt_undos->add_item(undo.release());
    }
  }

  EntityView::EntityView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
			 Glib::ustring gladename,
			 const Glib::ustring &parent_title)
    : visible_columns_dialog(NULL),
      visible_columns_dialog_parent_title(parent_title)
  {
    init(refGlade, gladename);
  }

  void EntityView::context_menu_handler(GdkEventButton * event)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> selected = tree->get_selection();
    if(selected)
      {
        std::set<PackagesAction> actions;

        Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
        for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
             path != selected_rows.end(); ++path)
          {
            Gtk::TreeModel::iterator iter = model->get_iter(*path);
	    cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
            ent->add_actions(actions);
          }

        if(!actions.empty())
          {
	    Gtk::Menu *menu(manage(new Gtk::Menu));
            fill_package_menu(actions, sigc::mem_fun(this, &EntityView::apply_action_to_selected), menu);
	    menu->popup(event->button, event->time);
          }
      }
  }

  std::set<PackagesAction> EntityView::get_package_menu_actions() const
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> selected = tree->get_selection();
    std::set<PackagesAction> actions;

    if(selected)
      {
        Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
        for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
             path != selected_rows.end(); ++path)
          {
            Gtk::TreeModel::iterator iter = model->get_iter(*path);
            cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
            ent->add_actions(actions);
          }
      }

    return actions;
  }

  EntityView::~EntityView()
  {
    delete visible_columns_dialog;
    visible_columns_dialog = NULL;
  }

  bool EntityView::column_drop_handler(Gtk::TreeView *self, Gtk::TreeViewColumn *column,
				       Gtk::TreeViewColumn *prev_column,
				       Gtk::TreeViewColumn *next_column)
  {
    // Ensure that the expander column is always first.
    if(column == EditColumns || next_column == EditColumns)
      return false;

    return true;
  }

  void EntityView::row_activated_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      Gtk::TreeModel::iterator iter = get_model()->get_iter(path);
      cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
      ent->activated(path, column, this);
  }

  namespace
  {
    bool post_process_model(const Gtk::TreeModel::iterator &iter,
			    const Glib::RefPtr<Gtk::TreeModel> &model,
			    const EntityColumns *columns,
			    std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *revstore)
    {
      std::set<pkgCache::PkgIterator> packages;

      const Gtk::TreeModel::Row &row = *iter;

      cwidget::util::ref_ptr<Entity> ent = row[columns->EntObject];
      ent->add_packages(packages);

      for(std::set<pkgCache::PkgIterator>::const_iterator it = packages.begin();
	  it != packages.end(); ++it)
	revstore->insert(std::pair<pkgCache::PkgIterator, Gtk::TreeModel::iterator>(*it, iter));

      return false;
    }
  }

  void EntityView::set_model(const Glib::RefPtr<Gtk::TreeModel> &model)
  {
    Glib::RefPtr<Gtk::TreeSortable> sortable = Glib::RefPtr<Gtk::TreeSortable>::cast_dynamic<Gtk::TreeModel>(model);
    sortable->set_sort_func(get_columns()->Version,
			    sigc::mem_fun(this, &EntityView::compare_rows_by_version));

    revstore.clear();
    model->foreach_iter(sigc::bind(sigc::ptr_fun(&post_process_model),
				   model,
				   get_columns(),
				   get_reverse_store()));
    get_treeview()->set_model(model);
  }
}
