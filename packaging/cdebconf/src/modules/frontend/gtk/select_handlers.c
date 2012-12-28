/*****************************************************************************
 *
 * cdebconf - An implementation of the Debian Configuration Management
 *            System
 *
 * cdebconf is (c) 2000-2007 Randolph Chung and others under the following
 * license.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *****************************************************************************/

/** @file select_handlers.c
 * select and multiselect question handlers of the GTK+ frontend
 *
 * @see handlers.c
 */

#include "select_handlers.h"

#include <string.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "descriptions.h"
#include "choice_model.h"
#include "ui.h"
#include "align_text_renderer.h"

/** Setter function for the select handler in multiple questions form.
 *
 * @param question question being set
 * @param combo_box handler's combo box widget
 */
static void set_value_from_combo(struct question * question,
                                 GtkComboBox * combo_box)
{
    GtkTreeModel * model;
    GtkTreeIter iter;
    char * value;

    if (gtk_combo_box_get_active_iter(combo_box, &iter)) {
        model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo_box));
        gtk_tree_model_get(model, &iter,
                           /* column: */ CHOICE_MODEL_VALUE, &value,
                           -1 /* end of list */);
        question_setvalue(question, value);
        g_free(value);
    } else {
        question_setvalue(question, "");
    }
}

/** Setter function for the select handler in single question form.
 *
 * @param question question being set
 * @param view handler's tree view
 */
static void set_value_from_select(struct question * question,
                                  GtkTreeView * view)
{
    GtkTreeSelection * selection;
    GtkTreeModel * model;
    GtkTreeIter iter;
    char * value;

    selection = gtk_tree_view_get_selection(view);
    if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
        gtk_tree_model_get(model, &iter,
                           /* column: */ CHOICE_MODEL_VALUE, &value,
                           -1 /* end of list */);
        question_setvalue(question, value);
        g_free(value);
    } else {
        question_setvalue(question, "");
    }
}

/** Determine if a choice is top-level for partman/choose_partition.
 *
 * @param index choice index
 * @param choice choice canonical value
 * @param choice_translated choice translated value
 * @return TRUE if top-level, FALSE otherwise
 */
static gboolean is_disk(int index, const char * choice,
                        const char * choice_translated)
{
    return '>' != choice_translated[0];
}

/** List of predicates used for questions renderered as tree. */
static const struct {
    /** question name */
    const char * tag;
    /** associated predicate */
    parent_predicate parent_predicate;
    /** expand all parents */
    gboolean expand_all;
} special_questions[] = {
    { "partman/choose_partition", is_disk, TRUE },
    { NULL, NULL, FALSE }
};

/** Returns the predicate for a question better rendered as a tree.
 *
 * @param tag question name
 * @return the predicate if found, NULL if there is no specific behaviour
 * @see special_questions
 */
static parent_predicate get_special_predicate(const char * tag) {
    int i;

    for (i = 0; NULL != special_questions[i].tag; i++) {
        if (0 == strcmp(tag, special_questions[i].tag)) {
            return special_questions[i].parent_predicate;
        }
    }
    return NULL;
}

/** Returns TRUE if a question is "special".
 *
 * @see special_question
 */
#define IS_SPECIAL_QUESTION(Question) \
        (NULL != get_special_predicate(Question->tag))

/** Should we expand all nodes for a question better rendered as a tree?
 *
 * @param tag question name
 * @return TRUE if all parents should be expanded, FALSE otherwise
 * @see special_questions
 */
static gboolean should_expand_all(const char * tag) {
    int i;

    for (i = 0; NULL != special_questions[i].tag; i++) {
        if (0 == strcmp(tag, special_questions[i].tag)) {
            return special_questions[i].expand_all;
        }
    }
    return FALSE;
}

/** Hide expanders in the given GtkTreeView.
 *
 * @todo
 * This should be replaced by a gtk_tree_view_set_show_expanders when
 * we switch to GTK+ 2.12.
 *
 * @param view the given GtkTreeView
 */
static void hide_expanders(GtkTreeView * view)
{
    GtkTreeViewColumn * column;

    column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_visible(column, FALSE);
    gtk_tree_view_insert_column(view, column, -1 /* at the end */);
    gtk_tree_view_set_expander_column(view, column);
}

/** Focus the given GtkTreeView to the given GtkTreePath.
 *
 * This will scroll and move the cursor appropriately.
 *
 * This function is required because there is no point in scrolling the widget
 * before it has been realized (= shown on screen) and so we need to do it
 * in a callback connected after the default "expose-event" signal.
 * See #340007 for more details.
 *
 * Once run, it will disconnect itself from the signal and free the given path.
 *
 * @param view the widget where the focus will be set
 * @param path path to the row that will be selected
 * @return FALSE to propage the event further
 */
static gboolean focus_path(GtkTreeView * view, GdkEventExpose * event,
                           GtkTreePath * path)
{
    gtk_tree_view_scroll_to_cell(
        view, path, NULL /* don't scroll to a particular column */,
        TRUE /* please align */, 0.5 /* center row */, 0 /* left column */);
    g_signal_handlers_disconnect_by_func(view, focus_path, path);
    gtk_tree_path_free(path);
    return FALSE; /* propagate the event */
}

/** Private structure for adjust_tabs and adjust_tabs_for_choice. */
struct adjust_data {
    /** Widget where the tabs will be renderered. */
    GtkWidget * widget;
    /** Tab array to update that will be updated. */
    PangoTabArray * tab_array;
};

/** Adjust tab stops for the given node to display all the given values.
 *
 * @param model model being handled
 * @param path path of the current node
 * @param iter iter of the current node
 * @param adjust_data data prepared by adjust_tabs
 * @return always FALSE (= never stop)
 */
static gboolean adjust_tabs_for_node(GtkTreeModel * model, GtkTreePath * path,
                                     GtkTreeIter * iter,
                                     struct adjust_data * adjust_data)
{
    char * choice;
    char ** values;
    gint columns;
    gint value_width;
    gint previous_location;
    gint location;
    gint i;

    gtk_tree_model_get(model, iter,
        /* column: */ CHOICE_MODEL_TRANSLATED_VALUE, &choice,
        -1 /* end of list */);
    values = g_strsplit(choice, "\t", 0 /* split all */);

    columns = g_strv_length(values);
    if (pango_tab_array_get_size(adjust_data->tab_array) < columns - 1) {
        pango_tab_array_resize(adjust_data->tab_array, columns - 1);
    }
    previous_location = 0;
    for (i = 0; columns > i + 1; i++) {
        value_width = cdebconf_gtk_get_text_width(
                          adjust_data->widget, values[i]) + COLUMN_SPACING;
        pango_tab_array_get_tab(adjust_data->tab_array, i,
                                NULL /* don't get alignment */, &location);
        if (location - previous_location < value_width) {
            location = previous_location + value_width;
            pango_tab_array_set_tab(adjust_data->tab_array, i, PANGO_TAB_LEFT,
                                    location);
        }
        previous_location = location;
    }

#if 0
    /* DEBUG: dump tabs */
    g_warning("dump after choice: %s", choice);
    for (i = 0; pango_tab_array_get_size(adjust_data->tab_array) > i; i++) {
        pango_tab_array_get_tab(adjust_data->tab_array, i,
                                NULL /* don't get alignment */,
                                &location);
        g_warning("%d: %d", i, location);
    }
#endif

    g_free(choice);
    g_strfreev(values);

    return FALSE;
}

/** Adjust tab stops in the given tab array to render all the "columns"
 * (separated by a tab) in the translated choice of the given model.
 *
 * @param widget widget where the choices will be rendered
 * @param tab_array tab array to update
 * @param model a choice model
 */
static void adjust_tabs(GtkWidget * widget, PangoTabArray * tab_array,
                        GtkTreeModel * model)
{
    struct adjust_data adjust_data;

    adjust_data.widget = widget;
    adjust_data.tab_array = tab_array;
    gtk_tree_model_foreach(
        model, (GtkTreeModelForeachFunc) adjust_tabs_for_node, &adjust_data);
}

/** Insert the column displaying translated choices in the given GtkTreeView.
 *
 * @param fe frontend
 * @param view column destination
 */
static void insert_choice_column(struct frontend * fe, GtkTreeView * view)
{
    GtkCellRenderer * renderer;
    PangoTabArray * tab_array;

    if (CAN_ALIGN(fe)) {
        /* XXX: check NULL */
        tab_array = pango_tab_array_new(0 /* start with no tabs */,
                                        FALSE /* use pango unit */);
        adjust_tabs(GTK_WIDGET(view), tab_array,
                    gtk_tree_view_get_model(view));
        renderer = cdebconf_gtk_align_text_renderer_new();
        cdebconf_gtk_align_text_renderer_set_tab_array(
            ALIGN_TEXT_RENDERER(renderer), tab_array);
        pango_tab_array_free(tab_array);
    } else {
        renderer = gtk_cell_renderer_text_new();
    }
    gtk_tree_view_insert_column_with_attributes(
        view, -1 /* insert at the end */,
        NULL /* no title */, renderer,
        "text", CHOICE_MODEL_TRANSLATED_VALUE,
        NULL /* end of attribute list */);
}

/** Create widget for select question in single question form.
 *
 * This will also register the corresponding setter function.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box the container for question widgets
 * @param model model for the possible choices
 * @return the created widget, NULL if something went wrong
 * @see focus_path()
 */
static int create_select_list(struct frontend * fe, struct question * question,
                              GtkWidget * question_box, GtkTreeModel * model)
{
    GtkTreePath * path = NULL;
    GtkWidget * view;
    GtkWidget * scroll;
    GtkWidget * frame;

    /* check NULL! */
    view = gtk_tree_view_new_with_model(model);

    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(view),
                                      FALSE /* no headers */);
    gtk_tree_view_set_enable_search(GTK_TREE_VIEW(view),
                                    TRUE /* enable typeahead */);
    gtk_tree_view_set_search_column(GTK_TREE_VIEW(view),
                                    CHOICE_MODEL_TRANSLATED_VALUE);
    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(view)),
        GTK_SELECTION_BROWSE);

    if (!IS_SPECIAL_QUESTION(question)) {
        hide_expanders(GTK_TREE_VIEW(view));
    }

    insert_choice_column(fe, GTK_TREE_VIEW(view));

    g_signal_connect_swapped(G_OBJECT(view), "row-activated",
                             G_CALLBACK(cdebconf_gtk_set_answer_ok), fe);

    if (should_expand_all(question->tag)) {
        gtk_tree_view_expand_all(GTK_TREE_VIEW(view));
    }

    path = cdebconf_gtk_choice_model_get_first_selected(model);
    /* Select first row if there is no other default */
    if (NULL == path) {
        path = gtk_tree_path_new_first();
    } else {
        /* Only expand path when there was a previous selection. */
        gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), path);
    }
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(view), path,
                             NULL /* don't focus a particular column */,
                             FALSE /* don't start editing */);
    /* We need to focus the row *after* the widget realization, see #340007. */
    g_signal_connect_after(view, "expose-event", G_CALLBACK(focus_path), path);
    /* path will be free'd in focus_path() */

    scroll = gtk_scrolled_window_new(NULL /* create horizontal adjustement */,
                                     NULL /* create vertical adjustement */);
    gtk_container_add(GTK_CONTAINER(scroll), view);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    frame = gtk_frame_new(NULL);
    gtk_container_add(GTK_CONTAINER(frame), scroll);

    cdebconf_gtk_add_common_layout(fe, question, question_box, frame);

    gtk_widget_grab_focus(view);

    cdebconf_gtk_register_setter(fe, SETTER_FUNCTION(set_value_from_select),
                                 question, view);

    return DC_OK;
}

/** Update the model when a multiselect option is toggled.
 *
 * @param cell toggled cell
 * @param path_string location of the cell in the model
 * @param model corresponding model
 */
static void update_selection_for_toggle(GtkCellRendererToggle * cell,
                                        const gchar * path_string,
                                        GtkTreeModel * model)
{
    GtkTreeIter iter;
    gboolean value;

    if (gtk_tree_model_get_iter_from_string(model, &iter, path_string)) {
        gtk_tree_model_get(model, &iter,
                           /* column: */ CHOICE_MODEL_SELECTED, &value,
                           -1 /* end of list */);
        value ^= TRUE;
        cdebconf_gtk_choice_model_set(
            model, &iter,
            /* column: */ CHOICE_MODEL_SELECTED, value,
            -1 /* end of list */);
    }
}

/** Setter function for multiselect question.
 *
 * @todo This function won't work if there is more than one level
 *       in the model.
 *
 * @param question question being set
 * @param model choices model
 */
static void set_value_from_multiselect(struct question * question,
                                       GtkTreeModel * model)
{
    GtkTreeIter iter;
    gboolean valid;
    gboolean selected;
    gchar ** selected_values;
    guint selected_index;
    gchar * result;

    /* check NULL! */
    selected_values = g_malloc0(
        sizeof (char *) * (1 /* terminating NULL */ +
                           cdebconf_gtk_choice_model_get_length(model)));
    selected_index = 0;
    valid = gtk_tree_model_get_iter_first(model, &iter);
    while (valid) {
        gtk_tree_model_get(model, &iter,
                           /* column: */ CHOICE_MODEL_SELECTED, &selected,
                           -1 /* end of list */);
        if (selected) {
            gtk_tree_model_get(model, &iter,
                               /* column: */ CHOICE_MODEL_VALUE,
                                             &selected_values[selected_index],
                               -1 /* end of list */);
            selected_index++;
        }
        valid = gtk_tree_model_iter_next(model, &iter);
    }
    if (0 < selected_index) {
        result = g_strjoinv(", ", selected_values);
    } else {
        /* no selections */
        result = g_strdup("");
    }
    question_setvalue(question, result);
    g_free(result);

    for (selected_index = 0; NULL != selected_values[selected_index];
         selected_index++) {
        g_free(selected_values[selected_index]);
    }
    g_free(selected_values);
}

/** Update the model when a toggle button is toggled.
 *
 * @param toggle_button the button which have been toggled
 * @param row_reference associated row in the model
 */
static void update_model_from_toggle_button(
    GtkToggleButton * toggle_button, GtkTreeRowReference * row_reference)
{
    GtkTreeModel * model = gtk_tree_row_reference_get_model(row_reference);
    GtkTreePath * path = gtk_tree_row_reference_get_path(row_reference);
    GtkTreeIter iter;

    g_assert(NULL != model);
    g_assert(NULL != path);

    if (gtk_tree_model_get_iter(model, &iter, path)) {
        gtk_list_store_set(
            GTK_LIST_STORE(model), &iter,
            /* column: */ CHOICE_MODEL_SELECTED,
                          gtk_toggle_button_get_active(toggle_button),
            -1 /* end of list */);
    }
}

/** Connect a signal to an object for a specific row in the model.
 *
 * @param object object which will be connected
 * @param signal signal name
 * @param model model to be associated
 * @param iter location of the row in the model
 * @param handler signal handler
 * @return signal identifier
 */
static gulong connect_signal_to_row(GObject * object, const gchar * signal,
                                    GtkTreeModel * model, GtkTreeIter * iter,
                                    GCallback handler)
{
    GtkTreePath * path;
    GtkTreeRowReference * row_reference;

    path = gtk_tree_model_get_path(model, iter);
    row_reference = gtk_tree_row_reference_new(model, path);
    gtk_tree_path_free(path);
    return g_signal_connect_data(object, "toggled", handler, row_reference,
                                 (GClosureNotify) gtk_tree_row_reference_free,
                                 0 /* no connect options */);
}

/** Handler for the "cursor-changed" signal that forces the cursor to stay on
 * the first column.
 *
 * This will disallow the selection of option descriptions during multiselect.
 *
 * @param view the tree view
 * @param fe cdebconf frontend
 */
static void handle_cursor_changed(GtkTreeView * view,
                                  struct frontend * fe)
{
    GtkTreePath * path;

    gtk_tree_view_get_cursor(view, &path, NULL);
    g_signal_handlers_block_by_func(view, handle_cursor_changed, fe);
    gtk_tree_view_set_cursor(view, path, gtk_tree_view_get_column(view, 0),
                             FALSE /* no editing */);
    g_signal_handlers_unblock_by_func(view, handle_cursor_changed, fe);
}

/** Create widget for multiselect question in single question form.
 *
 * This will also register the corresponding setter function.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for the question widgets
 * @param model model for the possible choices
 * @return the created widget, NULL if something went wrong
 */
static int create_multiselect_list(struct frontend * fe,
                                   struct question * question,
                                   GtkWidget * question_box,
                                   GtkTreeModel * model)
{
    GtkTreeIter iter;
    GtkWidget * view;
    GtkWidget * scroll;
    GtkWidget * frame;
    GtkCellRenderer * toggle_renderer;
    GtkTreePath * path;

    view = gtk_tree_view_new_with_model(model);

    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(view), FALSE);

    toggle_renderer = gtk_cell_renderer_toggle_new();
    g_signal_connect(G_OBJECT(toggle_renderer), "toggled",
                     G_CALLBACK(update_selection_for_toggle), model);
    gtk_tree_view_insert_column_with_attributes(
        GTK_TREE_VIEW(view), -1 /* insert at the end */,
        NULL /* no title */, toggle_renderer,
        "active", CHOICE_MODEL_SELECTED,
        NULL /* end of attribute list */);

    insert_choice_column(fe, GTK_TREE_VIEW(view));

    if (!IS_SPECIAL_QUESTION(question)) {
        hide_expanders(GTK_TREE_VIEW(view));
    }

    g_signal_connect(G_OBJECT(view), "cursor-changed",
                     G_CALLBACK(handle_cursor_changed), fe);

    /* select the first row */
    gtk_tree_model_get_iter_first(model, &iter);
    path = gtk_tree_model_get_path(model, &iter);
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(view), path,
                             NULL /* don't focus on a particular column */,
                             FALSE /* do not start editing */);
    gtk_tree_path_free(path);

    scroll = gtk_scrolled_window_new(NULL /* create horizontal adjustement */,
                                     NULL /* create vertical adjustement */);
    gtk_container_add(GTK_CONTAINER(scroll), view);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    frame = gtk_frame_new(NULL /* no label */);
    gtk_container_add(GTK_CONTAINER(frame), scroll);

    cdebconf_gtk_add_common_layout(fe, question, question_box, frame);
    gtk_widget_grab_focus(view);

    cdebconf_gtk_register_setter(
        fe, SETTER_FUNCTION(set_value_from_multiselect), question, model);
    return DC_OK;
}

/** Set the focus on the first child of the given container.
 *
 * @param container the container where the child will be found
 */
static void focus_first_child(GtkContainer * container)
{
    GList * children;

    children = gtk_container_get_children(container);
    gtk_widget_grab_focus(GTK_WIDGET(children->data));
    g_list_free(children);
}

/** Create a widget for multiselect question in multiple question form.
 *
 * This will also register the corresponding setter function.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for the question widgets
 * @param model model for possible choices
 * @return the created widget, NULL if something went wrong
 */
static int create_multiselect_checkboxes(struct frontend * fe,
                                         struct question * question,
                                         GtkWidget * question_box,
                                         GtkTreeModel * model)
{
    GtkWidget * check_container;
    GtkWidget * check;
    GtkTreeIter iter;
    gboolean valid;
    gchar * label;
    gboolean selected;

    /* XXX: should we return DC_OK? */
    g_assert(0 < cdebconf_gtk_choice_model_get_length(model));

    check_container = gtk_vbox_new(FALSE /* don't make children equal */,
                                   0 /* padding */);

    valid = gtk_tree_model_get_iter_first(model, &iter);
    while (valid) {
        gtk_tree_model_get(model, &iter,
                           /* column: */ CHOICE_MODEL_TRANSLATED_VALUE, &label,
                           /* column: */ CHOICE_MODEL_SELECTED, &selected,
                           -1 /* end of list */);

        check = gtk_check_button_new_with_label(label);
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), selected);

        connect_signal_to_row(G_OBJECT(check), "toggled", model, &iter,
                              G_CALLBACK(update_model_from_toggle_button));

        gtk_box_pack_start(GTK_BOX(check_container), check,
                           FALSE /* don't expand */, FALSE /* don't fill */,
                           0 /* padding */);

        g_free(label);
        valid = gtk_tree_model_iter_next(model, &iter);
    }

    cdebconf_gtk_add_common_layout(fe, question, question_box,
                                   check_container);

    if (cdebconf_gtk_is_first_question(question)) {
        focus_first_child(GTK_CONTAINER(check_container));
    }

    cdebconf_gtk_register_setter(
        fe, SETTER_FUNCTION(set_value_from_multiselect), question, model);

    return DC_OK;
}

/** Activate the selected choice in the combo box.
 *
 * @param combo_box combo box to modify
 * @param model model to lookup
 */
static void set_selected_active(GtkWidget * combo_box, GtkTreeModel * model)
{
    GtkTreePath * path;
    GtkTreeIter iter;
    gboolean valid;

    path = cdebconf_gtk_choice_model_get_first_selected(model);
    if (NULL != path) {
        valid = gtk_tree_model_get_iter(model, &iter, path);
        if (valid) {
            gtk_combo_box_set_active_iter(GTK_COMBO_BOX(combo_box), &iter);
        }
        gtk_tree_path_free(path);
    }
}

/** Create widget for select question in multiple question form.
 *
 * This will also register the corresponding setter function.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for the question widgets
 * @param model model for the possible choices
 * @return the created widget, NULL if something went wrong
 */
static int create_select_combo(struct frontend * fe,
                               struct question * question,
                               GtkWidget * question_box,
                               GtkTreeModel * model)
{
    GtkWidget * combo_box;
    GtkCellRenderer * text_renderer;

    /* XXX: check NULL! */
    combo_box = gtk_combo_box_new_with_model(model);
    /* XXX: check NULL! */
    text_renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo_box), text_renderer,
                               TRUE /* expand */);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combo_box), text_renderer,
                                   "text", CHOICE_MODEL_TRANSLATED_VALUE,
                                   NULL /* end of list */);
    set_selected_active(combo_box, model);

    cdebconf_gtk_add_common_layout(fe, question, question_box, combo_box);

    if (cdebconf_gtk_is_first_question(question)) {
        gtk_widget_grab_focus(combo_box);
    }

    cdebconf_gtk_register_setter(fe, SETTER_FUNCTION(set_value_from_combo),
                                 question, combo_box);

    return DC_OK;
}

/** Handle all possible select and multiselect questions.
 *
 * This will dispatch between the different handlers.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for the question widgets
 * @param multiselect should be TRUE if the question is a multiselect
 * @return DC_NOTOK if something went wrong, DC_OK otherwise
 */
static int handle_all(struct frontend * fe, struct question * question,
                      GtkWidget * question_box, gboolean multiselect)
{
    GtkTreeModel * model;

    model = cdebconf_gtk_choice_model_create_full(
        fe, question, get_special_predicate(question->tag));
    if (NULL == model) {
        g_warning("cdebconf_gtk_choice_model_create_full failed.");
        return DC_NOTOK;
    }

    if (multiselect && IS_QUESTION_SINGLE(question)) {
        return create_multiselect_list(fe, question, question_box, model);
    }
    if (multiselect) {
        return create_multiselect_checkboxes(fe, question, question_box,
                                             model);
    }
    if (IS_QUESTION_SINGLE(question)) {
        return create_select_list(fe, question, question_box, model);
    }
    return create_select_combo(fe, question, question_box, model);
}

/** Handler for select questions.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler widgets
 * @return DC_NOTOK if something went wrong, DC_OK otherwise
 */
int cdebconf_gtk_handle_select(struct frontend * fe,
                               struct question * question,
                               GtkWidget * question_box)
{
    return handle_all(fe, question, question_box, FALSE /* simple select */);
}

/** Handler for multiselect questions.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler widgets
 * @return DC_NOTOK if something went wrong, DC_OK otherwise
 */
int cdebconf_gtk_handle_multiselect(struct frontend * fe,
                                    struct question * question,
                                    GtkWidget * question_box)
{
    return handle_all(fe, question, question_box, TRUE /* multiselect */);
}

/* vim: et sw=4 si
 */
