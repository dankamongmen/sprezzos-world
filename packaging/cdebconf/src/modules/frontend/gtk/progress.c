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

/** @file progress.c
 * progress bar support for the GTK+ frontend of cdebconf
 */

#include "progress.h"

#include <string.h>
#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "frontend.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "ui.h"
#ifdef DI_UDEB
# include "di.h"
#endif /* DI_UDEB */

/** Horizontal padding of the progress bar */
#define PROGRESSBAR_HPADDING 60
/** Vertical padding of the progress bar */
#define PROGRESSBAR_VPADDING 60

/** Internal data for progress bar support.
 *
 * @see frontend_data#progress_data
 */
struct progress_data {
    /** cdebconf frontend */
    struct frontend * fe;

    /** progress bar widget */
    GtkWidget * progress_bar;

    /** label to display progress info */
    GtkWidget * progress_label;

    /** container for progress bar and progress info */
    GtkWidget * progress_box;

    /** cancel button
     *
     * @see frontend_data#action_box
     */
    GtkWidget * cancel_button;

    /** store title in order to restore it after a GO inside PROGRESS */
    gchar * fe_title;
};

/** Create the progress bar widget in the given container.
 *
 * @param progress_data progress data
 * @param container the container where will be added the progress bar
 */
static void create_progress_bar(struct progress_data * progress_data,
                                GtkWidget * container)
{
    GtkWidget * progress_bar;

    /* XXX: check NULL! */
    progress_bar = gtk_progress_bar_new();

    gtk_progress_bar_set_ellipsize(GTK_PROGRESS_BAR(progress_bar),
                                   PANGO_ELLIPSIZE_MIDDLE);

    gtk_box_pack_start(GTK_BOX(container), progress_bar,
                       FALSE /* don't expand */,
                       FALSE /* don't fill */, 0 /* padding */);

    g_object_ref(G_OBJECT(progress_bar));
    progress_data->progress_bar = progress_bar;
}

/** Unference the progress bar.
 *
 * The container has to take care of destroying the widget.
 *
 * @param progress_data progress data
 */
static void destroy_progress_bar(struct progress_data * progress_data)
{
    GtkWidget * progress_bar = progress_data->progress_bar;

    if (NULL != progress_bar) {
        progress_data->progress_bar = NULL;
        g_object_unref(progress_bar);
        /* widget will be destroyed by destroy_progress_box */
    }
}

/** Create the label widget in the given container.
 *
 * @param progress_data progress data
 * @param container the container where the label will be added
 */
static void create_progress_label(struct progress_data * progress_data,
                                  GtkWidget * container)
{
    struct frontend_data * fe_data = progress_data->fe->data;
    GtkStyle * style;
    GtkWidget * progress_label;
    PangoFontDescription * font_desc;

    /* XXX: check null! */
    progress_label = gtk_entry_new();

    style = gtk_widget_get_style(fe_data->window);
    gtk_widget_modify_base(progress_label, GTK_STATE_NORMAL, style->bg);
    gtk_editable_set_editable(GTK_EDITABLE(progress_label), FALSE);
    gtk_entry_set_has_frame(GTK_ENTRY(progress_label), FALSE);
    gtk_widget_set_can_focus(GTK_WIDGET(progress_label), FALSE);

    /* XXX: check null! */
    font_desc = pango_font_description_new();
    pango_font_description_set_style(font_desc, PANGO_STYLE_ITALIC);
    gtk_widget_modify_font(progress_label, font_desc);
    pango_font_description_free(font_desc);

    gtk_box_pack_start(GTK_BOX(container), progress_label,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       DEFAULT_PADDING);

    g_object_ref(G_OBJECT(progress_label));
    progress_data->progress_label = progress_label;
}

/** Unreference the label.
 *
 * The container has to take care of destroying the widget.
 *
 * @param progress_data progress data
 */
static void destroy_progress_label(struct progress_data * progress_data)
{
    GtkWidget * progress_label = progress_data->progress_label;

    if (NULL != progress_label) {
        progress_data->progress_label = NULL;
        g_object_unref(progress_label);
        /* widget will be destroyed by destroy_progress_box */
    }
}

/** Create the container including the progress bar and the associated
 * information label.
 *
 * @param progress_data progress data
 */
static void create_progress_box(struct progress_data * progress_data)
{
    GtkWidget * progress_box;

    /* check NULL! */
    progress_box = gtk_vbox_new(FALSE /* don't make children equal */,
                                0 /* padding */);

    create_progress_bar(progress_data, progress_box);
    create_progress_label(progress_data, progress_box);

    cdebconf_gtk_center_widget(&progress_box, PROGRESSBAR_HPADDING,
                               PROGRESSBAR_VPADDING);

    g_object_ref(G_OBJECT(progress_box));
    progress_data->progress_box = progress_box;
}

/** Unreference the container with the progress bar and the associated label.
 *
 * This will take care of destroying the label and the progress bar widgets.
 *
 * @param progress_data progress data
 */
static void destroy_progress_box(struct progress_data * progress_data)
{
    GtkWidget * progress_box = progress_data->progress_box;

    if (NULL != progress_box) {
        progress_data->progress_box = NULL;
        g_object_unref(progress_box);
        gtk_widget_destroy(progress_box);
    }
    destroy_progress_label(progress_data);
    destroy_progress_bar(progress_data);
}

/** Show the progress widgets.
 *
 * This will actually add the widgets to the corresponding containers.
 * The main title saved when starting the PROGRESS operation will be restored
 * from the value saved when START was called.  This is needed when GO is
 * called during a PROGRESS operation.
 *
 * @param fe cdebconf frontend
 * @see cdebconf_gtk_hide_progress
 */
void cdebconf_gtk_show_progress(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data = fe_data->progress_data;

    g_assert(NULL != progress_data);
    if (NULL == gtk_widget_get_parent(progress_data->progress_box)) {
        gtk_box_pack_start(
            GTK_BOX(fe_data->target_box), progress_data->progress_box,
            FALSE /* don't expand */, FALSE /* don't fill */, DEFAULT_PADDING);
    }
    if (NULL != progress_data->cancel_button &&
        NULL == gtk_widget_get_parent(progress_data->cancel_button)) {
        gtk_box_pack_start(
            GTK_BOX(fe_data->action_box), progress_data->cancel_button,
            TRUE /* expand */, TRUE /* fill */, DEFAULT_PADDING);
    }
    /* restore main title */
    g_free(fe->title);
    fe->title = g_strdup(progress_data->fe_title);
    cdebconf_gtk_update_frontend_title(fe);
    gtk_widget_show_all(progress_data->progress_box);
    gtk_widget_show_all(fe_data->action_box);
}

/** Hide the progress widgets.
 *
 * This will actually remove the widgets from their respective containers
 * in order to leave the room for GO handling.
 *
 * @param fe cdebconf frotend
 * @see cdebconf_gtk_show_progress
 */
void cdebconf_gtk_hide_progress(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data = fe_data->progress_data;

    if (NULL != progress_data) {
        if (NULL != progress_data->progress_box) {
            gtk_container_remove(GTK_CONTAINER(fe_data->target_box),
                                 progress_data->progress_box);
        }
        if (NULL != progress_data->cancel_button) {
            gtk_container_remove(GTK_CONTAINER(fe_data->action_box),
                                 progress_data->cancel_button);
        }
    }
}

/** Key event handler implementing the "Cancel" key shortcut.
 *
 * @param widget main window
 * @param key the pressed key
 * @param fe cdebconf frontend
 * @return TRUE if "Cancel" was handled, FALSE otherwise
 */
static gboolean handle_cancel_key(GtkWidget * widget, GdkEventKey * key,
                                  struct frontend * fe)
{
    if (GDK_KEY_Escape == key->keyval) {
        cdebconf_gtk_set_answer_goback(fe);
        return TRUE;
    }
    return FALSE;
}

/** Handler for the "clicked" signal on cancel button.
 *
 * @param widget button
 * @param fe cdebconf frontend
 */
static void handle_cancel_click(GtkWidget * widget,
                                struct frontend * fe)
{
    /* Give user feedback that button has been clicked. */
    gtk_widget_set_sensitive(widget, FALSE);
    cdebconf_gtk_set_answer_goback(fe);
}

/** Create the "Cancel" button.
 *
 * @param progress_data progress data
 * @see frontend_data#action_box
 */
static void create_cancel_button(struct progress_data * progress_data)
{
    struct frontend * fe = progress_data->fe;
    GtkWidget * button;
    char * label;

    /* XXX: check NULL! */
    label = cdebconf_gtk_get_text(fe, "debconf/button-cancel", "Cancel");
    button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(handle_cancel_click), fe);
    cdebconf_gtk_add_global_key_handler(
        fe, button, G_CALLBACK(handle_cancel_key));
    cdebconf_gtk_add_button(fe, button);

    g_object_ref(G_OBJECT(button));
    progress_data->cancel_button = button;
}

/** Unreference and destroy the "Cancel" button.
 *
 * @param progress_data progress data
 */
static void destroy_cancel_button(struct progress_data * progress_data)
{
    GtkWidget * cancel_button = progress_data->cancel_button;

    if (NULL != cancel_button) {
        progress_data->cancel_button = NULL;
        g_object_unref(G_OBJECT(cancel_button));
        gtk_widget_destroy(cancel_button);
    }
}

/** Implements the can_cancel_progress method of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 * @return TRUE if progress bar can be canceled
 */
bool cdebconf_gtk_can_cancel_progress(struct frontend * fe)
{
    return DCF_CAPB_PROGRESSCANCEL ==
               (fe->capability & DCF_CAPB_PROGRESSCANCEL);
}

/** Init progress handling.
 *
 * This will create the private data structure and the necessary widgets.
 *
 * @param fe cdebconf frontend
 * @return TRUE if initialization was successful, FALSE otherwise
 */
static gboolean init_progress(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data;

    g_assert(NULL == fe_data->progress_data);

    if (NULL == (progress_data = g_malloc0(sizeof (struct progress_data)))) {
        g_warning("g_malloc0 failed.");
        return FALSE;
    }
    progress_data->fe = fe;
    progress_data->fe_title = g_strdup(fe->title);
    create_progress_box(progress_data);
    if (CAN_CANCEL_PROGRESS(fe)) {
        create_cancel_button(progress_data);
    }
    fe_data->progress_data = progress_data;

    return TRUE;
}

/** Destroy the progress handling widgets and data structures.
 *
 * @param fe cdebconf frontend
 */
static void destroy_progress(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data = fe_data->progress_data;

    if (NULL == progress_data) {
        return;
    }
    g_free(progress_data->fe_title);
    fe_data->progress_data = NULL;
    destroy_cancel_button(progress_data);
    destroy_progress_box(progress_data);
    g_free(progress_data);
}

/** Update the progress bar filling and title.
 *
 * The title will be taken from the progress_title field of the frontend.
 *
 * @param fe cdebconf frontend
 * @param fraction progress bar filling value
 */
static void update_progress_bar(struct frontend * fe, gdouble fraction)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * progress_bar = fe_data->progress_data->progress_bar;
    gchar * title_desc;

    g_assert(NULL != progress_bar);

    title_desc = q_get_raw_description(fe->progress_title);
    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progress_bar), title_desc);
    g_free(title_desc);
    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress_bar),
                                  fraction /* empty progress bar */);
}

/** Implements the progress_start method of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 * @param min minimum value representing the operation in progress
 * @param max maximum value representing the operation in progress
 * @param title initial title of the operation in progress
 */
void cdebconf_gtk_progress_start(struct frontend * fe, int min, int max,
                                 struct question * title)
{
    struct frontend_data * fe_data = fe->data;

    if (NULL != fe_data->setters) {
        /* called out of order */
        return;
    }
    if (NULL != fe_data->progress_data) {
        /* nested progress bar... let's clean up first */
        cdebconf_gtk_progress_stop(fe);
    }
    cdebconf_gtk_set_answer(fe, DC_NO_ANSWER);

    gdk_threads_enter();

#ifdef DI_UDEB
    cdebconf_gtk_di_run_dialog(fe);
#endif /* DI_UDEB */

    init_progress(fe);

    question_deref(fe->progress_title);
    fe->progress_title = title;
    question_ref(fe->progress_title);

    update_progress_bar(fe, 0.0 /* empty */);

    /* XXX: I feel strange to have to set these fields here...
     *      Should them be moved elsewhere? And API of this handler
     *      changed accordingly? */
    fe->progress_min = min;
    fe->progress_max = max;
    fe->progress_cur = min;

    /* XXX: rename progress_box */
    cdebconf_gtk_show_progress(fe);

    gdk_threads_leave();
}

/** Implements the progress_set methods of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 * @param val current value representing the operation in progress
 * @return DC_GOBACK if progress was canceled, DC_NOTOK if error happened and
 *         DC_OK otherwise.
 */
int cdebconf_gtk_progress_set(struct frontend * fe, int val)
{
    struct frontend_data * fe_data = fe->data;
    gdouble progress;

    /* XXX: should this kind of sanity checks be in the frontend? */
    if (fe->progress_max < val || fe->progress_min > val) {
        return DC_NOTOK;
    }
    if (NULL == fe_data->progress_data) {
        /* called out of order */
        return DC_NOTOK;
    }

    gdk_threads_enter();
    fe->progress_cur = val;
    if ((fe->progress_max - fe->progress_min) > 0) {
        progress = (gdouble) (fe->progress_cur - fe->progress_min) /
                   (gdouble) (fe->progress_max - fe->progress_min);
        update_progress_bar(fe, progress);
    }
    cdebconf_gtk_show_progress(fe);
    gdk_threads_leave();

    return fe_data->answer;
}

/** Implements the progress_info method of cdebconf frontends.
 *
 * This will update the progress label.
 *
 * @param fe cdebconf frontend
 * @param info current information about the operation in progress
 * @see progress_data#progress_label
 */
int cdebconf_gtk_progress_info(struct frontend * fe, struct question * info)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data = fe_data->progress_data;
    gchar * info_desc;

    if (NULL == progress_data) {
        /* called out of order */
        return DC_NOTOK;
    }

    info_desc = q_get_raw_description(info);
    gdk_threads_enter();
    gtk_entry_set_text(GTK_ENTRY(progress_data->progress_label), info_desc);
    gdk_threads_leave();
    g_free(info_desc);

    if (DC_NO_ANSWER == fe_data->answer) {
        return DC_OK;
    }
    return fe_data->answer;
}

/** Implements the progress_stop method of cdebconf frontends.
 *
 * This will destroy the widgets and data structures.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_progress_stop(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct progress_data * progress_data = fe_data->progress_data;

    if (NULL == progress_data) {
        /* called out of order */
        return;
    }

    gdk_threads_enter();
    destroy_progress(fe);
    gdk_threads_leave();
}

/* vim: et sw=4 si
 */
