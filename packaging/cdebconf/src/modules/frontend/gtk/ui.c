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

/** @file ui.c
 * Common user interface related functions for the GTK+ frontend of cdebconf
 */

#include "ui.h"

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "descriptions.h"
#ifdef DI_UDEB
# include "di.h"
#endif

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_add_common_layout(struct frontend * fe,
                                    struct question * question,
                                    GtkWidget * container, GtkWidget * widget)
{
    GtkWidget * description_box;
    GtkWidget * hpadbox;
    GtkWidget * vpadbox;
    gboolean is_single;

    is_single = IS_QUESTION_SINGLE(question);

    description_box = cdebconf_gtk_create_description(fe, question);

    vpadbox = gtk_vbox_new(FALSE /* don't make children equal */,
                           DEFAULT_PADDING);
    gtk_box_pack_start(GTK_BOX(vpadbox), description_box,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* padding */);
    gtk_box_pack_start(GTK_BOX(vpadbox), widget, is_single /* expand? */,
                       is_single /* fill? */, 0 /* padding */);
    hpadbox = gtk_hbox_new(FALSE /* don't make children equal */,
                           DEFAULT_PADDING);
    gtk_box_pack_start(GTK_BOX(hpadbox), vpadbox, TRUE /* expand */,
                       TRUE /* fill */, 0 /* padding */);
    gtk_box_pack_start(GTK_BOX(container), hpadbox, is_single /* expand? */,
                       is_single /* fill? */, QUESTIONBOX_VPADDING);
}

/** Handle the expose event for the banner.
 *
 * This function will redraw the last INFO text on top of the banner.
 *
 * @param widget the banner
 * @param event expose event
 * @param fe cdebconf frontend
 * @return FALSE as the event does not need to propagate further
 */
static gboolean handle_exposed_banner(GtkWidget * widget,
                                      GdkEventExpose * event,
                                      struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GdkWindow * window;
    PangoFontDescription * font;
    PangoLayout * layout;
    gint text_width;
    gint text_height;
    gchar * message;
    char * text;

    if (NULL != fe->info) {
        text = q_get_description(fe, fe->info);
        message = g_strdup_printf(
            "<b><span foreground=\"#ffffff\">%s</span></b>", text);
        layout = gtk_widget_create_pango_layout(
            widget, NULL /* no text for the layout */);
        pango_layout_set_markup(layout, message, -1 /* until '\0' */);
        font = pango_font_description_from_string("Sans 12");
        pango_layout_set_font_description(layout, font);
        pango_layout_get_pixel_size(layout, &text_width, &text_height);
        window = gtk_widget_get_window(widget);
        /* Left-align, vertically-center */
        gdk_draw_layout(window, gdk_gc_new(window),
                        DEFAULT_PADDING * 2,
                        (fe_data->logo_height - text_height) / 2,
                        layout);
        g_object_unref(layout);
        pango_font_description_free(font);
        g_free(message);
        g_free(text);
    }
    return FALSE;
}

/** Create the banner with the logo inside the given container.
 *
 * The logo will be centered in the banner.
 *
 * @param fe cdebconf frontend
 * @param container container in which the banner will be added
 * @see LOGO_IMAGE_PATH
 */
static void create_banner(struct frontend * fe, GtkWidget * container)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * banner;
    GtkWidget * logo;
    GdkPixbuf * pixbuf;

    /* XXX: check NULL! */
    banner = gtk_event_box_new();
    logo = gtk_image_new_from_file(LOGO_IMAGE_PATH);
    gtk_misc_set_alignment(GTK_MISC(logo), 0.5 /* center */, 0 /* top */);
    gtk_misc_set_padding(GTK_MISC(logo), 0, 0);
    gtk_container_add(GTK_CONTAINER(banner), logo);

    /* Remember the logo size: */
    pixbuf = gtk_image_get_pixbuf(GTK_IMAGE(logo));
    fe_data->logo_width = gdk_pixbuf_get_width(pixbuf);
    fe_data->logo_height = gdk_pixbuf_get_height(pixbuf);

    g_signal_connect_after(G_OBJECT(banner), "expose_event",
                           G_CALLBACK(handle_exposed_banner), fe);

    gtk_box_pack_start(GTK_BOX(container), banner,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* padding */);
}

/** Create the label where the current operation will be displayed.
 *
 * @param fe cdebconf frontend
 * @param container the container in which the label will be added
 */
static void create_label_title(struct frontend * fe, GtkWidget * container)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * label_title;

    /* check NULL! */
    label_title = gtk_label_new(NULL /* no label */);

    gtk_misc_set_alignment(GTK_MISC(label_title), 0 /* left */, 0 /* top */);

    g_object_ref(G_OBJECT(label_title));
    fe_data->title = label_title;

    cdebconf_gtk_center_widget(&label_title,
                               DEFAULT_PADDING /* horizontal padding */,
                               0 /* no vertical padding */);
    gtk_box_pack_start(GTK_BOX(container), label_title,
                       FALSE /* expand */, FALSE /* fill */, DEFAULT_PADDING);
}

/** Create the main container where questions or progress will be added.
 *
 * @param fe cdebconf frontend
 * @param container the container in which the target box will be added
 */
static void create_target_box(struct frontend * fe, GtkWidget * container)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * target_box;

    /* This is the box were question(s) will be displayed */
    target_box = gtk_vbox_new(FALSE /* don't make children equal */,
                              0 /* padding */);

    gtk_box_pack_start(GTK_BOX(container), target_box, TRUE /* expand */,
                       TRUE /* fill */, DEFAULT_PADDING);

    g_object_ref(G_OBJECT(target_box));
    fe_data->target_box = target_box;
}

/** Create the container for buttons.
 *
 * @param fe cdebconf frontend
 * @param container the container in which the action box will be added
 */
static void create_action_box(struct frontend * fe, GtkWidget * container)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * action_box;

    g_assert(NULL == fe_data->action_box);

    /* check NULL! */
    action_box = gtk_hbutton_box_new();

    gtk_button_box_set_layout(GTK_BUTTON_BOX(action_box), GTK_BUTTONBOX_END);
    gtk_box_set_spacing(GTK_BOX(action_box), DEFAULT_PADDING);

    gtk_box_pack_start(GTK_BOX(container), action_box,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       DEFAULT_PADDING);

    g_object_ref(G_OBJECT(action_box));
    fe_data->action_box = action_box;
}

/** Create all common widgets inside the main window.
 *
 * This currentyl means the banner, the title label, the target box and
 * the action box.
 *
 * @param fe cdebconf frontend
 * @param window the main window
 */
static void create_main_widgets(struct frontend * fe, GtkWidget * window)
{
    GtkWidget * outer_box;
    GtkWidget * v_mainbox;
    GtkWidget * h_mainbox;

    /* check NULL! */
    outer_box = gtk_vbox_new(FALSE /* don't make children equal */,
                             0 /* padding */);
    create_banner(fe, outer_box);
    /* check NULL! */
    v_mainbox = gtk_vbox_new(FALSE /* don't make children equal */,
                             0 /* padding */);
    /* check NULL! */
    h_mainbox = gtk_hbox_new(FALSE /* don't make children equal */,
                             0 /* padding */);
    create_label_title(fe, v_mainbox);
    create_target_box(fe, v_mainbox);
    create_action_box(fe, v_mainbox);
    gtk_box_pack_start(GTK_BOX(h_mainbox), v_mainbox, TRUE /* expand */,
                       TRUE /* fill */, DEFAULT_PADDING * 2);
    gtk_box_pack_start(GTK_BOX(outer_box), h_mainbox, TRUE /* expand */,
                       TRUE /* fill */, DEFAULT_PADDING);
    gtk_container_add(GTK_CONTAINER(window), outer_box);
}

/** Handle the closing of the frontend window.
 *
 * This will unreference the window from frontend_data and force the
 * frontend to quit.
 *
 * @param fe cdebconf frontend
 * @param window the main window
 * @see cdebconf_gtk_force_quit
 */
static void handle_closed_main_window(struct frontend * fe,
                                      GtkWidget * window)
{
    struct frontend_data * fe_data = fe->data;

    fe_data->window = NULL;
    g_object_unref(G_OBJECT(window));
    cdebconf_gtk_force_quit(fe);
}

/** Create the main window.
 *
 * When the main window is destroyed (e.g. by user closing it)
 * handle_closed_main_window() will be called.
 *
 * @param fe cdebconf frontend
 * @return FALSE in case of errors
 */
gboolean cdebconf_gtk_create_main_window(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * window;

    g_assert(NULL != fe_data);
    g_assert(NULL == fe_data->window);
    if (NULL == (window = gtk_window_new(GTK_WINDOW_TOPLEVEL))) {
        g_critical("gtk_window_new failed.");
        return FALSE;
    }
    gtk_window_set_resizable(GTK_WINDOW(window), TRUE /* resizable */);
    gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
    gtk_window_set_decorated(GTK_WINDOW(window), TRUE /* resizable */);
    create_main_widgets(fe, window);

    g_signal_connect_swapped(window, "destroy",
                             G_CALLBACK(handle_closed_main_window), fe);

    g_object_ref(G_OBJECT(window));
    fe_data->window = window;

    return TRUE;
}

/** Internal structure for keyboard shortcuts handling.
 *
 * This will be used to give the necessary data to remove_shortcut().
 */
struct shortcut {
    /** the main window */
    GtkWidget * window;
    /** signal handler */
    gulong handler_id;
};

/** Remove the keyboard shortcut.
 *
 * This is a callback for the "destroy" event of the widget to which was
 * attached the shortcut.
 *
 * @param widget destroyed widget
 * @param shortcut shortcut data
 */
static void remove_shortcut(GtkObject * widget, struct shortcut * shortcut)
{
    g_signal_handler_disconnect(G_OBJECT(shortcut->window),
                                shortcut->handler_id);
    g_free(shortcut);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_add_global_key_handler(struct frontend * fe,
                                         GtkWidget * widget,
                                         GCallback key_event_handler)
{
    struct frontend_data * fe_data = fe->data;
    struct shortcut * shortcut;

    /* XXX: check NULL! */
    shortcut = g_malloc0(sizeof (struct shortcut));
    shortcut->window = fe_data->window;
    shortcut->handler_id = g_signal_connect_after(
        fe_data->window, "key_press_event", key_event_handler, fe);
    g_signal_connect(G_OBJECT(widget), "destroy",
                     G_CALLBACK(remove_shortcut), shortcut);
}

/** Destroy the main window.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_destroy_main_window(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * window = fe_data->window;

    if (NULL != window) {
        g_signal_handlers_disconnect_by_func(
            window, G_CALLBACK(handle_closed_main_window), fe);
        fe_data->window = NULL;
        g_object_unref(G_OBJECT(window));
        gtk_widget_destroy(window);
    }
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_center_widget(GtkWidget ** widget, guint horizontal_padding,
                                guint vertical_padding)
{
    GtkWidget * vbox;
    GtkWidget * hbox;

    /* XXX: check NULL! */
    vbox = gtk_vbox_new(FALSE /* don't make children equal */,
                        0 /* padding */);
    /* XXX: check NULL! */
    hbox = gtk_hbox_new(FALSE /* don't make children equal */,
                        0 /* padding */);
    gtk_box_pack_start(GTK_BOX(vbox), *widget, TRUE /* expand */,
                       TRUE /* fill */, vertical_padding);
    gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE /* expand */, TRUE /* fill */,
                       horizontal_padding);
    *widget = hbox;
}

/** Create buttons for the DIY dialog box.
 *
 * @param fe cdebconf frontend
 * @param dialog the dialog in which the buttons will be added
 * @return the created widget
 */
static GtkWidget * create_dialog_action_box(struct frontend * fe,
                                            GtkWidget * dialog)
{
    GtkWidget * action_box;
    GtkWidget * close_button;
    char * label;

    /* check NULL! */
    action_box = gtk_hbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(action_box), GTK_BUTTONBOX_END);

    label = cdebconf_gtk_get_text(fe, "debconf/button-continue", "Continue");
    /* check NULL! */
    close_button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect_swapped(G_OBJECT(close_button), "clicked",
                             G_CALLBACK(gtk_widget_destroy), dialog);

    gtk_box_pack_end(GTK_BOX(action_box), close_button,
                     TRUE /* expand */, TRUE /* fill */, DEFAULT_PADDING);
    return action_box;
}

/** Create a label displaying the title for a DIY dialog box.
 *
 * @param title the title to display
 * @return the created widget
 */
static GtkWidget * create_dialog_title_label(const gchar * title)
{
    GtkWidget * label;
    gchar * markup;

    /* check NULL! */
    label = gtk_label_new(NULL /* no text */);
    gtk_misc_set_alignment(GTK_MISC(label), 0 /* left aligned */,
                           0 /* top aligned */);

    markup = g_strdup_printf("<b>%s</b>", title);
    gtk_label_set_markup(GTK_LABEL(label), markup);
    g_free(markup);

    return label;
}

/* documented in cdebconf_gtk.h */
gboolean cdebconf_gtk_run_message_dialog(struct frontend * fe,
                                         const gchar * title,
                                         const gchar * message)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * dialog;
    GtkWidget * vbox;
    GtkWidget * frame;
    GtkWidget * label;

    /* XXX: check NULL! */
    dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE /* modal */);
    gtk_window_set_transient_for(GTK_WINDOW(dialog),
                                 GTK_WINDOW(fe_data->window));
    gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE /* not resizable */);
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
    gtk_window_set_decorated(GTK_WINDOW(dialog), FALSE /* no decoration */);
    gtk_container_set_border_width(GTK_CONTAINER(dialog), 0 /* no border */);

    /* Create a wrapped label */
    label = gtk_label_new(message);
    gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);

    /* XXX: check NULL! */
    vbox = gtk_vbox_new(FALSE /* don't make children equal */,
                        DEFAULT_PADDING);
    gtk_box_pack_start(GTK_BOX(vbox), create_dialog_title_label(title),
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* no padding */);
    gtk_box_pack_start(GTK_BOX(vbox), label,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       DEFAULT_PADDING);
    gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(),
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* no padding */);
    gtk_box_pack_start(GTK_BOX(vbox), create_dialog_action_box(fe, dialog),
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* no padding */);
    cdebconf_gtk_center_widget(&vbox, DEFAULT_PADDING, DEFAULT_PADDING);

    frame = gtk_frame_new(NULL /* no label */);
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_OUT);
    gtk_container_add(GTK_CONTAINER(frame), vbox);
    gtk_container_add(GTK_CONTAINER(dialog), frame);

    gtk_widget_show_all(dialog);
    return TRUE;
}

/* documented in cdebconf_gtk.h. */
void cdebconf_gtk_set_buttons_sensitive(struct frontend * fe,
                                        gboolean sensitive)
{
    struct frontend_data * fe_data = fe->data;
    GList * child;

    child = gtk_container_get_children(GTK_CONTAINER(fe_data->action_box));
    while (NULL != child) {
        gtk_widget_set_sensitive(GTK_WIDGET(child->data), sensitive);
        child = g_list_next(child);
    }
}

/** Show buttons in the main window.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_show_buttons(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    gtk_widget_show_all(fe_data->action_box);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_add_button(struct frontend * fe, GtkWidget * button)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * action_box = fe_data->action_box;
    GList * focus_chain;

    gtk_box_pack_start(GTK_BOX(action_box), button,
                       TRUE /* expand */, TRUE /* fill */, DEFAULT_PADDING);
    gtk_container_get_focus_chain(GTK_CONTAINER(action_box), &focus_chain);
    focus_chain = g_list_append(focus_chain, button);
    gtk_container_set_focus_chain(GTK_CONTAINER(action_box), focus_chain);
    g_list_free(focus_chain);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_set_button_secondary(struct frontend * fe,
                                       GtkWidget * button,
                                       gboolean secondary)
{
    struct frontend_data * fe_data = fe->data;

    gtk_button_box_set_child_secondary(GTK_BUTTON_BOX(fe_data->action_box),
                                       button, secondary);
}

/** Update the title on top of the frontend window.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_update_frontend_title(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    gchar * tmp;

    tmp = g_strdup_printf("<b>%s</b>", fe->title);
    gtk_label_set_markup(GTK_LABEL(fe_data->title), tmp);
    g_free(tmp);
}

/** Show the target box.
 *
 * @param fe cdebconf frontend
 * @see frontend_data#target_box
 */
void cdebconf_gtk_show_target_box(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    gtk_widget_show_all(fe_data->target_box);
}

/** Hide the target box.
 *
 * @param fe cdebconf frontend
 * @see frontend_data#target_box
 */
void cdebconf_gtk_hide_target_box(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    gtk_widget_hide(fe_data->target_box);
}

/** Remove every widgets inside the target box.
 *
 * @param fe cdebconf frontend
 * @see frontend_data#target_box
 */
void cdebconf_gtk_empty_target_box(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    gtk_container_forall(GTK_CONTAINER(fe_data->target_box),
                         (GtkCallback) gtk_widget_destroy, NULL /* no data */);
}

/** Returns the width (in pango units) of the given text.
 *
 * @param widget widget where the text will be renderered
 * @param text text to be rendered
 * @return width of the text in pango units
 */
gint cdebconf_gtk_get_text_width(GtkWidget * widget, gchar * text)
{
    PangoLayout * layout;
    gint width;

    layout = gtk_widget_create_pango_layout(widget, text);
    pango_layout_get_size(layout, &width, NULL /* no height */);
    g_object_unref(layout);

    return width;
}

/* vim: et sw=4 si
 */
