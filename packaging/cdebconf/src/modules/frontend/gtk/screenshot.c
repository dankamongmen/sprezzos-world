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

/** @file screenshot.c
 * screenshot related function of the GTK+ frontend
 *
 * This is really intended to be used inside the debian-installer, even if
 * nothing prevents the following code from working within a X11 environment.
 */

#include "screenshot.h"

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "frontend.h"
#include "ui.h"
#include "di.h"

/** Directory where screenshots will be saved. */
#define SCREENSHOT_DIRECTORY "/var/log"

/** Write a picture of the given window.
 *
 * @param gdk_window the window to capture
 * @param screenshot_path full path where the image will be saved
 * @return TRUE if the operation was successful, FALSE otherwise
 */
static gboolean save_screenshot(GdkWindow * gdk_window,
                                const gchar * screenshot_path)
{
    GdkPixbuf * pixbuf;
    gint x;
    gint y;
    gint width;
    gint height;
    gint depth;
    gboolean success;

    g_assert(NULL != gdk_window);
    g_assert(NULL != screenshot_path);

    gdk_window_get_geometry(gdk_window, &x, &y, &width, &height, &depth);
    pixbuf = gdk_pixbuf_get_from_drawable(
        NULL /* allocate a new pixbuf */, gdk_window,
        gdk_colormap_get_system(), 0 /* src_x */, 0 /* src_y */,
        0 /* dest_x */, 0 /* dest_y */, width, height);
    if (NULL == pixbuf) {
        g_warning("gdk_pixbuf_get_from_drawable failed.");
        return FALSE;
    }
    success = gdk_pixbuf_save(pixbuf, screenshot_path, "png",
                              NULL /* no GError */,
                              NULL /* end of option list */);
    /* This will free the pixbuf, see gdk_pixbuf_get_from_drawable doc. */
    g_object_unref(pixbuf);
    return success;
}

/** Make a path that can be used to write a screenshot.
 *
 * Screenshots will be named like the following: debian-installer_keymap_3.png
 *
 * The caller must free the returned string.
 *
 * @param question the question which tag will be used to create the path
 * @return a newly allocated string containing a path where a screenshot can be
 *         saved
 * @see SCREENSHOT_DIRECTORY
 */
static gchar * create_screenshot_path(struct question * question)
{
    gchar * question_tag;
    gchar * path;
    guint i;

    question_tag = g_strdup(question->tag);
    /* question_tag will be updated in place */
    (void) g_strdelimit(question_tag, "/", '_');

    for (i = 0; TRUE; i++) {
        path = g_strdup_printf(SCREENSHOT_DIRECTORY "/%s_%u.png",
                               question_tag, i);
        if (!g_file_test(path, G_FILE_TEST_EXISTS)) {
            break;
        }
        g_free(path);
    }
    g_free(question_tag);
    return path;
}

/** Display a dialog with the path of the saved screenshot.
 *
 * @param fe cdebconf frontend
 * @param screenshot_path path of the screenshot
 */
static void popup_success(struct frontend * fe, const gchar * screenshot_path)
{
    gchar * title;
    char * template;
    gchar * message;

    /* XXX: the translated button label might not be suitable for a title
     *      in all translations... should use a different template. */
    title = cdebconf_gtk_get_text(fe, "debconf/gtk-button-screenshot",
                                  "Screenshot");
    template = cdebconf_gtk_get_text(fe, "debconf/gtk-screenshot-saved",
                                     "Screenshot saved as %s");
    message = g_strdup_printf(template, screenshot_path);

    if (!cdebconf_gtk_run_message_dialog(fe, title, message)) {
        g_warning("cdebconf_gtk_run_message_dialog failed.");
    }
}

/** Take a screenshot of the current frontend display and inform the user
 * of the sucess of the operation.
 *
 * @todo This should also display a message in case of failure.
 *
 * @param fe cdebconf frontend
 * @param widget the main window
 */
static void do_screenshot(struct frontend * fe, GtkWidget * widget)
{
    gchar * screenshot_path;
    gboolean success;

    screenshot_path = create_screenshot_path(fe->questions);
    success = save_screenshot(gtk_widget_get_window(widget), screenshot_path);
    if (success) {
        popup_success(fe, screenshot_path);
    } else {
        g_warning("save_screenshot failed.");
    }
    g_free(screenshot_path);
}

/** Create a "Screenshot" button in the action box.
 *
 * The button is configured as "secondary": it will not be displayed
 * near other buttons in a GtkButtonBox.
 *
 * @param fe cdebconf frontend
 * @return the newly created button
 */
GtkWidget * cdebconf_gtk_create_screenshot_button(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * action_box = fe_data->action_box;
    GtkWidget * button;
    char * label;

    /* XXX: check NULL! */
    label = cdebconf_gtk_get_text(fe, "debconf/gtk-button-screenshot",
                                  "Screenshot");
    button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect_swapped(G_OBJECT(button), "clicked",
                             G_CALLBACK(do_screenshot), fe);

    gtk_box_pack_start(GTK_BOX(action_box), button,
                       TRUE /* expand */, TRUE /* fill */, DEFAULT_PADDING);
    gtk_button_box_set_child_secondary(GTK_BUTTON_BOX(action_box),
                                       button, TRUE);
    /* Remove the screenshot button from the focus chain. */
    gtk_container_set_focus_chain(GTK_CONTAINER(action_box), NULL);

    return button;
}

/* vim: et sw=4 si
 */
