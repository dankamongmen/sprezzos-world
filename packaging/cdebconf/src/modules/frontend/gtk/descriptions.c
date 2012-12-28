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

/** @file descriptions.c
 * tools to display question descriptions for the GTK+ frontend of cdebconf
 */

#include "descriptions.h"

#include <string.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"
#include "template.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"

/** A mapping between question type and icons for descriptions. */
static const struct {
    /** question type */
    const char * type;
    /** full path to the associated icon */
    const char * path;
} icon_mapping[] = {
    { "note",  BASE_IMAGE_PATH "/note_icon.png" },
    { "error", BASE_IMAGE_PATH "/warning_icon.png" },
    { NULL,    NULL }
};

/** Get the full path to an icon associated to a question type.
 *
 * @param question the question
 * @return a statically allocated path to an icon or NULL if no icon is
 *         associated with the question
 */
static const char * get_icon_path(struct question * question)
{
    int i;

    for (i = 0; NULL != icon_mapping[i].type; i++) {
        if (0 == strcmp(question->template->type, icon_mapping[i].type)) {
            return icon_mapping[i].path;
        }
    }
    return NULL;
}

/** Add an icon for a given question in the given container.
 *
 * @param question the question
 * @param container the container where the icon will be added
 */
static void add_icon(struct question * question, GtkWidget * container)
{
    const char * icon_path;
    GtkWidget * icon_box;
    GtkWidget * icon_button = NULL;

    icon_path = get_icon_path(question);
    if (NULL == icon_path) {
        return;
    }

    icon_box = gtk_vbox_new(FALSE /* don't make children equal */,
                            0 /* no spacing */);
    icon_button = gtk_image_new_from_file(icon_path);
    gtk_box_pack_start(GTK_BOX(icon_box), icon_button,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       3 /* padding */);
    gtk_box_pack_start(GTK_BOX(container), icon_box,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       3 /* padding */);
}

/** Get the background color for the main window of the frontend.
 *
 * @param fe cdebconf frontend
 * @return the background color of the main window
 */
static GdkColor * get_background_color(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkStyle * style;

    style = gtk_widget_get_style(fe_data->window);
    return style->bg;
}

/** margin around descriptions */
#define DESCRIPTION_MARGIN 4
/** vertical padding around descriptions */
#define DESCRIPTION_VPADDING 3

/** Add a description to a given container.
 *
 * @param fe cdebconf frontend
 * @param question the question from which the description will be read
 * @param container the container where the description will be added
 */
static void add_description(struct frontend * fe, struct question * question,
                            GtkWidget * container)
{
    GtkWidget * view;
    GtkTextBuffer * buffer;
    GtkTextIter start;
    GtkTextIter end;
    char * description;

    description = q_get_description(fe, question);
    /* XXX: check NULL! */
    view = gtk_text_view_new();
    /* XXX: check NULL! */
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
    gtk_text_buffer_set_text(buffer, description, -1 /* until '\0' */);
    g_free(description);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(view), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(view), FALSE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(view), GTK_WRAP_WORD_CHAR);
    gtk_text_view_set_left_margin(GTK_TEXT_VIEW(view), DESCRIPTION_MARGIN);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(view), DESCRIPTION_MARGIN);
    gtk_text_buffer_create_tag(buffer, "italic", "style",
                               PANGO_STYLE_ITALIC, NULL);
    gtk_text_buffer_get_start_iter(buffer, &start);
    gtk_text_buffer_get_end_iter(buffer, &end);
    gtk_text_buffer_apply_tag_by_name(buffer, "italic", &start, &end);
    gtk_widget_modify_base(view, GTK_STATE_NORMAL, get_background_color(fe));
    gtk_box_pack_start(GTK_BOX(container), view,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       DESCRIPTION_VPADDING);
}

/** vertical padding around extendend description */
#define EXT_DESCRIPTION_VPADDING 2

/** Add an extendend description to a given container.
 *
 * @param fe cdebconf frontend
 * @param question the question from which the extendend description will be
 *        read
 * @param container the container where the extendend description will be
 *        added
 */
static void add_extended_description(struct frontend * fe,
                                     struct question * question,
                                     GtkWidget * container)
{
    GtkTextBuffer * buffer;
    GtkWidget * view;
    char * ext_description;

    /* here is created the question's extended description, but only
     * if the question's extended description actually exists
     */
    ext_description = q_get_extended_description(fe, question);
    if ('\0' != ext_description[0]) {
        view = gtk_text_view_new();
        buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
        gtk_text_buffer_set_text(buffer, ext_description, -1);
        gtk_text_view_set_editable(GTK_TEXT_VIEW(view), FALSE);
        gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(view), FALSE);
        gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(view), GTK_WRAP_WORD_CHAR);
        gtk_widget_modify_base(view, GTK_STATE_NORMAL,
                               get_background_color(fe));
        gtk_box_pack_start(GTK_BOX(container), view, FALSE /* don't expand */,
                           FALSE /* don't fill */, EXT_DESCRIPTION_VPADDING);
    }
    g_free(ext_description);
}

/** Return a new widget with description and extendend description of a
 * given question.
 *
 * @param fe cdebconf frontend
 * @param question question that will be looked upon
 * @return a newly created widget or NULL on failure
 */
GtkWidget * cdebconf_gtk_create_description(struct frontend * fe,
                                            struct question * question)
{
    GtkWidget * returned_box;
    GtkWidget * description_box;

    returned_box = gtk_hbox_new(FALSE /* don't make children equal */,
                                0 /* no spacing */);
    add_icon(question, returned_box);

    description_box = gtk_vbox_new(FALSE /* don't make children equal */,
                                   0 /* no spacing */);
    if (0 == strcmp(question->template->type, "note") ||
        0 == strcmp(question->template->type, "error")) {
        add_description(fe, question, description_box);
        add_extended_description(fe, question, description_box);
    } else {
        add_extended_description(fe, question, description_box);
        add_description(fe, question, description_box);
    }
    gtk_container_set_focus_chain(GTK_CONTAINER(description_box),
                                  NULL /* empty list */);
    gtk_box_pack_start(GTK_BOX(returned_box), description_box,
                       TRUE /* expand */, TRUE /* fill */, 3 /* padding */);

    return returned_box;
}

/* vim: et sw=4 si
 */
