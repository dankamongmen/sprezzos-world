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

/** @file handlers.c
 * question handlers of the GTK+ frontend
 *
 * @see select_handlers.c
 */

#include "handlers.h"

#include <string.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "ui.h"

/** Update a boolean question value from the status of a toggle button.
 *
 * @param question question to set
 * @param toggle_button associated toggle button
 */
static void set_value_from_toggle_button(struct question * question,
                                         GtkToggleButton * toggle_button)
{
    gboolean check_value;

    check_value = gtk_toggle_button_get_active(toggle_button);
    question_setvalue(question, check_value ? "true" : "false");
}

/** The handler for boolean questions implemented as radio buttons.
 *
 * This handler is used when multiple questions are used in a row.
 *
 * @param fe cdebconf frontend
 * @param question question being handled
 * @param question_box container for question widgets
 * @return DC_NOTOK if an error happened, DC_OK otherwise
 */
static int handle_boolean_radio(struct frontend * fe,
                                struct question * question,
                                GtkWidget * question_box)
{
    GtkWidget * radio_false;
    GtkWidget * radio_true;
    GtkWidget * container;
    char * false_label;
    char * true_label;
    const char * value;

    false_label = cdebconf_gtk_get_text(fe, "debconf/no", "No");
    radio_false = gtk_radio_button_new_with_label(
        NULL /* new group */, false_label);
    g_free(false_label);

    true_label = cdebconf_gtk_get_text(fe, "debconf/yes", "Yes");
    radio_true = gtk_radio_button_new_with_label_from_widget(
        GTK_RADIO_BUTTON(radio_false), true_label);
    g_free(true_label);

    value = question_getvalue(question, "");
    if (NULL != value && 0 == strcmp(value, "true")) {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_true), TRUE);
    } else {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_true), FALSE);
    }

    container = gtk_vbox_new(FALSE /* don't make children equal */,
                             DEFAULT_PADDING);
    gtk_box_pack_start(GTK_BOX(container), radio_false,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* padding */);
    gtk_box_pack_start(GTK_BOX(container), radio_true,
                       FALSE /* don't expand */, FALSE /* don't fill */,
                       0 /* padding */);
    cdebconf_gtk_add_common_layout(fe, question, question_box, container);

    if (cdebconf_gtk_is_first_question(question)) {
        if (NULL != value && 0 == strcmp(value, "true")) {
            gtk_widget_grab_focus(radio_true);
        } else {
            gtk_widget_grab_focus(radio_false);
        }
    }

    cdebconf_gtk_register_setter(
        fe, SETTER_FUNCTION(set_value_from_toggle_button), question,
        radio_true);

    return DC_OK;
}

/** Handler for question of boolean type.
 *
 * This will register the corresponding setter.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler's widgets
 * @return DC_OK if everything is fine, DC_NOTOK otherwise
 */
int cdebconf_gtk_handle_boolean(struct frontend * fe,
                                struct question * question,
                                GtkWidget * question_box)
{
    return handle_boolean_radio(fe, question, question_box);
}

/** Handler for "question" of note type.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler's widgets
 * @return DC_OK if everything is fine, DC_NOTOK otherwise
 */
int cdebconf_gtk_handle_note(struct frontend * fe, struct question * question,
                             GtkWidget * question_box)
{
    /* Let's use an empty container... */
    cdebconf_gtk_add_common_layout(fe, question, question_box,
                                   gtk_vbox_new(FALSE, DEFAULT_PADDING));
    return DC_OK;
}

/** Handler for "question" of text type.
 *
 * This currently just calls cdebconf_gtk_handle_note()
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler's widgets
 * @return DC_OK if everything is fine, DC_NOTOK otherwise
 */
int cdebconf_gtk_handle_text(struct frontend * fe, struct question * question,
                             GtkWidget * question_box)
{
    return cdebconf_gtk_handle_note(fe, question, question_box);
}

/** Setter function for handler based on GtkEntry.
 *
 * @param question handled question
 * @param entry entry where the value will be read
 * @see setter_function
 */
static void set_value_from_entry(struct question * question, GtkEntry * entry)
{
    question_setvalue(question, gtk_entry_get_text(entry));
}

/** Create a GtkAlignment widget to correctly align entry widget inside the
 * question box.
 *
 * @param entry an entry widget
 * @return a widget containing the entry
 */
static GtkWidget * create_entry_alignment(GtkWidget * entry)
{
    GtkWidget * alignment;

    /* check NULL! */
    alignment = gtk_alignment_new(0.0 /* left */, 0.0 /* top */,
                                  1.0 /* expand horizontally */,
                                  0.0 /* no vertical expansion */);
    gtk_container_add(GTK_CONTAINER(alignment), entry);
    return alignment;
}

/** Handler for question of password type.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler's widgets
 * @return DC_OK if everything is fine, DC_NOTOK otherwise
 */
int cdebconf_gtk_handle_password(struct frontend * fe,
                                 struct question * question,
                                 GtkWidget * question_box)
{
    GtkWidget * entry;

    /* INFO(INFO_DEBUG, "GTK_DI - gtkhandler_password() called"); */

    entry = gtk_entry_new();
    gtk_entry_set_visibility(GTK_ENTRY(entry), FALSE /* password style */);
    gtk_entry_set_activates_default(GTK_ENTRY(entry),
                                    TRUE /* activate on Enter */);

    cdebconf_gtk_add_common_layout(fe, question, question_box,
                                   create_entry_alignment(entry));

    if (cdebconf_gtk_is_first_question(question)) {
        gtk_widget_grab_focus(entry);
    }

    cdebconf_gtk_register_setter(fe, SETTER_FUNCTION(set_value_from_entry),
                                 question, entry);

    return DC_OK;
}

/** Handler for question of string type.
 *
 * @param fe cdebconf frontend
 * @param question handled question
 * @param question_box container for handler's widgets
 * @return DC_OK if everything is fine, DC_NOTOK otherwise
 */
int cdebconf_gtk_handle_string(struct frontend * fe,
                               struct question * question,
                               GtkWidget * question_box)
{
    GtkWidget * entry;
    const char * defval;

    defval = question_getvalue(question, "");

    entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(entry), NULL != defval ? defval : "");
    gtk_entry_set_activates_default(
        GTK_ENTRY(entry), TRUE /* activate on Enter key */);

    cdebconf_gtk_add_common_layout(fe, question, question_box,
                                   create_entry_alignment(entry));

    if (cdebconf_gtk_is_first_question(question)) {
        gtk_widget_grab_focus(entry);
    }

    cdebconf_gtk_register_setter(fe, SETTER_FUNCTION(set_value_from_entry),
                                 question, entry);

    return DC_OK;
}

/* vim: et sw=4 si
 */
