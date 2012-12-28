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

/** @file go.c
 * GO support for the GTK+ frontend of cdebconf
 */

#include "go.h"

#include <string.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "frontend.h"
#include "question.h"
#include "template.h"
#include "plugin.h"
#include "database.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "ui.h"
#include "handlers.h"
#include "select_handlers.h"
#include "progress.h"
#ifdef DI_UDEB
# include "di.h"
# include "screenshot.h"
#endif

/** Store callbacks from question handlers run after "Continue".
 *
 * This structure form a single linked list of setters to be ran.
 */
struct setter {
    /** Callback function.
     *
     * This function should update the debconf database with the value entered
     * by the user. */
    setter_function func;

    /** Question for which this setter was registered. */
    struct question * question;

    /** Question handler private data. */
    void * user_data;

    /** Next setter. */
    struct setter * next;
};

/** Register the given setter.
 *
 * The setter will be prepended to the setters list.
 *
 * @param fe cdebconf frontend
 * @param func function updating the debconf database with user input
 * @param question question that will be updated
 * @param user_data question handler private data
 */
void cdebconf_gtk_register_setter(struct frontend * fe, setter_function func,
                                  struct question * question, void * user_data)
{
    struct frontend_data * fe_data = fe->data;
    struct setter * setter;

    /* check NULL! */
    setter = g_malloc0(sizeof (struct setter));
    setter->func = func;
    setter->question = question;
    setter->user_data = user_data;
    setter->next = fe_data->setters;
    fe_data->setters = setter;
}

/** Call all registered setters.
 *
 * @param fe cdebconf frontend
 */
static void call_setters(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct setter * setter;

    setter = fe_data->setters;
    while (NULL != setter) {
        (*setter->func)(setter->question, setter->user_data);
        setter = setter->next;
    }
}

/** Free all registered setters.
 *
 * @param fe_data the GTK+ frontend data
 */
static void free_setters(struct frontend_data * fe_data)
{
    struct setter * setter;
    struct setter * previous;

    setter = fe_data->setters;
    while (NULL != setter) {
        previous = setter;
        setter = setter->next;
        g_free(previous);
    }
    fe_data->setters = NULL;
}

/** Key event handler implementing the "Help" key shortcut.
 *
 * @param widget main widget
 * @param key pressed key
 * @param fe cdebconf frontend
 * @return TRUE if "Help" shortcut has been pressed, FALSE otherwise
 */
static gboolean handle_help_key(GtkWidget * widget, GdkEventKey * key,
                                struct frontend * fe)
{
    if (GDK_KEY_F1 == key->keyval || GDK_KEY_KP_F1 == key->keyval) {
        cdebconf_gtk_help(fe);
        return TRUE;
    }
    return FALSE;
}

/** Create the "Help" button.
 *
 * The button will be added to the main action box.
 *
 * @param fe cdebconf frontend
 */
static void create_help_button(struct frontend * fe)
{
    GtkWidget * button;
    char * label;

    label = cdebconf_gtk_get_text(fe, "debconf/button-help", "Help");
    /* XXX: check NULL! */
    button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect_swapped(G_OBJECT(button), "clicked",
                             G_CALLBACK(cdebconf_gtk_help), fe);

    cdebconf_gtk_add_button(fe, button);
    cdebconf_gtk_set_button_secondary(fe, button, TRUE);
    cdebconf_gtk_add_global_key_handler(
        fe, button, G_CALLBACK(handle_help_key));
}

/** Key event handler implementing the "Go Back" key shortcut.
 *
 * @param widget main window
 * @param key pressed key
 * @param fe cdebconf frontend
 * @return TRUE if "Go Back" shortcut has been pressed, FALSE otherwise
 */
static gboolean handle_goback_key(GtkWidget * widget, GdkEventKey * key,
                                  struct frontend * fe)
{
    if (GDK_KEY_Escape == key->keyval) {
        cdebconf_gtk_set_answer_goback(fe);
        return TRUE;
    }
    return FALSE;
}

/** Create the "Go Back" button.
 *
 * The button will be added to the main action box.
 *
 * @param fe cdebconf frontend
 */
static void create_goback_button(struct frontend * fe)
{
    GtkWidget * button;
    char * label;

    label = cdebconf_gtk_get_text(fe, "debconf/button-goback", "Go Back");
    /* XXX: check NULL! */
    button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect_swapped(G_OBJECT(button), "clicked",
                             G_CALLBACK(cdebconf_gtk_set_answer_goback), fe);

    cdebconf_gtk_add_button(fe, button);
    cdebconf_gtk_add_global_key_handler(
        fe, button, G_CALLBACK(handle_goback_key));
}

/* documented in cdebconf_gtk.h */
GtkWidget * cdebconf_gtk_create_continue_button(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * action_box = fe_data->action_box;
    GList * focus_chain;
    GtkWidget * button;
    char * label;

    /* XXX: check NULL! */
    label = cdebconf_gtk_get_text(fe, "debconf/button-continue", "Continue");
    button = gtk_button_new_with_label(label);
    g_free(label);

    g_signal_connect_swapped(G_OBJECT(button), "clicked",
                             G_CALLBACK(cdebconf_gtk_set_answer_ok), fe);
    gtk_box_pack_start(GTK_BOX(action_box), button,
                       TRUE /* expand */, TRUE /* fill */, DEFAULT_PADDING);
    gtk_container_get_focus_chain(GTK_CONTAINER(action_box), &focus_chain);
    focus_chain = g_list_prepend(focus_chain, button);
    gtk_container_set_focus_chain(GTK_CONTAINER(action_box), focus_chain);
    g_list_free(focus_chain);

    gtk_widget_set_can_default(GTK_WIDGET(button), TRUE);
    gtk_widget_grab_default(GTK_WIDGET(button));

    return button;
}

/** Test if the action box contains action buttons.
 *
 * The screenshot button and "Go back" button are not considered as action
 * buttons.
 *
 * @param fe cdebconf frontend
 * @returns TRUE if there already is action buttons, FALSE otherwise.
 */
static gboolean is_action_box_filled(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkContainer * action_box = GTK_CONTAINER(fe_data->action_box);
    GList * children;
    gboolean filled;

    children = gtk_container_get_children(action_box);
    if (CAN_GO_BACK(fe)) {
#ifdef DI_UDEB
        /* screenshot + go back button */
        filled = 2 != g_list_length(children);
#else
        /* go back button */
        filled = 1 != g_list_length(children);
#endif
    } else {
#ifdef DI_UDEB
        /* go back button */
        filled = 1 != g_list_length(children);
#else
        /* no buttons */
        filled = 0 != g_list_length(children);
#endif
    }
    g_list_free(children);
    return filled;
}

/** Create default buttons in the action box.
 *
 * Default buttons currently means the "Continue" button.
 *
 * @param fe cdebconf frontend
 */
static void create_default_buttons(struct frontend * fe)
{
    (void) cdebconf_gtk_create_continue_button(fe);
}

/** Destroy all buttons in the main action box.
 *
 * @param fe cdebconf frontend
 */
static void destroy_buttons(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkContainer * action_box = GTK_CONTAINER(fe_data->action_box);

    gtk_container_foreach(action_box, (GtkCallback) gtk_widget_destroy,
                          NULL /* no user_data */);
}

/** List of internal question handlers.
 */
static const struct {
    /** Question type */
    const char * type;
    /** Handler function for the corresponding type. */
    cdebconf_gtk_handler handler;
} question_handlers[] = {
    { "boolean",        cdebconf_gtk_handle_boolean },
    { "multiselect",    cdebconf_gtk_handle_multiselect },
    { "note",           cdebconf_gtk_handle_note },
    { "password",       cdebconf_gtk_handle_password },
    { "select",         cdebconf_gtk_handle_select },
    { "string",         cdebconf_gtk_handle_string },
    { "error",          cdebconf_gtk_handle_note },
    { "text",           cdebconf_gtk_handle_text },
    { "",               NULL },
};

/** Find a internal handler for the given question type.
 *
 * @param type the question type
 * @return the corresponding handler if found, NULL otherwise
 */
static cdebconf_gtk_handler find_internal_handler(const char * type)
{
    int i;

    for (i = 0; NULL != question_handlers[i].handler; i++) {
        if (0 == strcmp(type, question_handlers[i].type)) {
            return question_handlers[i].handler;
        }
    }
    return NULL;
}

/** Find an external handler (plugin) for the given question type.
 *
 * This will save the plugin in a cache for future use.
 *
 * @param fe cdebconf frontend
 * @param type the question type
 * @return the corresponding handler if found, NULL otherwise
 * @see frontend_data#plugins
 */
static cdebconf_gtk_handler find_external_handler(struct frontend * fe,
                                                  const char * type)
{
    struct frontend_data * fe_data = fe->data;
    struct plugin * plugin;

    /* Look in already loaded plugins first. */
    plugin = g_hash_table_lookup(fe_data->plugins, type);
    if (NULL == plugin) {
        /* Load if available */
        if (NULL == (plugin = plugin_find(fe, type))) {
            g_warning("No plugin for %s", type);
            return NULL;
        }
        /* Save for later use */
        g_hash_table_insert(fe_data->plugins, g_strdup(type), plugin);
    }
    return (cdebconf_gtk_handler) plugin->handler;
}

/** Update the question database with the value currently set to each
 * questions referenced by the frontend.
 *
 * @param fe cdebconf frontend
 */
static void update_question_database(struct frontend * fe)
{
    struct question * question;

    question = fe->questions;
    while (NULL != question) {
        frontend_qdb_set(fe->qdb, question, 0);
        question = question->next;
    }
}

/** Call the relevant handlers for every questions currently referenced
 * by the frontend.
 *
 * Handlers are expected to add their widgets in the given question box.
 *
 * @param fe cdebconf frontend
 * @param question_box container for question handler widgets
 * @return DC_NOTIMPL if no handlers are available, DC_NOTOK if an error
 *         happened, DC_OK otherwise
 */
static int call_question_handlers(struct frontend * fe,
                                 GtkWidget * question_box)
{
    struct question * question;
    cdebconf_gtk_handler handler;
    int ret;

    question = fe->questions;
    while (NULL != question) {
        handler = find_internal_handler(question->template->type);
        if (NULL == handler) {
            handler = find_external_handler(fe, question->template->type);
        }
        if (NULL == handler) {
            return DC_NOTIMPL;
        }
        if (DC_OK != (ret = handler(fe, question, question_box))) {
            g_warning("tag \"%s\" failed to display!", question->tag);
            return ret;
        }
        question = question->next;
    }
    return DC_OK;
}

/** Create the container for question widgets in the given container.
 *
 * This will add the question box to the container on top of returning it.
 *
 * The actual container will vary if GO has been called for only one question
 * or more: since all widgets used to display single questions have native
 * scrolling capabilities or do not need scrolling (too small) they can manage
 * scrolling be autonomously.
 *
 * Vice-versa the most simple approach when displaying multiple questions
 * togheter (whose handling wigets haven't native scrolling capabilities) is to
 * pack them all inside a viewport.
 *
 * @todo
 * This currently breaks the focus chain for widgets not currently shown by the
 * viewports.  Not sure on how to fix that correctly.
 *
 * @param fe cdebconf frontend
 * @param container the container in which the question box will be added
 * @return the newly created question box
 */
static GtkWidget * create_question_box(struct frontend * fe,
                                       GtkWidget * container)
{
    GtkWidget * question_box;
    GtkWidget * question_box_scroll;
    GtkWidget * hpadding_box;

    /* check NULL! */
    question_box = gtk_vbox_new(FALSE /* don't make children equal */,
                                0 /* padding */);

    if (0 && IS_QUESTION_SINGLE(fe->questions)) {
        gtk_box_pack_start(GTK_BOX(container), question_box,
                           TRUE /* expand */, TRUE /* fill */,
                           0 /* padding */);
    } else {
        hpadding_box = gtk_hbox_new(FALSE /* don't make children equal */,
                                    0 /* padding */);
        gtk_box_pack_start(GTK_BOX(hpadding_box), question_box,
                           TRUE /* expand */, TRUE /* fill */,
                           QUESTIONBOX_HPADDING);
        question_box_scroll = gtk_scrolled_window_new(
            NULL /* create horizontal adjustement */,
            NULL /* create vertical adjustement */);
        gtk_scrolled_window_add_with_viewport(
            GTK_SCROLLED_WINDOW(question_box_scroll), hpadding_box);
        gtk_scrolled_window_set_policy(
            GTK_SCROLLED_WINDOW(question_box_scroll),
            GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
        gtk_scrolled_window_set_shadow_type(
            GTK_SCROLLED_WINDOW(question_box_scroll), GTK_SHADOW_NONE);
        gtk_box_pack_start(GTK_BOX(container), question_box_scroll,
                           TRUE /* expand */, TRUE /* fill */,
                           DEFAULT_PADDING);
    }
    return question_box;
}

/** Wait for an answer to be set asynchronously, either by a button or a key
 * shortcut.
 *
 * @param fe cdebconf frontend
 * @see fe_data#answer
 */
static void wait_answer(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    g_mutex_lock(fe_data->answer_mutex);
    while (DC_NO_ANSWER == fe_data->answer) {
        g_cond_wait(fe_data->answer_cond, fe_data->answer_mutex);
    }
    g_mutex_unlock(fe_data->answer_mutex);
}

/** Implements the "go" method of cdebconf frontends.
 *
 * The processing is currently divided into three stages:
 *  - Setup widgets corresponding to questions asked:
 *    this is done by creating the question box and calling the
 *    various handlers to get the widgets added.  Default action
 *    buttons are added along the way if no handlers created a
 *    specific button.  The "Go Back" button is always added if
 *    supported by the frontend caller.
 *  - Wait for an answer.
 *  - Get the data back from the widgets and clean up:
 *    the "setters" registered by the various handlers are called
 *    to update the question database based on user input.  The
 *    question box and buttons are then destroyed to leave room
 *    for the next frontend call.
 *
 * If a progress was running, the widgets are hidden at the beginning
 * of the procedure and shown at the end.
 *
 * @param fe cdebconf frontend
 * @return the debconf status corresponding to user actions
 */
int cdebconf_gtk_go(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GtkWidget * question_box;
    struct question * question;
    int ret;

    if (NULL == fe->questions) {
        return DC_OK;
    }

    cdebconf_gtk_set_answer(fe, DC_NO_ANSWER);
    fe_data->help_question = NULL;

    gdk_threads_enter();
#ifdef DI_UDEB
    /* XXX: rename */
    cdebconf_gtk_di_run_dialog(fe);
    cdebconf_gtk_create_screenshot_button(fe);
#endif /* DI_UDEB */
    if (NULL != fe_data->progress_data) {
        cdebconf_gtk_hide_progress(fe);
    }
    question_box = create_question_box(fe, fe_data->target_box);
    /* Hide the target box while we create the questions to avoid flicker. */
    cdebconf_gtk_hide_target_box(fe);
    if (CAN_GO_BACK(fe)) {
        create_goback_button(fe);
    }
    if (DC_OK != (ret = call_question_handlers(fe, question_box))) {
        cdebconf_gtk_set_answer(fe, ret);
        gdk_threads_leave();
        goto end;
    }
    if (!is_action_box_filled(fe)) {
        create_default_buttons(fe);
    }

    /* Set up the help button if necessary. FIXME: We only offer help for
     * the first question with help text in any given question box. It's not
     * clear what the right thing to do here is.
     */
    question = fe->questions;
    while (NULL != question) {
        char *help = q_get_help(fe, question);
        if (help) {
            struct question *help_q = fe->qdb->methods.get(fe->qdb, help);
            if (help_q) {
                fe_data->help_question = help_q;
                create_help_button(fe);
                break;
            }
        }
        question = question->next;
    }

    cdebconf_gtk_show_target_box(fe);
    cdebconf_gtk_show_buttons(fe);
    gdk_threads_leave();

    /* frontend blocked here until a button has been pressed */
    wait_answer(fe);

    if (DC_NOTOK == fe_data->answer) {
        goto end;
    }

    gdk_threads_enter();
    cdebconf_gtk_set_buttons_sensitive(fe, FALSE);
    if (DC_OK == fe_data->answer) {
        call_setters(fe);
        update_question_database(fe);
    }
    cdebconf_gtk_empty_target_box(fe);
    destroy_buttons(fe);
    if (NULL != fe_data->progress_data) {
        cdebconf_gtk_show_progress(fe);
    }
    gdk_threads_leave();

end:
    question_deref(fe_data->help_question);
    fe_data->help_question = NULL;
    free_setters(fe_data);
    return fe_data->answer;
}


/* vim: et sw=4 si
 */
