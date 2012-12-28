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

/** @file fe_gtk.c
 * "Main" file for the GTK+ frontend of cdebconf.
 */

#include <syslog.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <signal.h>
#include <string.h>
#include <errno.h>

#include "common.h"
#include "template.h"
#include "question.h"
#include "frontend.h"
#include "database.h"
#include "plugin.h"
#include "strutl.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "handlers.h"
#include "align_text_renderer.h"
#include "select_handlers.h"
#include "descriptions.h"
#include "progress.h"
#include "go.h"
#include "ui.h"
#ifdef DI_UDEB
# include "di.h"
#endif /* DI_UDEB */

/** Get the description of a given template.
 *
 * The caller is responsible to free the returned string.
 *
 * @todo
 * There is no real reason that we can't use question_get_text defined in
 * cdebconf except that question_get_text doesn't strdup the fallback string.
 * This means than there is no way to have correct memory management.
 * question_get_text should be fixed and this function removed from the
 * GTK+ frontend.
 *
 * @param fe cdebconf frontend
 * @param template template name
 * @param fallback fallback string when the template does not exists
 * @return a newly allocated string containing the description of the template
 *         or a copy of fallback if the template was not found
 */
char * cdebconf_gtk_get_text(struct frontend * fe, const char * template,
                             const char * fallback)
{
    /* XXX: add macro? */
    struct question * question = fe->qdb->methods.get(fe->qdb, template);
    char * text;

    text = question ? q_get_description(fe, question) : g_strdup(fallback);
    question_deref(question);
    return text;
}

/** A GThreadFunc calling the GTK+ main loop.
 *
 * This function will be run in another thread and will happily process
 * GTK+ events resulting in user interaction.
 *
 * @param dummy an unused parameter
 * @return always return NULL
 */
static void * handle_gtk_events(void * dummy)
{
    gdk_threads_enter();
    gtk_main();
    gdk_threads_leave();

    return NULL /* no one cares what value is returned */;
}

/** Determines if a question is the first question requiring user input of the
 * current question row.
 *
 * @param question question that will be tested
 * @return TRUE if the question is the first question requiring user input
 */
gboolean cdebconf_gtk_is_first_question(struct question * question)
{
    struct question * crawl;

    crawl = question;

    while (NULL != crawl->prev) {
        if (0 != strcmp(crawl->prev->template->type, "note")) {
            return FALSE;
        }
        crawl = crawl->prev;
    }
    return TRUE;
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_force_quit(struct frontend * fe)
{
    if (-1 == kill(0 /* myself */, SIGTERM)) {
        g_critical("kill failed: %s", strerror(errno));
    }
}

/** De-allocate and de-init all elements of the GTK+ frontend data.
 *
 * @param fe cdebconf frontend
 */
static void destroy_frontend_data(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    if (NULL == fe_data) {
        return;
    }
    cdebconf_gtk_progress_stop(fe);
    /* Exit any running GO in order to cleanup. */
    cdebconf_gtk_set_answer_notok(fe);
    fe->data = NULL;
    if (NULL != fe_data->window) {
        g_object_unref(G_OBJECT(fe_data->window));
    }
    if (NULL != fe_data->title) {
        g_object_unref(G_OBJECT(fe_data->title));
    }
    if (NULL != fe_data->target_box) {
        g_object_unref(G_OBJECT(fe_data->target_box));
    }
    if (NULL != fe_data->action_box) {
        g_object_unref(G_OBJECT(fe_data->action_box));
    }
    /* There is no safe way to free the mutex: this function can be called
     * as part as a signal handler while the mutex is actually held in
     * go.c:wait_answer().
     */
    if (NULL == fe_data->plugins) {
        g_hash_table_destroy(fe_data->plugins);
    }
    g_free(fe_data);
}

/** Allocate and initialize non-graphical elements of the GTK+ frontend.
 *
 * The data field of the given frontend will be set to the newly
 * allocated structure.
 *
 * @param fe cdebconf frontend
 * @see frontend_data
 */
static gboolean create_frontend_data(struct frontend * fe)
{
    struct frontend_data * fe_data;

    g_assert(NULL == fe->data);

    if (NULL == (fe->data = g_malloc0(sizeof (struct frontend_data)))) {
        g_critical("Unable to allocate frontend_data.");
        return FALSE;
    }

    fe_data = fe->data;

#if GLIB_CHECK_VERSION(2, 31, 0)
    /* This could be a lot neater once we no longer need to support glib <=
     * 2.30, as we can just put GCond and GMutex structures directly in
     * struct frontend_data.
     */
    if (NULL == (fe_data->answer_cond = g_slice_new(GCond))) {
        g_critical("Unable to allocate fe_data->answer_cond.");
        goto failed;
    }
    g_cond_init(fe_data->answer_cond);
    if (NULL == (fe_data->answer_mutex = g_slice_new(GMutex))) {
        g_critical("g_mutex_new failed.");
        goto failed;
    }
    g_mutex_init(fe_data->answer_mutex);
#else
    if (NULL == (fe_data->answer_cond = g_cond_new())) {
        g_critical("g_cond_new failed.");
        goto failed;
    }
    if (NULL == (fe_data->answer_mutex = g_mutex_new())) {
        g_critical("g_mutex_new failed.");
        goto failed;
    }
#endif
    fe_data->plugins = g_hash_table_new_full(
        g_str_hash, g_str_equal, g_free /* key destroy function (strings) */,
        (GDestroyNotify) plugin_delete /* value destroy function (plugin) */);
    if (NULL == fe_data->plugins) {
        g_critical("g_hash_table_new_full failed.");
        goto failed;
    }

    fe->data = fe_data;

    return TRUE;

failed:
    destroy_frontend_data(fe);
    return FALSE;
}

/** Implements the shutdown method of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 */
static int cdebconf_gtk_shutdown(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    if (NULL != fe_data)
    {
        if (NULL != fe_data->event_listener) {
            gtk_main_quit();
            if (g_thread_self() != fe_data->event_listener) {
                (void) g_thread_join(fe_data->event_listener);
            }
            fe_data->event_listener = NULL;
        }
        cdebconf_gtk_destroy_main_window(fe);
#ifdef DI_UDEB
        cdebconf_gtk_di_shutdown(fe);
#endif /* DI_UDEB */
        destroy_frontend_data(fe);
    }

    return DC_OK;
}

/** Show the GTK+ frontend window.
 *
 * @param fe cdebconf frontend
 */
static void show_main_window(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;

    g_assert(NULL != fe_data->window);

    gtk_widget_show_all(fe_data->window);
}

/* documented in cdebconf_gtk.h */
int cdebconf_gtk_get_answer(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    int answer;

    g_mutex_lock(fe_data->answer_mutex);
    answer = fe_data->answer;
    g_mutex_unlock(fe_data->answer_mutex);
    return answer;
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_set_answer(struct frontend * fe, int answer)
{
    struct frontend_data * fe_data = fe->data;

    g_mutex_lock(fe_data->answer_mutex);
    fe_data->answer = answer;
    g_cond_broadcast(fe_data->answer_cond);
    g_mutex_unlock(fe_data->answer_mutex);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_set_answer_ok(struct frontend * fe)
{
    cdebconf_gtk_set_answer(fe, DC_OK);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_set_answer_notok(struct frontend * fe)
{
    cdebconf_gtk_set_answer(fe, DC_NOTOK);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_set_answer_goback(struct frontend * fe)
{
    cdebconf_gtk_set_answer(fe, DC_GOBACK);
}

/* documented in cdebconf_gtk.h */
void cdebconf_gtk_help(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    char * description;
    char * ext_description;

    if (NULL == fe_data || NULL == fe_data->help_question)
        return;

    description = q_get_description(fe, fe_data->help_question);
    ext_description = q_get_extended_description(fe, fe_data->help_question);
    cdebconf_gtk_run_message_dialog(fe, description, ext_description);
    g_free(ext_description);
    g_free(description);
}

/** Create the event listener thread.
 *
 * @param fe cdebconf frontend
 * @return FALSE in case of errors, TRUE otherwise
 * @see handle_gtk_events()
 */
static gboolean create_event_listener_thread(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    GError * error;

    g_assert(NULL == fe_data->event_listener);

#if GLIB_CHECK_VERSION(2, 31, 0)
    fe_data->event_listener = g_thread_try_new(
        "event_listener", (GThreadFunc) handle_gtk_events, NULL /* no data */,
        &error);
    if (NULL == fe_data->event_listener) {
        g_critical("g_thread_try_new failed: %s", error->message);
        g_error_free(error);
        return FALSE;
    }
#else
    fe_data->event_listener = g_thread_create(
        (GThreadFunc) handle_gtk_events, NULL /* no data */,
        TRUE /* joinable thread */, &error);
    if (NULL == fe_data->event_listener) {
        g_critical("g_thread_create failed: %s", error->message);
        g_error_free(error);
        return FALSE;
    }
#endif
    return TRUE;
}

/** Implements the initialize method of cdebconf frontends.
 *
 * This will initialize the GTK+ frontend data, the GTK+ window and common
 * widgets and forks the event handling thread.
 *
 * @return DC_OK if initialization goes fine, DC_NOTOK if something went wrong
 */
static int cdebconf_gtk_initialize(struct frontend * fe,
                                   struct configuration * conf)
{
    /* INFO(INFO_DEBUG, "GTK_DI - gtk_initialize() called"); */
#if !GLIB_CHECK_VERSION(2, 31, 0)
    g_thread_init(NULL /* default thread functions */);
    if (!g_thread_supported()) {
        g_critical("Threads not supported by this glib.");
        return DC_NOTOK;
    }
#endif
    gdk_threads_init();
    gtk_init(NULL /* no argc */, NULL /* no argv */);

    if (!create_frontend_data(fe)) {
        g_critical("create_frontend_data failed.");
        goto failed;
    }
    if (!cdebconf_gtk_create_main_window(fe)) {
        g_critical("cdebconf_gtk_create_main_window failed.");
        goto failed;
    }
    fe->interactive = TRUE;

#ifdef DI_UDEB
    if (!cdebconf_gtk_di_setup(fe)) {
        g_critical("cdebconf_gtk_di_setup failed.");
        goto failed;
    }
#endif /* DI_UDEB */

    /* XXX: here? */
    show_main_window(fe);

    if (!create_event_listener_thread(fe)) {
        g_critical("create_event_listener_thread failed.");
        goto failed;
    }

    return DC_OK;

failed:
    cdebconf_gtk_shutdown(fe);
    return DC_NOTOK;
}

/** Implements the set_title method of cdebconf frontends.
 *
 * When build for the debian-installer, this will only update the corresponding
 * fields in frontend data.  The title widget will then be updated by
 * cdebconf_gtk_di_run_dialog() on the next GO or progress start.
 *
 * @param fe cdebconf frontend
 * @param title new frontend title
 */
static void cdebconf_gtk_set_title(struct frontend * fe, const char * title)
{
    g_free(fe->title);
    fe->title = g_strdup(title);
#ifndef DI_UDEB /* Only for non d-i frontend */
    gdk_threads_enter();
    cdebconf_gtk_update_frontend_title(fe);
    gdk_threads_leave();
#endif
}

/** Implements the lookup_directive function of cdebconf frontends.
 *
 * When the align cabability is set, the TAB directive returns "\t".
 *
 * @param fe cdebconf frontend
 * @param directive the directive name
 * @return the expansion value, or NULL if expansion should be skipped
 */
static const char * cdebconf_gtk_lookup_directive(struct frontend * fe,
                                                  const char * directive)
{
    if (CAN_ALIGN(fe) && 0 == strcmp(directive, "TAB")) {
        return "\t";
    }
    if (CAN_ALIGN(fe) && 0 == strcmp(directive, "ALIGN=CENTER")) {
        return ALIGN_CENTER_STRING;
    }
    if (CAN_ALIGN(fe) && 0 == strcmp(directive, "ALIGN=RIGHT")) {
        return ALIGN_RIGHT_STRING;
    }
    /* Remove unhandled directives */
    return "";
}

/** Implements the can_go_back function of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 * @param question question which needs to be backed up
 * @return TRUE if backup is possible, FALSE otherwise
 */
static bool cdebconf_gtk_can_go_back(struct frontend * fe,
                                     struct question * question)
{
    return DCF_CAPB_BACKUP == (fe->capability & DCF_CAPB_BACKUP);
}

/** Implements the can_align function of cdebconf frontends.
 *
 * @param fe cdebconf frontend
 * @param question question which needs to be aligned
 * @return TRUE if align is possible, FALSE otherwise
 */
static bool cdebconf_gtk_can_align(struct frontend * fe,
                                   struct question * question)
{
    return DCF_CAPB_ALIGN == (fe->capability & DCF_CAPB_ALIGN);
}

/** Describe our frontend implementation to cdebconf.
 */
struct frontend_module debconf_frontend_module = {
    .initialize = cdebconf_gtk_initialize,
    .shutdown = cdebconf_gtk_shutdown,
    .can_go_back = cdebconf_gtk_can_go_back,
    .can_align = cdebconf_gtk_can_align,
    .set_title = cdebconf_gtk_set_title,
    .lookup_directive = cdebconf_gtk_lookup_directive,
    /* see go.c */
    .go = cdebconf_gtk_go,
    /* see progress.c */
    .can_cancel_progress = cdebconf_gtk_can_cancel_progress,
    .progress_start = cdebconf_gtk_progress_start,
    .progress_info = cdebconf_gtk_progress_info,
    .progress_set = cdebconf_gtk_progress_set,
    .progress_stop = cdebconf_gtk_progress_stop,
};

/* vim: et sw=4 si
 */
