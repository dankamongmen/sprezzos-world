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

/** @file di.c
 * specific debian-installer bits of the GTK+ frontend
 */

#include "di.h"

#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <syslog.h>

#if 0
#include <directfb.h>
/* for dfb_input_device_reload_keymap() and dfb_input_device_at() */
#include <core/input.h>
#endif
#include <gtk/gtk.h>

#include "question.h"
#include "database.h"

#include "cdebconf_gtk.h"
#include "fe_data.h"
#include "ui.h"

/** Private data for d-i specific bits. */
struct di_data {
    /** Previously known kepmap, taken from debian-installer/keymap. */
    char * previous_keymap;
    /** Previously known language, taken from debconf/language. */
    char * previous_language;
};

/** Get the current value for a given template.
 *
 * The caller needs to free the returned string.
 *
 * @param fe cdebconf frontend
 * @param template name of the template
 * @return a newly allocated string containing the value, or an empty string
 *         if the template was not found
 */
static gchar * get_question_value(struct frontend * fe, const char * template)
{
    struct question * question = fe->qdb->methods.get(fe->qdb, template);
    const char * result = NULL;

    if (NULL != question) {
        result = question_getvalue(question, "" /* no language */);
    }
    if (NULL == result) {
        result = "";
    }
    return g_strdup(result);
}

/** Print the given string to the "syslog".
 *
 * @param string string to print
 */
static void print_to_syslog(const gchar * string)
{
    syslog(LOG_USER | LOG_DEBUG, "%s", string);
}

/** Get a statically allocated string corresponding to the given GTK+
 * log level.
 *
 * @param log_level the log level
 * @return a statically allocated string corresponding to the log level
 */
static const char * get_prefix(GLogLevelFlags log_level)
{
    switch (log_level & G_LOG_LEVEL_MASK) {
        case G_LOG_LEVEL_ERROR: return "ERROR";
        case G_LOG_LEVEL_CRITICAL: return "CRITICAL";
        case G_LOG_LEVEL_WARNING: return "WARNING";
        case G_LOG_LEVEL_MESSAGE: return "Message";
        case G_LOG_LEVEL_INFO: return "INFO";
        case G_LOG_LEVEL_DEBUG: return "DEBUG";
        default: return "(unknown)";
    }
}

/** Implementation of GLogFunc for the GTK+ frontend.
 *
 * This will log messages going through the glib log system to the standard
 * syslog.
 *
 * @param log_domain the log domain of the message
 * @param log_level the log level of the message
 * @param message the message to process
 * @param user_data user data, set in g_log_set_handler()
 */
static void log_glib_to_syslog(const gchar * log_domain,
                               GLogLevelFlags log_level, gchar const * message,
                               gpointer user_data)
{
    GString * gstring;
    gchar * string;

    gstring = g_string_new(NULL);
    g_string_append_printf(gstring, "cdebconf_gtk ");
    g_string_append_printf(gstring, "(process:%lu): ", (gulong) getpid());
    if (NULL != log_domain) {
        g_string_append_printf(gstring, "%s - ", log_domain);
    }
    g_string_append_printf(gstring, "%s: ", get_prefix(log_level));
    g_string_append(gstring, message);
    string = g_string_free(gstring, FALSE);
    print_to_syslog(string);
    g_free(string);
}

/** Make the given window fullscreen.
 *
 * @param window main window
 */
static void make_fullscreen(GtkWidget * window)
{
    GdkScreen * screen;

    screen = gtk_window_get_screen(GTK_WINDOW(window));
    gtk_widget_set_size_request(window, gdk_screen_get_width(screen),
                                gdk_screen_get_height(screen));
    gtk_window_fullscreen(GTK_WINDOW(window));
}

/** Setup d-i specific bits.
 *
 * This will create and initialize the relevant data structure.
 *
 * This will allow setup the glib log handler to log through standard
 * syslog.
 *
 * @param fe cdebconf frontend
 * @return FALSE if initialization failed
 * @see log_glib_to_syslog()
 */
gboolean cdebconf_gtk_di_setup(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct di_data * di_data;
    GdkCursor * cursor;

    g_assert(NULL == fe_data->di_data);
    if (NULL == (di_data = g_malloc0(sizeof (struct di_data)))) {
        return FALSE;
    }
    di_data->previous_keymap = get_question_value(
        fe, "debian-installer/keymap");
    di_data->previous_language = get_question_value(fe, "debconf/language");

    fe_data->di_data = di_data;

    (void) g_set_printerr_handler(print_to_syslog);
    (void) g_log_set_default_handler(log_glib_to_syslog,  NULL);

    make_fullscreen(fe_data->window);

    cursor = gdk_cursor_new(GDK_LEFT_PTR);
    gdk_window_set_cursor(gdk_get_default_root_window(), cursor);
    gdk_cursor_unref(cursor);

    return TRUE;
}

/** Refresh the DirectFB keymap to the one actually in use on consoles.
 *
 * @todo This is a workaround to force dfb to reload keymap at every run,
 *       awaiting for dfb to support automatic keymap change detection and
 *       reloading (see also bug #381979).
 *
 * @param fe cdebconf frontend
 */
static void refresh_keymap(struct frontend * fe)
{
#if 0
    dfb_input_device_reload_keymap(dfb_input_device_at(DIDID_KEYBOARD));
#endif
}

/** Returns the current text direction.
 *
 * @param fe cdebconf frontend
 * @return the current text direction
 */
static GtkTextDirection get_text_direction(struct frontend * fe)
{
    char * dirstr;
    GtkTextDirection direction;

    dirstr = cdebconf_gtk_get_text(fe, "debconf/text-direction",
                                   "LTR - default");

    if ('R' == dirstr[0]) {
        direction = GTK_TEXT_DIR_RTL;
    } else {
        direction = GTK_TEXT_DIR_LTR;
    }
    g_free(dirstr);
    return direction;
}

/** Update various settings with the current language settings.
 *
 * @param fe cdebconf frontend
 */
static void refresh_language(struct frontend * fe)
{
    /* This will enable different fonts to be used for different language. */
    gtk_rc_reparse_all();
    /* Adapt text direction. */
    gtk_widget_set_default_direction(get_text_direction(fe));
}

/** Update what needs to be updated on a new user interaction.
 *
 * This must be called after gdk_threads_enter().
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_di_run_dialog(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct di_data * di_data = fe_data->di_data;
    char * keymap;
    char * language;

    g_assert(NULL != di_data);

    cdebconf_gtk_update_frontend_title(fe);
    keymap = get_question_value(fe, "debian-installer/keymap");
    if (0 != strcmp(keymap, di_data->previous_keymap)) {
        refresh_keymap(fe);
        g_free(di_data->previous_keymap);
        di_data->previous_keymap = keymap;
    } else {
        g_free(keymap);
    }

    language = get_question_value(fe, "debconf/language");
    if (0 != strcmp(language, di_data->previous_language)) {
        refresh_language(fe);
        g_free(di_data->previous_language);
        di_data->previous_language = language;
    } else {
        g_free(language);
    }
}

/** Clean up d-i specific data structures.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_di_shutdown(struct frontend * fe)
{
    struct frontend_data * fe_data = fe->data;
    struct di_data * di_data = fe_data->di_data;

    if (NULL != di_data) {
        fe_data->di_data = NULL;
        if (NULL != di_data->previous_keymap) {
            g_free(di_data->previous_keymap);
        }
        if (NULL != di_data->previous_language) {
            g_free(di_data->previous_language);
        }
        g_free(di_data);
    }
}

/* vim: et sw=4 si
 */
