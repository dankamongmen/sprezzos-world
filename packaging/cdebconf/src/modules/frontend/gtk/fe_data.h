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

/** @file fe_data.h
 * data structure handling specific informations about the GTK+ frontend
 */

#ifndef _FE_DATA_H
#define _FE_DATA_H

#include <gtk/gtk.h>

#include "progress.h"
#include "go.h"

/** A structure holding specific data of the GTK+ frontend.
 *
 * It is normally accessed by the "data" field of "struct frontend".
 */
struct frontend_data
{
    /** Main window of the frontend. */
    GtkWidget * window;

    /** Label used to display frontend's title.
     *
     * @see cdebconf_gtk_update_frontend_title()
     * @see cdebconf_gtk_set_title()
     */
    GtkWidget * title;

    /** Logo size, used to display info messages.
     *
     * @see create_banner()
     * @see handle_exposed_banner()
     */
    int logo_width;
    int logo_height;

    /** Internal data of the handler of progress commands.
     *
     * @see progress.c
     */
    struct progress_data * progress_data;

    /** Start of the single linked list of "setter" functions ran after
     * users have validated their choice.
     *
     * @see struct setter
     */
    struct setter * setters;

    /** Container for main buttons. */
    GtkWidget * action_box;

    /** Container for either the questions widgets or the progress widgets. */
    GtkWidget * target_box;

    /** Event listener thread.
     *
     * @see create_event_listener_thread()
     */
    GThread * event_listener;

    /** Answer to the current debconf command.
     *
     * This is used by both GO and progress handlers to synchronize the
     * frontend on user input.
     *
     * @see cdebconf_gtk_get_answer()
     * @see cdebconf_gtk_set_answer()
     */
    int answer;

    /** Allow non-blocking wait on the frontend answer.
     *
     * @see frontend_data#answer
     * @see wait_answer()
     */
    GCond * answer_cond;

    /** Lock around the frontend answer.
     *
     * @see frontend_data#answer
     */
    GMutex * answer_mutex;

    /** Associates question type and plugins.
     *
     * This table also serves as a cache, preventing us to load the same
     * plugins again and again.
     *
     * @see find_external_handler()
     */
    GHashTable * plugins;

    /** Current question for which help will be displayed. */
    struct question * help_question;

#ifdef DI_UDEB
    /** Internal data for specific handling related to the debian-installer.
     *
     * @see di.c
     */
    struct di_data * di_data;
#endif /* DI_UDEB */
};

#endif /* !_FE_DATA_H */

/* vim: et sw=4 si
 */
