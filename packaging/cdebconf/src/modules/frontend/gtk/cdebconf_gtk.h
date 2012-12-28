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

/** @file cdebconf_gtk.h
 * Public interface of the GTK+ frontend of cdebconf.
 *
 * You should find everything needed to write a plugin here.
 */

#ifndef _CDEBCONF_GTK_H_
#define _CDEBCONF_GTK_H_

#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

/** Horizontal padding between a question and the question box frame. */
#define QUESTIONBOX_HPADDING 6
/** Vertical padding between two adjacent questions inside the question box. */
#define QUESTIONBOX_VPADDING 3
/** Default padding used troughout the GTK+ frontend. */
#define DEFAULT_PADDING 6

/* Unused area between "aligned" columns in pango units. */
#define COLUMN_SPACING 20000

#ifdef DI_UDEB
/** Paths to the directory containing images used by the GTK+ frontend. */
# define BASE_IMAGE_PATH \
    "/usr/share/graphics"
#else /* !DI_UDEB */
/* XXX: we need to ship the relevant images in the package */
# define BASE_IMAGE_PATH \
    "/usr/share/debconf/graphics"
#endif /* DI_UDEB */

/** Full path to the logo that will be displayed in the banner.
 *
 * @see create_banner()
 */
#define LOGO_IMAGE_PATH \
    BASE_IMAGE_PATH "/logo_installer.png"

/** Can the progress be canceled within the current frontend run? */
#define CAN_CANCEL_PROGRESS(Frontend) \
    (Frontend->methods.can_cancel_progress(Frontend))

/** Can the user "go back" in the current frontend run? */
#define CAN_GO_BACK(Frontend) \
    (Frontend->methods.can_go_back(Frontend, Frontend->questions))

/** Should tab separated fields be aligned in select and multiselect? */
#define CAN_ALIGN(Frontend) \
    (Frontend->methods.can_align(Frontend, Frontend->questions))

/** Is the question the only one in the current GO? */
#define IS_QUESTION_SINGLE(Question) \
    (NULL == Question->prev && NULL == Question->next)

/** A function that will "set" the question to the value entered by the user.
 *
 * @param question question being set
 * @param user_data data given to cdebconf_gtk_register_setter()
 */
typedef void (* setter_function)(struct question * question,
                                 void * user_data);

/** Cast a function into a setter_function. */
#define SETTER_FUNCTION(X) ((setter_function) X)

void cdebconf_gtk_register_setter(struct frontend * fe, setter_function func,
                                  struct question * question,
                                  void * user_data);

gboolean cdebconf_gtk_is_first_question(struct question * question);

char * cdebconf_gtk_get_text(struct frontend * fe, const char * template,
                             const char * fallback);

/** A question handler.
 * XXX
 */
typedef int (* cdebconf_gtk_handler)(struct frontend * fe,
                                     struct question * question,
                                     GtkWidget * question_box);

/** Value of answer field when there no answer have been set. */
#define DC_NO_ANSWER (-1)

/** Get the pending debconf answer in a thread-safe way.
 *
 * @param fe cdebconf frontend
 * @return the current answer
 */
int cdebconf_gtk_get_answer(struct frontend * fe);

/** Set the pending debconf answer in a thread-safe way.
 *
 * @param fe cdebconf frontend
 * @param answer the new answer value
 */
void cdebconf_gtk_set_answer(struct frontend * fe, int answer);

/** Set the pending debconf answer to DC_OK.
 *
 * This function is provided by convenience as a possible argument
 * for g_signal_connect_swapped().
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_set_answer_ok(struct frontend * fe);

/** Set the pending debconf answer to DC_NOTOK.
 *
 * This function is provided by convenience as a possible argument
 * for g_signal_connect_swapped().
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_set_answer_notok(struct frontend * fe);

/** Set the pending debconf answer to DC_GOBACK.
 *
 * This function is provided by convenience as a possible argument
 * for g_signal_connect_swapped().
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_set_answer_goback(struct frontend * fe);

/** Display help for the current question.
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_help(struct frontend * fe);

/** Force cdebconf to quit.
 *
 * This function is currently used when the main window is closed.
 *
 * It is implemented by sending a TERM signal to the current process,
 *
 * @param fe cdebconf frontend
 */
void cdebconf_gtk_force_quit(struct frontend * fe);

/** Add common question layout around a given widget and add it to the given
 * container.
 *
 * The common layout include the description and extended description of the
 * question as well as any icons that has been associated with the question
 * type.
 *
 * @param fe cdebconf frontend
 * @param question question being handled
 * @param container container in which the widgets will be added
 * @param widget question widgets
 */
void cdebconf_gtk_add_common_layout(struct frontend * fe,
                                    struct question * question,
                                    GtkWidget * container,
                                    GtkWidget * widget);

/** Pack the widget inside a vbox inside a hbox to center it.
 *
 * The given widget needs to be parentless before calling this function.
 *
 * The given pointer is updated to point to the surrounding box.
 *
 * @param widget pointer to the widget to be centered
 * @param horizontal_padding horizontal padding around the widget
 * @param vertical_padding vertical padding around the widget
 */
void cdebconf_gtk_center_widget(GtkWidget ** widget, guint horizontal_padding,
                                guint vertical_padding);

/** Pop-up a informational dialog to the user.
 *
 * @todo
 * This function should just wrap around a standard GtkDialog when used outside
 * the debian-installer.
 *
 * @param fe cdebconf frontend
 * @param title title of the dialog window
 * @param message message of the dialog window
 * @return FALSE if an error happened, TRUE otherwise
 */
gboolean cdebconf_gtk_run_message_dialog(struct frontend * fe,
                                         const gchar * title,
                                         const gchar * message);

/** Add a keyboard shortcut to the main window.
 *
 * The added keyboard shortcut is tied to a specific widget
 * (usually a button), and will be removed when the widget is destroyed.
 *
 * @param fe cdebconf frontend
 * @param widget a widget which "carries" the signal
 * @param key_event_handler a shortcut handler
 */
void cdebconf_gtk_add_global_key_handler(struct frontend * fe,
                                         GtkWidget * widget,
                                         GCallback key_event_handler);

/** Set sensitivity for all buttons in the main window.
 *
 * @param fe cdebconf frontend
 * @param sensitive FALSE for insensitive buttons, TRUE for sensitive ones
 */
void cdebconf_gtk_set_buttons_sensitive(struct frontend * fe,
                                        gboolean sensitive);

/** Add the given button to the main window.
 *
 * Buttons will be added to the action box with the same order than
 * calls to this functions.  The focus chain will be updated as well on
 * the same basis.
 *
 * @param fe cdebconf frontend
 * @param button the button to add
 */
void cdebconf_gtk_add_button(struct frontend * fe, GtkWidget * button);

/** Set the "secondary" propriety for the given button.
 *
 * A "secondary" button will be displayed at the other end of the action
 * box.  This is currently the case for the "Go Back" button.
 *
 * This is basically a wrapper around gtk_button_set_secondary().
 *
 * @param fe cdebconf frontend
 * @param button the button that will set
 * @param secondary TRUE if the button is "secondary", FALSE otherwise
 */
void cdebconf_gtk_set_button_secondary(struct frontend * fe,
                                       GtkWidget * button,
                                       gboolean secondary);

/** Create the "Continue" button.
 *
 * The button will be added to the main action box on top of being
 * returned.  This also a specific question handler to have more
 * control over the button during the input.
 *
 * To ease navigation, the "Continue" button will be first in the action box
 * focus chain.
 *
 * @param fe cdebconf frontend
 * @return the newly created "Continue" button
 */
GtkWidget * cdebconf_gtk_create_continue_button(struct frontend * fe);

#endif /* !_CDEBCONF_GTK_H_ */

/* vim: et sw=4 si
 */
