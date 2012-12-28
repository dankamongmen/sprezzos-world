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

/** @file ui.h
 * Common user interface related functions for the GTK+ frontend of cdebconf
 * (header)
 */

#ifndef _UI_H_
#define _UI_H_

#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

gboolean cdebconf_gtk_create_main_window(struct frontend * fe);

void cdebconf_gtk_destroy_main_window(struct frontend * fe);

void cdebconf_gtk_update_frontend_title(struct frontend * fe);

void cdebconf_gtk_show_buttons(struct frontend * fe);

void cdebconf_gtk_show_target_box(struct frontend * fe);

void cdebconf_gtk_hide_target_box(struct frontend * fe);

void cdebconf_gtk_empty_target_box(struct frontend * fe);

gint cdebconf_gtk_get_text_width(GtkWidget * widget, gchar * text);

#endif /* !_UI_H_ */

/* vim: et sw=4 si
 */
