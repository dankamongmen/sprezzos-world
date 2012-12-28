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

/** @file handlers.h
 * question handlers of the GTK+ frontend (header)
 */

#ifndef _HANDLERS_H_
#define _HANDLERS_H_

#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

int cdebconf_gtk_handle_boolean(struct frontend * fe,
                                struct question * question,
                                GtkWidget * question_box);

int cdebconf_gtk_handle_note(struct frontend * fe,
                             struct question * question,
                             GtkWidget * question_box);

int cdebconf_gtk_handle_text(struct frontend * fe,
                             struct question * question,
                             GtkWidget * question_box);

int cdebconf_gtk_handle_password(struct frontend * fe,
                                 struct question * question,
                                 GtkWidget * question_box);

int cdebconf_gtk_handle_string(struct frontend * fe,
                               struct question * question,
                               GtkWidget * question_box);

#endif /* !_HANDLERS_H */
