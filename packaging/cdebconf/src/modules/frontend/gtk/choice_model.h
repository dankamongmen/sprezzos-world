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

/** @file choice_model.h
 * wrapper around GtkTreeModel for choice based question handlers (header)
 * @internal
 */

#ifndef _CHOICE_MODEL_H_
#define _CHOICE_MODEL_H_

#include <stdarg.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"

GtkTreeModel * cdebconf_gtk_choice_model_create(struct frontend * obj,
                                                struct question * question);

/** A function that determines if the given choice should be considered
 * as the parent of the next choices while constructing the model.
 *
 * @param index index of the examined choice
 * @param choice canonical value of the examined choice
 * @param translated_choice translated value of the examined choice
 * @return TRUE if the choice should be considered as a parent,
 *         FALSE otherwise
 */
typedef gboolean (* parent_predicate)(int index, const char * choice,
                                      const char * translated_choice);

GtkTreeModel * cdebconf_gtk_choice_model_create_full(
    struct frontend * obj, struct question * question,
    parent_predicate is_parent);

GtkTreePath * cdebconf_gtk_choice_model_find_value(
    GtkTreeModel * model, const char * searched_value);

GtkTreePath * cdebconf_gtk_choice_model_get_first_selected(
    GtkTreeModel * model);

guint cdebconf_gtk_choice_model_get_length(GtkTreeModel * model);

void cdebconf_gtk_choice_model_set(GtkTreeModel * model,
                                   GtkTreeIter * iter, ...);

/** Names for the columns in the model.
 */
enum {
    /* index of the choice */
    CHOICE_MODEL_INDEX,
    /* has the choice been selected */
    CHOICE_MODEL_SELECTED,
    /* canonical value of the choice */
    CHOICE_MODEL_VALUE,
    /* translated value of the choice */
    CHOICE_MODEL_TRANSLATED_VALUE,
    /* total number of columns in the model */
    CHOICE_MODEL_COLUMN_COUNT
};

#endif /* !_CHOICE_MODEL_H_ */

/* vim et sw=4 si
 */
