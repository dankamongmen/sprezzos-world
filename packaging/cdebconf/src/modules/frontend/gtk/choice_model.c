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

/** @file choice_model.c
 * wrapper around GtkTreeModel for choice based question handlers
 * @internal
 */

#include "choice_model.h"

#include <string.h>
#include <gtk/gtk.h>

#include "frontend.h"
#include "question.h"
#include "strutl.h"

/** Internal data structure used by cdebconf_gtk_choice_model_find_value().
 */
struct search_result {
    /** value searched in the model */
    const char * searched_value;

    /** path to the value when found, NULL otherwise */
    GtkTreePath * path;
};

/** GtkTreeModelForeachFunc used by cdebconf_gtk_choice_model_find_value().
 *
 * @param model searched model
 * @param path path of the current value
 * @param iter iter pointing to the current value
 * @param result search result structure
 * @return TRUE when the data has been found, FALSE otherwise
 */
static gboolean is_searched_value(GtkTreeModel * model, GtkTreePath * path,
                                  GtkTreeIter * iter,
                                  struct search_result * result)
{
    char * value;

    gtk_tree_model_get(model, iter,
                       /* column: */ CHOICE_MODEL_VALUE, &value,
                       -1 /* end of list */);
    if (0 == strcmp(value, result->searched_value)) {
        result->path = gtk_tree_path_copy(path);
    }
    g_free(value);
    return NULL != result->path;
}

/** Search for a given value in a model
 *
 * The allocated GtkTreePath must be freed by the caller.
 * @param model model to be searched
 * @param searched_value value to be found
 * @return the path to the value when found, NULL otherwise
 */
GtkTreePath * cdebconf_gtk_choice_model_find_value(GtkTreeModel * model,
                                                   const char * searched_value)
{
    struct search_result result;

    result.searched_value = searched_value;
    result.path = NULL;

    gtk_tree_model_foreach(model, (GtkTreeModelForeachFunc) is_searched_value,
                           &result);
    return result.path;
}

/** Full interface to create a tree model from a given question.
 *
 * @param fe cdebconf frontend
 * @param question select or multiselect question
 * @param is_parent helper determining if a choice is the parent of the next
 *                  choices
 * @return the newly created model or NULL in case of errors
 * @see parent_predicate
 */
GtkTreeModel * cdebconf_gtk_choice_model_create_full(
    struct frontend * fe, struct question * question,
    parent_predicate is_parent)
{
    GtkTreeStore * store;
    int count;
    int sorted_count;
    char * raw_indices;
    char * raw_choices;
    char * raw_translated_choices;
    int * sorted_indices;
    char ** choice_values;
    char ** choice_translated_values;
    char ** default_values;
    int default_count;
    int i;
    int sorted_index;
    GtkTreeIter iter;
    GtkTreeIter child;
    GtkTreePath * path;

    store = gtk_tree_store_new(CHOICE_MODEL_COLUMN_COUNT,
                               G_TYPE_BOOLEAN /* selected */,
                               G_TYPE_INT /* index */,
                               G_TYPE_STRING /* value */,
                               G_TYPE_STRING /* translated value */);
    if (NULL == store) {
        g_warning("gtk_tree_store_new failed.");
        return NULL;
    };

    raw_indices = q_get_indices(fe, question);
    raw_choices = q_get_choices_vals(fe, question);
    raw_translated_choices = q_get_choices(fe, question);
    count = strgetargc(raw_choices);
    g_assert(0 < count);
    /* check NULLs! */
    sorted_indices = g_malloc0(count * sizeof (int));
    choice_values = g_malloc0(count * sizeof (char *));
    choice_translated_values = g_malloc0(count * sizeof (char *));
    default_values = g_malloc0(count * sizeof (char *));

    /* XXX: strchoicesplitsort interface is really nasty... :(
     *      Maybe it would be better to re-implement something with the nifty
     *      glib functions... */
    sorted_count = strchoicesplitsort(raw_choices, raw_translated_choices,
                                      raw_indices, choice_values,
                                      choice_translated_values,
                                      sorted_indices, count);
    if (sorted_count != count) {
        store = NULL;
        goto exit;
    }

    default_count = strchoicesplit(question_getvalue(question, ""),
                                   default_values, count);
    g_assert(0 <= default_count);

    /* Populate model */
    for (i = 0; i < count; i++) {
        sorted_index = sorted_indices[i];
        g_assert(0 <= sorted_index && sorted_index < count);
        if (NULL == is_parent ||
            is_parent(sorted_indices[i], choice_values[sorted_index],
                      choice_translated_values[i])) {
            gtk_tree_store_append(store, &iter, NULL /* no parent */);
            gtk_tree_store_set(
                store, &iter,
                /* column: */ CHOICE_MODEL_SELECTED, FALSE,
                /* column: */ CHOICE_MODEL_INDEX, sorted_indices[i],
                /* column: */ CHOICE_MODEL_VALUE, choice_values[sorted_index],
                /* column: */ CHOICE_MODEL_TRANSLATED_VALUE,
                              choice_translated_values[i],
                -1 /* end of list */);
        } else {
            gtk_tree_store_append(store, &child, &iter);
            gtk_tree_store_set(
                store, &child,
                /* column: */ CHOICE_MODEL_SELECTED, FALSE,
                /* column: */ CHOICE_MODEL_INDEX, sorted_indices[i],
                /* column: */ CHOICE_MODEL_VALUE, choice_values[sorted_index],
                /* column: */ CHOICE_MODEL_TRANSLATED_VALUE,
                              choice_translated_values[i],
                -1 /* end of list */);
        }
    }

    /* Mark defaults as selected */
    for (i = 0; i < default_count; i++) {
        if (NULL != (path = cdebconf_gtk_choice_model_find_value(
                                GTK_TREE_MODEL(store), default_values[i]))) {
            if (gtk_tree_model_get_iter(GTK_TREE_MODEL(store), &iter, path)) {
                gtk_tree_store_set(store, &iter,
                                   /* column: */ CHOICE_MODEL_SELECTED, TRUE,
                                   -1 /* end of list */);
            }
            gtk_tree_path_free(path);
        }
    }

exit:
    g_free(default_values);
    g_free(sorted_indices);
    g_free(choice_values);
    g_free(choice_translated_values);
    g_free(raw_translated_choices);
    g_free(raw_choices);
    g_free(raw_indices);
    return GTK_TREE_MODEL(store);
}

/** Create a one level model for a given question.
 *
 * @param fe cdebconf frontend
 * @param question a select or multiselect question
 * @return a newly created model or NULL in case of errors
 */
GtkTreeModel * cdebconf_gtk_choice_model_create(struct frontend * fe,
                                                struct question * question)
{
    return cdebconf_gtk_choice_model_create_full(
        fe, question, NULL /* no parent predicate */);
}

/** GtkTreeModelForeachFunc used by cdebconf_gtk_choice_model_get_length().
 *
 * @param model traversed model
 * @param path path of the current value
 * @param iter iter pointing to the current value
 * @param length current known size of the model
 * @return FALSE in any case. :)
 */
static gboolean increment_model_length(GtkTreeModel * model,
                                       GtkTreePath * path,
                                       GtkTreeIter * iter, guint * length)
{
    *length = *length + 1;
    return FALSE; /* foreach should go on */
}

/** Get the total number of choices in the given model.
 *
 * @param model measured model
 * @return total number of choices in the given model
 */
guint cdebconf_gtk_choice_model_get_length(GtkTreeModel * model)
{
    guint length = 0;

    g_assert(NULL != model);

    gtk_tree_model_foreach(
        model, (GtkTreeModelForeachFunc) increment_model_length, &length);
    return length;
}

/** GtkTreeModelForeachFunc used by
 * cdebconf_gtk_choice_model_get_first_selected().
 *
 * @param model searched model
 * @param path path of the current value
 * @param iter iter pointing to the current value
 * @param result search result structure
 * @return TRUE when a selected choice has been found, FALSE otherwise
 */
static gboolean is_selected(GtkTreeModel * model, GtkTreePath * path,
                            GtkTreeIter * iter, GtkTreePath ** result)
{
    gboolean selected;

    gtk_tree_model_get(model, iter,
                       /* column: */ CHOICE_MODEL_SELECTED, &selected,
                       -1 /* end of list */);
    if (selected) {
        *result = gtk_tree_path_copy(path);
        return TRUE /* stop here */;
    }
    return FALSE;
}

/** Get the first selected choice in the given model.
 *
 * The returned path must be free'd by the caller.
 *
 * @param model searched model
 * @return path to the first selected value or NULL if no value is selected
 */
GtkTreePath * cdebconf_gtk_choice_model_get_first_selected(
    GtkTreeModel * model)
{
    GtkTreePath * result = NULL;

    gtk_tree_model_foreach(model, (GtkTreeModelForeachFunc) is_selected,
                           &result);
    return result;
}

/** Set a given row in the model.
 *
 * This function has the same interface than gtk_tree_store_set(),
 * and the same effect.  It is just here to abstract over the GtkTreeModel
 * implementation used for choice models.
 *
 * @param model the model to modify
 * @param iter position in the model
 */
void cdebconf_gtk_choice_model_set(GtkTreeModel * model,
                                   GtkTreeIter * iter, ...)
{
    va_list var_args;

    va_start(var_args, iter);
    gtk_tree_store_set_valist(GTK_TREE_STORE(model), iter, var_args);
    va_end(var_args);
}

/* vim: et sw=4 si
 */
