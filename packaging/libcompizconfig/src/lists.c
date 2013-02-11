/*
 * Compiz configuration system library
 *
 * Copyright (C) 2007  Dennis Kasprzyk <onestone@opencompositing.org>
 * Copyright (C) 2007  Danny Baumann <maniac@opencompositing.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#include <ccs.h>

#include "ccs-private.h"

typedef void (*freeFunc) (void *ptr);

#define CCSLIST(type,dtype,ocomp,compfunc) \
    \
    CCS##type##List ccs##type##ListAppend (CCS##type##List list, dtype *data) \
    { \
	CCS##type##List l = list; \
	CCS##type##List ne = malloc(sizeof(struct _CCS##type##List)); \
	if (!ne) \
	    return list; \
	ne->data = data; \
	ne->next = NULL; \
	if (!list) \
	    return ne; \
	while (l->next) l = l->next; \
	l->next = ne; \
	return list; \
    } \
    \
    CCS##type##List ccs##type##ListPrepend (CCS##type##List list, dtype *data) \
    { \
	CCS##type##List ne = malloc(sizeof(struct _CCS##type##List)); \
	if (!ne) \
	    return list; \
	ne->data = data; \
	ne->next = list; \
	return ne; \
    } \
    \
    CCS##type##List ccs##type##ListInsert (CCS##type##List list, dtype *data, int position) \
    { \
	CCS##type##List l = list; \
	CCS##type##List ne = malloc(sizeof(struct _CCS##type##List)); \
	if (!ne) \
	    return list; \
	ne->data = data; \
	ne->next = list; \
	if (!list || !position) \
	    return ne; \
	position--; \
	while (l->next && position) \
	{ \
	    l = l->next; \
	    position--; \
	} \
	ne->next = l->next; \
	l->next = ne; \
	return list; \
    } \
    \
    CCS##type##List ccs##type##ListInsertBefore (CCS##type##List list, CCS##type##List sibling, dtype *data) \
    { \
	CCS##type##List l = list; \
	CCS##type##List ne = malloc(sizeof(struct _CCS##type##List)); \
	if (!ne) \
	    return list; \
	while (l && (l != sibling)) l = l->next; \
	ne->data = data; \
	ne->next = l; \
	return ne; \
    } \
    \
    unsigned int ccs##type##ListLength (CCS##type##List list) \
    { \
	unsigned int count = 0; \
	CCS##type##List l = list; \
	while (l) \
	{ \
	    l = l->next; \
	    count++; \
	} \
	return count; \
    } \
    \
    CCS##type##List ccs##type##ListFind (CCS##type##List list, dtype *data) \
    { \
	CCS##type##List l = list; \
	while (l) \
	{ \
	    if (!data && !l->data) break; \
	    if ((ocomp)?(compfunc):(memcmp(l->data, data, sizeof(dtype)) == 0)) break; \
	    l = l->next; \
	} \
	return l; \
    } \
    \
    CCS##type##List ccs##type##ListGetItem (CCS##type##List list, unsigned int index) \
    { \
	CCS##type##List l = list; \
	while (l && index) \
	{ \
	    l = l->next; \
	    index--; \
	} \
	return l; \
    } \
    \
    CCS##type##List ccs##type##ListRemove (CCS##type##List list, dtype *data, Bool freeObj) \
    { \
	CCS##type##List l = list; \
	CCS##type##List prev = NULL; \
	Bool           found = FALSE; \
	if (!data) \
	    return list; \
	while (l) \
	{ \
	    if (!l->data) continue; \
	    if ((ocomp)?(compfunc):(memcmp(l->data, data, sizeof(dtype)) == 0)) \
	    { \
		found = TRUE; \
		break; \
	    } \
	    prev = l; \
	    l = l->next; \
	} \
	if (!found) \
	    return list; \
	if (l) \
	{ \
	    if (prev) prev->next = l->next; \
	    else list = l->next; \
	    if (freeObj) \
		ccsFree##type (l->data); \
	    free (l); \
	} \
	return list; \
    } \
    \
    CCS##type##List ccs##type##ListFree (CCS##type##List list, Bool freeObj) \
    { \
	CCS##type##List l = list; \
	CCS##type##List le = NULL; \
	while (l) \
	{ \
	    le = l; \
	    l = l->next; \
	    if (freeObj) \
		ccsFree##type (le->data); \
	    free(le); \
	} \
	return NULL; \
    }

CCSLIST (Plugin, CCSPlugin, FALSE, 0)
CCSLIST (Setting, CCSSetting, FALSE, 0)
CCSLIST (String, char, TRUE, strcmp (data, l->data) == 0)
CCSLIST (Group, CCSGroup, FALSE, 0)
CCSLIST (SubGroup, CCSSubGroup, FALSE, 0)
CCSLIST (SettingValue, CCSSettingValue, FALSE, 0)
CCSLIST (PluginConflict, CCSPluginConflict, FALSE, 0)
CCSLIST (BackendInfo, CCSBackendInfo, FALSE, 0)
CCSLIST (IntDesc, CCSIntDesc, FALSE, 0)
CCSLIST (StrRestriction, CCSStrRestriction, FALSE, 0)
CCSLIST (StrExtension, CCSStrExtension, FALSE, 0)

CCSSettingValueList ccsGetValueListFromStringList (CCSStringList list,
						   CCSSetting *parent)
{
    CCSSettingValueList rv = NULL;

    while (list)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return rv;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asString = strdup (list->data);
	rv = ccsSettingValueListAppend (rv, value);
	list = list->next;
    }

    return rv;
}

CCSStringList ccsGetStringListFromValueList (CCSSettingValueList list)
{
    CCSStringList rv = NULL;

    while (list)
    {
	rv = ccsStringListAppend (rv, strdup (list->data->value.asString) );
	list = list->next;
    }

    return rv;
}

char ** ccsGetStringArrayFromList (CCSStringList list, int *num)
{
    char ** rv = NULL;
    int length = ccsStringListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (char *));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = strdup (list->data);

    *num = length;

    return rv;
}

CCSStringList ccsGetListFromStringArray (char ** array, int num)
{
    CCSStringList rv = NULL;
    int i;

    for (i = 0; i < num; i++)
	rv = ccsStringListAppend (rv, strdup (array[i]) );

    return rv;
}

char ** ccsGetStringArrayFromValueList (CCSSettingValueList list, int *num)
{
    char ** rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (char *));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = strdup (list->data->value.asString);

    *num = length;

    return rv;
}

char ** ccsGetMatchArrayFromValueList (CCSSettingValueList list, int *num)
{
    char ** rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (char *));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = strdup (list->data->value.asMatch);

    *num = length;

    return rv;
}

int * ccsGetIntArrayFromValueList (CCSSettingValueList list, int *num)
{
    int * rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (int));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = list->data->value.asInt;

    *num = length;

    return rv;
}

float * ccsGetFloatArrayFromValueList (CCSSettingValueList list, int *num)
{
    float * rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (float));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = list->data->value.asFloat;

    *num = length;

    return rv;
}

Bool * ccsGetBoolArrayFromValueList (CCSSettingValueList list, int *num)
{
    Bool * rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (Bool));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	rv[i] = list->data->value.asBool;

    *num = length;

    return rv;
}

CCSSettingColorValue * ccsGetColorArrayFromValueList (CCSSettingValueList list,
						      int *num)
{
    CCSSettingColorValue * rv = NULL;
    int length = ccsSettingValueListLength (list);
    int i;

    if (length)
    {
	rv = calloc (length, sizeof (CCSSettingColorValue));
	if (!rv)
	    return NULL;
    }

    for (i = 0; i < length; i++, list = list->next)
	memcpy (&rv[i], &list->data->value.asColor,
		sizeof (CCSSettingColorValue));

    *num = length;

    return rv;
}

CCSSettingValueList ccsGetValueListFromStringArray (char ** array, int num,
						    CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asString = strdup (array[i]);
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

CCSSettingValueList ccsGetValueListFromMatchArray (char ** array, int num,
						   CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asMatch = strdup (array[i]);
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

CCSSettingValueList ccsGetValueListFromIntArray (int * array, int num,
						 CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asInt = array[i];
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

CCSSettingValueList ccsGetValueListFromFloatArray (float * array, int num,
						   CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asFloat = array[i];
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

CCSSettingValueList ccsGetValueListFromBoolArray (Bool * array, int num,
						  CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asBool = array[i];
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

CCSSettingValueList ccsGetValueListFromColorArray (CCSSettingColorValue * array,
						   int num, CCSSetting *parent)
{
    CCSSettingValueList l = NULL;
    int i;

    for (i = 0; i < num; i++)
    {
	CCSSettingValue *value = calloc (1, sizeof (CCSSettingValue));
	if (!value)
	    return l;

	value->isListChild = TRUE;
	value->parent = parent;
	value->value.asColor = array[i];
	l = ccsSettingValueListAppend (l, value);
    }

    return l;
}

