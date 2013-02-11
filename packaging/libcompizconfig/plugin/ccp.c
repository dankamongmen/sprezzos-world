/*
 * Compiz configuration system library plugin
 *
 * Copyright (C) 2007  Dennis Kasprzyk <onestone@opencompositing.org>
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

#ifdef HAVE_CONFIG_H
#  include "../config.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <compiz-core.h>

#include <ccs.h>

static int corePrivateIndex;
static CompMetadata ccpMetadata;

typedef struct _CCPCore
{
    CCSContext *context;
    Bool applyingSettings;

    CompTimeoutHandle timeoutHandle;
    CompTimeoutHandle reloadHandle;

    InitPluginForObjectProc initPluginForObject;
    SetOptionForPluginProc  setOptionForPlugin;
}
CCPCore;

#define GET_CCP_CORE(c)				      \
    ((CCPCore *) (c)->base.privates[corePrivateIndex].ptr)

#define CCP_CORE(c)		     \
    CCPCore *cc = GET_CCP_CORE (c)

#define CCP_UPDATE_MIN_TIMEOUT 250
#define CCP_UPDATE_MAX_TIMEOUT 4000
#define CORE_VTABLE_NAME  "core"

static void
ccpSetValueToValue (CompObject      *object,
		    CCSSettingValue *sv,
		    CompOptionValue *v,
		    CCSSettingType  type)
{
    switch (type)
    {
    case TypeInt:
	v->i = sv->value.asInt;
	break;
    case TypeFloat:
	v->f = sv->value.asFloat;
	break;
    case TypeBool:
	v->b = sv->value.asBool;
	break;
    case TypeColor:
	{
	    int i;

	    for (i = 0; i < 4; i++)
		v->c[i] = sv->value.asColor.array.array[i];
	}
	break;
    case TypeString:
	v->s = strdup (sv->value.asString);
	break;
    case TypeMatch:
	matchInit (&v->match);
	matchAddFromString (&v->match, sv->value.asMatch);
	break;
    case TypeKey:
	{
	    CompDisplay *d;

	    while (object && object->type != COMP_OBJECT_TYPE_DISPLAY)
		object = object->parent;

	    if (!object)
		return;

	    d = GET_CORE_DISPLAY (object);

	    v->action.key.keycode =
		(sv->value.asKey.keysym != NoSymbol) ?
		XKeysymToKeycode (d->display, sv->value.asKey.keysym) : 0;

	    v->action.key.modifiers = sv->value.asKey.keyModMask;

	    if (v->action.key.keycode || v->action.key.modifiers)
		v->action.type = CompBindingTypeKey;
	    else
		v->action.type = CompBindingTypeNone;
	}
	break;
    case TypeButton:
	{
	    v->action.button.button = sv->value.asButton.button;
	    v->action.button.modifiers = sv->value.asButton.buttonModMask;
	    v->action.edgeMask = sv->value.asButton.edgeMask;

	    if (v->action.button.button || v->action.button.modifiers)
	    {
		if (sv->value.asButton.edgeMask)
		    v->action.type = CompBindingTypeEdgeButton;
		else
		    v->action.type = CompBindingTypeButton;
	    }
	    else
		v->action.type = CompBindingTypeNone;
	}
	break;
    case TypeEdge:
	{
	    v->action.edgeMask = sv->value.asEdge;
	}
	break;
    case TypeBell:
	{
	    v->action.bell = sv->value.asBell;
	}
	break;
    default:
	break;
    }
}

static CompOptionType
ccpCCSTypeToCompizType (CCSSettingType st, CompOptionType *ct)
{
    switch (st) {
    case TypeBool:
	*ct = CompOptionTypeBool;
	break;
    case TypeInt:
	*ct = CompOptionTypeInt;
	break;
    case TypeFloat:
	*ct = CompOptionTypeFloat;
	break;
    case TypeColor:
	*ct = CompOptionTypeColor;
	break;
    case TypeString:
	*ct = CompOptionTypeString;
	break;
    case TypeMatch:
	*ct = CompOptionTypeMatch;
	break;
    case TypeKey:
	*ct = CompOptionTypeKey;
	break;
    case TypeButton:
	*ct = CompOptionTypeButton;
	break;
    case TypeEdge:
	*ct = CompOptionTypeEdge;
	break;
    case TypeBell:
	*ct = CompOptionTypeBell;
	break;
    case TypeList:
	*ct = CompOptionTypeList;
	break;
    default:
	return FALSE;
    }

    return TRUE;
}

static void
ccpConvertPluginList (CCSSetting          *s,
		      CCSSettingValueList list,
		      CompOptionValue     *v)
{
    CCSStringList       sl, l;
    int                 i;

    sl = ccsGetStringListFromValueList (list);

    while (ccsStringListFind(sl,"ccp"))
	sl = ccsStringListRemove (sl, "ccp", TRUE);

    while (ccsStringListFind(sl,"core"))
	sl = ccsStringListRemove (sl, "core", TRUE);

    sl = ccsStringListPrepend (sl, strdup ("ccp"));
    sl = ccsStringListPrepend (sl, strdup ("core"));

    v->list.nValue = ccsStringListLength (sl);
    v->list.value  = calloc (v->list.nValue, sizeof (CompOptionValue));
    if (!v->list.value)
    {
	v->list.nValue = 0;
	return;
    }

    for (l = sl, i = 0; l; l = l->next)
    {
	if (l->data)
	    v->list.value[i].s = strdup (l->data);
	i++;
    }

    ccsStringListFree (sl, TRUE);
}

static void
ccpSettingToValue (CompObject      *object,
		   CCSSetting      *s,
		   CompOptionValue *v)
{
    if (s->type != TypeList)
	ccpSetValueToValue (object, s->value, v, s->type);
    else
    {
	CCSSettingValueList list;
	int                 i = 0;

	ccsGetList (s, &list);

	if (!ccpCCSTypeToCompizType (s->info.forList.listType, &v->list.type))
	    v->list.type = CompOptionTypeBool;

	if ((strcmp (s->name, "active_plugins") == 0) &&
	    (strcmp (s->parent->name, CORE_VTABLE_NAME) == 0))
	{
	    ccpConvertPluginList (s, list, v);
	}
	else
	{
    	    v->list.nValue = ccsSettingValueListLength (list);
    	    v->list.value  = calloc (v->list.nValue, sizeof (CompOptionValue));

    	    while (list)
    	    {
    		ccpSetValueToValue (object, list->data,
    				    &v->list.value[i],
				    s->info.forList.listType);
		list = list->next;
		i++;
	    }
	}
    }
}

static void
ccpInitValue (CompObject      *object,
	      CCSSettingValue *value,
	      CompOptionValue *from,
	      CCSSettingType  type)
{
    switch (type)
    {
    case TypeInt:
	value->value.asInt = from->i;
	break;
    case TypeFloat:
	value->value.asFloat = from->f;
	break;
    case TypeBool:
	value->value.asBool = from->b;
	break;
    case TypeColor:
	{
	    int i;

	    for (i = 0; i < 4; i++)
		value->value.asColor.array.array[i] = from->c[i];
	}
	break;
    case TypeString:
	value->value.asString = strdup (from->s);
	break;
    case TypeMatch:
	value->value.asMatch = matchToString (&from->match);
	break;
    case TypeKey:
	if (from->action.type & CompBindingTypeKey)
	{
	    CompDisplay *d;

	    while (object && object->type != COMP_OBJECT_TYPE_DISPLAY)
		object = object->parent;

	    if (!object)
		return;

	    d = GET_CORE_DISPLAY (object);

	    value->value.asKey.keysym =
		XKeycodeToKeysym (d->display, from->action.key.keycode, 0);
	    value->value.asKey.keyModMask = from->action.key.modifiers;
	}
	else
	{
	    value->value.asKey.keysym = 0;
	    value->value.asKey.keyModMask = 0;
	}
    case TypeButton:
	if (from->action.type & CompBindingTypeButton)
	{
	    value->value.asButton.button = from->action.button.button;
	    value->value.asButton.buttonModMask =
		from->action.button.modifiers;
	    value->value.asButton.edgeMask = 0;
	}
	else if (from->action.type & CompBindingTypeEdgeButton)
	{
	    value->value.asButton.button = from->action.button.button;
	    value->value.asButton.buttonModMask =
		from->action.button.modifiers;
	    value->value.asButton.edgeMask = from->action.edgeMask;
	}
	else
	{
	    value->value.asButton.button = 0;
	    value->value.asButton.buttonModMask = 0;
	    value->value.asButton.edgeMask = 0;
	}
	break;
    case TypeEdge:
	value->value.asEdge = from->action.edgeMask;
	break;
    case TypeBell:
	value->value.asBell = from->action.bell;
	break;
    default:
	break;
    }
}

static void
ccpValueToSetting (CompObject      *object,
		   CCSSetting      *s,
		   CompOptionValue *v)
{
    CCSSettingValue *value;

    value = calloc (1, sizeof (CCSSettingValue));
    if (!value)
	return;

    value->parent = s;

    if (s->type == TypeList)
    {
	int i;

	for (i = 0; i < v->list.nValue; i++)
	{
	    CCSSettingValue *val;

	    val = calloc (1, sizeof (CCSSettingValue));
	    if (val)
	    {
		val->parent = s;
		val->isListChild = TRUE;
		ccpInitValue (object, val, &v->list.value[i],
			      s->info.forList.listType);
		value->value.asList =
		    ccsSettingValueListAppend (value->value.asList, val);
	    }
	}
    }
    else
	ccpInitValue (object, value, v, s->type);

    ccsSetValue (s, value);
    ccsFreeSettingValue (value);
}

static Bool
ccpTypeCheck (CCSSetting *s, CompOption *o)
{
    CompOptionType ot;

    switch (s->type)
    {
    case TypeList:
	return ccpCCSTypeToCompizType (s->type, &ot) && (ot == o->type) &&
	       ccpCCSTypeToCompizType (s->info.forList.listType, &ot) &&
	       (ot == o->value.list.type);
	break;
    default:
	return ccpCCSTypeToCompizType (s->type, &ot) && (ot == o->type);
	break;
    }

    return FALSE;
}

static void
ccpSetOptionFromContext (CompObject *object,
			 CompOption *o,
			 const char *plugin)
{
    CCP_CORE (&core);

    CCSPlugin       *bsp;
    CCSSetting      *setting;
    CompOptionValue value;

    Bool screen = (object->type == COMP_OBJECT_TYPE_SCREEN);
    int  screenNum = 0;

    /* we currently only support screen and display opton types */
    if (object->type != COMP_OBJECT_TYPE_SCREEN &&
	object->type != COMP_OBJECT_TYPE_DISPLAY)
	return;
    
    if (screen)
    {
	char *name = compObjectName (object);
	if (name)
	{
	    screenNum = atoi (name);
	    free (name);
	}
    }
    
    bsp = ccsFindPlugin (cc->context, (plugin) ? plugin : CORE_VTABLE_NAME);
    if (!bsp)
	return;

    setting = ccsFindSetting (bsp, o->name, screen, screenNum);
    if (!setting)
	return;

    if (!ccpTypeCheck (setting, o))
	return;

    compInitOptionValue (&value);
    ccpSettingToValue (object, setting, &value);

    cc->applyingSettings = TRUE;
    (*core.setOptionForPlugin) (object, plugin, o->name, &value);
    cc->applyingSettings = FALSE;

    compFiniOptionValue (&value, o->type);
}

static void
ccpSetContextFromOption (CompObject *object,
			 CompOption *o,
			 const char *plugin)
{
    CCP_CORE (&core);

    CCSPlugin       *bsp;
    CCSSetting      *setting;

    Bool screen = (object->type == COMP_OBJECT_TYPE_SCREEN);
    int  screenNum = 0;

    /* we currently only support screen and display opton types */
    if (object->type != COMP_OBJECT_TYPE_SCREEN &&
	object->type != COMP_OBJECT_TYPE_DISPLAY)
	return;
    
    if (screen)
    {
	char *name = compObjectName (object);
	if (name)
	{
	    screenNum = atoi (name);
	    free (name);
	}
    }
    
    bsp = ccsFindPlugin (cc->context, (plugin) ? plugin : CORE_VTABLE_NAME);
    if (!bsp)
	return;

    setting = ccsFindSetting (bsp, o->name, screen, screenNum);
    if (!setting)
	return;

    if (!ccpTypeCheck (setting, o))
	return;

    ccpValueToSetting (object, setting, &o->value);
    ccsWriteChangedSettings (cc->context);
}


static CompBool
ccpReloadObjectTree (CompObject *object,
		     void       *closure);

static CompBool
ccpReloadObjectsWithType (CompObjectType type,
			  CompObject     *parent,
			  void	         *closure)
{
    compObjectForEach (parent, type, ccpReloadObjectTree, closure);

    return TRUE;
}

static CompBool
ccpReloadObjectTree (CompObject *object,
		     void       *closure)
{
    CompPlugin *p = (CompPlugin *) closure;
    CompOption *option;
    int	       nOption;

    option = (*p->vTable->getObjectOptions) (p, object, &nOption);
    while (nOption--)
	ccpSetOptionFromContext (object, option++, p->vTable->name);

    compObjectForEachType (object, ccpReloadObjectsWithType, closure);

    return TRUE;
}

static Bool
ccpReload (void *closure)
{
    CompPlugin *p;

    CCP_CORE (&core);

    for (p = getPlugins (); p; p = p->next)
    {
	if (!p->vTable->getObjectOptions)
	    continue;

	ccpReloadObjectTree (&core.base, (void *) p);
    }
    cc->reloadHandle = 0;

    return FALSE;
}

static Bool
ccpTimeout (void *closure)
{
    unsigned int flags = 0;

    CCP_CORE (&core);

    if (findActivePlugin ("glib"))
	flags |= ProcessEventsNoGlibMainLoopMask;

    ccsProcessEvents (cc->context, flags);

    if (ccsSettingListLength (cc->context->changedSettings))
    {
	CCSSettingList list = cc->context->changedSettings;
	CCSSettingList l = list;	
	CCSSetting     *s;
	CompObject     *object;
    	CompPlugin     *p;
    	CompOption     *option;
    	int            nOption;

    	char tmp[256];

	cc->context->changedSettings = NULL;
	
	while (l)
	{
	    s = l->data;
	    l = l->next;

	    if (s->isScreen)
	    {
		snprintf (tmp, 256, "%d", s->screenNum);
		object = compObjectFind (&core.base, COMP_OBJECT_TYPE_DISPLAY,
					 NULL);
		object = compObjectFind (object, COMP_OBJECT_TYPE_SCREEN,
					 tmp);
	    }
	    else
	    {
		object = compObjectFind (&core.base, COMP_OBJECT_TYPE_DISPLAY,
					 NULL);
	    }

	    if (!object)
		continue;

	    p = findActivePlugin (s->parent->name);

	    if (!p)
		continue;

	    option = (*p->vTable->getObjectOptions) (p, object, &nOption);
	    option = compFindOption (option, nOption, s->name, 0);
	    if (option)
		ccpSetOptionFromContext (object, option, s->parent->name);
	    D (D_FULL, "Setting Update \"%s\"\n", s->name);
	}

	ccsSettingListFree (list, FALSE);
	cc->context->changedSettings =
	    ccsSettingListFree (cc->context->changedSettings, FALSE);
    }

    return TRUE;
}

static CompBool
ccpSetOptionForPlugin (CompObject      *object,
		       const char      *plugin,
		       const char      *name,
		       CompOptionValue *value)
{
    CompBool status;

    CCP_CORE (&core);

    UNWRAP (cc, &core, setOptionForPlugin);
    status = (*core.setOptionForPlugin) (object, plugin, name, value);
    WRAP (cc, &core, setOptionForPlugin, ccpSetOptionForPlugin);

    if (status && !cc->applyingSettings && !cc->reloadHandle)
    {
	CompPlugin *p;

	p = findActivePlugin (plugin);
	if (p && p->vTable->getObjectOptions)
	{
	    CompOption *option;
	    int	       nOption;

	    option = (*p->vTable->getObjectOptions) (p, object, &nOption);
	    option = compFindOption (option, nOption, name, 0);
	    if (option)
		ccpSetContextFromOption (object, option, p->vTable->name);
	}
    }

    return status;
}

static CompBool
ccpInitPluginForObject (CompPlugin *p,
			CompObject *o)
{
    CompBool status;

    CCP_CORE (&core);

    UNWRAP (cc, &core, initPluginForObject);
    status = (*core.initPluginForObject) (p, o);
    WRAP (cc, &core, initPluginForObject, ccpInitPluginForObject);

    if (status && p->vTable->getObjectOptions)
    {
	CompOption *option;
	int	   nOption;

	option = (*p->vTable->getObjectOptions) (p, o, &nOption);
	while (nOption--)
	    ccpSetOptionFromContext (o, option++, p->vTable->name);
    }

    return status;
}

static Bool
ccpInitCore (CompPlugin *p,
	     CompCore   *c)
{
    CCPCore *cc;
    CompObject  *o;

    int i;
    unsigned int *screens;

    if (!checkPluginABI ("core", CORE_ABIVERSION))
	return FALSE;

    cc = malloc (sizeof (CCPCore));
    if (!cc)
	return FALSE;

    ccsSetBasicMetadata (TRUE);

    o = compObjectFind (&core.base, COMP_OBJECT_TYPE_DISPLAY, NULL);

    if (o)
    {
	CompScreen *s;
	CORE_DISPLAY (o);
	
	for (s = d->screens, i = 0; s; s = s->next, i++);

	screens = calloc (i, sizeof (unsigned int));
	if (!screens)
	{
	    free (cc);
	    return FALSE;
	}

	for (s = d->screens, i = 0; s; s = s->next)
	    screens[i++] = s->screenNum;

	cc->context = ccsContextNew (screens, i);

	free (screens);
    }
    else
	cc->context = ccsContextNew (NULL, 0);

    if (!cc->context)
    {
	free (cc);
	return FALSE;
    }

    ccsReadSettings (cc->context);

    cc->context->changedSettings =
	ccsSettingListFree (cc->context->changedSettings, FALSE);

    cc->applyingSettings = FALSE;

    cc->reloadHandle = compAddTimeout (0, 0, ccpReload, 0);
    cc->timeoutHandle = compAddTimeout (CCP_UPDATE_MIN_TIMEOUT,
					CCP_UPDATE_MAX_TIMEOUT,
					ccpTimeout, 0);

    core.base.privates[corePrivateIndex].ptr = cc;

    WRAP (cc, c, initPluginForObject, ccpInitPluginForObject);
    WRAP (cc, c, setOptionForPlugin, ccpSetOptionForPlugin);

    return TRUE;
}

static void
ccpFiniCore (CompPlugin *p,
	     CompCore   *c)
{
    CCP_CORE (&core);

    UNWRAP (cc, c, initPluginForObject);
    UNWRAP (cc, c, setOptionForPlugin);

    compRemoveTimeout (cc->timeoutHandle);

    ccsContextDestroy (cc->context);

    free (cc);
}

static CompBool
ccpInitObject (CompPlugin *p,
	       CompObject *o)
{
    static InitPluginObjectProc dispTab[] = {
	(InitPluginObjectProc) ccpInitCore,
    };

    RETURN_DISPATCH (o, dispTab, ARRAY_SIZE (dispTab), TRUE, (p, o));
}

static void
ccpFiniObject (CompPlugin *p,
	       CompObject *o)
{
    static FiniPluginObjectProc dispTab[] = {
	(FiniPluginObjectProc) ccpFiniCore,
    };

    DISPATCH (o, dispTab, ARRAY_SIZE (dispTab), (p, o));
}


static Bool
ccpInit (CompPlugin *p)
{
    if (!compInitPluginMetadataFromInfo (&ccpMetadata, p->vTable->name,
					 0, 0, 0, 0))
	return FALSE;

    corePrivateIndex = allocateCorePrivateIndex ();
    if (corePrivateIndex < 0)
    {
	compFiniMetadata (&ccpMetadata);
	return FALSE;
    }

    compAddMetadataFromFile (&ccpMetadata, p->vTable->name);

    return TRUE;
}

static void
ccpFini (CompPlugin *p)
{
    freeCorePrivateIndex (corePrivateIndex);
    compFiniMetadata (&ccpMetadata);
}

static CompMetadata*
ccpGetMetadata (CompPlugin *plugin)
{
    return &ccpMetadata;
}

CompPluginVTable ccpVTable = {
    "ccp",
    ccpGetMetadata,
    ccpInit,
    ccpFini,
    ccpInitObject,
    ccpFiniObject,
    0,
    0
};

CompPluginVTable *
getCompPluginInfo20070830 (void)
{
    return &ccpVTable;
}
