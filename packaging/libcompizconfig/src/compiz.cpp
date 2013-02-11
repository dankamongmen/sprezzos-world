/*
 * Compiz configuration system library
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

#ifdef USE_PROTOBUF
#include "compizconfig.pb.h"
#include <google/protobuf/io/zero_copy_stream_impl.h>
#endif

extern "C"
{
#ifdef HAVE_CONFIG_H
#  include "../config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>

#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>

#include <locale.h>

#include <compiz-core.h>
#include <ccs.h>
#include "ccs-private.h"
}

extern int xmlLoadExtDtdDefaultValue;


#ifdef USE_PROTOBUF

Bool usingProtobuf = TRUE;

#define PB_ABI_VERSION 20090314

typedef metadata::PluginInfo PluginInfoMetadata;
typedef metadata::PluginBrief PluginBriefMetadata;
typedef metadata::Plugin PluginMetadata;

typedef PluginInfoMetadata::Dependencies DependenciesMetadata;
typedef PluginMetadata::Screen ScreenMetadata;
typedef PluginMetadata::Option OptionMetadata;
typedef PluginMetadata::Extension ExtensionMetadata;
typedef OptionMetadata::GenericValue GenericValueMetadata;

typedef google::protobuf::RepeatedPtrField< std::string > StringList;

PluginBriefMetadata persistentPluginBriefPB;
PluginMetadata persistentPluginPB; // Made global so that it gets reused,
					 // for better performance (to avoid
					 // mem alloc/free for each plugin)

std::string metadataCacheDir = "";

static char *
getLocale ()
{
    char *lang = getenv ("LANG");

    if (!lang || !strlen (lang))
	lang = getenv ("LC_ALL");

    if (!lang || !strlen (lang))
	lang = getenv ("LC_MESSAGES");

    return lang ? lang : (char *)"";
}

std::string curLocale = std::string (getLocale ());
std::string shortLocale = curLocale.find ('.') == std::string::npos ?
    curLocale : curLocale.substr (0, curLocale.find ('.'));

#endif

static void
ccsAddRestrictionToStringInfo (CCSSettingStringInfo *forString,
			       const char *name,
			       const char *value)
{
    CCSStrRestriction *restriction;

    restriction = (CCSStrRestriction *) calloc (1, sizeof (CCSStrRestriction));
    if (restriction)
    {
	restriction->name = strdup (name);
	restriction->value = strdup (value);
	forString->restriction =
	    ccsStrRestrictionListAppend (forString->restriction,
					 restriction);
    }
}

static void
ccsAddRestrictionToStringExtension (CCSStrExtension *extension,
				    const char *name,
				    const char *value)
{
    CCSStrRestriction *restriction;

    restriction = (CCSStrRestriction *) calloc (1, sizeof (CCSStrRestriction));
    if (restriction)
    {
	restriction->name = strdup (name);
	restriction->value = strdup (value);
	extension->restriction =
	    ccsStrRestrictionListAppend (extension->restriction, restriction);
    }
}


#ifdef USE_PROTOBUF

static void
initBoolValuePB (CCSSettingValue * v,
		 const GenericValueMetadata & value)
{
    v->value.asBool = FALSE;

    if (value.has_bool_value ())
    {
	v->value.asBool = value.bool_value ();
    }
}

static void
initIntValuePB (CCSSettingValue * v,
		CCSSettingInfo * i,
		const GenericValueMetadata & value)
{
    v->value.asInt = (i->forInt.min + i->forInt.max) / 2;

    if (value.has_int_value ())
    {
	int val = value.int_value ();
	if (val >= i->forInt.min && val <= i->forInt.max)
	    v->value.asInt = val;
    }
}

static void
initFloatValuePB (CCSSettingValue * v,
		  CCSSettingInfo * i,
		  const GenericValueMetadata & value)
{
    v->value.asFloat = (i->forFloat.min + i->forFloat.max) / 2;

    if (value.has_float_value ())
    {
	float val = value.float_value ();
	if (val >= i->forFloat.min && val <= i->forFloat.max)
	    v->value.asFloat = val;
    }
}

static void
initStringValuePB (CCSSettingValue * v,
		   CCSSettingInfo * i,
		   const GenericValueMetadata & value)
{
    free (v->value.asString);

    if (value.has_string_value ())
	v->value.asString = strdup (value.string_value ().c_str ());
    else
	v->value.asString = strdup ("");
}

static void
initColorValuePB (CCSSettingValue * v,
		  const GenericValueMetadata & value)
{
    memset (&v->value.asColor, 0, sizeof (v->value.asColor));
    v->value.asColor.color.alpha = 0xffff;

    if (!value.has_color_value ())
	return;

    const OptionMetadata::ColorValue &val = value.color_value ();

    if (val.has_red ())
    {
	int color = strtol ((char *) val.red ().c_str (), NULL, 0);

	v->value.asColor.color.red = MAX (0, MIN (0xffff, color));
    }

    if (val.has_green ())
    {
	int color = strtol ((char *) val.green ().c_str (), NULL, 0);

	v->value.asColor.color.green = MAX (0, MIN (0xffff, color));
    }

    if (val.has_blue ())
    {
	int color = strtol ((char *) val.blue ().c_str (), NULL, 0);

	v->value.asColor.color.blue = MAX (0, MIN (0xffff, color));
    }

    if (val.has_alpha ())
    {
	int color = strtol ((char *) val.alpha ().c_str (), NULL, 0);

	v->value.asColor.color.alpha = MAX (0, MIN (0xffff, color));
    }
}

static void
initMatchValuePB (CCSSettingValue * v,
		  const GenericValueMetadata & value)
{
    free (v->value.asMatch);

    if (value.has_string_value ())
	v->value.asMatch = strdup (value.string_value ().c_str ());
    else
	v->value.asMatch = strdup ("");
}

static void
initKeyValuePB (CCSSettingValue * v,
		CCSSettingInfo * i,
		const GenericValueMetadata & value)
{
    memset (&v->value.asKey, 0, sizeof (v->value.asKey));

    if (value.has_string_value ())
    {
	const char * val = value.string_value ().c_str ();

	if (strcasecmp (val, "disabled"))
	{
	    ccsStringToKeyBinding (val, &v->value.asKey);
	}
    }
}

static void
initButtonValuePB (CCSSettingValue * v,
		   CCSSettingInfo * i,
		   const GenericValueMetadata & value)
{
    memset (&v->value.asButton, 0, sizeof (v->value.asButton));

    if (value.has_string_value ())
    {
	const char * val = value.string_value ().c_str ();

	if (strcasecmp (val, "disabled"))
	{
	    ccsStringToButtonBinding (val, &v->value.asButton);
	}
    }
}

static void
initEdgeValuePB (CCSSettingValue * v,
		 CCSSettingInfo * i,
		 const GenericValueMetadata & value)
{
    v->value.asEdge = 0;

    if (value.has_edge_value ())
	v->value.asEdge = value.edge_value ();
}

static void
initBellValuePB (CCSSettingValue * v,
		 CCSSettingInfo * i,
		 const GenericValueMetadata & value)
{
    v->value.asBell = FALSE;

    if (value.has_bool_value ())
	v->value.asBell = value.bool_value ();
}

static void
initListValuePB (CCSSettingValue * v,
		 CCSSettingInfo * i,
		 const OptionMetadata & option)
{
    int num, j;

    num = option.default_value_size ();

    if (num)
    {
	for (j = 0; j < num; j++)
	{
	    CCSSettingValue *val;
	    val = (CCSSettingValue *) calloc (1, sizeof (CCSSettingValue));
	    if (!val)
		continue;

	    val->parent = v->parent;
	    val->isListChild = TRUE;

	    switch (i->forList.listType)
	    {
	    case TypeBool:
		initBoolValuePB (val, option.default_value (j));
		break;
	    case TypeInt:
		initIntValuePB (val, i->forList.listInfo,
				option.default_value (j));
		break;
	    case TypeFloat:
		initFloatValuePB (val, i->forList.listInfo,
				  option.default_value (j));
		break;
	    case TypeString:
		initStringValuePB (val, i->forList.listInfo,
				   option.default_value (j));
		break;
	    case TypeColor:
		initColorValuePB (val, option.default_value (j));
		break;
	    case TypeKey:
		initKeyValuePB (val, i->forList.listInfo,
				option.default_value (j));
		break;
	    case TypeButton:
		initButtonValuePB (val, i->forList.listInfo,
				   option.default_value (j));
		break;
	    case TypeEdge:
		initEdgeValuePB (val, i->forList.listInfo,
				 option.default_value (j));
		break;
	    case TypeBell:
		initBellValuePB (val, i->forList.listInfo,
				 option.default_value (j));
		break;
	    case TypeMatch:
		initMatchValuePB (val, option.default_value (j));
	    default:
		break;
	    }
	    v->value.asList = ccsSettingValueListAppend (v->value.asList, val);
	}
    }
}

static void
initIntInfoPB (CCSSettingInfo * i, const OptionMetadata & option)
{
    int num, j;
    i->forInt.min = MINSHORT;
    i->forInt.max = MAXSHORT;
    i->forInt.desc = NULL;

    if (option.has_int_min ())
	i->forInt.min = option.int_min ();

    if (option.has_int_max ())
	i->forInt.max = option.int_max ();

    if (!basicMetadata)
    {
	num = option.int_desc_size ();
	for (j = 0; j < num; j++)
	{
	    const OptionMetadata::IntDescription & intDescMetadata =
		option.int_desc (j);

	    int val = intDescMetadata.value ();

	    if (val >= i->forInt.min && val <= i->forInt.max)
	    {
		CCSIntDesc *intDesc;

		intDesc = (CCSIntDesc *) calloc (1, sizeof (CCSIntDesc));
		if (intDesc)
		{
		    intDesc->name = strdup (intDescMetadata.name ().c_str ());
		    intDesc->value = val;
		    i->forInt.desc =
			ccsIntDescListAppend (i->forInt.desc, intDesc);
		}
	    }
	}
    }
}

static void
initFloatInfoPB (CCSSettingInfo * i, const OptionMetadata & option)
{
    i->forFloat.min = MINSHORT;
    i->forFloat.max = MAXSHORT;
    i->forFloat.precision = 0.1f;

    if (option.has_float_min ())
	i->forFloat.min = option.float_min ();

    if (option.has_float_max ())
	i->forFloat.max = option.float_max ();

    if (option.precision ())
	i->forFloat.precision = option.precision ();
}

static void
initStringInfoPB (CCSSettingInfo * i, const OptionMetadata & option)
{
    int num, j;
    i->forString.restriction = NULL;
    i->forString.sortStartsAt = -1;
    i->forString.extensible = FALSE;

    if (!basicMetadata)
    {
	if (option.has_extensible () && option.extensible ())
	    i->forString.extensible = TRUE;

	if (option.has_sort_start ())
	    i->forString.sortStartsAt = option.sort_start ();

	num = option.str_restriction_size ();
	for (j = 0; j < num; j++)
	{
	    const OptionMetadata::StringRestriction &
		restrictionMetadata = option.str_restriction (j);

	    const char *value = restrictionMetadata.value ().c_str ();
	    const char *name = restrictionMetadata.name ().c_str ();

	    ccsAddRestrictionToStringInfo (&i->forString, value, name);
	}
    }
}

static void
initListInfoPB (CCSSettingInfo * i, const OptionMetadata & option)
{
    CCSSettingInfo *info;

    i->forList.listType = TypeBool;
    i->forList.listInfo = NULL;

    if (option.has_list_type ())
    {
	i->forList.listType = (CCSSettingType) option.list_type ();
    }
    switch (i->forList.listType)
    {
    case TypeInt:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initIntInfoPB (info, option);
	    i->forList.listInfo = info;
	}
	break;
    case TypeFloat:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initFloatInfoPB (info, option);
	    i->forList.listInfo = info;
	}
	break;
    case TypeString:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initStringInfoPB (info, option);
	    i->forList.listInfo = info;
	}
	break;
    default:
	break;
    }
}

static void
initActionInfoPB (CCSSettingInfo * i, const OptionMetadata & option)
{
    i->forAction.internal = FALSE;

    if (option.has_internal () && option.internal ())
	i->forAction.internal = TRUE;
}

static void
addOptionForPluginPB (CCSPlugin * plugin,
		      const char * name,
		      Bool isScreen,
		      unsigned int screen,
		      const StringList & groups,
		      const StringList & subgroups,
		      const OptionMetadata & option)
{
    CCSSetting *setting;

    if (ccsFindSetting (plugin, name, isScreen, screen))
    {
	fprintf (stderr, "[ERROR]: Option \"%s\" already defined\n", name);
	return;
    }

    setting = (CCSSetting *) calloc (1, sizeof (CCSSetting));
    if (!setting)
	return;

    setting->parent = plugin;
    setting->isScreen = isScreen;
    setting->screenNum = screen;
    setting->isDefault = TRUE;
    setting->name = strdup (name);

    if (!basicMetadata)
    {
	setting->shortDesc =
	    strdup (option.has_short_desc () ?
		    option.short_desc ().c_str () :
		    name);
	setting->longDesc =
	    strdup (option.has_long_desc () ?
		    option.long_desc ().c_str () :
		    name);
	setting->hints = strdup (option.has_hints () ?
				 option.hints ().c_str () :
				 name);
	setting->group =
	    strdup (option.group_id () >= 0 ?
		    groups.Get (option.group_id ()).c_str () :
		    "");
	setting->subGroup =
	    strdup (option.subgroup_id () >= 0 ?
		    subgroups.Get (option.subgroup_id ()).c_str () :
		    "");
    }
    else
    {
	setting->shortDesc = strdup (name);
	setting->longDesc  = strdup ("");
	setting->hints     = strdup ("");
	setting->group     = strdup ("");
	setting->subGroup  = strdup ("");
    }

    setting->type = (CCSSettingType) option.type ();
    setting->value = &setting->defaultValue;
    setting->defaultValue.parent = setting;

    switch (setting->type)
    {
    case TypeInt:
	initIntInfoPB (&setting->info, option);
	break;
    case TypeFloat:
	initFloatInfoPB (&setting->info, option);
	break;
    case TypeString:
	initStringInfoPB (&setting->info, option);
	break;
    case TypeList:
	initListInfoPB (&setting->info, option);
	break;
    case TypeKey:
    case TypeButton:
    case TypeEdge:
    case TypeBell:
	initActionInfoPB (&setting->info, option);
	break;
    case TypeAction: // do nothing and fall through
    default:
	break;
    }

    if (option.default_value_size () > 0)
    {
	switch (setting->type)
	{
	case TypeInt:
	    initIntValuePB (&setting->defaultValue, &setting->info,
			    option.default_value (0));
	    break;
	case TypeBool:
	    initBoolValuePB (&setting->defaultValue, option.default_value (0));
	    break;
	case TypeFloat:
	    initFloatValuePB (&setting->defaultValue, &setting->info,
			      option.default_value (0));
	    break;
	case TypeString:
	    initStringValuePB (&setting->defaultValue, &setting->info,
			       option.default_value (0));
	    break;
	case TypeColor:
	    initColorValuePB (&setting->defaultValue, option.default_value (0));
	    break;
	case TypeKey:
	    initKeyValuePB (&setting->defaultValue, &setting->info,
			    option.default_value (0));
	    break;
	case TypeButton:
	    initButtonValuePB (&setting->defaultValue, &setting->info,
			       option.default_value (0));
	    break;
	case TypeEdge:
	    initEdgeValuePB (&setting->defaultValue, &setting->info,
			     option.default_value (0));
	    break;
	case TypeBell:
	    initBellValuePB (&setting->defaultValue, &setting->info,
			     option.default_value (0));
	    break;
	case TypeMatch:
	    initMatchValuePB (&setting->defaultValue,
			      option.default_value (0));
	    break;
	case TypeList:
	    initListValuePB (&setting->defaultValue, &setting->info,
			     option);
	    break;
	case TypeAction: // do nothing and fall through
	default:
	    break;
	}
    }
    else
    {
	/* if we have no set defaults, we have at least to set
	   the string defaults to empty strings */
	switch (setting->type)
	{
	case TypeString:
	    setting->defaultValue.value.asString = strdup ("");
	    break;
	case TypeMatch:
	    setting->defaultValue.value.asMatch = strdup ("");
	    break;
	default:
	    break;
	}
    }

    PLUGIN_PRIV (plugin);

    pPrivate->settings = ccsSettingListAppend (pPrivate->settings, setting);
}

static void
addOptionFromPB (CCSPlugin * plugin,
		 Bool isScreen,
		 const StringList & groups,
		 const StringList & subgroups,
		 const OptionMetadata & option)
{
    const char *name;
    Bool readonly = FALSE;

    name = option.name ().c_str ();

    readonly = option.has_read_only () && option.read_only ();

    if (!strlen (name) || readonly)
	return;

    if (isScreen)
    {
	for (unsigned i = 0; i < plugin->context->numScreens; i++)
	    addOptionForPluginPB (plugin, name, TRUE,
				  plugin->context->screens[i],
				  groups, subgroups, option);
    }
    else
	addOptionForPluginPB (plugin, name, FALSE, 0, groups, subgroups, option);
}

static void
initOptionsFromPB (CCSPlugin * plugin,
		   const PluginMetadata & pluginPB)
{
    int numOpt, i;

    if (pluginPB.has_display ())
    {
	const ScreenMetadata &displayPB = pluginPB.display ();

	// Display options
	numOpt = displayPB.option_size ();
	for (i = 0; i < numOpt; i++)
	    addOptionFromPB (plugin, FALSE,
			     displayPB.group_desc (),
			     displayPB.subgroup_desc (),
			     displayPB.option (i));
    }

    if (pluginPB.has_screen ())
    {
	const ScreenMetadata &screenPB = pluginPB.screen ();

	// Screen options
	numOpt = screenPB.option_size ();
	for (i = 0; i < numOpt; i++)
	    addOptionFromPB (plugin, TRUE,
			     screenPB.group_desc (),
			     screenPB.subgroup_desc (),
			     screenPB.option (i));
    }
}

static void
addStringsFromPB (CCSStringList * list,
		  const StringList & strings)
{
    StringList::const_iterator it;

    for (it = strings.begin (); it != strings.end (); it++)
    {
	const char *value = (*it).c_str ();

	if (strlen (value))
	    *list = ccsStringListAppend (*list, strdup (value));
    }
}

static void
addStringExtensionFromPB (CCSPlugin * plugin,
			  const ExtensionMetadata & extensionPB)
{
    int j;
    CCSStrExtension *extension;

    extension = (CCSStrExtension *) calloc (1, sizeof (CCSStrExtension));
    if (!extension)
	return;

    extension->isScreen = !(extensionPB.has_display () &&
			    extensionPB.display ());

    extension->restriction = NULL;

    extension->basePlugin = strdup (extensionPB.base_plugin ().c_str ());

    addStringsFromPB (&extension->baseSettings,
		      extensionPB.base_option ());

    int numRestrictions = extensionPB.str_restriction_size ();
    if (!numRestrictions)
    {
	free (extension);
	return;
    }

    for (j = 0; j < numRestrictions; j++)
    {
	const OptionMetadata::StringRestriction & restrictionPB =
	    extensionPB.str_restriction (j);

	const char *value = restrictionPB.value ().c_str ();
	const char *name  = restrictionPB.name ().c_str ();

	ccsAddRestrictionToStringExtension (extension, name, value);
    }

    PLUGIN_PRIV (plugin);

    pPrivate->stringExtensions =
	ccsStrExtensionListAppend (pPrivate->stringExtensions, extension);
}

static void
initStringExtensionsFromPB (CCSPlugin * plugin,
			    const PluginMetadata & pluginPB)
{
    int numExtensions, i;

    numExtensions = pluginPB.extension_size ();
    for (i = 0; i < numExtensions; i++)
	addStringExtensionFromPB (plugin, pluginPB.extension (i));
}

static void
initRulesFromPB (CCSPlugin * plugin, const PluginInfoMetadata & pluginInfoPB)
{
    addStringsFromPB (&plugin->providesFeature, pluginInfoPB.feature ());

    if (!pluginInfoPB.has_deps ())
	return;

    const DependenciesMetadata & deps = pluginInfoPB.deps ();

    addStringsFromPB (&plugin->loadAfter, deps.after_plugin ());
    addStringsFromPB (&plugin->loadBefore, deps.before_plugin ());
    addStringsFromPB (&plugin->requiresPlugin, deps.require_plugin ());
    addStringsFromPB (&plugin->requiresFeature, deps.require_feature ());
    addStringsFromPB (&plugin->conflictPlugin, deps.conflict_plugin ());
    addStringsFromPB (&plugin->conflictFeature, deps.conflict_feature ());
}

static void
addPluginFromPB (CCSContext * context,
		 const PluginInfoMetadata & pluginInfoPB,
		 char *file,
		 char *xmlFile)
{
    const char *name;
    CCSPlugin *plugin;
    CCSPluginPrivate *pPrivate;

    name = pluginInfoPB.name ().c_str ();

    if (!strlen (name))
	return;

    if (ccsFindPlugin (context, name))
	return;

    if (!strcmp (name, "ini") || !strcmp (name, "gconf") ||
	!strcmp (name, "ccp") || !strcmp (name, "kconfig"))
	return;

    plugin = (CCSPlugin *) calloc (1, sizeof (CCSPlugin));
    if (!plugin)
	return;

    pPrivate = (CCSPluginPrivate *) calloc (1, sizeof (CCSPluginPrivate));
    if (!pPrivate)
    {
	free (plugin);
	return;
    }
    pPrivate->loaded = FALSE;

    plugin->ccsPrivate = (void *) pPrivate;

    if (file)
	pPrivate->pbFilePath = strdup (file);

    if (xmlFile)
    {
	pPrivate->xmlFile = strdup (xmlFile);
	asprintf (&pPrivate->xmlPath, "/compiz/plugin[@name = '%s']", name);
    }

    plugin->context = context;
    plugin->name = strdup (name);

    if (!basicMetadata)
    {
	plugin->shortDesc =
	    strdup (pluginInfoPB.has_short_desc () ?
		    pluginInfoPB.short_desc ().c_str () :
		    name);
	plugin->longDesc =
	    strdup (pluginInfoPB.has_long_desc () ?
		    pluginInfoPB.long_desc ().c_str () :
		    name);
	plugin->category = strdup (pluginInfoPB.has_category () ?
				   pluginInfoPB.category ().c_str () :
				   "");
    }
    else
    {
	plugin->shortDesc = strdup (name);
	plugin->longDesc  = strdup (name);
	plugin->category  = strdup ("");
    }

    initRulesFromPB (plugin, pluginInfoPB);

    context->plugins = ccsPluginListAppend (context->plugins, plugin);
}

static void
addCoreSettingsFromPB (CCSContext * context,
		       const PluginInfoMetadata & pluginInfoPB,
		       char *file,
		       char *xmlFile)
{
    CCSPlugin *plugin;
    CCSPluginPrivate *pPrivate;

    if (ccsFindPlugin (context, "core"))
	return;

    plugin = (CCSPlugin*) calloc (1, sizeof (CCSPlugin));
    if (!plugin)
	return;

    pPrivate = (CCSPluginPrivate *) calloc (1, sizeof (CCSPluginPrivate));
    if (!pPrivate)
    {
	free (plugin);
	return;
    }

    plugin->ccsPrivate = (void *) pPrivate;

    if (file)
	pPrivate->pbFilePath = strdup (file);

    if (xmlFile)
    {
	pPrivate->xmlFile = strdup (xmlFile);
	pPrivate->xmlPath = strdup ("/compiz/core");
    }

    plugin->context = context;
    plugin->name = strdup ("core");
    plugin->category = strdup ("General");

    if (!basicMetadata)
    {
	plugin->shortDesc =
	    strdup (pluginInfoPB.has_short_desc () ?
		    pluginInfoPB.short_desc ().c_str () :
		    "General Options");

	    plugin->longDesc =
	    strdup (pluginInfoPB.has_long_desc () ?
		    pluginInfoPB.long_desc ().c_str () :
		    "General Compiz Options");
    }
    else
    {
	plugin->shortDesc = strdup ("General Options");
	plugin->longDesc  = strdup ("General Compiz Options");
    }

    initRulesFromPB (plugin, pluginInfoPB);
    context->plugins = ccsPluginListAppend (context->plugins, plugin);
}

#endif


static int
pluginNameFilter (const struct dirent *name)
{
    int length = strlen (name->d_name);

    if (length < 7)
	return 0;

    if (strncmp (name->d_name, "lib", 3) ||
	strncmp (name->d_name + length - 3, ".so", 3))
	return 0;

    return 1;
}

static int
pluginXMLFilter (const struct dirent *name)
{
    int length = strlen (name->d_name);

    if (length < 5)
	return 0;

    if (strncmp (name->d_name + length - 4, ".xml", 4))
	return 0;

    return 1;
}


/* XML parsing */

static CCSSettingType
getOptionType (const char *name)
{
    static struct _TypeMap
    {
	const char *name;
	CCSSettingType type;
    } map[] = {
	{ "bool", TypeBool },
	{ "int", TypeInt },
	{ "float", TypeFloat },
	{ "string", TypeString },
	{ "color", TypeColor },
	{ "action", TypeAction },
	{ "key", TypeKey },
	{ "button", TypeButton },
	{ "edge", TypeEdge },
	{ "bell", TypeBell },
	{ "match", TypeMatch },
	{ "list", TypeList }
    };
    for (unsigned i = 0; i < sizeof (map) / sizeof (map[0]); i++)
	if (strcasecmp (name, map[i].name) == 0)
	    return map[i].type;

    return TypeNum;
}

static char *
getStringFromXPath (xmlDoc * doc, xmlNode * base, const char *path)
{
    xmlXPathObjectPtr xpathObj;
    xmlXPathContextPtr xpathCtx;
    char *rv = NULL;

    xpathCtx = xmlXPathNewContext (doc);
    if (!xpathCtx)
	return NULL;

    if (base)
	xpathCtx->node = base;

    xpathObj = xmlXPathEvalExpression (BAD_CAST path, xpathCtx);

    if (!xpathObj)
    {
	xmlXPathFreeContext (xpathCtx);
	return NULL;
    }

    xpathObj = xmlXPathConvertString (xpathObj);

    if (xpathObj->type == XPATH_STRING && xpathObj->stringval
	&& strlen ((char *) xpathObj->stringval))
    {
	rv = strdup ((char *) xpathObj->stringval);
    }

    xmlXPathFreeObject (xpathObj);
    xmlXPathFreeContext (xpathCtx);
    return rv;
}

static xmlNode **
getNodesFromXPath (xmlDoc * doc, xmlNode * base, const char *path, int *num)
{
    xmlXPathObjectPtr xpathObj;
    xmlXPathContextPtr xpathCtx;
    xmlNode **rv = NULL;
    int size;
    int i;

    *num = 0;

    xpathCtx = xmlXPathNewContext (doc);
    if (!xpathCtx)
	return NULL;

    if (base)
	xpathCtx->node = base;

    xpathObj = xmlXPathEvalExpression (BAD_CAST path, xpathCtx);
    if (!xpathObj)
    {
	xmlXPathFreeContext (xpathCtx);
	return NULL;
    }

    size = (xpathObj->nodesetval) ? xpathObj->nodesetval->nodeNr : 0;
    if (!size)
    {
	xmlXPathFreeObject (xpathObj);
	xmlXPathFreeContext (xpathCtx);
	return NULL;
    }

    rv = (xmlNode **) malloc (size * sizeof (xmlNode *));
    if (!rv)
    {
	xmlXPathFreeObject (xpathObj);
	xmlXPathFreeContext (xpathCtx);
	return NULL;
    }
    *num = size;

    for (i = 0; i < size; i++)
	rv[i] = xpathObj->nodesetval->nodeTab[i];

    xmlXPathFreeObject (xpathObj);
    xmlXPathFreeContext (xpathCtx);

    return rv;
}

static Bool
nodeExists (xmlNode * node, const char *path)
{
    xmlNode **nodes = NULL;
    int num;
    nodes = getNodesFromXPath (node->doc, node, path, &num);

    if (num)
    {
	free (nodes);
	return TRUE;
    }

    return FALSE;
}

static char *
stringFromNodeDef (xmlNode * node, const char *path, const char *def)
{
    char *val;
    char *rv = NULL;

    val = getStringFromXPath (node->doc, node, path);

    if (val)
    {
	rv = strdup (val);
	free (val);
    }
    else if (def)
	rv = strdup (def);

    return rv;
}

static char *
stringFromNodeDefTrans (xmlNode * node, const char *path, const char *def)
{
    char *lang;
    char newPath[1024];
    char *rv = NULL;

    lang = getenv ("LANG");

    if (!lang || !strlen (lang))
	lang = getenv ("LC_ALL");

    if (!lang || !strlen (lang))
	lang = getenv ("LC_MESSAGES");

    if (!lang || !strlen (lang))
	return stringFromNodeDef (node, path, def);

    snprintf (newPath, 1023, "%s[lang('%s')]", path, lang);
    rv = stringFromNodeDef (node, newPath, NULL);
    if (rv)
	return rv;

    snprintf (newPath, 1023, "%s[lang(substring-before('%s','.'))]", path, lang);
    rv = stringFromNodeDef (node, newPath, NULL);
    if (rv)
	return rv;

    snprintf (newPath, 1023, "%s[lang(substring-before('%s','_'))]", path, lang);
    rv = stringFromNodeDef (node, newPath, NULL);
    if (rv)
	return rv;

    snprintf (newPath, 1023, "%s[lang('C')]", path);
    rv = stringFromNodeDef (node, newPath, NULL);
    if (rv)
	return rv;

    return stringFromNodeDef (node, path, def);
}

static void
initBoolValue (CCSSettingValue * v,
	       xmlNode * node,
	       void * valuePBv)
{
    char *value;

    v->value.asBool = FALSE;

    value = getStringFromXPath (node->doc, node, "child::text()");

    if (value)
    {
	if (strcasecmp ((char *) value, "true") == 0)
	{
	    v->value.asBool = TRUE;
#ifdef USE_PROTOBUF
	    if (valuePBv)
		((GenericValueMetadata *) valuePBv)->set_bool_value (TRUE);
#endif
	}
	free (value);
    }
}

static void
initIntValue (CCSSettingValue * v,
	      CCSSettingInfo * i,
	      xmlNode * node,
	      void * valuePBv)
{
    char *value;

    v->value.asInt = (i->forInt.min + i->forInt.max) / 2;

    value = getStringFromXPath (node->doc, node, "child::text()");

    if (value)
    {
	int val = strtol ((char *) value, NULL, 0);

	if (val >= i->forInt.min && val <= i->forInt.max)
	{
	    v->value.asInt = val;
#ifdef USE_PROTOBUF
	    if (valuePBv)
		((GenericValueMetadata *) valuePBv)->set_int_value (val);
#endif
	}

	free (value);
    }
}

static void
initFloatValue (CCSSettingValue * v,
		CCSSettingInfo * i,
		xmlNode * node,
		void * valuePBv)
{
    char *value;
    char *loc;

    v->value.asFloat = (i->forFloat.min + i->forFloat.max) / 2;

    loc = setlocale (LC_NUMERIC, NULL);
    setlocale (LC_NUMERIC, "C");
    value = getStringFromXPath (node->doc, node, "child::text()");

    if (value)
    {
	float val = strtod ((char *) value, NULL);

	if (val >= i->forFloat.min && val <= i->forFloat.max)
	{
	    v->value.asFloat = val;
#ifdef USE_PROTOBUF
	    if (valuePBv)
		((GenericValueMetadata *) valuePBv)->set_float_value (val);
#endif
	}

	free (value);
    }

    setlocale (LC_NUMERIC, loc);
}

static void
initStringValue (CCSSettingValue * v,
		 CCSSettingInfo * i,
		 xmlNode * node,
		 void * valuePBv)
{
    char *value;

    value = getStringFromXPath (node->doc, node, "child::text()");

    if (value)
    {
	free (v->value.asString);
	v->value.asString = strdup (value);

#ifdef USE_PROTOBUF
	if (valuePBv)
	    ((GenericValueMetadata *) valuePBv)->set_string_value (value);
#endif
	free (value);
    }
    else
	v->value.asString = strdup ("");
}

static void
initColorValue (CCSSettingValue * v, xmlNode * node, void * valuePBv)
{
    char *value;

    memset (&v->value.asColor, 0, sizeof (v->value.asColor));
    v->value.asColor.color.alpha = 0xffff;

#ifdef USE_PROTOBUF
    OptionMetadata::ColorValue *colorPB = NULL;
    if (valuePBv)
	colorPB = ((GenericValueMetadata *) valuePBv)->mutable_color_value ();
#endif

    value = getStringFromXPath (node->doc, node, "red/child::text()");
    if (value)
    {
	int color = strtol ((char *) value, NULL, 0);

	v->value.asColor.color.red = MAX (0, MIN (0xffff, color));
#ifdef USE_PROTOBUF
	if (colorPB)
	    colorPB->set_red (value);
#endif
	free (value);
    }

    value = getStringFromXPath (node->doc, node, "green/child::text()");
    if (value)
    {
	int color = strtol ((char *) value, NULL, 0);

	v->value.asColor.color.green = MAX (0, MIN (0xffff, color));
#ifdef USE_PROTOBUF
	if (colorPB)
	    colorPB->set_green (value);
#endif
	free (value);
    }

    value = getStringFromXPath (node->doc, node, "blue/child::text()");
    if (value)
    {
	int color = strtol ((char *) value, NULL, 0);

	v->value.asColor.color.blue = MAX (0, MIN (0xffff, color));
#ifdef USE_PROTOBUF
	if (colorPB)
	    colorPB->set_blue (value);
#endif
	free (value);
    }

    value = getStringFromXPath (node->doc, node, "alpha/child::text()");
    if (value)
    {
	int color = strtol (value, NULL, 0);

	v->value.asColor.color.alpha = MAX (0, MIN (0xffff, color));
#ifdef USE_PROTOBUF
	if (colorPB)
	    colorPB->set_alpha (value);
#endif
	free (value);
    }
}

static void
initMatchValue (CCSSettingValue * v, xmlNode * node, void * valuePBv)
{
    char *value;

    value = getStringFromXPath (node->doc, node, "child::text()");
    if (value)
    {
	free (v->value.asMatch);
	v->value.asMatch = strdup (value);

#ifdef USE_PROTOBUF
	if (valuePBv)
	    ((GenericValueMetadata *) valuePBv)->set_string_value (value);
#endif
	free (value);
    }
    else
	v->value.asMatch = strdup ("");
}

static void
initKeyValue (CCSSettingValue * v,
	      CCSSettingInfo * i,
	      xmlNode * node,
	      void * valuePBv)
{
    char *value;

    memset (&v->value.asKey, 0, sizeof (v->value.asKey));

    value = getStringFromXPath (node->doc, node, "child::text()");
    if (value)
    {
#ifdef USE_PROTOBUF
	if (valuePBv)
	    ((GenericValueMetadata *) valuePBv)->set_string_value (value);
#endif
	if (strcasecmp (value, "disabled"))
	{
	    ccsStringToKeyBinding (value, &v->value.asKey);
	}
	free (value);
    }
}

static void
initButtonValue (CCSSettingValue * v,
		 CCSSettingInfo * i,
		 xmlNode * node,
		 void * valuePBv)
{
    char *value;

    memset (&v->value.asButton, 0, sizeof (v->value.asButton));

    value = getStringFromXPath (node->doc, node, "child::text()");
    if (value)
    {
#ifdef USE_PROTOBUF
	if (valuePBv)
	    ((GenericValueMetadata *) valuePBv)->set_string_value (value);
#endif
	if (strcasecmp (value, "disabled"))
	{
	    ccsStringToButtonBinding (value, &v->value.asButton);
	}
	free (value);
    }
}

static void
initEdgeValue (CCSSettingValue * v,
	       CCSSettingInfo * i,
	       xmlNode * node,
	       void * valuePBv)
{
    xmlNode **nodes;
    char *value;
    int k, num;

    v->value.asEdge = 0;

    static const char *edge[] = {
	"Left",
	"Right",
	"Top",
	"Bottom",
	"TopLeft",
	"TopRight",
	"BottomLeft",
	"BottomRight"
    };

    nodes = getNodesFromXPath (node->doc, node, "edge", &num);

    for (k = 0; k < num; k++)
    {
	value = getStringFromXPath (node->doc, nodes[k], "@name");
	if (value)
	{
	    for (unsigned j = 0; j < sizeof (edge) / sizeof (edge[0]); j++)
	    {
		if (strcasecmp ((char *) value, edge[j]) == 0)
		    v->value.asEdge |= (1 << j);
	    }
	    free (value);
	}
    }
    if (num)
	free (nodes);

#ifdef USE_PROTOBUF
    if (valuePBv)
	((GenericValueMetadata *) valuePBv)->set_edge_value (v->value.asEdge);
#endif
}

static void
initBellValue (CCSSettingValue * v,
	       CCSSettingInfo * i,
	       xmlNode * node,
	       void * valuePBv)
{
    char *value;

    v->value.asBell = FALSE;

    value = getStringFromXPath (node->doc, node, "child::text()");
    if (value)
    {
	if (!strcasecmp (value, "true"))
	{
	    v->value.asBell = TRUE;
#ifdef USE_PROTOBUF
	    if (valuePBv)
		((GenericValueMetadata *) valuePBv)->set_bool_value (TRUE);
#endif
	}
	free (value);
    }
}

static void
initListValue (CCSSettingValue * v,
	       CCSSettingInfo * i,
	       xmlNode * node,
	       void * optionPBv)
{
    xmlNode **nodes;
    int num, j;

    nodes = getNodesFromXPath (node->doc, node, "value", &num);
    if (num)
    {
	for (j = 0; j < num; j++)
	{
	    void *valuePBv = NULL;
#ifdef USE_PROTOBUF
	    if (optionPBv)
		valuePBv = ((OptionMetadata *) optionPBv)->add_default_value ();
#endif
	    CCSSettingValue *val;
	    val = (CCSSettingValue *) calloc (1, sizeof (CCSSettingValue));
	    if (!val)
		continue;

	    val->parent = v->parent;
	    val->isListChild = TRUE;

	    switch (i->forList.listType)
	    {
	    case TypeBool:
		initBoolValue (val, nodes[j], valuePBv);
		break;
	    case TypeInt:
		initIntValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeFloat:
		initFloatValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeString:
		initStringValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeColor:
		initColorValue (val, nodes[j], valuePBv);
		break;
	    case TypeKey:
		initKeyValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeButton:
		initButtonValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeEdge:
		initEdgeValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeBell:
		initBellValue (val, i->forList.listInfo, nodes[j], valuePBv);
		break;
	    case TypeMatch:
		initMatchValue (val, nodes[j], valuePBv);
	    default:
		break;
	    }
	    v->value.asList = ccsSettingValueListAppend (v->value.asList, val);
	}
	free (nodes);
    }
}

static void
initIntInfo (CCSSettingInfo * i, xmlNode * node, void * optionPBv)
{
    xmlNode **nodes;
    char *name;
    char *value;
    int num, j;
    i->forInt.min = MINSHORT;
    i->forInt.max = MAXSHORT;
    i->forInt.desc = NULL;

    value = getStringFromXPath (node->doc, node, "min/child::text()");
    if (value)
    {
	int val = strtol (value, NULL, 0);
	i->forInt.min = val;
	free (value);
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_int_min (val);
#endif
    }

    value = getStringFromXPath (node->doc, node, "max/child::text()");
    if (value)
    {
	int val = strtol (value, NULL, 0);
	i->forInt.max = val;
	free (value);
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_int_max (val);
#endif
    }

    if (!basicMetadata)
    {
	nodes = getNodesFromXPath (node->doc, node, "desc", &num);
	if (num)
	{
	    for (j = 0; j < num; j++)
	    {
		value = getStringFromXPath (node->doc, nodes[j],
					    "value/child::text()");
		if (value)
		{
		    int val = strtol (value, NULL, 0);
		    free (value);

		    if (val >= i->forInt.min && val <= i->forInt.max)
		    {
			name = stringFromNodeDefTrans (nodes[j],
						       "name/child::text()",
						       NULL);
			if (name)
			{
			    CCSIntDesc *intDesc;

			    intDesc = (CCSIntDesc *) calloc (1, sizeof (CCSIntDesc));
			    if (intDesc)
			    {
				intDesc->name = strdup (name);
				intDesc->value = val;
				i->forInt.desc =
				    ccsIntDescListAppend (i->forInt.desc,
							  intDesc);
#ifdef USE_PROTOBUF
				if (optionPBv)
				{
				    OptionMetadata::IntDescription *intDescPB =
					((OptionMetadata *) optionPBv)->
					add_int_desc ();
				    intDescPB->set_value (val);
				    intDescPB->set_name (name);
				}
#endif
			    }
			    free (name);
			}
		    }
		}
	    }
	    free (nodes);
	}
    }
}

static void
initFloatInfo (CCSSettingInfo * i, xmlNode * node, void * optionPBv)
{
    char *value;
    char *loc;

    i->forFloat.min = MINSHORT;
    i->forFloat.max = MAXSHORT;
    i->forFloat.precision = 0.1f;

    loc = setlocale (LC_NUMERIC, NULL);
    setlocale (LC_NUMERIC, "C");
    value = getStringFromXPath (node->doc, node, "min/child::text()");
    if (value)
    {
	float val = strtod (value, NULL);
	i->forFloat.min = val;
	free (value);
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_float_min (val);
#endif
    }

    value = getStringFromXPath (node->doc, node, "max/child::text()");
    if (value)
    {
	float val = strtod (value, NULL);
	i->forFloat.max = val;
	free (value);
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_float_max (val);
#endif
    }

    value = getStringFromXPath (node->doc, node, "precision/child::text()");
    if (value)
    {
	float val = strtod (value, NULL);
	i->forFloat.precision = val;
	free (value);
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_precision (val);
#endif
    }

    setlocale (LC_NUMERIC, loc);
}

static void
initStringInfo (CCSSettingInfo * i, xmlNode * node, void * optionPBv)
{
    xmlNode **nodes;
    char *name;
    char *value;
    int num, j;
    i->forString.restriction = NULL;
    i->forString.sortStartsAt = -1;
    i->forString.extensible = FALSE;

    if (!basicMetadata)
    {
	if (nodeExists (node, "extensible"))
	{
	    i->forString.extensible = TRUE;
#ifdef USE_PROTOBUF
	    if (optionPBv)
		((OptionMetadata *) optionPBv)->set_extensible (TRUE);
#endif
	}

	nodes = getNodesFromXPath (node->doc, node, "sort", &num);
	if (num)
	{
	    int val = 0; /* Start sorting at 0 unless otherwise specified. */

	    value = getStringFromXPath (node->doc, nodes[0], "@start");
	    if (value)
	    {
		/* Custom starting value specified. */
		val = strtol (value, NULL, 0);
		if (val < 0)
		    val = 0;
		free (value);
	    }
	    i->forString.sortStartsAt = val;
#ifdef USE_PROTOBUF
	    if (optionPBv)
		((OptionMetadata *) optionPBv)->set_sort_start (val);
#endif
	    free (nodes);
	}

	nodes = getNodesFromXPath (node->doc, node, "restriction", &num);
	if (num)
	{
	    for (j = 0; j < num; j++)
	    {
#ifdef USE_PROTOBUF
		OptionMetadata::StringRestriction * strRestrictionPB = NULL;
		if (optionPBv)
		    strRestrictionPB =
			((OptionMetadata *) optionPBv)->add_str_restriction ();
#endif
		value = getStringFromXPath (node->doc, nodes[j],
					   "value/child::text()");
		if (value)
		{
		    name = stringFromNodeDefTrans (nodes[j],
						   "name/child::text()",
						   NULL);
		    if (name)
		    {
			ccsAddRestrictionToStringInfo (&i->forString,
						       name, value);
#ifdef USE_PROTOBUF
			if (strRestrictionPB)
			{
			    strRestrictionPB->set_value (value);
			    strRestrictionPB->set_name (name);
			}
#endif
			free (name);
		    }
		    free (value);
		}
	    }
	    free (nodes);
	}
    }
}

static void
initListInfo (CCSSettingInfo * i, xmlNode * node, void * optionPBv)
{
    char *value;
    CCSSettingInfo *info;

    i->forList.listType = TypeBool;
    i->forList.listInfo = NULL;

    value = getStringFromXPath (node->doc, node, "type/child::text()");

    if (!value)
	return;

    i->forList.listType = getOptionType (value);
#ifdef USE_PROTOBUF
    if (optionPBv)
	((OptionMetadata *) optionPBv)->set_list_type
	    ((OptionMetadata::Type) i->forList.listType);
#endif

    free (value);

    switch (i->forList.listType)
    {
    case TypeInt:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initIntInfo (info, node, optionPBv);
	    i->forList.listInfo = info;
	}
	break;
    case TypeFloat:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initFloatInfo (info, node, optionPBv);
	    i->forList.listInfo = info;
	}
	break;
    case TypeString:
	{
	    info = (CCSSettingInfo *) calloc (1, sizeof (CCSSettingInfo));
	    if (info)
		initStringInfo (info, node, optionPBv);
	    i->forList.listInfo = info;
	}
	break;
    default:
	break;
    }
}

static void
initActionInfo (CCSSettingInfo * i, xmlNode * node, void * optionPBv)
{
    char *value;

    i->forAction.internal = FALSE;

    value = getStringFromXPath (node->doc, node, "internal/child::text()");
    if (value)
    {
	if (strcasecmp (value, "true") == 0)
	{
	    i->forAction.internal = TRUE;
#ifdef USE_PROTOBUF
	    if (optionPBv)
		((OptionMetadata *) optionPBv)->set_internal (TRUE);
#endif
	}
	free (value);
	return;
    }
    if (nodeExists (node, "internal"))
    {
	i->forAction.internal = TRUE;
#ifdef USE_PROTOBUF
	if (optionPBv)
	    ((OptionMetadata *) optionPBv)->set_internal (TRUE);
#endif
    }
}

#ifdef USE_PROTOBUF
static void
checkAddGroupSubgroup (OptionMetadata *optPB,
		       StringList *descList,
		       char *name,
		       Bool isGroup)
{
    // Check if group has the same name as the last group in the groups list
    int len = descList->size ();
    if (len > 0 &&
	strcmp (name, descList->Get (len - 1).c_str ()) == 0)
    {
	if (isGroup)
	    optPB->set_group_id (len - 1);
	else
	    optPB->set_subgroup_id (len - 1);
    }
    else
    {
	// Add new group to the list
        descList->Add ()->assign (name);

	if (isGroup)
	    optPB->set_group_id (len);
	else
	    optPB->set_subgroup_id (len);
    }
}

static Bool
createProtoBufCacheDir ()
{
    if (metadataCacheDir.length () > 0)
    {
	// Cache dir must have been created already, since otherwise it would
	// be "". So we can return here.
	return TRUE;
    }
    char *cacheBaseDir = NULL;
    char *cacheHome = getenv ("XDG_CACHE_HOME");

    if (cacheHome && strlen (cacheHome))
    {
	asprintf (&cacheBaseDir, "%s", cacheHome);
    }
    else
    {
	char *home = getenv ("HOME");
	if (home && strlen (home))
	{
	    asprintf (&cacheBaseDir, "%s/.cache", home);
	}
    }

    if (cacheBaseDir)
    {
	metadataCacheDir = cacheBaseDir;
	if (metadataCacheDir[metadataCacheDir.length () - 1] != '/')
	    metadataCacheDir += "/";
	metadataCacheDir += "compizconfig";
	std::string metadataCacheFileDummy = metadataCacheDir + "/dummy";

	// Create cache dir
	Bool success = ccsCreateDirFor (metadataCacheFileDummy.c_str ());
	if (!success)
	    fprintf (stderr, "[ERROR]: Error creating directory \"%s\"\n",
		     metadataCacheDir.c_str ());
	free (cacheBaseDir);

	if (success)
	    return TRUE; // metadataCacheDir will be used later in this case

	metadataCacheDir = ""; // invalidate metadataCacheDir
    }

    usingProtobuf = FALSE; // Disable protobuf if cache dir cannot be created
    return FALSE;
}

#endif

static void
addOptionForPlugin (CCSPlugin * plugin,
		    char * name,
		    char * type,
		    Bool isReadonly,
		    Bool isScreen,
		    unsigned int screen,
		    xmlNode * node,
		    void * groupListPBv,
		    void * subgroupListPBv,
		    void * optionPBv)
{
    xmlNode **nodes;
    int num = 0;
    CCSSetting *setting;

    if (ccsFindSetting (plugin, name, isScreen, screen))
    {
	fprintf (stderr, "[ERROR]: Option \"%s\" already defined\n", name);
	return;
    }

    if (getOptionType (type) == TypeNum)
	return;

    setting = (CCSSetting *) calloc (1, sizeof (CCSSetting));
    if (!setting)
	return;

    setting->parent = plugin;
    setting->isScreen = isScreen;
    setting->screenNum = screen;
    setting->isDefault = TRUE;
    setting->name = strdup (name);

    if (!basicMetadata)
    {
	setting->shortDesc =
	    stringFromNodeDefTrans (node, "short/child::text()", name);
	setting->longDesc =
	    stringFromNodeDefTrans (node, "long/child::text()", "");
	setting->hints = stringFromNodeDef (node, "hints/child::text()", "");
	setting->group =
	    stringFromNodeDefTrans (node, "ancestor::group/short/child::text()",
				    "");
	setting->subGroup =
	    stringFromNodeDefTrans (node,
				    "ancestor::subgroup/short/child::text()",
				    "");
    }
    else
    {
	setting->shortDesc = strdup (name);
	setting->longDesc  = strdup ("");
	setting->hints     = strdup ("");
	setting->group     = strdup ("");
	setting->subGroup  = strdup ("");
    }
    setting->type = getOptionType (type);

#ifdef USE_PROTOBUF
    OptionMetadata *optPB = NULL;

    if (optionPBv)
    {
	optPB = (OptionMetadata *) optionPBv;

	optPB->set_name (name);
	optPB->set_type ((OptionMetadata::Type) setting->type);
	if (isReadonly)
	    optPB->set_read_only (isReadonly);

	optPB->set_short_desc (setting->shortDesc);
	optPB->set_long_desc (setting->longDesc);

	if (strlen (setting->hints) > 0)
	    optPB->set_hints (setting->hints);

	if (groupListPBv && strlen (setting->group) > 0)
	    checkAddGroupSubgroup (optPB, (StringList *) groupListPBv,
				   setting->group, TRUE);
	if (subgroupListPBv && strlen (setting->subGroup) > 0)
	    checkAddGroupSubgroup (optPB, (StringList *) subgroupListPBv,
				   setting->subGroup, FALSE);
    }
#endif
    setting->value = &setting->defaultValue;
    setting->defaultValue.parent = setting;

    switch (setting->type)
    {
    case TypeInt:
	initIntInfo (&setting->info, node, optionPBv);
	break;
    case TypeFloat:
	initFloatInfo (&setting->info, node, optionPBv);
	break;
    case TypeString:
	initStringInfo (&setting->info, node, optionPBv);
	break;
    case TypeList:
	initListInfo (&setting->info, node, optionPBv);
	break;
    case TypeKey:
    case TypeButton:
    case TypeEdge:
    case TypeBell:
	initActionInfo (&setting->info, node, optionPBv);
	break;
    default:
	break;
    }

    nodes = getNodesFromXPath (node->doc, node, "default", &num);
    if (num)
    {
	void * valuePBv = NULL;
#ifdef USE_PROTOBUF
	if (optPB && setting->type != TypeList)
	    valuePBv = optPB->add_default_value ();
#endif
	switch (setting->type)
	{
	case TypeInt:
	    initIntValue (&setting->defaultValue, &setting->info, nodes[0],
			  valuePBv);
	    break;
	case TypeBool:
	    initBoolValue (&setting->defaultValue, nodes[0],
			   valuePBv);
	    break;
	case TypeFloat:
	    initFloatValue (&setting->defaultValue, &setting->info, nodes[0],
			    valuePBv);
	    break;
	case TypeString:
	    initStringValue (&setting->defaultValue, &setting->info, nodes[0],
			     valuePBv);
	    break;
	case TypeColor:
	    initColorValue (&setting->defaultValue, nodes[0], valuePBv);
	    break;
	case TypeKey:
	    initKeyValue (&setting->defaultValue, &setting->info, nodes[0],
			  valuePBv);
	    break;
	case TypeButton:
	    initButtonValue (&setting->defaultValue, &setting->info, nodes[0],
			     valuePBv);
	    break;
	case TypeEdge:
	    initEdgeValue (&setting->defaultValue, &setting->info, nodes[0],
			   valuePBv);
	    break;
	case TypeBell:
	    initBellValue (&setting->defaultValue, &setting->info, nodes[0],
			   valuePBv);
	    break;
	case TypeMatch:
	    initMatchValue (&setting->defaultValue, nodes[0],
			    valuePBv);
	    break;
	case TypeList:
	    initListValue (&setting->defaultValue, &setting->info, nodes[0],
			   optionPBv);
	    break;
	default:
	    break;
	}
    }
    else
    {
	/* if we have no set defaults, we have at least to set
	   the string defaults to empty strings */
	switch (setting->type)
	{
	case TypeString:
	    setting->defaultValue.value.asString = strdup ("");
	    break;
	case TypeMatch:
	    setting->defaultValue.value.asMatch = strdup ("");
	    break;
	default:
	    break;
	}
    }

    if (nodes)
	free (nodes);

    if (isReadonly)
    {
	// Will come here only when protobuf is enabled
	ccsFreeSetting (setting);
	return;
    }
    //	printSetting (setting);
    PLUGIN_PRIV (plugin);
    pPrivate->settings = ccsSettingListAppend (pPrivate->settings, setting);
}

static void
addOptionFromXMLNode (CCSPlugin * plugin,
		      xmlNode * node,
		      Bool isScreen,
		      void * groupListPBv,
		      void * subgroupListPBv,
		      void * optionPBv)
{
    char *name;
    char *type;
    char *readonly;
    Bool isReadonly;

    if (!node)
	return;

    name = getStringFromXPath (node->doc, node, "@name");

    type = getStringFromXPath (node->doc, node, "@type");

    readonly = getStringFromXPath (node->doc, node, "@read_only");
    isReadonly = readonly && !strcmp (readonly, "true");

    // If optionPBv is non-NULL, we still want to get the option info to write
    // to .pb file, so we don't return immediately in that case.

    if (!name || !strlen (name) || !type || !strlen (type) ||
	(!optionPBv && isReadonly))
    {
	if (name)
	    free (name);
	if (type)
	    free (type);
	if (readonly)
	    free (readonly);

	return;
    }

    if (isScreen)
    {
	for (unsigned i = 0; i < plugin->context->numScreens; i++)
	    addOptionForPlugin (plugin, name, type, isReadonly, TRUE,
				plugin->context->screens[i], node,
				groupListPBv, subgroupListPBv, optionPBv);
    }
    else
    {
	addOptionForPlugin (plugin, name, type, isReadonly, FALSE, 0, node,
			    groupListPBv, subgroupListPBv, optionPBv);
    }
    free (name);
    free (type);

    if (readonly)
	free (readonly);
}

static void
initDisplayScreenFromRootNode (CCSPlugin * plugin,
			       xmlNode * node,
			       Bool isScreen,
			       void * pluginPBv)
{
    xmlNode **nodes;
    xmlNode **optNodes;
    int num, i;
    void *groupListPBv = NULL;
    void *subgroupListPBv = NULL;

    nodes = getNodesFromXPath (node->doc, node,
			       (isScreen ? "screen" : "display"), &num);
    if (!num)
	return;

#ifdef USE_PROTOBUF
    ScreenMetadata *screenPB = NULL;

    if (pluginPBv)
    {
	PluginMetadata *pluginPB = (PluginMetadata *) pluginPBv;
	screenPB = (isScreen ?
		    pluginPB->mutable_screen () :
		    pluginPB->mutable_display ());
	groupListPBv = screenPB->mutable_group_desc ();
	subgroupListPBv = screenPB->mutable_subgroup_desc ();
    }
#endif
    optNodes = getNodesFromXPath
	(node->doc, nodes[0],
	 "option | group/subgroup/option | group/option | subgroup/option",
	 &num);
    if (num)
    {
	for (i = 0; i < num; i++)
	{
	    void *optionPBv = NULL;
    #ifdef USE_PROTOBUF
	    if (screenPB)
		optionPBv = screenPB->add_option ();
    #endif
	    addOptionFromXMLNode (plugin, optNodes[i], isScreen,
				  groupListPBv, subgroupListPBv, optionPBv);
	}

	free (optNodes);
    }
    free (nodes);
}

static inline void
initOptionsFromRootNode (CCSPlugin * plugin,
			 xmlNode * node,
			 void * pluginPBv)
{
    // For display
    initDisplayScreenFromRootNode (plugin, node, FALSE, pluginPBv);

    // For screen
    initDisplayScreenFromRootNode (plugin, node, TRUE, pluginPBv);
}

static void
addStringsFromPath (CCSStringList * list,
		    const char * path,
		    xmlNode * node,
		    void * stringListPBv)
{
    xmlNode **nodes;
    int num, i;
    nodes = getNodesFromXPath (node->doc, node, path, &num);

    if (num)
    {
	for (i = 0; i < num; i++)
	{
	    char *value = stringFromNodeDef (nodes[i], "child::text()", NULL);

	    if (value && strlen (value))
	    {
		*list = ccsStringListAppend (*list, value);
#ifdef USE_PROTOBUF
		if (stringListPBv)
		    ((StringList *) stringListPBv)->Add ()->assign (value);
#endif
	    }
	    if (value && !strlen (value))
		free (value);
	}

	free (nodes);
    }
}

static void
addStringExtensionFromXMLNode (CCSPlugin * plugin,
			       xmlNode * node,
			       void * extensionPBv)
{
    xmlNode **nodes;
    int num, j;
    CCSStrExtension *extension;
    char *name;
    char *value;
    char *isDisplay;
    void * stringListPBv = NULL;

    extension = (CCSStrExtension *) calloc (1, sizeof (CCSStrExtension));
    if (!extension)
	return;

    isDisplay = getStringFromXPath (node->doc, node, "@display");

    extension->isScreen = !(isDisplay && !strcmp (isDisplay, "true"));

    if (isDisplay)
	free (isDisplay);

    extension->restriction = NULL;

    extension->basePlugin = getStringFromXPath (node->doc, node, "@base_plugin");
    if (!extension->basePlugin)
	extension->basePlugin = strdup ("");

#ifdef USE_PROTOBUF
    ExtensionMetadata * extensionPB = NULL;
    if (extensionPBv)
    {
	extensionPB = (ExtensionMetadata *) extensionPBv;
	extensionPB->set_display (!extension->isScreen);
	extensionPB->set_base_plugin (extension->basePlugin);
	stringListPBv = extensionPB->mutable_base_option ();
    }
#endif

    addStringsFromPath (&extension->baseSettings, "base_option", node,
			stringListPBv);

    nodes = getNodesFromXPath (node->doc, node, "restriction", &num);
    if (!num)
    {
	free (extension);
	return;
    }

    for (j = 0; j < num; j++)
    {
	value = getStringFromXPath (node->doc, nodes[j], "value/child::text()");
	if (value)
	{
	    name = stringFromNodeDefTrans (nodes[j], "name/child::text()",
					   NULL);
	    if (name)
	    {
		ccsAddRestrictionToStringExtension (extension, name, value);
#ifdef USE_PROTOBUF
		if (extensionPB)
		{
		    OptionMetadata::StringRestriction *strRestrictionPB =
			extensionPB->add_str_restriction ();
		    strRestrictionPB->set_value (value);
		    strRestrictionPB->set_name (name);
		}
#endif
		free (name);
	    }
	    free (value);
	}
    }
    free (nodes);

    PLUGIN_PRIV (plugin);

    pPrivate->stringExtensions =
	ccsStrExtensionListAppend (pPrivate->stringExtensions, extension);
}

static void
initStringExtensionsFromRootNode (CCSPlugin * plugin,
				  xmlNode * node,
				  void * pluginPBv)
{
    xmlNode **nodes;
    int num, i;
    nodes = getNodesFromXPath (node->doc, node, "/compiz/*/extension", &num);

    for (i = 0; i < num; i++)
    {
	void *extensionPBv = NULL;
#ifdef USE_PROTOBUF
	if (pluginPBv)
	{
	    PluginMetadata *pluginPB = (PluginMetadata *) pluginPBv;
	    extensionPBv = pluginPB->add_extension ();
	}
#endif
	addStringExtensionFromXMLNode (plugin, nodes[i], extensionPBv);
    }
    free (nodes);
}

static void
initRulesFromRootNode (CCSPlugin * plugin, xmlNode * node, void * pluginInfoPBv)
{
    void *featureListPBv = NULL;
    void *pluginAfterListPBv = NULL;
    void *pluginBeforeListPBv = NULL;
    void *requirePluginListPBv = NULL;
    void *requireFeatureListPBv = NULL;
    void *conflictPluginListPBv = NULL;
    void *conflictFeatureListPBv = NULL;
#ifdef USE_PROTOBUF
    if (pluginInfoPBv)
    {
	PluginInfoMetadata *pluginInfoPB = (PluginInfoMetadata *) pluginInfoPBv;
	featureListPBv = pluginInfoPB->mutable_feature ();

	DependenciesMetadata *deps = pluginInfoPB->mutable_deps ();
	pluginAfterListPBv     = deps->mutable_after_plugin ();
	pluginBeforeListPBv    = deps->mutable_before_plugin ();
	requirePluginListPBv   = deps->mutable_require_plugin ();
	requireFeatureListPBv  = deps->mutable_require_feature ();
	conflictPluginListPBv  = deps->mutable_conflict_plugin ();
	conflictFeatureListPBv = deps->mutable_conflict_feature ();
    }
#endif

    addStringsFromPath (&plugin->providesFeature, "feature", node,
			featureListPBv);

    addStringsFromPath (&plugin->loadAfter,
			"deps/relation[@type = 'after']/plugin", node,
			pluginAfterListPBv);
    addStringsFromPath (&plugin->loadBefore,
			"deps/relation[@type = 'before']/plugin", node,
			pluginBeforeListPBv);
    addStringsFromPath (&plugin->requiresPlugin,
			"deps/requirement/plugin", node, requirePluginListPBv);
    addStringsFromPath (&plugin->requiresFeature,
			"deps/requirement/feature", node, requireFeatureListPBv);
    addStringsFromPath (&plugin->conflictPlugin,
			"deps/conflict/plugin", node, conflictPluginListPBv);
    addStringsFromPath (&plugin->conflictFeature,
			"deps/conflict/feature", node, conflictFeatureListPBv);
}

#ifdef USE_PROTOBUF
static void
fillBasicInfoIntoPB (CCSPlugin *plugin, PluginInfoMetadata *pluginInfoPB)
{
    if (!pluginInfoPB)
	return;

    pluginInfoPB->set_name (plugin->name);
    pluginInfoPB->set_short_desc (plugin->shortDesc);
    pluginInfoPB->set_long_desc (plugin->longDesc);
    pluginInfoPB->set_category (plugin->category);
}
#endif

/* Returns TRUE on success. */
static Bool
addPluginFromXMLNode (CCSContext * context,
		      xmlNode * node,
		      char * file,
		      void * pluginInfoPBv)
{
    char *name;
    CCSPlugin *plugin;
    CCSPluginPrivate *pPrivate;

    if (!node)
	return FALSE;

    name = getStringFromXPath (node->doc, node, "@name");

    if (!name || !strlen (name))
    {
	if (name)
	    free (name);
	return FALSE;
    }

    if (!strcmp (name, "ini") || !strcmp (name, "gconf") ||
	!strcmp (name, "ccp") || !strcmp (name, "kconfig"))
    {
	free (name);
	return FALSE;
    }

    if (ccsFindPlugin (context, name))
    {
	free (name);
	return FALSE;
    }

    plugin = (CCSPlugin *) calloc (1, sizeof (CCSPlugin));
    if (!plugin)
	return FALSE;

    pPrivate = (CCSPluginPrivate *) calloc (1, sizeof (CCSPluginPrivate));
    if (!pPrivate)
    {
	free (plugin);
	return FALSE;
    }

    plugin->ccsPrivate = (void *) pPrivate;

    if (file)
	pPrivate->xmlFile = strdup (file);

    asprintf (&pPrivate->xmlPath, "/compiz/plugin[@name = '%s']", name);
    plugin->context = context;
    plugin->name = strdup (name);

    if (!basicMetadata)
    {
	plugin->shortDesc =
	    stringFromNodeDefTrans (node, "short/child::text()", name);
	plugin->longDesc =
	    stringFromNodeDefTrans (node, "long/child::text()",	name);
	plugin->category =
	    stringFromNodeDef (node, "category/child::text()", "");
    }
    else
    {
	plugin->shortDesc = strdup (name);
	plugin->longDesc  = strdup (name);
	plugin->category  = strdup ("");
    }
#ifdef USE_PROTOBUF
    fillBasicInfoIntoPB (plugin, (PluginInfoMetadata *) pluginInfoPBv);
#endif

    initRulesFromRootNode (plugin, node, pluginInfoPBv);

    context->plugins = ccsPluginListAppend (context->plugins, plugin);
    free (name);

    return TRUE;
}

/* Returns TRUE on success. */
static Bool
addCoreSettingsFromXMLNode (CCSContext * context,
			    xmlNode * node,
			    char *file,
			    void * pluginInfoPBv)
{
    CCSPlugin *plugin;
    CCSPluginPrivate *pPrivate;

    if (!node)
	return FALSE;

    if (ccsFindPlugin (context, "core"))
	return FALSE;

    plugin = (CCSPlugin *) calloc (1, sizeof (CCSPlugin));
    if (!plugin)
	return FALSE;

    pPrivate = (CCSPluginPrivate *) calloc (1, sizeof (CCSPluginPrivate));
    if (!pPrivate)
    {
	free (plugin);
	return FALSE;
    }

    plugin->ccsPrivate = (void *) pPrivate;

    if (file)
	pPrivate->xmlFile = strdup (file);

    pPrivate->xmlPath = strdup ("/compiz/core");
    plugin->context = context;
    plugin->name = strdup ("core");
    plugin->category = strdup ("General");

    if (!basicMetadata)
    {
	plugin->shortDesc =
	    stringFromNodeDefTrans (node, "short/child::text()",
				    "General Options");
	plugin->longDesc =
	    stringFromNodeDefTrans (node, "long/child::text()",
				    "General Compiz Options");
    }
    else
    {
	plugin->shortDesc = strdup ("General Options");
	plugin->longDesc  = strdup ("General Compiz Options");
    }
#ifdef USE_PROTOBUF
    fillBasicInfoIntoPB (plugin, (PluginInfoMetadata *) pluginInfoPBv);
#endif

    initRulesFromRootNode (plugin, node, pluginInfoPBv);
    context->plugins = ccsPluginListAppend (context->plugins, plugin);

    return TRUE;
}

/* End of XML parsing */

#ifdef USE_PROTOBUF

// Either pluginMinMetadata or pluginMetadata should be non-NULL
static Bool
loadPluginMetadataFromProtoBuf (char *pbPath,
				PluginBriefMetadata *pluginMinMetadata,
				PluginMetadata *pluginMetadata)
{
    Bool success = FALSE;

    FILE *pbFile = fopen (pbPath, "rb");
    if (pbFile)
    {
	google::protobuf::io::FileInputStream inputStream (fileno (pbFile));
	if ((pluginMinMetadata &&
	     pluginMinMetadata->ParseFromZeroCopyStream (&inputStream)) ||
	    (pluginMetadata &&
	     pluginMetadata->ParseFromZeroCopyStream (&inputStream)))
	    success = TRUE;
	inputStream.Close ();
    }

    return success;
}

// Returns TRUE if successfully loads .pb file and .pb is up to date.
static Bool
checkAndLoadProtoBuf (char *pbPath,
		      struct stat *pbStat,
		      struct stat *xmlStat,
		      PluginBriefMetadata *pluginBriefPB)
{
    const PluginInfoMetadata &pluginInfoPB = pluginBriefPB->info ();

    if (pbStat->st_mtime < xmlStat->st_mtime ||     // is .pb older than .xml?
	!loadPluginMetadataFromProtoBuf (pbPath, pluginBriefPB, NULL) ||
	(!basicMetadata && pluginBriefPB->info ().basic_metadata ()) ||
	pluginInfoPB.pb_abi_version () != PB_ABI_VERSION ||
	pluginInfoPB.time () != (unsigned long)xmlStat->st_mtime ||
	// xml modification time mismatch?
	(pluginInfoPB.locale () != "NONE" &&
	 pluginInfoPB.locale () != shortLocale))
    {
	// .pb needs update
	return FALSE;
    }
    return TRUE;
}

// Write .pb data to .pb file
static void
writePBFile (char *pbFilePath,
	     PluginMetadata *pluginPB,
	     PluginBriefMetadata *pluginBriefPB,
	     struct stat *xmlStat)
{
    if (!createProtoBufCacheDir ())
	return;

    PluginInfoMetadata *pluginInfoPB;

    if (pluginPB)
    {
	pluginInfoPB = pluginPB->mutable_info ();
	pluginInfoPB->set_brief_metadata (FALSE);
    }
    else
    {
	pluginInfoPB = pluginBriefPB->mutable_info ();
	pluginInfoPB->set_pb_abi_version (PB_ABI_VERSION);
	pluginInfoPB->set_locale (shortLocale);
	pluginInfoPB->set_time ((unsigned long)xmlStat->st_mtime);
	pluginInfoPB->set_brief_metadata (TRUE);
    }

    pluginInfoPB->set_basic_metadata (basicMetadata);

    FILE *pbFile = fopen (pbFilePath, "wb");
    if (pbFile)
    {
	google::protobuf::io::FileOutputStream
	    outputStream (fileno (pbFile));
	if (pluginPB)
	    pluginPB->SerializeToZeroCopyStream (&outputStream);
	else
	    pluginBriefPB->SerializeToZeroCopyStream (&outputStream);
	outputStream.Close ();
    }
}
#endif

/* Returns TRUE on success. */
static Bool
loadPluginFromXML (CCSContext * context,
		   xmlDoc * doc,
		   char *filename,
		   void * pluginInfoPBv)
{
    xmlNode **nodes;
    int num;
    Bool success = FALSE;

    nodes = getNodesFromXPath (doc, NULL, "/compiz/core", &num);
    if (num)
    {
	success = addCoreSettingsFromXMLNode (context, nodes[0], filename,
					      pluginInfoPBv);
	free (nodes);
	return success;
    }

    nodes = getNodesFromXPath (doc, NULL, "/compiz/plugin", &num);
    if (num)
    {
	success = addPluginFromXMLNode (context, nodes[0], filename,
					pluginInfoPBv);
	free (nodes);
    }
    return success;
}

#ifdef USE_PROTOBUF
static void
updatePBFilePath (CCSContext * context, char *name, char *pbFilePath)
{
    CCSPlugin *plugin = ccsFindPlugin (context, name);
    if (plugin)
    {
	PLUGIN_PRIV (plugin);

	if (pPrivate->pbFilePath)
	    free (pPrivate->pbFilePath);
	pPrivate->pbFilePath = strdup (pbFilePath);
    }
}
#endif

static void
loadPluginFromXMLFile (CCSContext * context, char *xmlName, char *xmlDirPath)
{
    char *xmlFilePath = NULL;
    char *pbFilePath = NULL;
    void *pluginInfoPBv = NULL;

    asprintf (&xmlFilePath, "%s/%s", xmlDirPath, xmlName);
    if (!xmlFilePath)
    {
	fprintf (stderr, "[ERROR]: Can't allocate memory\n");
	return;
    }

#ifdef USE_PROTOBUF
    char *name = NULL;
    struct stat xmlStat;
    Bool removePB = FALSE;

    if (usingProtobuf)
    {
	if (stat (xmlFilePath, &xmlStat))
	{
	    free (xmlFilePath);
	    return;
	}

	// Check if the corresponding .pb exists in cache
	Bool error = TRUE;
	struct stat pbStat;

	name = strndup (xmlName, strlen (xmlName) - 4);
	if (!name)
	{
	    fprintf (stderr, "[ERROR]: Can't allocate memory\n");
	    free (xmlFilePath);
	    return;
	}

	if (createProtoBufCacheDir () &&
	    metadataCacheDir.length () > 0)
	{
	    asprintf (&pbFilePath, "%s/%s.pb", metadataCacheDir.c_str (), name);
	    if (!pbFilePath)
	    {
		fprintf (stderr, "[ERROR]: Can't allocate memory\n");
		free (xmlFilePath);
		free (name);
		return;
	    }
	    error = stat (pbFilePath, &pbStat);
	}

	if (!error)
	{
	    if (checkAndLoadProtoBuf (pbFilePath, &pbStat, &xmlStat,
				      &persistentPluginBriefPB))
	    {
		// Found and loaded .pb
		if (!strcmp (name, "core"))
		    addCoreSettingsFromPB (context,
					   persistentPluginBriefPB.info (),
					   pbFilePath, xmlFilePath);
		else
		    addPluginFromPB (context, persistentPluginBriefPB.info (),
				     pbFilePath, xmlFilePath);
		
		updatePBFilePath (context, name, pbFilePath);
		
		free (xmlFilePath);
		free (pbFilePath);
		free (name);
		return;
	    }
	    else
	    {
		removePB = TRUE;
	    }
	}
	persistentPluginBriefPB.Clear ();
	pluginInfoPBv = persistentPluginBriefPB.mutable_info ();
    }
#endif

    // Load from .xml
    FILE *fp = fopen (xmlFilePath, "r");
    Bool xmlLoaded = FALSE;

    if (fp)
    {
	fclose (fp);
	xmlDoc *doc = xmlReadFile (xmlFilePath, NULL, 0);
	if (doc)
	{
	    xmlLoaded = loadPluginFromXML (context, doc, xmlFilePath,
					   pluginInfoPBv);
	    xmlFreeDoc (doc);
	}
    }
    free (xmlFilePath);

#ifdef USE_PROTOBUF
    if (usingProtobuf && xmlLoaded)
    {
	if (removePB)
	    remove (pbFilePath); // Attempt to remove .pb
	writePBFile (pbFilePath, NULL, &persistentPluginBriefPB, &xmlStat);
	updatePBFilePath (context, name, pbFilePath);
    }

    if (pbFilePath)
	free (pbFilePath);
    if (name)
	free (name);
#endif
}

static void
loadPluginsFromXMLFiles (CCSContext * context, char *path)
{
    struct dirent **nameList;
    int nFile, i;

    if (!path)
	return;

    nFile = scandir (path, &nameList, pluginXMLFilter, NULL);

    if (nFile <= 0)
	return;

    for (i = 0; i < nFile; i++)
    {
	loadPluginFromXMLFile (context, nameList[i]->d_name, path);
	free (nameList[i]);
    }
    free (nameList);
}

static void
addPluginNamed (CCSContext * context, char *name)
{
    CCSPlugin *plugin;
    CCSPluginPrivate *pPrivate;

    if (ccsFindPlugin (context, name))
	return;

    if (!strcmp (name, "ini") || !strcmp (name, "gconf") ||
	!strcmp (name, "ccp") || !strcmp (name, "kconfig"))
	return;

    plugin = (CCSPlugin *) calloc (1, sizeof (CCSPlugin));
    if (!plugin)
	return;

    pPrivate = (CCSPluginPrivate *) calloc (1, sizeof (CCSPluginPrivate));
    if (!pPrivate)
    {
	free (plugin);
	return;
    }

    plugin->ccsPrivate = (void *) pPrivate;

    plugin->context = context;
    plugin->name = strdup (name);

    if (!plugin->shortDesc)
	plugin->shortDesc = strdup (name);
    if (!plugin->longDesc)
	plugin->longDesc = strdup (name);
    if (!plugin->category)
	plugin->category = strdup ("");

    pPrivate->loaded = TRUE;
    collateGroups (pPrivate);
    context->plugins = ccsPluginListAppend (context->plugins, plugin);
}

static void
loadPluginsFromName (CCSContext * context, char *path)
{
    struct dirent **nameList;
    int nFile, i;

    if (!path)
	return;

    nFile = scandir (path, &nameList, pluginNameFilter, NULL);
    if (nFile <= 0)
	return;

    for (i = 0; i < nFile; i++)
    {
	char name[1024];
	sscanf (nameList[i]->d_name, "lib%s", name);
	if (strlen (name) > 3)
	    name[strlen (name) - 3] = 0;
	free (nameList[i]);
	addPluginNamed (context, name);
    }
    free (nameList);
}

#ifdef USE_PROTOBUF
static inline void
initPBLoading ()
{
    // Update usingProtobuf with the COMPIZ_NO_PROTOBUF environment variable
    char *compizNoProtobuf = getenv ("COMPIZ_NO_PROTOBUF");
    usingProtobuf = !(compizNoProtobuf &&
		      (strcasecmp (compizNoProtobuf, "1") == 0 ||
		       strcasecmp (compizNoProtobuf, "yes") == 0 ||
		       strcasecmp (compizNoProtobuf, "true") == 0));
    if (usingProtobuf)
    {
	// Verify that the version of the library that we linked against is
	// compatible with the version of the headers we compiled against.
	GOOGLE_PROTOBUF_VERIFY_VERSION;
    }
}
#endif

Bool
ccsLoadPlugin (CCSContext * context, char *name)
{
#ifdef USE_PROTOBUF
    initPBLoading ();
#endif

    char *xmlDirPath = NULL;
    char *xmlName = NULL;
    asprintf (&xmlName, "%s.xml", name);

    if (xmlName)
    {
	char *home = getenv ("HOME");
	if (home && strlen (home))
	{
	    char *home = getenv ("HOME");
	    asprintf (&xmlDirPath, "%s/.compiz/metadata", home);
	    if (xmlDirPath)
	    {
		loadPluginFromXMLFile (context, xmlName, xmlDirPath);
		free (xmlDirPath);
	    }
	}

	loadPluginFromXMLFile (context, xmlName, (char *) METADATADIR);
	free (xmlName);
    }

    return (ccsFindPlugin (context, name) != NULL);
}

void
ccsLoadPlugins (CCSContext * context)
{
    D (D_FULL, "Adding plugins\n");

#ifdef USE_PROTOBUF
    initPBLoading ();
#endif

    char *home = getenv ("HOME");
    if (home && strlen (home))
    {
	char *homeplugins = NULL;
	asprintf (&homeplugins, "%s/.compiz/metadata", home);
	if (homeplugins)
	{
	    loadPluginsFromXMLFiles (context, homeplugins);
	    free (homeplugins);
	}
    }
    loadPluginsFromXMLFiles (context, (char *)METADATADIR);

    if (home && strlen (home))
    {
	char *homeplugins = NULL;
	asprintf (&homeplugins, "%s/.compiz/plugins", home);
	if (homeplugins)
	{
	    loadPluginsFromName (context, homeplugins);
	    free (homeplugins);
	}
    }
    loadPluginsFromName (context, (char *)PLUGINDIR);
}

static void
loadOptionsStringExtensionsFromXML (CCSPlugin * plugin,
				    void * pluginPBv,
				    struct stat *xmlStat)
{
    PLUGIN_PRIV (plugin);

    xmlDoc *doc = NULL;
    xmlNode **nodes;
    int num;

    if (stat (pPrivate->xmlFile, xmlStat))
	return;

    FILE *fp = fopen (pPrivate->xmlFile, "r");
    if (!fp)
	return;

    fclose (fp);
    doc = xmlReadFile (pPrivate->xmlFile, NULL, 0);

    nodes = getNodesFromXPath (doc, NULL, pPrivate->xmlPath, &num);
    if (num)
    {
	initOptionsFromRootNode (plugin, nodes[0], pluginPBv);
	if (!basicMetadata)
	    initStringExtensionsFromRootNode (plugin, nodes[0], pluginPBv);
	free (nodes);
    }
    if (doc)
	xmlFreeDoc (doc);
}

void
ccsLoadPluginSettings (CCSPlugin * plugin)
{
    Bool ignoreXML = FALSE;
    Bool loadedAtLeastBriefPB = FALSE;
    void *pluginPBToWrite = NULL;

#ifdef USE_PROTOBUF
    initPBLoading ();
#endif

    PLUGIN_PRIV (plugin);

    if (pPrivate->loaded)
	return;

    pPrivate->loaded = TRUE;
    D (D_FULL, "Initializing %s options...", plugin->name);

#ifdef USE_PROTOBUF
    if (usingProtobuf && pPrivate->pbFilePath)
    {
	loadedAtLeastBriefPB =
	    loadPluginMetadataFromProtoBuf (pPrivate->pbFilePath,
					    NULL, &persistentPluginPB);
	if (loadedAtLeastBriefPB)
	{
	    if (!persistentPluginPB.info ().brief_metadata () &&
		(basicMetadata ||
		 !persistentPluginPB.info ().basic_metadata ()))
	    {
		initOptionsFromPB (plugin, persistentPluginPB);
		if (!basicMetadata)
		    initStringExtensionsFromPB (plugin, persistentPluginPB);
		ignoreXML = TRUE;
	    }
	    else
		pluginPBToWrite = &persistentPluginPB;
	}
	else
	    pluginPBToWrite = &persistentPluginPB;
    }
#endif

    struct stat xmlStat;

    // Load from .xml
    if (!ignoreXML && pPrivate->xmlFile)
	loadOptionsStringExtensionsFromXML (plugin, pluginPBToWrite, &xmlStat);

#ifdef USE_PROTOBUF
    if (pluginPBToWrite && pPrivate->pbFilePath && loadedAtLeastBriefPB)
	writePBFile (pPrivate->pbFilePath, (PluginMetadata *) pluginPBToWrite,
		     NULL, &xmlStat);
#endif
    D (D_FULL, "done\n");

    collateGroups (pPrivate);
    ccsReadPluginSettings (plugin);
}

