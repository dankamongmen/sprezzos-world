/*
 * Compiz configuration system library
 *
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
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>

#include <ccs.h>
#include "iniparser.h"

/** 
 * Creates the parent directory for @fileName, recursively creating a directory
 * tree if necessary.
 *
 * @param fileName: The absolute path to the desired file
 * @return: True if the parent directory of the file now exists
**/

Bool
ccsCreateDirFor (const char *fileName)
{
    char *path, *delim;
    Bool success;

    delim = strrchr (fileName, '/');
    if (!delim)
	return FALSE;	/* Input string is not a valid absolue path! */

    path = malloc (delim - fileName + 1);
    if (!path)
	return FALSE;

    strncpy (path, fileName, delim - fileName);
    path[delim - fileName] = '\0';

    success = !mkdir (path, 0700);	/* Mkdir returns 0 on success */
    success |= (errno == EEXIST);

    if (!success && (errno == ENOENT))	/* ENOENT means we must recursively */
    {					/* create the parent's parent */
	if (ccsCreateDirFor (path))
	    success = !mkdir (path, 0700);
    }

    free (path);
    return success;
}

IniDictionary * ccsIniOpen (const char * fileName)
{
    FILE *file;

    if (!ccsCreateDirFor(fileName))
	return NULL;

    /* create file if it doesn't exist or is desired */
    file = fopen (fileName, "a+");
    if (file)
	fclose (file);

    return iniparser_new ((char*) fileName);
}

IniDictionary*
ccsIniNew (void)
{
    return dictionary_new (0);
}

void
ccsIniClose (IniDictionary *dictionary)
{
    iniparser_free (dictionary);
}

void
ccsIniSave (IniDictionary *dictionary,
	    const char    *fileName)
{
    if (!ccsCreateDirFor (fileName))
	return;

    iniparser_dump_ini (dictionary, fileName);
}

static char*
getIniString (IniDictionary *dictionary,
	      const char    *section,
	      const char    *entry)
{
    char *sectionName;
    char *retValue;

    asprintf (&sectionName, "%s:%s", section, entry);

    retValue = iniparser_getstring (dictionary, sectionName, NULL);
    free (sectionName);

    return retValue;
}

static void
setIniString (IniDictionary *dictionary,
	      const char    *section,
	      const char    *entry,
	      const char    *value)
{
    char *sectionName;

    asprintf (&sectionName, "%s:%s", section, entry);

    if (!iniparser_find_entry (dictionary, (char*) section))
	iniparser_add_entry (dictionary, (char*) section, NULL, NULL);

    iniparser_setstr (dictionary, sectionName, (char*) value);

    free (sectionName);
}

Bool
ccsIniGetString (IniDictionary *dictionary,
	    	 const char    *section,
		 const char    *entry,
		 char          **value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
    {
	*value = strdup (retValue);
	return TRUE;
    }
    else
	return FALSE;
}

Bool
ccsIniGetInt (IniDictionary *dictionary,
	      const char    *section,
	      const char    *entry,
	      int           *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
    {
	*value = strtoul (retValue, NULL, 10);
	return TRUE;
    }
    else
	return FALSE;
}

Bool
ccsIniGetFloat (IniDictionary *dictionary,
		const char    *section,
		const char    *entry,
		float         *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
    {
	*value = (float) strtod (retValue, NULL);
	return TRUE;
    }
    else
	return FALSE;
}

Bool
ccsIniGetBool (IniDictionary *dictionary,
	       const char    *section,
   	       const char    *entry,
	       Bool          *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
    {
	if ((retValue[0] == 't') || (retValue[0] == 'T') ||
	    (retValue[0] == 'y') || (retValue[0] == 'Y') ||
	    (retValue[0] == '1'))
	{
	    *value = TRUE;
	}
	else
	    *value = FALSE;

	return TRUE;
    }
    else
	return FALSE;
}

Bool
ccsIniGetColor (IniDictionary        *dictionary,
		const char           *section,
		const char           *entry,
		CCSSettingColorValue *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue && ccsStringToColor (retValue, value))
	return TRUE;
    else
	return FALSE;
}

Bool
ccsIniGetKey (IniDictionary      *dictionary,
	      const char         *section,
	      const char         *entry,
              CCSSettingKeyValue *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
	return ccsStringToKeyBinding (retValue, value);
    else
	return FALSE;
}

Bool
ccsIniGetButton (IniDictionary         *dictionary,
	   	 const char            *section,
   		 const char            *entry,
		 CCSSettingButtonValue *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
	return ccsStringToButtonBinding (retValue, value);
    else
	return FALSE;
}

Bool
ccsIniGetEdge (IniDictionary  *dictionary,
	   	 const char   *section,
   		 const char   *entry,
		 unsigned int *value)
{
    char *retValue;

    retValue = getIniString (dictionary, section, entry);
    if (retValue)
    {
	*value = ccsStringToEdges (retValue);
	return TRUE;
    }
    else
	return FALSE;
}

Bool
ccsIniGetBell (IniDictionary *dictionary,
	       const char    *section,
               const char    *entry,
               Bool          *value)
{
    return ccsIniGetBool (dictionary, section, entry, value);
}

static Bool
isEmptyString (char *value)
{
    int len, i = 0;

    len = strlen (value);
    for (i = 0; i < len; i++)
    {
	if (!isblank (value[i]))
	    return FALSE;
    }
    return TRUE;
}

Bool
ccsIniGetList (IniDictionary       *dictionary,
   	       const char          *section,
	       const char          *entry,
	       CCSSettingValueList *value,
	       CCSSetting          *parent)
{
    CCSSettingValueList list = NULL;
    char                *valueString, *valueStart, *valString;
    char                *token;
    int                 nItems = 1, i = 0, len;

    valString = getIniString (dictionary, section, entry);
    if (!valString)
	return FALSE;

    if (isEmptyString (valString))
    {
	*value = NULL;
	return TRUE;
    }

    valueString = strdup (valString);
    valueStart = valueString;

    /* remove trailing semicolon that we added to be able to differentiate
       between an empty list and a list with one empty item */
    len = strlen (valueString);
    if (valueString[len - 1] == ';')
	valueString[len - 1] = 0;

    token = strchr (valueString, ';');
    while (token)
    {
	token = strchr (token + 1, ';');
	nItems++;
    }

    token = strsep (&valueString, ";");
    switch (parent->info.forList.listType)
    {
    case TypeString:
    case TypeMatch:
	{
	    char **array = malloc (nItems * sizeof (char*));
	    if (!array)
		break;

	    while (token)
	    {
		array[i++] = strdup (token);
		token = strsep (&valueString, ";");
	    }

	    list = ccsGetValueListFromStringArray (array, nItems, parent);

	    for (i = 0; i < nItems; i++)
		free (array[i]);

	    free (array);
	}
	break;
    case TypeColor:
	{
	    CCSSettingColorValue *array;
	    array = malloc (nItems * sizeof (CCSSettingColorValue));
	    if (!array)
		break;

	    while (token)
	    {
		memset (&array[i], 0, sizeof (CCSSettingColorValue));
		ccsStringToColor (token, &array[i]);
		token = strsep (&valueString, ";");
		i++;
	    }

	    list = ccsGetValueListFromColorArray (array, nItems, parent);
	    free (array);
	}
	break;
    case TypeBool:
	{
	    Bool *array = malloc (nItems * sizeof (Bool));
	    Bool isTrue;
	    if (!array)
		break;

	    while (token)
	    {
		isTrue = (token[0] == 'y' || token[0] == 'Y' || 
			  token[0] == '1' ||
			  token[0] == 't' || token[0] == 'T');
		array[i++] = isTrue;
		token = strsep (&valueString, ";");
	    }

	    list = ccsGetValueListFromBoolArray (array, nItems, parent);
	    free (array);
	}
	break;
    case TypeInt:
	{
	    int *array = malloc (nItems * sizeof (int));
	    if (!array)
		break;

	    while (token)
	    {
		array[i++] = strtoul (token, NULL, 10);
		token = strsep (&valueString, ";");
	    }

	    list = ccsGetValueListFromIntArray (array, nItems, parent);
	    free (array);
	}
	break;
    case TypeFloat:
	{
	    float *array = malloc (nItems * sizeof (float));
	    if (!array)
		break;

	    while (token)
	    {
		array[i++] = strtod (token, NULL);
		token = strsep (&valueString, ";");
	    }

	    list = ccsGetValueListFromFloatArray (array, nItems, parent);
	    free (array);
	}
	break;
    case TypeKey:
	{
	    CCSSettingValue *val = NULL;
	    list = NULL;

	    while (token)
	    {
		val = malloc (sizeof (CCSSettingValue));
		if (!val)
		    break;
		if (ccsStringToKeyBinding (token, &val->value.asKey))
		    list = ccsSettingValueListAppend (list, val);
		else
		    free (val);
		token = strsep (&valueString, ";");
	    }
	}
	break;
    case TypeButton:
	{
	    CCSSettingValue *val = NULL;
	    list = NULL;

	    while (token)
	    {
		val = malloc (sizeof (CCSSettingValue));
		if (!val)
		    break;
		if (ccsStringToButtonBinding (token, &val->value.asButton))
		    list = ccsSettingValueListAppend (list, val);
		else
		    free (val);
		token = strsep (&valueString, ";");
	    }
	}
	break;
    case TypeEdge:
	{
	    CCSSettingValue *val = NULL;
	    list = NULL;

	    while (token)
	    {
		val = malloc (sizeof (CCSSettingValue));
		if (!val)
		    break;
		val->value.asEdge = ccsStringToEdges (token);
		list = ccsSettingValueListAppend (list, val);
		token = strsep (&valueString, ";");
	    }
	}
	break;
    case TypeBell:
	{
	    CCSSettingValue *val = NULL;
	    list = NULL;
	    Bool isTrue;

	    while (token)
	    {
		val = malloc (sizeof (CCSSettingValue));
		if (!val)
		    break;

		isTrue = (token[0] == 'y' || token[0] == 'Y' || 
			  token[0] == '1' ||
			  token[0] == 't' || token[0] == 'T');
		
		val->value.asBell = isTrue;
		list = ccsSettingValueListAppend (list, val);
		token = strsep (&valueString, ";");
	    }
	}
	break;
    default:
	break;
    }

    *value = list;
    free (valueStart);

    return TRUE;
}

void
ccsIniSetString (IniDictionary * dictionary,
		 const char    * section,
		 const char    * entry,
		 char          * value)
{
    setIniString (dictionary, section, entry, value);
}

void
ccsIniSetInt (IniDictionary *dictionary,
	      const char    *section,
	      const char    *entry,
	      int           value)
{
    char *string = NULL;

    asprintf (&string, "%i", value);
    if (string)
    {
	setIniString (dictionary, section, entry, string);
	free (string);
    }
}

void
ccsIniSetFloat (IniDictionary *dictionary,
		const char    *section,
		const char    *entry,
		float         value)
{
    char *string = NULL;

    asprintf (&string, "%f", value);
    if (string)
    {
	setIniString (dictionary, section, entry, string);
	free (string);
    }
}

void
ccsIniSetBool (IniDictionary *dictionary,
	       const char    *section,
	       const char    *entry,
	       Bool          value)
{
    setIniString (dictionary, section, entry,
		  value ? "true" : "false");
}

void
ccsIniSetColor (IniDictionary        *dictionary,
		const char           *section,
	   	const char           *entry,
   		CCSSettingColorValue value)
{
    char *string;

    string = ccsColorToString (&value);
    if (string)
    {
	setIniString (dictionary, section, entry, string);
	free (string);
    }
}

void
ccsIniSetKey (IniDictionary      *dictionary,
	      const char         *section,
	      const char         *entry,
	      CCSSettingKeyValue value)
{
    char *str;

    str = ccsKeyBindingToString (&value);
    if (str)
    {
	setIniString (dictionary, section, entry, str);
	free (str);
    }
}

void
ccsIniSetButton (IniDictionary         *dictionary,
		 const char            *section,
		 const char            *entry,
		 CCSSettingButtonValue value)
{
    char *str;

    str = ccsButtonBindingToString (&value);
    if (str)
    {
	setIniString (dictionary, section, entry, str);
	free (str);
    }
}

void
ccsIniSetEdge (IniDictionary *dictionary,
	       const char    *section,
	       const char    *entry,
	       unsigned int  value)
{
    char *str;

    str = ccsEdgesToString (value);
    if (str)
    {
	setIniString (dictionary, section, entry, str);
	free (str);
    }
}

void
ccsIniSetBell (IniDictionary *dictionary,
	       const char    *section,
	       const char    *entry,
	       Bool          value)
{
    ccsIniSetBool (dictionary, section, entry, value);
}

void
ccsIniSetList (IniDictionary       *dictionary,
	       const char          *section,
	       const char          *entry,
	       CCSSettingValueList value,
	       CCSSettingType      listType)
{
    char         *stringBuffer, *valueString;
    char         valueBuffer[100];
    unsigned int bufferSize = 1024, fill;

    stringBuffer = calloc (1, bufferSize);
    if (!stringBuffer)
	return;

    while (value)
    {
	switch (listType)
	{
	case TypeString:
	    valueString = value->data->value.asString;
	    break;
	case TypeMatch:
	    valueString = value->data->value.asMatch;
	    break;
	case TypeInt:
	    snprintf (valueBuffer, 100, "%d", value->data->value.asInt);
	    valueString = valueBuffer;
	    break;
	case TypeBool:
	    strncpy (valueBuffer,
		     (value->data->value.asBool) ? "true" : "false", 100);
	    valueString = valueBuffer;
	    break;
	case TypeFloat:
	    snprintf (valueBuffer, 100, "%f", value->data->value.asFloat);
	    valueString = valueBuffer;
	    break;
	case TypeColor:
	    valueString = ccsColorToString (&value->data->value.asColor);
	    break;
	case TypeKey:
	    valueString = ccsKeyBindingToString (&value->data->value.asKey);
	    break;
	case TypeButton:
	    valueString =
		ccsButtonBindingToString (&value->data->value.asButton);
	    break;
	case TypeEdge:
	    valueString = ccsEdgesToString (value->data->value.asEdge);
	    break;
	case TypeBell:
    	    strncpy (valueBuffer,
		     (value->data->value.asBell) ? "true" : "false", 100);
	    valueString = valueBuffer;
	    break;
	default:
	    valueString = NULL;
	    break;
	}

	if (!valueString)
	    return;

	fill = strlen (stringBuffer);
	/* the + 1 is the semicolon we're going to add */
	if ((fill + strlen (valueString) + 1) >= bufferSize)
	{
	    /* buffer is too small, make it larger */
	    bufferSize *= 2;
	    stringBuffer = realloc (stringBuffer, bufferSize);
	    if (!stringBuffer)
		return;

	    /* properly NULL terminate it */
	    stringBuffer[fill] = 0;
	}

	/* we made sure that the buffer is large enough before, so
	   there is no need for strncat */
	strcat (stringBuffer, valueString);
	strcat (stringBuffer, ";");

	if (listType == TypeColor  || listType == TypeKey ||
	    listType == TypeButton || listType == TypeEdge)
	{
	    free (valueString);
	}
	
	value = value->next;
    }

    setIniString (dictionary, section, entry, stringBuffer);
    free (stringBuffer);
}

void ccsIniRemoveEntry (IniDictionary * dictionary,

			const char * section,
			const char * entry)
{
    char *sectionName;

    asprintf (&sectionName, "%s:%s", section, entry);
    iniparser_unset (dictionary, sectionName);
    free (sectionName);
}
