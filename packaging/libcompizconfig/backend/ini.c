/**
 *
 * INI libccs backend
 *
 * ini.c
 *
 * Copyright (c) 2007 Danny Baumann <maniac@opencompositing.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 **/

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>

#include <ccs.h>
#include <ccs-backend.h>

#include <X11/X.h>
#include <X11/Xlib.h>

#define DEFAULTPROF "Default"
#define SETTINGPATH "compiz/compizconfig"

typedef struct _IniPrivData
{
    CCSContext *context;
    char * lastProfile;
    IniDictionary * iniFile;
    unsigned int iniWatchId;
}

IniPrivData;

static IniPrivData *privData = NULL;

static int privDataSize = 0;

/* forward declaration */
static void setProfile (IniPrivData *data, char *profile);

static IniPrivData*
findPrivFromContext (CCSContext *context)
{
    int i;
    IniPrivData *data;

    for (i = 0, data = privData; i < privDataSize; i++, data++)
	if (data->context == context)
	    break;

    if (i == privDataSize)
	return NULL;

    return data;
}

static char*
getIniFileName (char *profile)
{
    char *configDir = NULL;
    char *fileName = NULL;

    configDir = getenv ("XDG_CONFIG_HOME");
    if (configDir && strlen (configDir))
    {
	asprintf (&fileName, "%s/%s/%s.ini", configDir, SETTINGPATH, profile);
	return fileName;
    }

    configDir = getenv ("HOME");
    if (configDir && strlen (configDir))
    {
	asprintf (&fileName, "%s/.config/%s/%s.ini", configDir, SETTINGPATH,
		  profile);
	return fileName;
    }

    return NULL;
}

static void
processFileEvent (unsigned int watchId,
		  void         *closure)
{
    IniPrivData *data = (IniPrivData *) closure;
    char        *fileName;

    /* our ini file has been modified, reload it */

    if (data->iniFile)
	ccsIniClose (data->iniFile);

    fileName = getIniFileName (data->lastProfile);

    if (!fileName)
	return;

    data->iniFile = ccsIniOpen (fileName);

    ccsReadSettings (data->context);

    free (fileName);
}

static void
setProfile (IniPrivData *data,
	    char        *profile)
{
    char        *fileName;
    struct stat fileStat;

    if (data->iniFile)
	ccsIniClose (data->iniFile);

    if (data->iniWatchId)
	ccsRemoveFileWatch (data->iniWatchId);

    data->iniFile = NULL;

    data->iniWatchId = 0;

    /* first we need to find the file name */
    fileName = getIniFileName (profile);

    if (!fileName)
	return;

    /* if the file does not exist, we have to create it */
    if (stat (fileName, &fileStat) == -1)
    {
	if (errno == ENOENT)
	{
	    FILE *file;
	    file = fopen (fileName, "w");

	    if (!file)
	    {
		free (fileName);
		return;
	    }
	    fclose (file);
	}
	else
	{
	    free (fileName);
	    return;
	}
    }

    data->iniWatchId = ccsAddFileWatch (fileName, TRUE,
					processFileEvent, data);

    /* load the data from the file */
    data->iniFile = ccsIniOpen (fileName);

    free (fileName);
}

static Bool
initBackend (CCSContext * context)
{
    IniPrivData *newData;

    privData = realloc (privData, (privDataSize + 1) * sizeof (IniPrivData));
    newData = privData + privDataSize;

    /* initialize the newly allocated part */
    memset (newData, 0, sizeof (IniPrivData));
    newData->context = context;

    privDataSize++;

    return TRUE;
}

static Bool
finiBackend (CCSContext * context)
{
    IniPrivData *data;

    data = findPrivFromContext (context);

    if (!data)
	return FALSE;

    if (data->iniFile)
	ccsIniClose (data->iniFile);

    if (data->iniWatchId)
	ccsRemoveFileWatch (data->iniWatchId);

    if (data->lastProfile)
	free (data->lastProfile);

    privDataSize--;

    if (privDataSize)
	privData = realloc (privData, privDataSize * sizeof (IniPrivData));
    else
    {
	free (privData);
	privData = NULL;
    }

    return TRUE;
}

static Bool
readInit (CCSContext * context)
{
    char *currentProfile;
    IniPrivData *data;

    data = findPrivFromContext (context);

    if (!data)
	return FALSE;

    currentProfile = ccsGetProfile (context);

    if (!currentProfile || !strlen (currentProfile))
	currentProfile = strdup (DEFAULTPROF);
    else
	currentProfile = strdup (currentProfile);

    if (!data->lastProfile || (strcmp (data->lastProfile, currentProfile) != 0))
	setProfile (data, currentProfile);

    if (data->lastProfile)
	free (data->lastProfile);

    data->lastProfile = currentProfile;

    return (data->iniFile != NULL);
}

static void
readSetting (CCSContext *context,
	     CCSSetting *setting)
{
    Bool         status = FALSE;
    char        *keyName;
    IniPrivData *data;

    data = findPrivFromContext (context);
    if (!data)
	return;

    if (setting->isScreen)
	asprintf (&keyName, "s%d_%s", setting->screenNum, setting->name);
    else
	asprintf (&keyName, "as_%s", setting->name);

    switch (setting->type)
    {
    case TypeString:
	{
	    char *value;
	    if (ccsIniGetString (data->iniFile, setting->parent->name,
				 keyName, &value))
	    {
		ccsSetString (setting, value);
		free (value);
		status = TRUE;
	    }
	}
	break;
    case TypeMatch:
	{
	    char *value;
	    if (ccsIniGetString (data->iniFile, setting->parent->name,
				 keyName, &value))
	    {
		ccsSetMatch (setting, value);
		free (value);
		status = TRUE;
	    }
	}
	break;
    case TypeInt:
	{
	    int value;
	    if (ccsIniGetInt (data->iniFile, setting->parent->name,
			      keyName, &value))
	    {
		ccsSetInt (setting, value);
		status = TRUE;
	    }
	}
	break;
    case TypeBool:
	{
	    Bool value;
	    if (ccsIniGetBool (data->iniFile, setting->parent->name,
			       keyName, &value))
	    {
		ccsSetBool (setting, (value != 0));
		status = TRUE;
	    }
	}
	break;
    case TypeFloat:
	{
	    float value;
	    if (ccsIniGetFloat (data->iniFile, setting->parent->name,
				keyName, &value))
	    {
		ccsSetFloat (setting, value);
		status = TRUE;
	    }
	}
	break;
    case TypeColor:
	{
	    CCSSettingColorValue color;

	    if (ccsIniGetColor (data->iniFile, setting->parent->name,
				keyName, &color))
	    {
		ccsSetColor (setting, color);
		status = TRUE;
	    }
	}
	break;
    case TypeKey:
	{
	    CCSSettingKeyValue key;
	    if (ccsIniGetKey (data->iniFile, setting->parent->name,
			      keyName, &key))
	    {
		ccsSetKey (setting, key);
		status = TRUE;
	    }
	}
	break;
    case TypeButton:
	{
	    CCSSettingButtonValue button;
	    if (ccsIniGetButton (data->iniFile, setting->parent->name,
				 keyName, &button))
	    {
		ccsSetButton (setting, button);
		status = TRUE;
	    }
	}
	break;
    case TypeEdge:
	{
	    unsigned int edges;
	    if (ccsIniGetEdge (data->iniFile, setting->parent->name,
				 keyName, &edges))
	    {
		ccsSetEdge (setting, edges);
		status = TRUE;
	    }
	}
	break;
    case TypeBell:
	{
	    Bool bell;
	    if (ccsIniGetBell (data->iniFile, setting->parent->name,
			       keyName, &bell))
	    {
		ccsSetBell (setting, bell);
		status = TRUE;
	    }
	}
	break;
    case TypeList:
	{
	    CCSSettingValueList value;
	    if (ccsIniGetList (data->iniFile, setting->parent->name,
			       keyName, &value, setting))
	    {
		ccsSetList (setting, value);
		ccsSettingValueListFree (value, TRUE);
		status = TRUE;
	    }
	}
	break;
    default:
	break;
    }

    if (!status)
    {
	/* reset setting to default if it could not be read */
	ccsResetToDefault (setting);
    }

    if (keyName)
	free (keyName);
}

static void
readDone (CCSContext * context)
{
}

static Bool
writeInit (CCSContext * context)
{
    char *currentProfile;
    IniPrivData *data;

    data = findPrivFromContext (context);

    if (!data)
	return FALSE;

    currentProfile = ccsGetProfile (context);

    if (!currentProfile || !strlen (currentProfile))
	currentProfile = strdup (DEFAULTPROF);
    else
	currentProfile = strdup (currentProfile);

    if (!data->lastProfile || (strcmp (data->lastProfile, currentProfile) != 0))
	setProfile (data, currentProfile);

    if (data->lastProfile)
	free (data->lastProfile);

    ccsDisableFileWatch (data->iniWatchId);

    data->lastProfile = currentProfile;

    return (data->iniFile != NULL);
}

static void
writeSetting (CCSContext *context,
	      CCSSetting *setting)
{
    char        *keyName;
    IniPrivData *data;

    data = findPrivFromContext (context);
    if (!data)
	return;

    if (setting->isScreen)
	asprintf (&keyName, "s%d_%s", setting->screenNum, setting->name);
    else
	asprintf (&keyName, "as_%s", setting->name);

    if (setting->isDefault)
    {
	ccsIniRemoveEntry (data->iniFile, setting->parent->name, keyName);
	free (keyName);
	return;
    }

    switch (setting->type)
    {
    case TypeString:
	{
	    char *value;
	    if (ccsGetString (setting, &value))
		ccsIniSetString (data->iniFile, setting->parent->name,
				 keyName, value);
	}
	break;
    case TypeMatch:
	{
	    char *value;
	    if (ccsGetMatch (setting, &value))
		ccsIniSetString (data->iniFile, setting->parent->name,
				 keyName, value);
	}
	break;
    case TypeInt:
	{
	    int value;
	    if (ccsGetInt (setting, &value))
		ccsIniSetInt (data->iniFile, setting->parent->name,
			      keyName, value);
	}
	break;
    case TypeFloat:
	{
	    float value;
	    if (ccsGetFloat (setting, &value))
		ccsIniSetFloat (data->iniFile, setting->parent->name,
				keyName, value);
	}
	break;
    case TypeBool:
	{
	    Bool value;
	    if (ccsGetBool (setting, &value))
		ccsIniSetBool (data->iniFile, setting->parent->name,
			       keyName, value);
	}
	break;
    case TypeColor:
	{
	    CCSSettingColorValue value;
	    if (ccsGetColor (setting, &value))
		ccsIniSetColor (data->iniFile, setting->parent->name,
				keyName, value);
	}
	break;
    case TypeKey:
	{
	    CCSSettingKeyValue value;
	    if (ccsGetKey (setting, &value))
		ccsIniSetKey (data->iniFile, setting->parent->name,
			      keyName, value);
	}
	break;
    case TypeButton:
	{
	    CCSSettingButtonValue value;
	    if (ccsGetButton (setting, &value))
		ccsIniSetButton (data->iniFile, setting->parent->name,
				 keyName, value);
	}
	break;
    case TypeEdge:
	{
	    unsigned int value;
	    if (ccsGetEdge (setting, &value))
		ccsIniSetEdge (data->iniFile, setting->parent->name,
			       keyName, value);
	}
	break;
    case TypeBell:
	{
	    Bool value;
	    if (ccsGetBell (setting, &value))
		ccsIniSetBell (data->iniFile, setting->parent->name,
			       keyName, value);
	}
	break;
    case TypeList:
	{
	    CCSSettingValueList value;
	    if (ccsGetList (setting, &value))
		ccsIniSetList (data->iniFile, setting->parent->name,
			       keyName, value, setting->info.forList.listType);
	}
	break;
    default:
	break;
    }

    if (keyName)
	free (keyName);
}

static void
writeDone (CCSContext * context)
{
    /* export the data to ensure the changes are on disk */
    char        *fileName;
    char        *currentProfile;
    IniPrivData *data;

    data = findPrivFromContext (context);
    if (!data)
	return;

    currentProfile = ccsGetProfile (context);

    if (!currentProfile || !strlen (currentProfile))
	currentProfile = strdup (DEFAULTPROF);
    else
	currentProfile = strdup (currentProfile);

    fileName = getIniFileName (currentProfile);

    free (currentProfile);

    ccsIniSave (data->iniFile, fileName);

    ccsEnableFileWatch (data->iniWatchId);

    free (fileName);
}

static Bool
getSettingIsReadOnly (CCSSetting * setting)
{
    /* FIXME */
    return FALSE;
}

static int
profileNameFilter (const struct dirent *name)
{
    int length = strlen (name->d_name);

    if (strncmp (name->d_name + length - 4, ".ini", 4))
	return 0;

    return 1;
}

static CCSStringList
scanConfigDir (char * filePath)
{
    CCSStringList  ret = NULL;
    struct dirent  **nameList;
    char           *pos;
    int            nFile, i;

    nFile = scandir (filePath, &nameList, profileNameFilter, NULL);
    if (nFile <= 0)
	return NULL;

    for (i = 0; i < nFile; i++)
    {
	pos = strrchr (nameList[i]->d_name, '.');
	if (pos)
	{
	    *pos = 0;

	    if (strcmp (nameList[i]->d_name, DEFAULTPROF) != 0)
		ret = ccsStringListAppend (ret, strdup (nameList[i]->d_name));
	}

	free (nameList[i]);
    }

    free (nameList);
    
    return ret;
}

static CCSStringList
getExistingProfiles (CCSContext * context)
{
    CCSStringList  ret = NULL;
    char	   *filePath = NULL;
    char           *homeDir = NULL;
    char	   *configDir = NULL;
    
    configDir = getenv ("XDG_CONFIG_HOME");
    if (configDir && strlen (configDir))
    {
	asprintf (&filePath, "%s/%s", configDir, SETTINGPATH);
	
	ret = scanConfigDir(filePath);
	free(filePath);

	if (ret)
	    return ret;
    }
    
    homeDir = getenv ("HOME");
    if (!homeDir)
	return NULL;

    asprintf (&filePath, "%s/.config/%s", homeDir, SETTINGPATH);
    if (!filePath)
	return NULL;

    ret = scanConfigDir(filePath);
    free(filePath);

    return ret;
}

static Bool
deleteProfile (CCSContext * context, char * profile)
{
    char *fileName;

    fileName = getIniFileName (profile);

    if (!fileName)
	return FALSE;

    remove (fileName);
    free (fileName);

    return TRUE;
}


static CCSBackendVTable iniVTable = {
    "ini",
    "Flat-file Configuration Backend",
    "Flat file Configuration Backend for libccs",
    FALSE,
    TRUE,
    NULL,
    initBackend,
    finiBackend,
    readInit,
    readSetting,
    readDone,
    writeInit,
    writeSetting,
    writeDone,
    NULL,
    getSettingIsReadOnly,
    getExistingProfiles,
    deleteProfile
};

CCSBackendVTable *
getBackendInfo (void)
{
    return &iniVTable;
}

