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

#ifndef CCS_BACKEND_H
#define CCS_BACKEND_H

#include <ccs.h>

typedef struct _CCSBackend	  CCSBackend;
typedef struct _CCSBackendVTable  CCSBackendVTable;

struct _CCSBackend
{
    void             *dlhand;
    CCSBackendVTable *vTable;
};

typedef CCSBackendVTable * (*BackendGetInfoProc) (void);

typedef void (*CCSExecuteEventsFunc) (unsigned int flags);

typedef Bool (*CCSInitBackendFunc) (CCSContext * context);
typedef Bool (*CCSFiniBackendFunc) (CCSContext * context);

typedef Bool (*CCSContextReadInitFunc) (CCSContext * context);
typedef void (*CCSContextReadSettingFunc)
(CCSContext * context, CCSSetting * setting);
typedef void (*CCSContextReadDoneFunc) (CCSContext * context);

typedef Bool (*CCSContextWriteInitFunc) (CCSContext * context);
typedef void (*CCSContextWriteSettingFunc)
(CCSContext * context, CCSSetting * setting);
typedef void (*CCSContextWriteDoneFunc) (CCSContext * context);

typedef Bool (*CCSGetIsIntegratedFunc) (CCSSetting * setting);
typedef Bool (*CCSGetIsReadOnlyFunc) (CCSSetting * setting);

typedef CCSStringList (*CCSGetExistingProfilesFunc) (CCSContext * context);
typedef Bool (*CCSDeleteProfileFunc) (CCSContext * context, char * name);

struct _CCSBackendVTable
{
    char *name;
    char *shortDesc;
    char *longDesc;
    Bool integrationSupport;
    Bool profileSupport;

    /* something like a event loop call for the backend,
       so it can check for file changes (gconf changes in the gconf backend)
       no need for reload settings signals anymore */
    CCSExecuteEventsFunc executeEvents;

    CCSInitBackendFunc	       backendInit;
    CCSFiniBackendFunc	       backendFini;

    CCSContextReadInitFunc     readInit;
    CCSContextReadSettingFunc  readSetting;
    CCSContextReadDoneFunc     readDone;

    CCSContextWriteInitFunc    writeInit;
    CCSContextWriteSettingFunc writeSetting;
    CCSContextWriteDoneFunc    writeDone;


    CCSGetIsIntegratedFunc     getSettingIsIntegrated;
    CCSGetIsReadOnlyFunc       getSettingIsReadOnly;

    CCSGetExistingProfilesFunc getExistingProfiles;
    CCSDeleteProfileFunc       deleteProfile;
};

CCSBackendVTable* getBackendInfo (void);

#endif
