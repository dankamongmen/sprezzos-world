/*
 * Compiz configuration system library
 *
 * Copyright (C) 2007  Dennis Kasprzyk <onestone@beryl-project.org>
 * Copyright (C) 2007  Danny Baumann <maniac@beryl-project.org>
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


#ifndef _CSS_H
#define _CSS_H

#define D_NONE   0
#define D_NORMAL 1
#define D_FULL   2

#ifndef DEBUGLEVEL
# define DEBUGLEVEL D_NONE
#endif
#define D(x, fmt, args...)  { if (x <= DEBUGLEVEL) printf(fmt, ##args); }

#ifndef Bool
#define Bool int
#endif

#ifndef TRUE
#define TRUE ~0
#endif

#ifndef FALSE
#define FALSE 0
#endif

/**
 * list functions:
 * for each list there is a set of functions, explained using String as example
 *
 * ccsStringListAppend (list, item)
 * Adds an item at the end of the list. Returns the new list.
 *
 * ccsStringListPrepend (list, item)
 * Adds an item at the beginning of the list. Returns the new list.
 *
 * ccsStringListInsert (list, item, position)
 * Adds an item at a given position. Position is 0-based. If position is
 * larger than the amount of items in the list, the item is inserted at the
 * end of the list. Returns the new list.
 *
 * ccsStringListInsertBefore (list, sibling, item)
 * Inserts item before sibling into the list. If sibling is no list member,
 * item is inserted at the end. Returns the new list.
 *
 * ccsStringListLength (list)
 * Returns the amount of items in list.
 *
 * ccsStringListFind (list, item)
 * Finds and returns an item matching <item>. If nothing is found, returns NULL.
 *
 * ccsStringListGetItem (list, index)
 * Returns the list item at position <index>. If index is larger than the
 * amount of items in the list, returns NULL.
 *
 * ccsStringListRemove (list, item, freeObj)
 * Removes item from the list. If freeObj is TRUE, also frees the data item.
 * Returns the new list.
 *
 * ccsStringListFree (list, freeObj)
 * Frees the complete list. If freeObj is TRUE, also frees the data items.
 * Returns the new list (NULL).
 */
#define CCSLIST_HDR(type,dtype)		\
    typedef struct _CCS##type##List *	CCS##type##List;\
    struct _CCS##type##List	\
    {								\
	dtype   * data;			\
	CCS##type##List next;		\
    }; \
    CCS##type##List ccs##type##ListAppend (CCS##type##List list, dtype *data); \
    CCS##type##List ccs##type##ListPrepend (CCS##type##List list, dtype *data); \
    CCS##type##List ccs##type##ListInsert (CCS##type##List list, dtype *data, int position); \
    CCS##type##List ccs##type##ListInsertBefore (CCS##type##List list, CCS##type##List sibling, dtype *data); \
    unsigned int ccs##type##ListLength (CCS##type##List list); \
    CCS##type##List ccs##type##ListFind (CCS##type##List list, dtype *data); \
    CCS##type##List ccs##type##ListGetItem (CCS##type##List list, unsigned int index); \
    CCS##type##List ccs##type##ListRemove (CCS##type##List list, dtype *data, Bool freeObj); \
    CCS##type##List ccs##type##ListFree (CCS##type##List list, Bool freeObj);

typedef struct _CCSContext	  CCSContext;
typedef struct _CCSPlugin	  CCSPlugin;
typedef struct _CCSSetting	  CCSSetting;
typedef struct _CCSGroup	  CCSGroup;
typedef struct _CCSSubGroup	  CCSSubGroup;
typedef struct _CCSPluginCategory CCSPluginCategory;
typedef struct _CCSSettingValue	  CCSSettingValue;
typedef struct _CCSPluginConflict CCSPluginConflict;
typedef struct _CCSBackendInfo	  CCSBackendInfo;
typedef struct _CCSIntDesc	  CCSIntDesc;
typedef struct _CCSStrRestriction CCSStrRestriction;
typedef struct _CCSStrExtension   CCSStrExtension;

CCSLIST_HDR (Plugin, CCSPlugin)
CCSLIST_HDR (Setting, CCSSetting)
CCSLIST_HDR (String, char)
CCSLIST_HDR (Group, CCSGroup)
CCSLIST_HDR (SubGroup, CCSSubGroup)
CCSLIST_HDR (SettingValue, CCSSettingValue)
CCSLIST_HDR (PluginConflict, CCSPluginConflict)
CCSLIST_HDR (BackendInfo, CCSBackendInfo)
CCSLIST_HDR (IntDesc, CCSIntDesc)
CCSLIST_HDR (StrRestriction, CCSStrRestriction)
CCSLIST_HDR (StrExtension, CCSStrExtension)

struct _CCSContext
{
    CCSPluginList     plugins;         /* list of plugins settings
					  were loaded for */
    CCSPluginCategory *categories;     /* list of plugin categories */
    void              *privatePtr;     /* private pointer that can be used
					  by the caller */
    void              *ccsPrivate;     /* private pointer for compizconfig
					  internal usage */

    CCSSettingList    changedSettings; /* list of settings changed since last
					  settings write */

    unsigned int      *screens;        /* numbers of the available screens */
    unsigned int      numScreens;      /* number of screens */
};

struct _CCSBackendInfo
{
    char *name;              /* name of the backend */
    char *shortDesc;         /* backend's short description */
    char *longDesc;          /* backend's long description */
    Bool integrationSupport; /* does the backend support DE integration? */
    Bool profileSupport;     /* does the backend support profiles? */
};

struct _CCSPlugin
{
    char *name;                    /* plugin name */
    char *shortDesc;		   /* plugin short description */
    char *longDesc;		   /* plugin long description */
    char *hints;                   /* currently unused */
    char *category;		   /* plugin category name */

    CCSStringList loadAfter;       /* list of plugin names this plugin needs to
				      be loaded after */
    CCSStringList loadBefore;      /* list of plugin names this plugin needs to
				      be loaded before */
    CCSStringList requiresPlugin;  /* list of plugin names this plugin
				      requires */
    CCSStringList conflictPlugin;  /* list of plugin names this plugin
				      conflicts with */
    CCSStringList conflictFeature; /* list of feature names this plugin
				      conflicts with */
    CCSStringList providesFeature; /* list of feature names this plugin
				      provides */
    CCSStringList requiresFeature; /* list of feature names this plugin
				      requires */

    void       *privatePtr;        /* private pointer that can be used
				      by the caller */
    CCSContext *context;           /* context this plugin belongs to */

    void *ccsPrivate;              /* private pointer for compizconfig
				      internal usage */
};

typedef enum _CCSSettingType
{
    /* This needs to be in the same order as CompOptionType for consistency */
    TypeBool,
    TypeInt,
    TypeFloat,
    TypeString,
    TypeColor,
    TypeAction,
    TypeKey,
    TypeButton,
    TypeEdge,
    TypeBell,
    TypeMatch,
    TypeList,
    TypeNum
} CCSSettingType;

struct _CCSSubGroup
{
    char           *name;    /* sub group name in current locale */
    CCSSettingList settings; /* list of settings in this sub group */
};

struct _CCSGroup
{
    char            *name;     /* group name in current locale */
    CCSSubGroupList subGroups; /* list of sub groups in this group */
};

typedef enum _CCSPluginConflictType
{
    /* produced on plugin activation */
    ConflictRequiresPlugin,
    ConflictRequiresFeature,
    ConflictFeature,
    ConflictPlugin,
    /* produced on plugin deactivation */
    ConflictFeatureNeeded,
    ConflictPluginNeeded,
    ConflictPluginError,
} CCSPluginConflictType;

struct _CCSPluginConflict
{
    char *                value;   /* item (plugin / feature) name that
				      caused the conflict */
    CCSPluginConflictType type;    /* type of the conflict */
    CCSPluginList         plugins; /* list of conflicting plugins */
};

union _CCSSettingInfo;

struct _CCSIntDesc
{
    int  value; /* value the description is assigned to */
    char *name; /* description */
};

struct _CCSStrRestriction
{
    char *value; /* value the restriction is assigned to */
    char *name;  /* description */
};

struct _CCSStrExtension
{
    char *basePlugin;           /* plugin this extension extends */
    CCSStringList baseSettings; /* list of settings this extension extends */
    CCSStrRestrictionList restriction; /* list of added restriction items */

    Bool isScreen;          /* is this extension a screen setting extension? */
};

typedef struct _CCSSettingIntInfo
{
    int            min;  /* minimum value for this setting */
    int            max;  /* maximum value */
    CCSIntDescList desc; /* list of item descriptions */
} CCSSettingIntInfo;

typedef struct _CCSSettingFloatInfo
{
    float min;       /* minimum value for this setting */
    float max;       /* maximum value */
    float precision; /* precision (allowed increment) */
} CCSSettingFloatInfo;

typedef struct _CCSSettingStringInfo
{
    CCSStrRestrictionList restriction;  /* list of restriction items */
    int                sortStartsAt; /* the restriction index to start sorting
					at (defaults to -1 for no sorting) */
    Bool               extensible;   /* whether extension is allowed for
					this setting */
} CCSSettingStringInfo;

typedef struct _CCSSettingListInfo
{
    CCSSettingType        listType;  /* type of setting this list contains */
    union _CCSSettingInfo *listInfo; /* list of settings */
} CCSSettingListInfo;

typedef struct _CCSSettingActionInfo
{
    Bool internal; /* is this binding global or plugin internal*/
} CCSSettingActionInfo;

typedef union _CCSSettingInfo
{
    CCSSettingIntInfo    forInt;
    CCSSettingFloatInfo  forFloat;
    CCSSettingStringInfo forString;
    CCSSettingListInfo   forList;
    CCSSettingActionInfo forAction;
} CCSSettingInfo;

typedef struct _CCSSettingColorValueColor
{
    unsigned short red;
    unsigned short green;
    unsigned short blue;
    unsigned short alpha;
}

CCSSettingColorValueColor;

typedef struct _CCSSettingColorValueArray
{
    unsigned short array[4];
}

CCSSettingColorValueArray;

typedef union _CCSSettingColorValue
{
    CCSSettingColorValueColor color;
    CCSSettingColorValueArray array;
} CCSSettingColorValue;


typedef struct _CCSSettingKeyValue
{
    int          keysym;
    unsigned int keyModMask;
} CCSSettingKeyValue;

typedef struct _CCSSettingButtonValue
{
    int          button;
    unsigned int buttonModMask;
    unsigned int edgeMask;
} CCSSettingButtonValue;

typedef union _CCSSettingValueUnion
{
    Bool		  asBool;
    int			  asInt;
    float		  asFloat;
    char *		  asString;
    char *		  asMatch;
    CCSSettingColorValue  asColor;
    CCSSettingValueList   asList;
    CCSSettingKeyValue    asKey;
    CCSSettingButtonValue asButton;
    unsigned int	  asEdge;
    Bool		  asBell;
} CCSSettingValueUnion;

struct _CCSSettingValue
{
    CCSSettingValueUnion value;
    CCSSetting *	 parent;
    Bool		 isListChild;
};

struct _CCSSetting
{
    char *name;             /* setting name */
    char *shortDesc;        /* setting short description in current locale */
    char *longDesc;         /* setting long description in current locale */

    CCSSettingType type;    /* setting type */

    Bool	 isScreen;  /* is this setting a screen setting? */
    unsigned int screenNum; /* screen number this setting is assigned to, valid
			       if isScreen is TRUE */

    CCSSettingInfo info;    /* information assigned to this setting,
			       valid if the setting is an int, float, string
			       or list setting */

    char *group;	    /* group name in current locale */
    char *subGroup;	    /* sub group name in current locale */
    char *hints;	    /* hints in current locale */

    CCSSettingValue defaultValue; /* default value of this setting */
    CCSSettingValue *value;       /* actual value of this setting */
    Bool	    isDefault;    /* does the actual value match the default
				     value? */

    CCSPlugin *parent;            /* plugin this setting belongs to */
    void      *privatePtr;        /* private pointer for usage by the caller */
};

struct _CCSPluginCategory
{
    const char *name;      /* plugin category name */
    const char *shortDesc; /* plugin category short description */
    const char *longDesc;  /* plugin category long description */

    CCSStringList plugins; /* list of plugins in this category */
};

/* set basic metadata to TRUE and no additional
   metadata informations will be parsed */
void ccsSetBasicMetadata (Bool value);

/* Creates a new context for the screens given in screens and numScreens.
   Set numScreens to 0 to initialize for all screens.
   All plugin settings are automatically enumerated. */
CCSContext* ccsContextNew (unsigned int *screens,
			   unsigned int numScreens);

/* Creates a new context without auto-enumerating any plugin or setting.
   Behaves otherwise exactly like ccsContextNew. */
CCSContext* ccsEmptyContextNew (unsigned int *screens,
				unsigned int numScreens);

/* Destroys the allocated context. */
void ccsContextDestroy (CCSContext * context);

/* Load the plugin and setting metadata for a given plugin.
   Returns TRUE on success, FALSE otherwise. */
Bool ccsLoadPlugin (CCSContext *context,
		    char       *name);

/* Searches for a plugin identified by its name in the context.
   Returns the plugin struct if it could be found, NULL otherwise. */
CCSPlugin* ccsFindPlugin (CCSContext *context,
			  const char *name);

/* Searches for a setting in a plugin. screenNum is only valid if isScreen is
   TRUE. Returns the setting struct if the search was successful (setting with
   name <name> found and isScreen and screenNum matched the values of the
   setting), NULL otherwise. */
CCSSetting* ccsFindSetting (CCSPlugin    *plugin,
			    const char   *name,
			    Bool         isScreen,
			    unsigned int screenNum);

/* Returns TRUE if the named plugin is in the context and marked as currently
   active in Compiz, FALSE otherwise. */
Bool ccsPluginIsActive (CCSContext *context,
			char       *name);

void ccsFreeContext (CCSContext *context);
void ccsFreePlugin (CCSPlugin *plugin);
void ccsFreeSetting (CCSSetting *setting);
void ccsFreeGroup (CCSGroup *group);
void ccsFreeSubGroup (CCSSubGroup *subGroup);
void ccsFreeSettingValue (CCSSettingValue *value);
void ccsFreePluginConflict (CCSPluginConflict *value);
void ccsFreeBackendInfo (CCSBackendInfo *value);
void ccsFreeIntDesc (CCSIntDesc *value);
void ccsFreeStrRestriction (CCSStrRestriction *restriction);
void ccsFreeStrExtension (CCSStrExtension *extension);

#define ccsFreeString(val) free(val)

/* Setting setters. Set <setting> to value <data>. Return TRUE if new value
   matches data. If the new value doesn't match the old value, the setting
   is added to the context's changedSettings list. */
Bool ccsSetInt (CCSSetting *setting,
		int        data);
Bool ccsSetFloat (CCSSetting *setting,
		  float      data);
Bool ccsSetBool (CCSSetting *setting,
		 Bool       data);
Bool ccsSetString (CCSSetting *setting,
		   const char *data);
Bool ccsSetColor (CCSSetting           *setting,
		  CCSSettingColorValue data);
Bool ccsSetMatch (CCSSetting *setting,
		  const char *data);
Bool ccsSetKey (CCSSetting         *setting,
		CCSSettingKeyValue data);
Bool ccsSetButton (CCSSetting            *setting,
		   CCSSettingButtonValue data);
Bool ccsSetEdge (CCSSetting   *setting,
		 unsigned int data);
Bool ccsSetBell (CCSSetting *setting,
		 Bool       data);
Bool ccsSetList (CCSSetting          *setting,
		 CCSSettingValueList data);
Bool ccsSetValue (CCSSetting      *setting,
		  CCSSettingValue *data);

/* Compares two setting values. Returns TRUE if values match,
   FALSE otherwise. */
Bool ccsIsEqualColor (CCSSettingColorValue c1,
		      CCSSettingColorValue c2);
Bool ccsIsEqualKey (CCSSettingKeyValue c1,
		    CCSSettingKeyValue c2);
Bool ccsIsEqualButton (CCSSettingButtonValue c1,
		       CCSSettingButtonValue c2);

/* Setting getters. Returns TRUE if the setting value was successfully
   copied into <data>, FALSE otherwise. */
Bool ccsGetInt (CCSSetting *setting,
		int        *data);
Bool ccsGetFloat (CCSSetting *setting,
		  float      *data);
Bool ccsGetBool (CCSSetting *setting,
		 Bool       *data);
Bool ccsGetString (CCSSetting *setting,
		   char       **data);
Bool ccsGetColor (CCSSetting           *setting,
		  CCSSettingColorValue *data);
Bool ccsGetMatch (CCSSetting *setting,
		  char       **data);
Bool ccsGetKey (CCSSetting         *setting,
		CCSSettingKeyValue *data);
Bool ccsGetButton (CCSSetting            *setting,
		   CCSSettingButtonValue *data);
Bool ccsGetEdge (CCSSetting  *setting,
		 unsigned int *data);
Bool ccsGetBell (CCSSetting *setting,
		 Bool       *data);
Bool ccsGetList (CCSSetting          *setting,
		 CCSSettingValueList *data);

/* Retrieves a list of settings in a plugin */
CCSSettingList ccsGetPluginSettings (CCSPlugin *plugin);

/* Retrieves a list of setting groups in a plugin */
CCSGroupList ccsGetPluginGroups (CCSPlugin *plugin);

/* Converts a string list into a list of string settings.
   Return value needs to be freed by the caller. */
CCSSettingValueList ccsGetValueListFromStringList (CCSStringList list,
						   CCSSetting    *parent);
/* Converts a string setting value list into a string list.
   Return value needs to be freed by the caller. */
CCSStringList ccsGetStringListFromValueList (CCSSettingValueList list);

/* Converts a string list into a string array. If the return value is not
   NULL, the item count is copied into <num>. Return value needs to be freed
   by the caller. */
char** ccsGetStringArrayFromList (CCSStringList list,
				  int           *num);
/* Converts a string array with <num> items into a string list. Return value
   needs to be freed by the caller. */
CCSStringList ccsGetListFromStringArray (char **array,
					 int  num);

/* Converts a setting value list into an array of the setting item data type.
   Behaves similar to ccsGetStringArrayFromList. */
char** ccsGetStringArrayFromValueList (CCSSettingValueList list,
				       int                 *num);
char** ccsGetMatchArrayFromValueList (CCSSettingValueList list,
				      int                 *num);

int* ccsGetIntArrayFromValueList (CCSSettingValueList list,
				  int                 *num);
float* ccsGetFloatArrayFromValueList (CCSSettingValueList list,
				      int                 *num);
Bool * ccsGetBoolArrayFromValueList (CCSSettingValueList list,
				     int                 *num);
CCSSettingColorValue* ccsGetColorArrayFromValueList (CCSSettingValueList list,
	       					     int                 *num);

/* Converts an array of data items to a setting value list. Behaves similar
   to ccsGetListFromStringArray */
CCSSettingValueList ccsGetValueListFromStringArray (char       **array,
						    int        num,
						    CCSSetting *parent);
CCSSettingValueList ccsGetValueListFromMatchArray (char       **array,
						   int        num,
						   CCSSetting *parent);
CCSSettingValueList ccsGetValueListFromIntArray (int        *array,
						 int        num,
						 CCSSetting *parent);
CCSSettingValueList ccsGetValueListFromFloatArray (float      *array,
						   int        num,
						   CCSSetting *parent);
CCSSettingValueList ccsGetValueListFromBoolArray (Bool       *array,
						  int        num,
						  CCSSetting *parent);
CCSSettingValueList ccsGetValueListFromColorArray (CCSSettingColorValue *array,
						   int                  num,
						   CCSSetting           *parent);

/* Retrieves a list of plugins marked as active in Compiz for this context */
CCSPluginList ccsGetActivePluginList (CCSContext *context);

/* Retrieves a list of plugin names which are active in Compiz for a given
   context, sorted as needed according to load after/before/etc. rules */
CCSStringList ccsGetSortedPluginStringList (CCSContext *context);

/* Switches the backend for a context. Returns TRUE on successful switch,
   FALSE otherwise. */
Bool ccsSetBackend (CCSContext *context,
		    char       *name);
/* Retrieves the name of the backend active for the context. */
char * ccsGetBackend (CCSContext *context);

/* Enable/disable DE integration for a context. */
void ccsSetIntegrationEnabled (CCSContext *context,
			       Bool       value);

/* Sets the profile for a context. */
void ccsSetProfile (CCSContext *context,
		    char       *name);

/* Set plugin list autosort for a context. */
void ccsSetPluginListAutoSort (CCSContext *context,
			       Bool       value);

/* Retrieve current profile of the context. */
char * ccsGetProfile (CCSContext *context);

/* Retrieves current DE integration status for a context */
Bool ccsGetIntegrationEnabled (CCSContext *context);

/* Retrieves the autosort setting for a context. */
Bool ccsGetPluginListAutoSort (CCSContext *context);

/* Changes the plugin activeness status in compiz. If plugin list autosort
   is enabled, automatically writes a new sorted plugin list to the
   active_plugins setting. If autosort is disabled, it's up to the caller
   to do that. */
Bool ccsPluginSetActive (CCSPlugin *plugin,
			 Bool      value);

/* functions parsing/creating an action string -
   the returned strings must be free'd after usage! */

char * ccsModifiersToString (unsigned int modMask);

char * ccsEdgesToString (unsigned int edge);

char * ccsEdgesToModString (unsigned int edge);

char * ccsKeyBindingToString (CCSSettingKeyValue *key);

char * ccsButtonBindingToString (CCSSettingButtonValue *button);

char * ccsColorToString (CCSSettingColorValue *color);

unsigned int ccsStringToModifiers (const char *binding);

unsigned int ccsStringToEdges (const char *edge);

unsigned int ccsModStringToEdges (const char *edge);

Bool ccsStringToKeyBinding (const char         *binding,
			    CCSSettingKeyValue *key);

Bool ccsStringToButtonBinding (const char            *binding,
			       CCSSettingButtonValue *button);

Bool ccsStringToColor (const char           *value,
		       CCSSettingColorValue *color);

/* flag values for ccsProcessEvents */
#define ProcessEventsNoGlibMainLoopMask (1 << 0)

void ccsProcessEvents (CCSContext   *context,
		       unsigned int flags);

/* Read all setting values from disk */
void ccsReadSettings (CCSContext *context);

/* Read setting values for a given plugin */
void ccsReadPluginSettings (CCSPlugin *plugin);

/* Write all settings to disk */
void ccsWriteSettings (CCSContext *context);

/* Write changed settings to disk */
void ccsWriteChangedSettings (CCSContext *context);

/* Reset all settings to defaults. Settings that were non-default
   previously are added to the changedSettings list of the context. */
void ccsResetToDefault (CCSSetting * setting);

/* Exports a profile to a file. If skipDefaults is TRUE, only exports
   non-default settings. Returns TRUE on successful export, FALSE otherwise. */
Bool ccsExportToFile (CCSContext *context,
		      const char *fileName,
		      Bool skipDefaults);

/* Imports a profile from a file. If overwriteNonDefault is TRUE, also
   overwrites settings that were non-default before. Returns TRUE on success,
   FALSE otherwise. */
Bool ccsImportFromFile (CCSContext *context,
			const char *fileName,
			Bool       overwriteNonDefault);

/* File watch stuff */

typedef void (*FileWatchCallbackProc) (unsigned int watchId, void *closure);

unsigned int ccsAddFileWatch (const char            *fileName,
			      Bool                  enable,
			      FileWatchCallbackProc callback,
			      void                  *closure);

void ccsRemoveFileWatch (unsigned int watchId);
void ccsDisableFileWatch (unsigned int watchId);
void ccsEnableFileWatch (unsigned int watchId);

/* INI file stuff */

typedef struct _dictionary_
{
    /** Number of entries in dictionary */
    int n;
    /** Storage size */
    int size;
    /** List of string values */
    char **val;
    /** List of string keys */
    char **key ;
    /** List of hash values for keys */
    unsigned *hash;
} IniDictionary;

IniDictionary* ccsIniNew (void);
IniDictionary* ccsIniOpen (const char *fileName);
void ccsIniClose (IniDictionary *dictionary);
void ccsIniSave (IniDictionary *dictionary,
		 const char    *fileName);

Bool ccsCreateDirFor (const char *fileName);

Bool ccsIniGetString (IniDictionary *dictionary,
		      const char    *section,
		      const char    *entry,
		      char          **value);
Bool ccsIniGetInt (IniDictionary *dictionary,
		   const char    *section,
		   const char    *entry,
		   int           *value);
Bool ccsIniGetFloat (IniDictionary *dictionary,
		     const char    *section,
		     const char    *entry,
		     float         *value);
Bool ccsIniGetBool (IniDictionary *dictionary,
		    const char    *section,
		    const char    *entry,
		    Bool          *value);
Bool ccsIniGetColor (IniDictionary        *dictionary,
		     const char           *section,
		     const char           *entry,
		     CCSSettingColorValue *value);
Bool ccsIniGetKey (IniDictionary        *dictionary,
		   const char           *section,
		   const char           *entry,
		   CCSSettingKeyValue   *value);
Bool ccsIniGetButton (IniDictionary         *dictionary,
		      const char            *section,
		      const char            *entry,
		      CCSSettingButtonValue *value);
Bool ccsIniGetEdge (IniDictionary *dictionary,
		    const char    *section,
		    const char    *entry,
		    unsigned int  *value);
Bool ccsIniGetBell (IniDictionary *dictionary,
		    const char    *section,
		    const char    *entry,
		    Bool          *value);
Bool ccsIniGetList (IniDictionary       *dictionary,
		    const char          *section,
		    const char          *entry,
		    CCSSettingValueList *value,
		    CCSSetting          *parent);

void ccsIniSetString (IniDictionary *dictionary,
		      const char    *section,
		      const char    *entry,
		      char          *value);
void ccsIniSetInt (IniDictionary *dictionary,
		   const char    *section,
		   const char    *entry,
		   int           value);
void ccsIniSetFloat (IniDictionary *dictionary,
		     const char    *section,
		     const char    *entry,
		     float         value);
void ccsIniSetBool (IniDictionary *dictionary,
		    const char    *section,
		    const char    *entry,
		    Bool          value);
void ccsIniSetColor (IniDictionary        *dictionary,
		     const char           *section,
		     const char           *entry,
		     CCSSettingColorValue value);
void ccsIniSetKey (IniDictionary      *dictionary,
		   const char         *section,
		   const char         *entry,
		   CCSSettingKeyValue value);
void ccsIniSetButton (IniDictionary         *dictionary,
		      const char            *section,
		      const char            *entry,
		      CCSSettingButtonValue value);
void ccsIniSetEdge (IniDictionary *dictionary,
		     const char   *section,
		     const char   *entry,
		     unsigned int value);
void ccsIniSetBell (IniDictionary *dictionary,
		    const char    *section,
		    const char    *entry,
		    Bool          value);
void ccsIniSetList (IniDictionary       *dictionary,
		    const char          *section,
		    const char          *entry,
		    CCSSettingValueList value,
		    CCSSettingType      listType);

void ccsIniRemoveEntry (IniDictionary *dictionary,
			const char    *section,
			const char    *entry);

/* Checks if a plugin can be enabled. Returns a list of conflicts that
   would occur when loading the plugin. A return value of NULL means that
   the plugin can be enabled without problems. */
CCSPluginConflictList ccsCanEnablePlugin (CCSContext *context,
					  CCSPlugin  *plugin);

/* Checks if a plugin can be disabled. The meaning of the return value is the
   same as for ccsCanEnablePlugin */
CCSPluginConflictList ccsCanDisablePlugin (CCSContext *context,
					   CCSPlugin *plugin);

/* Enumerates the available profiles for the current backend. */
CCSStringList ccsGetExistingProfiles (CCSContext * context);

/* Deletes the profile with the given name. */
void ccsDeleteProfile (CCSContext *context,
		       char       *name);

/* Enumerates the available backends. */
CCSBackendInfoList ccsGetExistingBackends (void);

/* Checks if a given setting is integrated in the desktop environment. */
Bool ccsSettingIsIntegrated (CCSSetting *setting);

/* Checks if a given setting is read-only. */
Bool ccsSettingIsReadOnly (CCSSetting *setting);

CCSStrExtensionList ccsGetPluginStrExtensions (CCSPlugin *plugin);

#endif
