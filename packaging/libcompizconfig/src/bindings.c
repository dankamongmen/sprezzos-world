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

/*
 * Based on Compiz option.c
 * Copyright © 2005 Novell, Inc.
 * Author: David Reveman <davidr@novell.com>
 */


#define _GNU_SOURCE

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include <X11/X.h>
#include <X11/Xlib.h>

#include <ccs.h>

#define CompAltMask        (1 << 16)
#define CompMetaMask       (1 << 17)
#define CompSuperMask      (1 << 18)
#define CompHyperMask      (1 << 19)
#define CompModeSwitchMask (1 << 20)
#define CompNumLockMask    (1 << 21)
#define CompScrollLockMask (1 << 22)

#define SCREEN_EDGE_LEFT	(1 << 0)
#define SCREEN_EDGE_RIGHT	(1 << 1)
#define SCREEN_EDGE_TOP		(1 << 2)
#define SCREEN_EDGE_BOTTOM	(1 << 3)
#define SCREEN_EDGE_TOPLEFT	(1 << 4)
#define SCREEN_EDGE_TOPRIGHT	(1 << 5)
#define SCREEN_EDGE_BOTTOMLEFT	(1 << 6)
#define SCREEN_EDGE_BOTTOMRIGHT (1 << 7)

struct _Modifier
{
    char *name;
    int  modifier;
}

modifierList[] = {
    { "<Shift>",      ShiftMask		 },
    { "<Control>",    ControlMask	 },
    { "<Mod1>",	      Mod1Mask		 },
    { "<Mod2>",	      Mod2Mask		 },
    { "<Mod3>",	      Mod3Mask		 },
    { "<Mod4>",	      Mod4Mask		 },
    { "<Mod5>",	      Mod5Mask		 },
    { "<Alt>",	      CompAltMask        },
    { "<Meta>",	      CompMetaMask       },
    { "<Super>",      CompSuperMask      },
    { "<Hyper>",      CompHyperMask	 },
    { "<ModeSwitch>", CompModeSwitchMask },
};

#define N_MODIFIERS (sizeof (modifierList) / sizeof (struct _Modifier))

struct _Edge {
    char *name;
    char *modName;
    int  modifier;
}

edgeList[] = {
    { "Left",        "<LeftEdge>",	  SCREEN_EDGE_LEFT },
    { "Right",       "<RightEdge>",	  SCREEN_EDGE_RIGHT },
    { "Top",         "<TopEdge>",	  SCREEN_EDGE_TOP },
    { "Bottom",      "<BottomEdge>",	  SCREEN_EDGE_BOTTOM },
    { "TopLeft",     "<TopLeftEdge>",	  SCREEN_EDGE_TOPLEFT },
    { "TopRight",    "<TopRightEdge>",	  SCREEN_EDGE_TOPRIGHT },
    { "BottomLeft",  "<BottomLeftEdge>",  SCREEN_EDGE_BOTTOMLEFT },
    { "BottomRight", "<BottomRightEdge>", SCREEN_EDGE_BOTTOMRIGHT }
};

#define N_EDGES (sizeof (edgeList) / sizeof (edgeList[0]))

static char *
stringAppend (char *s,
	      char *a)
{
    char *r;
    int  len;

    if (!a)
	return s;

    len = strlen (a);

    if (s)
	len += strlen (s);

    r = malloc (len + 1);

    if (r)
    {
	if (s)
	{
	    sprintf (r, "%s%s", s, a);
	    free (s);
	}
	else
	{
	    sprintf (r, "%s", a);
	}

	s = r;
    }

    return s;
}

char *
ccsModifiersToString (unsigned int modMask)
{
    char *binding = NULL;
    int  i;

    for (i = 0; i < N_MODIFIERS; i++)
    {
	if (modMask & modifierList[i].modifier)
	    binding = stringAppend (binding, modifierList[i].name);
    }

    return binding;
}

char *
ccsEdgesToModString (unsigned int edgeMask)
{
    char *binding = NULL;
    int  i;

    for (i = 0; i < N_EDGES; i++)
    {
	if (edgeMask & edgeList[i].modifier)
	    binding = stringAppend (binding, edgeList[i].modName);
    }

    return binding;
}

char *
ccsEdgesToString (unsigned int edgeMask)
{
    char *binding = NULL;
    int  i;

    for (i = 0; i < N_EDGES; i++)
    {
	if (edgeMask & edgeList[i].modifier)
	{
	    if (binding)
		binding = stringAppend (binding, "|");
	    binding = stringAppend (binding, edgeList[i].name);
	}
    }

    if (!binding)
	return strdup ("");

    return binding;
}

char *
ccsKeyBindingToString (CCSSettingKeyValue *key)
{
    char *binding;

    binding = ccsModifiersToString (key->keyModMask);

    if (key->keysym != NoSymbol)
    {
	char *keyname;

	keyname = XKeysymToString (key->keysym);
	if (keyname)
	{
	    binding = stringAppend (binding, keyname);
	}
    }

    if (!binding)
	return strdup ("Disabled");
    return binding;
}

char *
ccsButtonBindingToString (CCSSettingButtonValue *button)
{
    char *binding;
    char *edges;
    char buttonStr[256];

    edges = ccsEdgesToModString (button->edgeMask);
    binding = stringAppend (edges, ccsModifiersToString (button->buttonModMask));

    if (button->button)
    {
        snprintf (buttonStr, 256, "Button%d", button->button);
        binding = stringAppend (binding, buttonStr);
    }

    if (!binding)
	return strdup ("Disabled");
    return binding;
}

unsigned int
ccsStringToModifiers (const char *binding)
{
    unsigned int mods = 0;
    int		 i;

    for (i = 0; i < N_MODIFIERS; i++)
    {
	if (strcasestr (binding, modifierList[i].name))
	    mods |= modifierList[i].modifier;
    }

    return mods;
}

unsigned int
ccsStringToEdges (const char *binding)
{
    unsigned int edgeMask = 0;
    const char   *needle;
    int          i;

    for (i = 0; i < N_EDGES; i++)
    {
        int edgeLen = strlen (edgeList[i].name);

        /* Look for all occurrences of edgeList[i].name in binding */
        needle = binding;
        while ((needle = strstr (needle, edgeList[i].name)) != NULL)
        {
            if (needle != binding && isalnum (*(needle - 1)))
            {
                needle += edgeLen;
                continue;
            }

            needle += edgeLen;

            if (*needle && isalnum (*needle))
                continue;

            edgeMask |= 1 << i;
        }
    }

    return edgeMask;

}

unsigned int
ccsModStringToEdges (const char *binding)
{
    unsigned int mods = 0;
    int		 i;

    for (i = 0; i < N_EDGES; i++)
    {
	if (strcasestr (binding, edgeList[i].modName))
	    mods |= edgeList[i].modifier;
    }

    return mods;
}

Bool
ccsStringToKeyBinding (const char         *binding,
		       CCSSettingKeyValue *value)
{
    char	  *ptr;
    unsigned int  mods;
    KeySym	  keysym;

    if (!binding || !strlen(binding) ||
	strncasecmp (binding, "Disabled", strlen ("Disabled")) == 0)
    {
	value->keysym     = 0;
	value->keyModMask = 0;
	return TRUE;
    }

    mods = ccsStringToModifiers (binding);

    ptr = strrchr (binding, '>');

    if (ptr)
	binding = ptr + 1;

    while (*binding && !isalnum (*binding))
	binding++;

    if (!*binding)
    {
	if (mods)
	{
	    value->keysym     = 0;
	    value->keyModMask = mods;

	    return TRUE;
	}

	return FALSE;
    }

    keysym = XStringToKeysym (binding);

    if (keysym != NoSymbol)
    {
	value->keysym     = keysym;
	value->keyModMask = mods;

	return TRUE;
    }

    return FALSE;
}

Bool
ccsStringToButtonBinding (const char            *binding,
			  CCSSettingButtonValue *value)
{
    char	 *ptr;
    unsigned int mods;
    unsigned int edges;

    if (!binding || !strlen(binding) ||
	strncmp (binding, "Disabled", strlen ("Disabled")) == 0)
    {
	value->button        = 0;
	value->buttonModMask = 0;
	value->edgeMask      = 0;
	return TRUE;
    }

    mods = ccsStringToModifiers (binding);
    edges = ccsModStringToEdges (binding);

    ptr = strrchr (binding, '>');

    if (ptr)
	binding = ptr + 1;

    while (*binding && !isalnum (*binding))
	binding++;

    if (strncmp (binding, "Button", strlen ("Button")) == 0)
    {
	int buttonNum;

	if (sscanf (binding + strlen ("Button"), "%d", &buttonNum) == 1)
	{
	    value->button        = buttonNum;
	    value->buttonModMask = mods;
	    value->edgeMask      = edges;

	    return TRUE;
	}
    }

    return FALSE;
}

Bool
ccsStringToColor (const char          *value,
		  CCSSettingColorValue *color)
{
    int c[4];

    if (sscanf (value, "#%2x%2x%2x%2x", &c[0], &c[1], &c[2], &c[3]) == 4)
    {
	color->color.red   = c[0] << 8 | c[0];
	color->color.green = c[1] << 8 | c[1];
	color->color.blue  = c[2] << 8 | c[2];
	color->color.alpha = c[3] << 8 | c[3];

	return TRUE;
    }

    return FALSE;
}

char *
ccsColorToString (CCSSettingColorValue *color)
{
    char tmp[256];

    snprintf (tmp, 256, "#%.2x%.2x%.2x%.2x",
	      color->color.red >> 8, color->color.green >> 8,
	      color->color.blue >> 8, color->color.alpha >> 8);

    return strdup (tmp);
}

