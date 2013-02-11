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

#ifdef HAVE_CONFIG_H
# include "../config.h"
#endif

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>

#if HAVE_SYS_INOTIFY_H
#include <sys/inotify.h>
#endif

#include <fcntl.h>

#include <ccs.h>
#include "ccs-private.h"

typedef struct _FilewatchData
{
    char *fileName;
    int watchDesc;
    unsigned int watchId;
    FileWatchCallbackProc callback;
    void *closure;
}

FilewatchData;

static FilewatchData *fwData = NULL;
static int           fwDataSize = 0;
static int           inotifyFd = 0;

static inline int
findDataIndexById (unsigned int watchId)
{
    int i, index = -1;

    for (i = 0; i < fwDataSize; i++)
	if (fwData[i].watchId == watchId)
	{
	    index = i;
	    break;
	}

    return index;
}

void ccsCheckFileWatches (void)
{
#if HAVE_SYS_INOTIFY_H
    char                 buf[256 * (sizeof (struct inotify_event) + 16)];
    struct inotify_event *event;
    int	                 len, i = 0, j;

    if (!inotifyFd)
	return;

    len = read (inotifyFd, buf, sizeof (buf));
    if (len < 0)
	return;

    while (i < len)
    {
	event = (struct inotify_event *) & buf[i];

	for (j = 0; j < fwDataSize; j++)
	    if ((fwData[j].watchDesc == event->wd) && fwData[j].callback)
		(*fwData[j].callback) (fwData[j].watchId, fwData[j].closure);

	i += sizeof (*event) + event->len;
    }
#endif
}

unsigned int ccsAddFileWatch (const char            *fileName,
			      Bool                  enable,
			      FileWatchCallbackProc callback,
			      void                  *closure)
{
    unsigned int i, maxWatchId = 0;

#if HAVE_SYS_INOTIFY_H
    if (!inotifyFd)
    {
	inotifyFd = inotify_init ();
	fcntl (inotifyFd, F_SETFL, O_NONBLOCK);
    }
#endif

    fwData = realloc (fwData, (fwDataSize + 1) * sizeof (FilewatchData));
    if (!fwData)
    {
	fwDataSize = 0;
	return 0;
    }

    fwData[fwDataSize].fileName  = strdup (fileName);

#if HAVE_SYS_INOTIFY_H
    if (enable)
	fwData[fwDataSize].watchDesc =
	    inotify_add_watch (inotifyFd, fileName,
			       IN_MODIFY | IN_MOVE | IN_MOVE_SELF |
			       IN_DELETE_SELF | IN_CREATE | IN_DELETE);
    else
#endif
	fwData[fwDataSize].watchDesc = 0;

    fwData[fwDataSize].callback  = callback;
    fwData[fwDataSize].closure   = closure;

    /* determine current highest ID */
    for (i = 0; i < fwDataSize; i++)
	if (fwData[i].watchId > maxWatchId)
	    maxWatchId = fwData[i].watchId;

    fwData[fwDataSize].watchId = maxWatchId + 1;
    fwDataSize++;

    return (maxWatchId + 1);
}

void
ccsRemoveFileWatch (unsigned int watchId)

{
    int selectedIndex, i;

    selectedIndex = findDataIndexById (watchId);
    /* not found */
    if (selectedIndex < 0)
	return;

    /* clear entry */
    free (fwData[selectedIndex].fileName);

#if HAVE_SYS_INOTIFY_H
    if (fwData[selectedIndex].watchDesc)
	inotify_rm_watch (inotifyFd, fwData[selectedIndex].watchDesc);
#endif

    /* shrink array */
    for (i = selectedIndex; i < (fwDataSize - 1); i++)
	fwData[i] = fwData[i+1];

    fwDataSize--;

    if (fwDataSize > 0)
    {
	fwData = realloc (fwData, fwDataSize * sizeof (FilewatchData));
	if (!fwData)
	    fwDataSize = 0;
    }
    else
    {
	free (fwData);
	fwData = NULL;
    }

    if (!fwDataSize)
    {
	if (inotifyFd)
	    close (inotifyFd);
	inotifyFd = 0;
    }
}

void
ccsDisableFileWatch (unsigned int watchId)
{
    int index;

    index = findDataIndexById (watchId);
    if (index < 0)
	return;

#if HAVE_SYS_INOTIFY_H
    if (fwData[index].watchDesc)
    {
	inotify_rm_watch (inotifyFd, fwData[index].watchDesc);
	fwData[index].watchDesc = 0;
    }
#endif
}

void
ccsEnableFileWatch (unsigned int watchId)
{
    int index;

    index = findDataIndexById (watchId);
    if (index < 0)
	return;

#if HAVE_SYS_INOTIFY_H
    if (!fwData[index].watchDesc)
	fwData[index].watchDesc =
	    inotify_add_watch (inotifyFd,
			       fwData[index].fileName,
			       IN_MODIFY | IN_MOVE | IN_MOVE_SELF |
			       IN_DELETE_SELF | IN_CREATE | IN_DELETE);
#endif
}

