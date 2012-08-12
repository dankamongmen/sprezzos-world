/* Copyright (C) 2011, Aurelien Jarno <aurelien@aurel32.net>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/time.h>

void *memcpy(void *dst, const void *src, size_t n)
{
#ifndef NOLOG
    uintptr_t usrc, udst;

    /* Convert to unsigned as arithmetic on pointer is undefined */ 
    udst = (uintptr_t) dst;
    usrc = (uintptr_t) src;

    /* Check if source and destination overlap */
    if (((udst < usrc) && ((udst + n) > usrc)) ||
        ((usrc < udst) && ((usrc + n) > udst))) {

        static time_t lastlog = -1;
        struct timeval tv;

        /* gettimeofday() is not expensive for the conditions we target in 
         * the Debian package (kernel >= 2.6.26, x86-64 architecture), this
         * might need to be changed if this wrapper is later needed with 
         * different conditions */
        gettimeofday(&tv, NULL);

        /* Don't spam syslog, limit to (roughly) one log entry per second. */
        if (tv.tv_sec > lastlog) {
            lastlog = tv.tv_sec;
            syslog(LOG_WARNING | LOG_USER, 
                   "source and destination overlap in memcpy() at ip %p",
		   __builtin_return_address(0));
        }
    }
#endif 

    /* Call memmove() instead of memcpy() */
    return memmove(dst, src, n);
}

