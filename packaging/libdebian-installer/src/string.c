/*
 * string.c
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *               2003 Bastian Blank <waldi@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <debian-installer/string.h>

#include <debian-installer/hash.h>
#include <debian-installer/mem.h>

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

int di_snprintfcat (char *str, size_t size, const char *format, ...)
{
  va_list ap;
  int retval;
  size_t len = strlen (str);

  va_start (ap, format);
  retval = vsnprintf (str + len, size - len, format, ap);
  va_end (ap);

  return retval;
}

char *di_stradup (const char *s, size_t n)
{
  size_t len = n + 1;
  char *string = di_new (char, len);
  string[n] = '\0';
  return memcpy (string, s, n);
}

bool di_rstring_equal (const void *key1, const void *key2)
{
  const di_rstring *rstring1 = key1;
  const di_rstring *rstring2 = key2;

  if (rstring1->size == rstring2->size)
    return strncasecmp (rstring1->string, rstring2->string, rstring1->size) == 0;
  return false;
}

uint32_t di_rstring_hash (const void *key)
{
  const di_rstring *rstring = key;
  const char *p = rstring->string;
  size_t n = 0;
  uint32_t h = 0;

  for (; n < rstring->size; n++)
    h = (h << 5) - h + tolower (*p++);

  return h;
}

#if 0
bool di_string_equal (const void *key1, const void *key2)
{
  const char *string1 = key1;
  const char *string2 = key2;

  return strcmp (string1, string2) == 0;
}

uint32_t di_string_hash (const void *key)
{
  const char *p = key;
  uint32_t h = *p;

  if (h)
    for (p += 1; *p != '\0'; p++)
      h = (h << 5) - h + *p;

  return h;
}
#endif

