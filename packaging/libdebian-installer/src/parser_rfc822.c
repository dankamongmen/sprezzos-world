/*
 * parser_rfc822.c
 *
 * Copyright (C) 2003 Bastian Blank <waldi@debian.org>
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

#include <debian-installer/parser_rfc822.h>

#include <debian-installer/log.h>
#include <debian-installer/macros.h>
#include <debian-installer/string.h>

#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define READSIZE 16384

int di_parser_rfc822_read (char *begin, size_t size, di_parser_info *info, di_parser_read_entry_new entry_new, di_parser_read_entry_finish entry_finish, void *user_data)
{
  char *cur, *end;
  char *field_begin, *field_end;
#if MODIFIER
  char *field_modifier_begin, *field_modifier_end;
#endif
  char *value_begin, *value_end;
#ifndef HAVE_MEMRCHR
  char *temp;
#endif
  int nr = 0;
  size_t readsize;
  size_t field_size;
#if MODIFIER
  size_t field_modifier_size;
#endif
  size_t value_size;
  const di_parser_fieldinfo *fip = NULL;
  di_rstring field_string;
  di_rstring field_modifier_string;
  di_rstring value_string;
  void *act = NULL;
  bool pgp_mode = false;
  const char pgp_begin_msg[] = "-----BEGIN PGP SIGNED MESSAGE-----";
  const char pgp_begin_sig[] = "-----BEGIN PGP SIGNATURE-----";

  cur = begin;
  end = begin + size;

  if (!strncmp(cur, pgp_begin_msg, strlen(pgp_begin_msg)))
  {
    pgp_mode = true;
    // Skip PGP header
    cur = strstr (cur, "\n\n");
  }

  while (cur < end)
  {
    if (*cur == '\n')
    {
      cur++;
      continue;
    }

    nr++;

    if (entry_new)
      act = entry_new (user_data);
    else
      act = NULL;

    while (1)
    {
      if (pgp_mode && !strncmp(cur, pgp_begin_sig, strlen(pgp_begin_sig)))
      {
        // Let's exit, the rest of the file is not interesting
        cur += size;
        break;
      }

      field_begin = cur;
      readsize = end - field_begin < READSIZE ? end - field_begin : READSIZE;
      if (!readsize)
        break;
      field_end = memchr (cur, ':', readsize);
#if MODIFIER
      field_modifier_end = field_end;
#endif
      if (!field_end)
      {
        di_warning ("parser_rfc822: Iek! Don't find end of field!");
        return -1;
      }
      field_size = field_end - field_begin;

#if MODIFIER
#ifdef HAVE_MEMRCHR
      if ((field_modifier_begin = memrchr (field_begin, '-', field_end - field_begin)))
        field_modifier_begin++;
      if (field_modifier_begin)
#else
      field_modifier_begin = field_begin;
      while ((temp = memchr (field_modifier_begin, '-', field_end - field_modifier_begin)))
        field_modifier_begin = temp + 1;
      if (field_modifier_begin != field_begin)
#endif
      {
        field_modifier_size = field_modifier_end - field_modifier_begin;
      }
      else
      {
        field_modifier_begin = 0;
        field_modifier_size = 0;
      }
#endif

      value_begin = field_end + 1;
      while (value_begin < end && (*value_begin == ' ' || *value_begin == '\t'))
        value_begin++;
      readsize = end - field_begin < READSIZE ? end - field_begin : READSIZE;
      value_end = memchr (field_begin, '\n', readsize);
      if (!value_end)
      {
        di_warning ("parser_rfc822: Iek! Don't find end of value!");
        return -1;
      }
      if (value_end < field_end)
      {
        di_warning ("parser_rfc822: Iek! Don't find end of field, it seems to be after the end of the line!");
        return -1;
      }

      /* while (isblank (value_end[1])) FIXME: C99 */
      while (value_end[1] == ' ' || value_end[1] == '\t')
      {
        readsize = end - value_end + 1 < READSIZE ? end - value_end + 1 : READSIZE;
        if ((value_end = memchr (value_end + 1, '\n', readsize)) == NULL)
        {
          di_warning ("Iek! Don't find end of large value\n");
          return -1;
        }
      }
      value_size = value_end - value_begin;

      field_string.string = field_begin;
      field_string.size = field_size;
      value_string.string = value_begin;
      value_string.size = value_size;

      fip = di_hash_table_lookup (info->table, &field_string);

      if (fip)
      {
        fip->read (&act, fip, NULL, &value_string, user_data);
        goto next;
      }

#if MODIFIER
      if (info->wildcard)
        goto wildcard;
      else if (!info->modifier)
        goto next;

      field_string.size = field_size - field_modifier_size - 1;

      fip = di_hash_table_lookup (info->table, &field_string);

      if (fip)
      {
        field_modifier_string.string = field_modifier_begin;
        field_modifier_string.size = field_modifier_size;

        fip->read (&act, fip, &field_modifier_string, &value_string, user_data);

        goto next;
      }
#endif

      if (!info->wildcard)
        goto next;

#if MODIFIER
wildcard:
#endif
      field_string.size = 0;

      fip = di_hash_table_lookup (info->table, &field_string);

      if (fip)
      {
        field_modifier_string.string = field_begin;
        field_modifier_string.size = field_size;

        fip->read (&act, fip, &field_modifier_string, &value_string, user_data);
      }

next:
      cur = value_end + 1;
      if (cur >= end || *cur == '\n')
        break;
    }

    if (entry_finish && entry_finish (act, user_data))
      return -1;
  }

  return nr;
}

int di_parser_rfc822_read_file (const char *file, di_parser_info *info, di_parser_read_entry_new entry_new, di_parser_read_entry_finish entry_finish, void *user_data)
{
  struct stat statbuf;
  char *begin;
  int fd, ret = -1;

  if ((fd = open (file, O_RDONLY)) < 0)
    return ret;
  if (fstat (fd, &statbuf))
    goto cleanup;
  if (!statbuf.st_size)
  {
    ret = 0;
    goto cleanup;
  }
  begin = mmap (NULL, statbuf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (begin == MAP_FAILED)
    goto cleanup;
  madvise (begin, statbuf.st_size, MADV_SEQUENTIAL);

  ret = di_parser_rfc822_read (begin, statbuf.st_size, info, entry_new, entry_finish, user_data);

  munmap (begin, statbuf.st_size);

cleanup:
  close (fd);

  return ret;
}

static void callback (const di_rstring *field, const di_rstring *value, void *data)
{
  FILE *f = data;
  fwrite (field->string, field->size, 1, f);
  fputs (": ", f);
  fwrite (value->string, value->size, 1, f);
  fputs ("\n", f);
}

int di_parser_rfc822_write_file (const char *file, di_parser_info *info, di_parser_write_entry_next entry_next, void *user_data)
{
  int nr = 0;
  const di_parser_fieldinfo *fip;
  void *act = NULL, *state_data = NULL;
  di_slist_node *node;
  FILE *f;
  char tmpfile[PATH_MAX];


  if (!strncmp (file, "-", 1))
  {
    tmpfile[0] = '\0';
    f = stdout;
  }
  else
  {
    snprintf (tmpfile, sizeof (tmpfile), "%s.tmp", file);
    f = fopen (tmpfile, "w");
  }

  if (!f)
    return -1;

  while (1)
  {
    act = entry_next (&state_data, user_data);
    if (!act)
      break;

    nr++;

    for (node = info->list.head; node; node = node->next)
    {
      fip = node->data;
      if (fip->write)
        fip->write (&act, fip, callback, f, user_data);
    }
    fputc ('\n', f);
  }

  if (*tmpfile)
  {
    fclose (f);
    if (rename (tmpfile, file))
      return -1;
  }

  return nr;
}

