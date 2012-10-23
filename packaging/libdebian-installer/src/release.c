/*
 * release.c
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

#include <debian-installer/release.h>

#include <debian-installer/mem.h>
#include <debian-installer/parser_rfc822.h>
#include <debian-installer/string.h>

#include <stddef.h>
#include <string.h>

/**
 * @addtogroup di_release_parser
 * @{
 */
/**
 * @internal
 * parser info
 */
const di_parser_fieldinfo
  internal_di_release_parser_field_origin =
    DI_PARSER_FIELDINFO
    (
      "Origin",
      di_parser_read_string,
      NULL,
      offsetof (di_release, origin)
    ),
  internal_di_release_parser_field_suite =
    DI_PARSER_FIELDINFO
    (
      "Suite",
      di_parser_read_string,
      NULL,
      offsetof (di_release, suite)
    ),
  internal_di_release_parser_field_codename =
    DI_PARSER_FIELDINFO
    (
      "Codename",
      di_parser_read_string,
      NULL,
      offsetof (di_release, codename)
    ),
  internal_di_release_parser_field_md5sum =
    DI_PARSER_FIELDINFO
    (
      "MD5Sum",
      di_release_parser_read_file,
      NULL,
      0
    ),
  internal_di_release_parser_field_sha1 =
    DI_PARSER_FIELDINFO
    (
      "SHA1",
      di_release_parser_read_file,
      NULL,
      1
    );

/**
 * Standard Release file
 */
const di_parser_fieldinfo *di_release_parser_fieldinfo[] =
{
  &internal_di_release_parser_field_origin,
  &internal_di_release_parser_field_suite,
  &internal_di_release_parser_field_codename,
  &internal_di_release_parser_field_md5sum,
  &internal_di_release_parser_field_sha1,
  NULL
};

/** @} */

static void internal_di_release_file_destroy_func (void *data)
{
  di_release_file *file = data;

  di_free (file->filename);
  di_free (file->sum[0]);
  di_free (file->sum[1]);
}

/**
 * Allocate di_release
 */
di_release *di_release_alloc (void)
{
  di_release *ret;

  ret = di_new0 (di_release, 1);
  ret->md5sum = di_hash_table_new_full (di_rstring_hash, di_rstring_equal, NULL, internal_di_release_file_destroy_func);
  ret->release_file_mem_chunk = di_mem_chunk_new (sizeof (di_release_file), 4096);

  return ret;
}

/**
 * Free di_release
 */
void di_release_free (di_release *release)
{
  di_free (release->origin);
  di_free (release->suite);
  di_free (release->codename);
  di_hash_table_destroy (release->md5sum);
  di_mem_chunk_destroy (release->release_file_mem_chunk);
  di_free (release);
}

static void *parser_new (void *user_data)
{
  return user_data;
}

/**
 * Read a standard Release file
 *
 * @param file file to read
 */
di_release *di_release_read_file (const char *file)
{
  di_release *release;
  di_parser_info *info;

  release = di_release_alloc ();
  info = di_parser_info_alloc ();
  di_parser_info_add (info, di_release_parser_fieldinfo);

  if (di_parser_rfc822_read_file (file, info, parser_new, NULL, release) < 0)
  {
    di_release_free (release);
    return NULL;
  }

  return release;
}

void di_release_parser_read_file (data, fip, field_modifier, value, user_data)
  void **data;
  const di_parser_fieldinfo *fip __attribute__ ((unused));
  di_rstring *field_modifier __attribute__ ((unused));
  di_rstring *value;
  void *user_data __attribute__ ((unused));
{
  char *begin = value->string, *next = begin, *end = value->string + value->size;
  char *buf_sum, buf_filename[129];
  int ret;
  size_t buf_size;
  di_release *release = *data;
  di_hash_table *table = release->md5sum;

  while (1)
  {
    next = memchr (begin, '\n', end - begin);
    if (!next)
      next = end;

    ret = sscanf (begin, "%ms %zu %128s", &buf_sum, &buf_size, buf_filename);

    if (ret == 3)
    {
      di_rstring key = { buf_filename, strlen (buf_filename) };
      di_release_file *file = di_hash_table_lookup (table, &key);
      if (!file)
      {
        file = di_mem_chunk_alloc0 (release->release_file_mem_chunk);
        file->key.string = strdup (buf_filename);
        file->key.size = strlen (buf_filename);
        di_hash_table_insert (table, &file->key, file);
      }
      file->size = buf_size;
      file->sum[fip->integer] = buf_sum;
    }

    begin = next + 1;
    if (begin >= end)
      break;
  }
}

