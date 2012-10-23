/*
 * parser.c
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

#include <debian-installer/parser.h>

#include <debian-installer/mem.h>

di_parser_info *di_parser_info_alloc (void)
{
  di_parser_info *info;

  info = di_new0 (di_parser_info, 1);
  info->table = di_hash_table_new (di_rstring_hash, di_rstring_equal);

  return info;
}

void di_parser_info_free (di_parser_info *info)
{
  di_hash_table_destroy (info->table);
  di_slist_destroy (&info->list, NULL);
  di_free (info);
}

void di_parser_info_add (di_parser_info *info, const di_parser_fieldinfo *fieldinfo[])
{
  di_parser_fieldinfo **fip;

  for (fip = (di_parser_fieldinfo **) fieldinfo; *fip; fip++)
  {
    di_hash_table_insert (info->table, &(*fip)->key, *fip);
    di_slist_append (&info->list, *fip);
  }
}

void di_parser_read_boolean (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  int *f = (int *)((char *)*data + fip->integer);
  if (strncasecmp (value->string, "yes", 3) == 0)
    *f = 1;
  else
    *f = 0;
}

void di_parser_write_boolean (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  int *f = (int *)((char *)*data + fip->integer);
  di_rstring value = { "Yes", 3 };

  if (*f)
    callback (&fip->key, &value, callback_data);
}

void di_parser_read_int (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  int *f = (int *)((char *)*data + fip->integer);
  *f = strtol (value->string, NULL, 10);
}

void di_parser_write_int (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  int *f = (int *)((char *)*data + fip->integer);
  char value_buf[32];
  di_rstring value = { value_buf, 0 };

  if (*f)
  {
    value.size = snprintf (value_buf, sizeof (value_buf), "%d", *f);
    callback (&fip->key, &value, callback_data);
  }
}

void di_parser_read_rstring (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  di_rstring *f = (di_rstring *)((char *)*data + fip->integer);
  if (f->string)
    di_free (f->string);
  f->string = di_stradup (value->string, value->size);
  f->size = value->size;
}

void di_parser_write_rstring (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  di_rstring *f = (di_rstring *)((char *)*data + fip->integer);
  callback (&fip->key, f, callback_data);
}

void di_parser_read_string (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  char **f = (char **)((char *)*data + fip->integer);
  if (*f)
    di_free (*f);
  *f = di_stradup (value->string, value->size);
}

void di_parser_write_string (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  char **f = (char **)((char *)*data + fip->integer);
  di_rstring value;

  if (*f)
  {
    value.string = *f;
    value.size = strlen (*f);
    callback (&fip->key, &value, callback_data);
  }
}

