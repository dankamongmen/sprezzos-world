/*
 * package_parser.c
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

#include <debian-installer/package_internal.h>

#include <debian-installer/packages_internal.h>
#include <debian-installer/parser_rfc822.h>

#include <ctype.h>
#include <stddef.h>

const di_parser_fieldinfo 
  internal_di_package_parser_field_package = 
    DI_PARSER_FIELDINFO
    (
      "Package",
      di_parser_read_rstring,
      di_parser_write_rstring,
      offsetof (di_package, package)
    ),
  internal_di_package_parser_field_status =
    DI_PARSER_FIELDINFO
    (
      "Status",
      di_package_parser_read_status,
      di_package_parser_write_status,
      0
    ),
  internal_di_package_parser_field_essential =
    DI_PARSER_FIELDINFO
    (
      "Essential",
      di_parser_read_boolean,
      di_parser_write_boolean,
      offsetof (di_package, essential)
    ),
  internal_di_package_parser_field_priority =
    DI_PARSER_FIELDINFO
    (
      "Priority",
      di_package_parser_read_priority,
      di_package_parser_write_priority,
      0
    ),
  internal_di_package_parser_field_section =
    DI_PARSER_FIELDINFO
    (
      "Section",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, section)
    ),
  internal_di_package_parser_field_installed_size =
    DI_PARSER_FIELDINFO
    (
      "Installed-Size",
      di_parser_read_int,
      di_parser_write_int,
      offsetof (di_package, installed_size)
    ),
  internal_di_package_parser_field_maintainer =
    DI_PARSER_FIELDINFO
    (
      "Maintainer",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, maintainer)
    ),
  internal_di_package_parser_field_architecture =
    DI_PARSER_FIELDINFO
    (
      "Architecture",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, architecture)
    ),
  internal_di_package_parser_field_version =
    DI_PARSER_FIELDINFO
    (
      "Version",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, version)
    ),
  internal_di_package_parser_field_replaces =
    DI_PARSER_FIELDINFO
    (
      "Replaces",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_replaces
    ),
  internal_di_package_parser_field_provides =
    DI_PARSER_FIELDINFO
    (
      "Provides",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_provides
    ),
  internal_di_package_parser_field_depends =
    DI_PARSER_FIELDINFO
    (
      "Depends",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_depends
    ),
  internal_di_package_parser_field_pre_depends =
    DI_PARSER_FIELDINFO
    (
      "Pre-Depends",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_pre_depends
    ),
  internal_di_package_parser_field_recommends =
    DI_PARSER_FIELDINFO
    (
      "Recommends",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_recommends
    ),
  internal_di_package_parser_field_suggests =
    DI_PARSER_FIELDINFO
    (
      "Suggests",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_suggests
    ),
  internal_di_package_parser_field_conflicts =
    DI_PARSER_FIELDINFO
    (
      "Conflicts",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_conflicts
    ),
  internal_di_package_parser_field_enhances =
    DI_PARSER_FIELDINFO
    (
      "Enhances",
      di_package_parser_read_dependency,
      di_package_parser_write_dependency,
      di_package_dependency_type_enhances
    ),
  internal_di_package_parser_field_filename =
    DI_PARSER_FIELDINFO
    (
      "Filename",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, filename)
    ),
  internal_di_package_parser_field_size =
    DI_PARSER_FIELDINFO
    (
      "Size",
      di_parser_read_int,
      di_parser_write_int,
      offsetof (di_package, size)
    ),
  internal_di_package_parser_field_md5sum =
    DI_PARSER_FIELDINFO
    (
      "MD5sum",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_package, md5sum)
    ),
  internal_di_package_parser_field_description =
    DI_PARSER_FIELDINFO
    (
      "Description",
      di_package_parser_read_description,
      di_package_parser_write_description,
      0
    );

const di_parser_fieldinfo *di_package_parser_fieldinfo[] =
{
  &internal_di_package_parser_field_package,
  &internal_di_package_parser_field_essential,
  &internal_di_package_parser_field_priority,
  &internal_di_package_parser_field_section,
  &internal_di_package_parser_field_installed_size,
  &internal_di_package_parser_field_maintainer,
  &internal_di_package_parser_field_architecture,
  &internal_di_package_parser_field_version,
  &internal_di_package_parser_field_replaces,
  &internal_di_package_parser_field_provides,
  &internal_di_package_parser_field_depends,
  &internal_di_package_parser_field_pre_depends,
  &internal_di_package_parser_field_recommends,
  &internal_di_package_parser_field_suggests,
  &internal_di_package_parser_field_conflicts,
  &internal_di_package_parser_field_enhances,
  &internal_di_package_parser_field_filename,
  &internal_di_package_parser_field_size,
  &internal_di_package_parser_field_md5sum,
  &internal_di_package_parser_field_description,
  NULL
};

static void *internal_di_package_parser_new (void *user_data)
{
  internal_di_package_parser_data *parser_data = user_data;
  parser_data->package = di_package_alloc (parser_data->allocator);
  parser_data->package->type = di_package_type_real_package;
  return parser_data->package;
}

di_parser_info *di_package_parser_info (void)
{
  di_parser_info *info;

  info = di_parser_info_alloc ();
  di_parser_info_add (info, di_package_parser_fieldinfo);

  return info;
}

di_package *di_package_special_read_file (const char *file, di_packages *packages, di_packages_allocator *allocator, di_parser_info *(get_info) (void))
{
  di_parser_info *info = get_info ();
  internal_di_package_parser_data data;

  data.allocator = allocator;
  data.packages = packages;

  di_parser_rfc822_read_file (file, info, internal_di_package_parser_new, NULL, &data);

  di_parser_info_free (info);

  return data.package;
}

void di_package_parser_read_dependency (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  internal_di_package_parser_data *parser_data = user_data;
  di_package *p = *data, *q;
  char *cur = value->string, *end = value->string + value->size;
  char *namebegin, *fieldend;
  size_t namelen;
  di_package_dependency *d, *d1;

  /*
   * basic depends line parser. can ignore versioning
   * info since the depends are already satisfied.
   */
  while (cur < end)
  {
    namebegin = cur;
    namelen = strcspn (cur, " \t\n(,|");

    d = di_package_dependency_alloc (parser_data->allocator);

    if (parser_data->packages)
    {
      q = di_packages_get_package_new (parser_data->packages, parser_data->allocator, namebegin, namelen);
      d->ptr = q;
    }
    else
      q = NULL;

    d->type = fip->integer;
    di_slist_append_chunk (&p->depends, d, parser_data->allocator->slist_node_mem_chunk);

    if (q && (d->type == di_package_dependency_type_provides || d->type == di_package_dependency_type_enhances))
    {
      if (q->type == di_package_type_non_existent)
        q->type = di_package_type_virtual_package;
      if (q->type == di_package_type_virtual_package && q->priority < p->priority)
        q->priority = p->priority;

      d1 = di_package_dependency_alloc (parser_data->allocator);
      d1->ptr = p;
      if (d->type == di_package_dependency_type_provides)
        d1->type = di_package_dependency_type_reverse_provides;
      else if (d->type == di_package_dependency_type_enhances)
        d1->type = di_package_dependency_type_reverse_enhances;
      di_slist_append_chunk (&q->depends, d1, parser_data->allocator->slist_node_mem_chunk);
    }

    fieldend = cur + strcspn (cur, "\n,");
    while (isspace(*++fieldend));
    cur = fieldend;
  }
}

void di_package_parser_write_dependency (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  di_slist_node *node;
  di_rstring value = { NULL, 0 };
  size_t value_size = 0, value_size_needed;

  for (node = p->depends.head; node; node = node->next)
  {
    di_package_dependency *d = node->data;

    if (d->type == fip->integer && d->ptr)
    {
      size_t size = strlen (d->ptr->package);
      if (value.size)
        value_size_needed = size + 2;
      else
        value_size_needed = size;
      if (value.size + value_size_needed > value_size)
      {
        size_t new_value_size = value_size + 1024;
        value.string = di_renew (char, value.string, new_value_size);
        value.string[value_size] = 0;
        value_size = new_value_size;
      }
      if (value.size)
        strcat (value.string, ", ");
      strcat (value.string, d->ptr->package);
      value.size += value_size_needed;
    }
  }

  if (value.size)
    callback (&fip->key, &value, callback_data);

  di_free (value.string);
}

void di_package_parser_read_description (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  char *temp;

  temp = memchr (value->string, '\n', value->size);
  if (temp)
  {
    p->short_description = di_stradup (value->string, temp - value->string);
    p->description = di_stradup (temp + 1, value->string + value->size - temp - 1);
#if 0
    fwrite (value->string, value->size, 1, stdout);
    fputs ("\n-----\n", stdout);
    fwrite (temp + 1, value->string + value->size - temp - 1, 1, stdout);
    fputs ("\n=====\n", stdout);
#endif
  }
  else
    p->short_description = di_stradup (value->string, value->size);
}

void di_package_parser_write_description (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  di_rstring value;

  if (p->short_description)
  {
    if (p->description)
    {
      value.size = strlen (p->short_description) + strlen (p->description) + 1;
      value.string = di_malloc (value.size + 1);
      snprintf (value.string, value.size + 1, "%s\n%s", p->short_description, p->description);
#if 0
      fprintf(stdout, "%s", p->description);
      fputs ("\n-----\n", stdout);
      fprintf(stdout, "%s", value.string);
      fputs ("\n=====\n", stdout);
#endif
    }
    else
    {
      value.size = strlen (p->short_description);
      value.string = p->short_description;
    }
    callback (&fip->key, &value, callback_data);
    if (p->description)
      di_free (value.string);
  }
}

di_parser_fields_function_read di_package_parser_read_name_real_4_0 __attribute__ ((unused));

void di_package_parser_read_name_real_4_0 (data, fip, field_modifier, value, user_data)
  void **data;
  const di_parser_fieldinfo *fip __attribute__ ((unused));
  di_rstring *field_modifier __attribute__ ((unused));
  di_rstring *value;
  void *user_data __attribute__ ((unused));
{
  di_package *p = *data;
  p->key.string = di_stradup (value->string, value->size);
  p->key.size = value->size;
}

__asm__ (".symver di_package_parser_read_name_real_4_0,di_package_parser_read_name@LIBDI_4.0");

void di_package_parser_read_priority (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  p->priority = internal_di_package_priority_text_from_rstring (value);
}

void di_package_parser_write_priority (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  di_rstring value;
  value.string = (char *) di_package_priority_text_to (p->priority);
  value.size = strlen(value.string);

  callback (&fip->key, &value, callback_data);
}

void di_package_parser_read_status (
  void **data,
  const di_parser_fieldinfo *fip __attribute__ ((unused)),
  di_rstring *field_modifier __attribute__ ((unused)),
  di_rstring *value,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  di_rstring temp;
  char *next;

  next = memchr (value->string, ' ', value->size);
  temp.string = value->string;
  temp.size = next - value->string;
  p->status_want = internal_di_package_status_want_text_from_rstring (&temp);

  next = memchr (next + 1, ' ', value->size - (next - value->string)) + 1;
  temp.string = next;
  temp.size = value->size - (next - value->string);

  p->status = internal_di_package_status_text_from_rstring (&temp);
}

void di_package_parser_write_status (
  void **data,
  const di_parser_fieldinfo *fip,
  di_parser_fields_function_write_callback callback,
  void *callback_data,
  void *user_data __attribute__ ((unused)))
{
  di_package *p = *data;
  char value_buf[128];
  di_rstring value = { value_buf, 0 };

  value.size = snprintf (value.string, sizeof (value_buf), "%s ok %s", 
      di_package_status_want_text_to(p->status_want),
      di_package_status_text_to(p->status));
  callback (&fip->key, &value, callback_data);
}

