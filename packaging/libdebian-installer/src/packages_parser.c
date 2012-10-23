/*
 * packages_parser.c
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

#include <debian-installer/packages_internal.h>

#include <debian-installer/package_internal.h>
#include <debian-installer/parser_rfc822.h>

/**
 * @addtogroup di_packages_parser
 * @{
 */
/**
 * @internal
 * parser info
 */
const di_parser_fieldinfo 
  internal_di_packages_parser_field_package = 
    DI_PARSER_FIELDINFO
    (
      "Package",
      di_packages_parser_read_name,
      di_parser_write_string,
      offsetof (di_package, package)
    );

/**
 * Standard Packages file
 */
const di_parser_fieldinfo *di_packages_parser_fieldinfo[] =
{
  &internal_di_packages_parser_field_package,
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

/**
 * Standard status file
 */
const di_parser_fieldinfo *di_packages_status_parser_fieldinfo[] =
{
  &internal_di_packages_parser_field_package,
  &internal_di_package_parser_field_status,
  &internal_di_package_parser_field_essential,
  &internal_di_package_parser_field_priority,
  &internal_di_package_parser_field_section,
  &internal_di_package_parser_field_installed_size,
  &internal_di_package_parser_field_maintainer,
  &internal_di_package_parser_field_version,
  &internal_di_package_parser_field_replaces,
  &internal_di_package_parser_field_provides,
  &internal_di_package_parser_field_depends,
  &internal_di_package_parser_field_pre_depends,
  &internal_di_package_parser_field_recommends,
  &internal_di_package_parser_field_suggests,
  &internal_di_package_parser_field_conflicts,
  &internal_di_package_parser_field_enhances,
  &internal_di_package_parser_field_description,
  NULL
};

/**
 * Minimal Packages file
 */
const di_parser_fieldinfo *di_packages_minimal_parser_fieldinfo[] =
{
  &internal_di_packages_parser_field_package,
  &internal_di_package_parser_field_essential,
  &internal_di_package_parser_field_priority,
  &internal_di_package_parser_field_installed_size,
  &internal_di_package_parser_field_version,
  &internal_di_package_parser_field_provides,
  &internal_di_package_parser_field_depends,
  &internal_di_package_parser_field_pre_depends,
  &internal_di_package_parser_field_filename,
  &internal_di_package_parser_field_md5sum,
  &internal_di_package_parser_field_size,
  NULL
};

/** @} */

/**
 * @internal
 * Get parser info for standard Packages file
 */
di_parser_info *di_packages_parser_info (void)
{
  di_parser_info *info;

  info = di_parser_info_alloc ();
  di_parser_info_add (info, di_packages_parser_fieldinfo);

  return info;
}

/**
 * @internal
 * Get parser info for minimal Packages file
 */
di_parser_info *di_packages_minimal_parser_info ()
{
  di_parser_info *info;

  info = di_parser_info_alloc ();
  di_parser_info_add (info, di_packages_minimal_parser_fieldinfo);

  return info;
}

/**
 * @internal
 * Get parser info for standard status file
 */
di_parser_info *di_packages_status_parser_info (void)
{
  di_parser_info *info;

  info = di_parser_info_alloc ();
  di_parser_info_add (info, di_packages_status_parser_fieldinfo);

  return info;
}

/**
 * Read a special Packages file
 *
 * @param file file to read
 * @param info parser info
 */
di_packages *di_packages_special_read_file (const char *file, di_packages_allocator *allocator, di_parser_info *(get_info) (void))
{
  di_parser_info *info = get_info ();
  internal_di_package_parser_data data = {allocator, NULL, NULL};

  data.packages = di_packages_alloc ();

  if (di_parser_rfc822_read_file (file, info, NULL, NULL, &data) < 0)
  {
    di_packages_free (data.packages);
    data.packages = NULL;
  }

  di_parser_info_free (info);

  return data.packages;
}

/**
 * Write a special Packages file
 *
 * @param packages a di_packages
 * @param file file to write
 *
 * @return number of dumped entries
 */
int di_packages_special_write_file (di_packages *packages, const char *file, di_parser_info *(get_info) (void))
{
  int ret;
  di_parser_info *info = get_info ();

  ret = di_parser_rfc822_write_file (file, info, internal_di_packages_parser_write_entry_next, packages);
  di_parser_info_free (info);

  return ret;
}

void *internal_di_packages_parser_write_entry_next (void **state_data, void *user_data)
{
  if (!*state_data)
  {
    di_packages *p = user_data;
    if (p->list.head)
    {
      *state_data = p->list.head;
      return p->list.head->data;
    }
    else
      return NULL;
  }
  else
  {
    di_slist_node *n = *state_data;
    n = n->next;

    if (n)
    {
      *state_data = n;
      return n->data;
    }
    else
      return NULL;
  }
}

void di_packages_parser_read_name (data, fip, field_modifier, value, user_data)
  void **data;
  const di_parser_fieldinfo *fip __attribute__ ((unused));
  di_rstring *field_modifier __attribute__ ((unused));
  di_rstring *value;
  void *user_data;
{
  internal_di_package_parser_data *parser_data = user_data;
  di_package *p;
  p = di_packages_get_package_new (parser_data->packages, parser_data->allocator, value->string, value->size);
  p->type = di_package_type_real_package;
  di_slist_append_chunk (&parser_data->packages->list, p, parser_data->allocator->slist_node_mem_chunk);
  *data = p;
}


