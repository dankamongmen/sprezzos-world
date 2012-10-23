/*
 * packages.c
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

#include <debian-installer/system/packages.h>

#include <debian-installer/log.h>
#include <debian-installer/package_internal.h>
#include <debian-installer/packages_internal.h>
#include <debian-installer/parser_rfc822.h>
#include <debian-installer/slist_internal.h>

#include <ctype.h>
#include <sys/utsname.h>

const di_parser_fieldinfo
  internal_di_system_package_parser_field_installer_menu_item =
    DI_PARSER_FIELDINFO (
      "Installer-Menu-Item",
      di_parser_read_int,
      di_parser_write_int,
      offsetof (di_system_package, installer_menu_item)
    ),
  internal_di_system_package_parser_field_subarchitecture =
    DI_PARSER_FIELDINFO (
      "Subarchitecture",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_system_package, subarchitecture)
    ),
  internal_di_system_package_parser_field_kernel_version =
    DI_PARSER_FIELDINFO (
      "Kernel-Version",
      di_parser_read_string,
      di_parser_write_string,
      offsetof (di_system_package, kernel_version)
    );

const di_parser_fieldinfo *di_system_package_parser_fieldinfo[] =
{
  &internal_di_system_package_parser_field_installer_menu_item,
  &internal_di_system_package_parser_field_subarchitecture,
  &internal_di_system_package_parser_field_kernel_version,
  NULL
};

static void internal_di_system_package_destroy_func (void *data)
{
  di_system_package_destroy (data);
}

void di_system_package_destroy (di_system_package *package)
{
  di_free (package->subarchitecture);

  di_package_destroy (&package->p);
}

di_packages_allocator *di_system_packages_allocator_alloc (void)
{
  di_packages_allocator *ret;

  ret = internal_di_packages_allocator_alloc ();
  ret->package_mem_chunk = di_mem_chunk_new (sizeof (di_system_package), 16384);

  return ret;
}

di_packages *di_system_packages_alloc (void)
{
  di_packages *ret;

  ret = di_new0 (di_packages, 1);
  ret->table = di_hash_table_new_full (di_rstring_hash, di_rstring_equal, NULL, internal_di_system_package_destroy_func);

  return ret;
}

bool di_system_package_check_subarchitecture (di_package *package, const char *subarchitecture)
{
  char *begin, *end, *temp;
  size_t len_subarchitecture = strlen (subarchitecture);

  begin = ((di_system_package *) package)->subarchitecture;
  if (!begin)
    return true;

  end = begin + strlen (begin);
  while (begin < end)
  {
    begin += strspn (begin, " \t\n");
    temp = begin;
    temp += strcspn (temp, " \t\n");

    if ((size_t) (temp - begin) == len_subarchitecture && strncmp (begin, subarchitecture, len_subarchitecture) == 0)
      return true;

    begin = temp;
  }

  return false;
}

di_parser_info *di_system_package_parser_info (void)
{
  di_parser_info *info;

  info = di_package_parser_info ();
  di_parser_info_add (info, di_system_package_parser_fieldinfo);

  return info;
}

di_parser_info *di_system_packages_parser_info (void)
{
  di_parser_info *info;

  info = di_packages_parser_info ();
  di_parser_info_add (info, di_system_package_parser_fieldinfo);

  return info;
}

di_parser_info *di_system_packages_status_parser_info (void)
{
  di_parser_info *info;

  info = di_packages_status_parser_info ();
  di_parser_info_add (info, di_system_package_parser_fieldinfo);

  return info;
}

di_slist *di_system_packages_resolve_dependencies_array_permissive (di_packages *packages, di_package **array, di_packages_allocator *allocator)
{
  struct di_packages_resolve_dependencies_do_real_list_append_data data =
  {
    { NULL, NULL },
    allocator,
  };
  struct di_packages_resolve_dependencies_check s =
  {
    di_packages_resolve_dependencies_check_real,
    di_packages_resolve_dependencies_check_virtual,
    di_packages_resolve_dependencies_check_non_existant_permissive,
    di_packages_resolve_dependencies_do_real_list_append,
    0,
    NULL,
    &data,
  };

  return di_packages_resolve_dependencies_array_special (packages, array, &s, allocator);
}

struct check
{
  const char *subarchitecture;
  const char *kernel;
};

static bool check_real_anna (di_packages_resolve_dependencies_check *r, di_package *package, di_package_dependency *d)
{
  if (d->ptr->status >= di_package_status_unpacked)
  {
#ifdef ENABLE_EXTENSIVE_DEBUG
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): accept %s, already installed", package->package, d->ptr->package);
#endif
    return true;
  }
#ifdef ENABLE_EXTENSIVE_DEBUG
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): check recursive %s", package->package, d->ptr->package);
#endif
  return di_packages_resolve_dependencies_recurse (r, d->ptr, package);
}

static di_package_dependency *check_virtual_anna (di_package *package __attribute__ ((unused)), di_package_dependency *best, di_package_dependency *d, void *data)
{
  struct check *sc = data;
  if (((di_system_package *)d->ptr)->kernel_version &&
      strcmp (((di_system_package *)d->ptr)->kernel_version, sc->kernel))
  {
#ifdef ENABLE_EXTENSIVE_DEBUG
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): discard %s: wrong kernel", package->package, d->ptr->package);
#endif
    return best;
  }
  if (!di_system_package_check_subarchitecture (d->ptr, sc->subarchitecture))
  {
#ifdef ENABLE_EXTENSIVE_DEBUG
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): discard %s: wrong architecture", package->package, d->ptr->package);
#endif
    return best;
  }
#ifdef ENABLE_EXTENSIVE_DEBUG
  if (!best)
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): select %s: first try", package->package, d->ptr->package);
  else if (best->ptr->priority < d->ptr->priority)
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): select %s: better priority", package->package, d->ptr->package);
  else if (d->ptr->status >= di_package_status_unpacked && best->ptr->status < di_package_status_unpacked)
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): select %s: installed", package->package, d->ptr->package);
  else if (d->ptr->status_want == di_package_status_want_install && best->ptr->status_want != di_package_status_want_install)
    di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): select %s: marked for installation", package->package, d->ptr->package);
#endif
  if (!best || best->ptr->priority < d->ptr->priority ||
      (d->ptr->status >= di_package_status_unpacked && best->ptr->status < di_package_status_unpacked) ||
      (d->ptr->status_want == di_package_status_want_install && best->ptr->status_want != di_package_status_want_install))
    return d;
#ifdef ENABLE_EXTENSIVE_DEBUG
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): discard %s", package->package, d->ptr->package);
#endif
  return best;
}

void di_system_packages_resolve_dependencies_mark_anna (di_packages *packages, const char *subarchitecture, const char *kernel)
{
  struct check sc =
  {
    subarchitecture,
    kernel,
  };
  struct di_packages_resolve_dependencies_check s =
  {
    check_real_anna,
    check_virtual_anna,
    di_packages_resolve_dependencies_check_non_existant_permissive,
    di_packages_resolve_dependencies_do_real_mark,
    0,
    &sc,
    NULL,
  };

  return di_packages_resolve_dependencies_mark_special (packages, &s);
}

void di_system_packages_resolve_dependencies_mark_kernel_real_4_2_unstable (di_packages *packages) __attribute__ ((unused));
void di_system_packages_resolve_dependencies_mark_kernel_real_4_2_unstable (di_packages *packages)
{
  struct utsname uts;
  uname (&uts);
  di_system_packages_resolve_dependencies_mark_anna (packages, "unknown", uts.release);
}

__asm__ (".symver di_system_packages_resolve_dependencies_mark_kernel_real_4_2_unstable,di_system_packages_resolve_dependencies_mark_kernel@LIBDI_4.2_UNSTABLE");

