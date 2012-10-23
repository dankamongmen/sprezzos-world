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

#include <debian-installer/packages_internal.h>

#include <debian-installer/log.h>
#include <debian-installer/package_internal.h>
#include <debian-installer/slist_internal.h>
#include <debian-installer/string.h>

#include <ctype.h>
#include <limits.h>

/**
 * Allocate di_packages
 */
di_packages *di_packages_alloc (void)
{
  di_packages *ret;

  ret = di_new0 (di_packages, 1);
  ret->table = di_hash_table_new_full (di_rstring_hash, di_rstring_equal, NULL, internal_di_package_destroy_func);

  return ret;
}

/**
 * Allocate di_packages_allocator
 */
di_packages_allocator *di_packages_allocator_alloc (void)
{
  di_packages_allocator *ret;

  ret = internal_di_packages_allocator_alloc ();
  ret->package_mem_chunk = di_mem_chunk_new (sizeof (di_package), 16384);

  return ret;
}

/**
 * @internal
 * Partially allocate di_packages_allocator
 */
di_packages_allocator *internal_di_packages_allocator_alloc (void)
{
  di_packages_allocator *ret;

  ret = di_new0 (di_packages_allocator, 1);
  ret->package_dependency_mem_chunk = di_mem_chunk_new (sizeof (di_package_dependency), 4096);
  ret->slist_node_mem_chunk = di_mem_chunk_new (sizeof (di_slist_node), 4096);

  return ret;
}

/**
 * Free di_packages
 */
void di_packages_free (di_packages *packages)
{
  if (!packages)
    return;
  di_hash_table_destroy (packages->table);
  di_free (packages);
}

/**
 * Free di_packages_allocator
 */
void di_packages_allocator_free (di_packages_allocator *allocator)
{
  di_mem_chunk_destroy (allocator->package_mem_chunk);
  di_mem_chunk_destroy (allocator->package_dependency_mem_chunk);
  di_mem_chunk_destroy (allocator->slist_node_mem_chunk);
  di_free (allocator);
}

/**
 * append a package.
 *
 * @param packages a di_packages
 */
void di_packages_append_package (di_packages *packages, di_package *package, di_packages_allocator *allocator)
{
  di_package *tmp;

  tmp = di_packages_get_package (packages, package->package, 0);

  if (!tmp)
    di_slist_append_chunk (&packages->list, package, allocator->slist_node_mem_chunk);

  di_hash_table_insert (packages->table, &package->key, package);
}

/**
 * get a named package.
 *
 * @param packages a di_packages
 * @param name the name of the package
 * @param n size of the name or 0
 *
 * @return the package or NULL
 */
di_package *di_packages_get_package (di_packages *packages, const char *name, size_t n)
{
  di_rstring key;
  size_t size;

  if (n)
    size = n;
  else
    size = strlen (name);

  /* i know that is bad, but i know it is not written by the lookup */
  key.string = (char *) name;
  key.size = size;

  return di_hash_table_lookup (packages->table, &key);
}

/**
 * get a named package.
 * creates a new one if non-existant.
 *
 * @param packages a di_packages
 * @param name the name of the package
 * @param n size of the name
 *
 * @return the package
 */
di_package *di_packages_get_package_new (di_packages *packages, di_packages_allocator *allocator, char *name, size_t n)
{
  di_package *ret = di_packages_get_package (packages, name, n);

  if (!ret)
  {
    ret = di_package_alloc (allocator);
    ret->key.string = di_stradup (name, n);
    ret->key.size = n;

    di_hash_table_insert (packages->table, &ret->key, ret);
  }

  return ret;
}

bool di_packages_resolve_dependencies_recurse (di_packages_resolve_dependencies_check *r, di_package *package, di_package *dependend_package)
{
  di_slist_node *node;

  /* did we already check this package? */
  if (package->resolver & r->resolver)
  {
#ifdef ENABLE_EXTENSIVE_DEBUG
    if (package->resolver & (r->resolver << 1))
      di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): already done, okay", package->package);
    else
      di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): already done, not okay", package->package);
#endif
    return package->resolver & (r->resolver << 1);
  }

  package->resolver |= r->resolver;
  package->resolver |= (r->resolver << 1);

#ifdef ENABLE_EXTENSIVE_DEBUG
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): start", package->package);
#endif

  switch (package->type)
  {
    case di_package_type_real_package:
      for (node = package->depends.head; node; node = node->next)
      {
        di_package_dependency *d = node->data;

        if ((d->type == di_package_dependency_type_depends ||
             d->type == di_package_dependency_type_pre_depends) &&
            !r->check_real (r, package, d))
          goto error;
      }

#ifdef ENABLE_EXTENSIVE_DEBUG
      if (dependend_package)
        di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): mark, dependency from %s", package->package, dependend_package->package);
      else
        di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): mark", package->package);
#endif
      
      r->do_real (package, r->do_real_data);
      break;

    case di_package_type_virtual_package:

#ifdef ENABLE_EXTENSIVE_DEBUG
      if (dependend_package)
        di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): search, dependency from %s", package->package, dependend_package->package);
      else
        di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): search", package->package);
#endif
      
      for (node = package->depends.head; node; node = node->next)
      {
        di_package_dependency *d = node->data;

        if (d->type == di_package_dependency_type_reverse_provides)
          package->resolver &= ~(r->resolver << 2);
      }

      while (1)
      {
        di_package_dependency *best_provide = NULL;

        for (node = package->depends.head; node; node = node->next)
        {
          di_package_dependency *d = node->data;

          if (d->type == di_package_dependency_type_reverse_provides)
          {
            if (!(package->resolver & (r->resolver << 2)))
              best_provide = r->check_virtual (package, best_provide, d, r->check_virtual_data);
          }
        }

        if (best_provide)
        {
          if (r->check_real (r, dependend_package, best_provide))
            break;
          else
            package->resolver |= (r->resolver << 2);
        }
        else if (!r->check_non_existant (r, package, NULL))
          goto error;
        else
          break;
      }

      break;

    case di_package_type_non_existent:
      if (!r->check_non_existant (r, package, NULL))
        goto error;
  }

  return true;

error:
#ifdef ENABLE_EXTENSIVE_DEBUG
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): not okay", package->package);
#endif
  package->resolver &= ~(r->resolver << 1);
  return false;
}

bool di_packages_resolve_dependencies_check_real (di_packages_resolve_dependencies_check *r, di_package *package, di_package_dependency *d)
{
  return di_packages_resolve_dependencies_recurse (r, d->ptr, package);
}

di_package_dependency *di_packages_resolve_dependencies_check_virtual (di_package *package __attribute__ ((unused)), di_package_dependency *best, di_package_dependency *d, void  *data __attribute__ ((unused)))
{
  if (!best || best->ptr->priority < d->ptr->priority ||
      (d->ptr->status >= di_package_status_unpacked && best->ptr->status < di_package_status_unpacked))
    return d;
  return best;
}

bool di_packages_resolve_dependencies_check_non_existant (di_packages_resolve_dependencies_check *r __attribute__ ((unused)), di_package *package, di_package_dependency *d __attribute__ ((unused)))
{
  di_log (DI_LOG_LEVEL_WARNING, "resolver (%s): package doesn't exist", package->package);
  return false;
}

bool di_packages_resolve_dependencies_check_non_existant_quiet (di_packages_resolve_dependencies_check *r __attribute__ ((unused)), di_package *package __attribute__ ((unused)), di_package_dependency *d __attribute__ ((unused)))
{
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): package doesn't exist", package->package);
  return false;
}

bool di_packages_resolve_dependencies_check_non_existant_permissive (di_packages_resolve_dependencies_check *r __attribute__ ((unused)), di_package *package __attribute__ ((unused)), di_package_dependency *d __attribute__ ((unused)))
{
  di_log (DI_LOG_LEVEL_DEBUG, "resolver (%s): package doesn't exist (ignored)", package->package);
  return true;
}

void di_packages_resolve_dependencies_do_real_list_append (di_package *package, void *_data)
{
  struct di_packages_resolve_dependencies_do_real_list_append_data *data = _data;
  di_slist_append_chunk (&data->list, package, data->allocator->slist_node_mem_chunk);
}

void di_packages_resolve_dependencies_do_real_mark (di_package *package, void *data __attribute__ ((unused)))
{
  package->status_want = di_package_status_want_install;
}

static void resolve_dependencies_marker_reset (void *key __attribute__ ((unused)), void *value, void *user_data __attribute__ ((unused)))
{
  di_package *p = value;
  p->resolver = 0;
}

void di_packages_resolve_dependencies_marker (di_packages *packages)
{
  if (!packages->resolver)
    packages->resolver = 1;
  else if (packages->resolver > (INT_MAX >> 2))
  {
    di_hash_table_foreach (packages->table, resolve_dependencies_marker_reset, NULL);
    packages->resolver = 1;
  }
  else
    packages->resolver <<= 3;

}

di_slist *di_packages_resolve_dependencies_special (di_packages *packages, di_slist *list, di_packages_resolve_dependencies_check *s, di_packages_allocator *allocator)
{
  struct di_packages_resolve_dependencies_do_real_list_append_data data =
  {
    { NULL, NULL },
    allocator,
  };

  di_slist *install = di_slist_alloc ();
  di_slist_node *node;

  s->do_real_data = &data;

  di_packages_resolve_dependencies_marker (packages);

  s->resolver = packages->resolver;

  for (node = list->head; node; node = node->next)
  {
    di_package *p = node->data;
    if (di_packages_resolve_dependencies_recurse (s, p, NULL))
      internal_di_slist_append_list (install, &data.list);
  }

  return install;
}

di_slist *di_packages_resolve_dependencies (di_packages *packages, di_slist *list, di_packages_allocator *allocator)
{
  struct di_packages_resolve_dependencies_check s =
  {
    di_packages_resolve_dependencies_check_real,
    di_packages_resolve_dependencies_check_virtual,
    di_packages_resolve_dependencies_check_non_existant,
    di_packages_resolve_dependencies_do_real_list_append,
    0,
    NULL,
    NULL,
  };

  return di_packages_resolve_dependencies_special (packages, list, &s, allocator);
}

di_slist *di_packages_resolve_dependencies_array_special (di_packages *packages, di_package **array, di_packages_resolve_dependencies_check *s, di_packages_allocator *allocator)
{
  struct di_packages_resolve_dependencies_do_real_list_append_data data =
  {
    { NULL, NULL },
    allocator,
  };

  di_slist *install = di_slist_alloc ();

  s->do_real_data = &data;

  di_packages_resolve_dependencies_marker (packages);

  s->resolver = packages->resolver;

  while (*array)
    if (di_packages_resolve_dependencies_recurse (s, *array++, NULL))
      internal_di_slist_append_list (install, &data.list);

  return install;
}

di_slist *di_packages_resolve_dependencies_array (di_packages *packages, di_package **array, di_packages_allocator *allocator)
{
  struct di_packages_resolve_dependencies_check s =
  {
    di_packages_resolve_dependencies_check_real,
    di_packages_resolve_dependencies_check_virtual,
    di_packages_resolve_dependencies_check_non_existant,
    di_packages_resolve_dependencies_do_real_list_append,
    0,
    NULL,
    NULL,
  };

  return di_packages_resolve_dependencies_array_special (packages, array, &s, allocator);
}

void di_packages_resolve_dependencies_mark_special (di_packages *packages, di_packages_resolve_dependencies_check *s)
{
  di_slist_node *node;

  di_packages_resolve_dependencies_marker (packages);

  s->resolver = packages->resolver;

  for (node = packages->list.head; node; node = node->next)
  {
    di_package *package = node->data;
    if (!(package->resolver & packages->resolver) && package->status_want == di_package_status_want_install)
      di_packages_resolve_dependencies_recurse (s, package, NULL);
  }
}

void di_packages_resolve_dependencies_mark (di_packages *packages)
{
  struct di_packages_resolve_dependencies_check s =
  {
    di_packages_resolve_dependencies_check_real,
    di_packages_resolve_dependencies_check_virtual,
    di_packages_resolve_dependencies_check_non_existant_quiet,
    di_packages_resolve_dependencies_do_real_mark,
    0,
    NULL,
    NULL,
  };

  di_packages_resolve_dependencies_mark_special (packages, &s);
}

