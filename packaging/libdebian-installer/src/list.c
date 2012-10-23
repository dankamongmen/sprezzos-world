/*
 * list.c
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

#include <debian-installer/list.h>

#include <debian-installer/mem.h>

di_list *di_list_alloc (void)
{
  di_list *list;

  list = di_new0 (di_list, 1);

  return list;
}

void di_list_destroy (di_list *list, di_destroy_notify destroy_func)
{
  di_list_node *node, *temp;

  node = list->head;
  while (node)
  {
    temp = node;
    node = node->next;
    if (destroy_func)
      destroy_func (temp->data);
    di_free (temp);
  }
}

void di_list_free (di_list *list)
{
  di_free (list);
}

static void internal_di_list_append (di_list *list, void *data, di_list_node *new_node)
{
  new_node->data = data;
  new_node->next = NULL;
  new_node->prev = list->bottom;

  if (list->bottom)
    list->bottom->next = new_node;
  else
    list->head = new_node;

  list->bottom = new_node;
}

void di_list_append (di_list *list, void *data)
{
  return internal_di_list_append (list, data, di_new (di_list_node, 1));
}

void di_list_append_chunk (di_list *list, void *data, di_mem_chunk *mem_chunk)
{
  return internal_di_list_append (list, data, di_mem_chunk_alloc (mem_chunk));
}

static void internal_di_list_prepend (di_list *list, void *data, di_list_node *new_node)
{
  new_node->data = data;
  new_node->next = list->head;
  new_node->prev = NULL;

  if (new_node->next)
    new_node->next->prev = new_node;
  else
    list->bottom = new_node;

  list->head = new_node;
}

void di_list_prepend (di_list *list, void *data)
{
  return internal_di_list_prepend (list, data, di_new (di_list_node, 1));
}

void di_list_prepend_chunk (di_list *list, void *data, di_mem_chunk *mem_chunk)
{
  return internal_di_list_prepend (list, data, di_mem_chunk_alloc (mem_chunk));
}

