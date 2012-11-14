/* $Id$ */
/*-
 * Copyright (c) 2003-2006 Benedikt Meurer <benny@xfce.org>
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#if !defined(LIBXFCE4UTIL_INSIDE_LIBXFCE4UTIL_H) && !defined(LIBXFCE4UTIL_COMPILATION)
#error "Only <libxfce4util/libxfce4util.h> can be included directly, this file may disappear or change contents"
#endif

#ifndef __XFCE_GENERICS_H__
#define __XFCE_GENERICS_H__

#include <glib.h>

G_BEGIN_DECLS

#define XFCE_GENERIC_STACK(Type)                                            \
  struct                                                                    \
  {                                                                         \
    Type  *elements;                                                        \
    gint   nelements;                                                       \
    gint   top;                                                             \
  }


#ifdef __GNUC__
#define xfce_stack_new(StackType)                                           \
  ({                                                                        \
    StackType *stack;                                                       \
                                                                            \
    stack            = g_new (StackType, 1);                                \
    stack->elements  = g_malloc (20 * sizeof (*(stack->elements)));         \
    stack->nelements = 20;                                                  \
    stack->top       = -1;                                                  \
                                                                            \
    stack;                                                                  \
  })
#else
static inline gpointer
xfce_stack_alloc (gsize element_size)
{
  typedef struct { gpointer elements; gint nelements; gint top; } Stack;
  Stack *stack = g_new (Stack, 1);
  stack->elements = g_malloc (20 * element_size);
  stack->nelements = 20;
  stack->top = -1;
  return stack;
}
#define xfce_stack_new(StackType)                                           \
  ((StackType *) xfce_stack_alloc (sizeof (*(((StackType *) 0)->elements))))
#endif


#define xfce_stack_free(stack)                                              \
  G_STMT_START                                                              \
    {                                                                       \
      g_free (stack->elements);                                             \
      g_free (stack);                                                       \
    }                                                                       \
  G_STMT_END


#ifdef __GNUC__
#define xfce_stack_top(stack)                                               \
  ({                                                                        \
    g_assert (stack->top >= 0);                                             \
    stack->elements[stack->top];                                            \
  })
#else
#define xfce_stack_top(stack) ((stack)->elements[(stack)->top])
#endif


#define xfce_stack_pop(stack)                                               \
  G_STMT_START                                                              \
    {                                                                       \
      g_assert (stack->top > 0);                                            \
      stack->top--;                                                         \
    }                                                                       \
  G_STMT_END


#define xfce_stack_push(stack, value)                                       \
  G_STMT_START                                                              \
    {                                                                       \
      stack->top++;                                                         \
                                                                            \
      if (G_UNLIKELY (stack->top >= stack->nelements))                      \
        {                                                                   \
          stack->nelements *= 2;                                            \
          stack->elements = g_realloc (stack->elements,                     \
                                       stack->nelements * sizeof(value));   \
        }                                                                   \
                                                                            \
      stack->elements[stack->top] = value;                                  \
    }                                                                       \
  G_STMT_END

G_END_DECLS

#endif /* !__XFCE_GENERICS_H__ */
