/* $Id$ */
/*-
 * Copyright (c) 2003-2007 Benedikt Meurer <benny@xfce.org>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libxfce4util/xfce-private.h>
#include <libxfce4util/xfce-rc-private.h>
#include <libxfce4util/libxfce4util-alias.h>


#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#ifdef LINE_MAX
#undef LINE_MAX
#endif
#define LINE_MAX 8192

/* name of the NULL group */
#define NULL_GROUP "[NULL]"


typedef struct _Entry  Entry;
typedef struct _LEntry LEntry;
typedef struct _Group  Group;



static Group*   simple_add_group  (XfceRcSimple *simple,
                                   const gchar  *name);
static Entry*   simple_add_entry  (XfceRcSimple *simple,
                                   const gchar  *key,
                                   const gchar  *value,
                                   const gchar  *locale);
static gboolean simple_parse_line (gchar        *line,
                                   gchar       **section,
                                   gchar       **key,
                                   gchar       **value,
                                   gchar       **locale);
static gchar*   simple_escape     (gchar        *buffer,
                                   gssize        size,
                                   const gchar  *string);
static gboolean simple_write      (XfceRcSimple *simple,
                                   const gchar  *filename);
static void     simple_entry_free (Entry        *entry);
static void     simple_group_free (Group        *group);



struct _XfceRcSimple
{
  XfceRc        __parent__;

  GStringChunk *string_chunk;

  gchar        *filename;

  Group        *gfirst;
  Group        *glast;
  Group        *group;

  guint         shared_chunks : 1;
  guint         dirty : 1;
  guint         readonly : 1;
};

struct _Entry
{
  gchar  *key;
  gchar  *value;
  Entry  *next;
  Entry  *prev;
  LEntry *lfirst;
  LEntry *llast;
};

struct _LEntry
{
  gchar  *locale;
  gchar  *value;
  LEntry *next;
  LEntry *prev;
};

struct _Group
{
  gchar *name;
  Group *next;
  Group *prev;
  Entry *efirst;
  Entry *elast;
};



/* because strcmp is by far the most called function in this code,
 * we inline the comparison of the first character */
#define str_is_equal(a,b) (*(a) != *(b) ? FALSE : strcmp ((a), (b)) == 0)



static Group*
simple_add_group (XfceRcSimple *simple,
                  const gchar  *name)
{
  Group *group;

  for (group = simple->gfirst; group != NULL; group = group->next)
    if (str_is_equal (group->name, name))
      return group;

  group         = g_slice_new (Group);
  group->name   = g_string_chunk_insert (simple->string_chunk, name);
  group->efirst = NULL;
  group->elast  = NULL;

  if (G_UNLIKELY (simple->gfirst == NULL))
    {
      group->next = group->prev = NULL;
      simple->gfirst = simple->glast = group;
    }
  else
    {
      group->next = NULL;
      group->prev = simple->glast;
      simple->glast->next = group;
      simple->glast = group;
    }

  return group;
}



static Entry*
simple_add_entry (XfceRcSimple *simple,
                  const gchar  *key,
                  const gchar  *value,
                  const gchar  *locale)
{
  LEntry *lentry_before;
  LEntry *lentry;
  Entry  *entry;
  gint    result;

  for (entry = simple->group->efirst; entry != NULL; entry = entry->next)
    if (str_is_equal (entry->key, key))
      break;

  if (G_UNLIKELY (entry == NULL))
    {
      if (locale != NULL) {
        /* this point is reached when there is a .desktop file that lists a language specific key for something that had no language-neutral key yet.
           Example:
             b.desktop

             [Desktop Entry]
             Version=1.0
             Name=xyz
             GenericName[de_AT]=Test

           here GenericName[de_AT] will end up here.
           The previous way with g_return_val_if_fail would call an assertion failure and terminate the _whole application_(!!).

           Saner ways to react are either just ignoring GenericName[de_AT] altogether, or, alternatively, just set the normal GenericName
           to Test too (i.e. imply GenericName=Test).

           For now, we are just ignoring the line altogether. But we aren't assert-failing anymore and the apps dont crash anymore.
           */
        return NULL;
      }
      /* why you annoying macro, will you stop borking libxfceutil? thanks. DO NOT DO THAT: g_return_val_if_fail (locale == NULL, NULL); */

      entry         = g_slice_new (Entry);
      entry->key    = g_string_chunk_insert (simple->string_chunk, key);
      entry->value  = g_string_chunk_insert (simple->string_chunk, value);
      entry->lfirst = NULL;
      entry->llast  = NULL;

      if (simple->group->efirst == NULL)
        {
          entry->next = entry->prev = NULL;
          simple->group->efirst = simple->group->elast = entry;
        }
      else
        {
          entry->next = NULL;
          entry->prev = simple->group->elast;
          simple->group->elast->next = entry;
          simple->group->elast = entry;
        }

      return entry;
    }

  if (G_UNLIKELY (locale == NULL))
    {
      /* overwrite existing value */
      if (!str_is_equal (entry->value, value))
        entry->value = g_string_chunk_insert (simple->string_chunk, value);
    }
  else
    {
      /* Add the localized value. Note the optimization used within here: We store
       * the locales in alphabetic order (e.g. de, fr, ...). And we start the search
       * at the end of the locales list, since locales are usually sorted in the
       * rc files. Once we notice that the locale in question is less than the
       * current list locale, we cancel the search and remember the list element as
       * the element before the new list element (remember the sorting order). This
       * makes parsing a usual resource config file with lots of locales amazingly
       * fast.
       */
      lentry_before = NULL;
      for (lentry = entry->llast; lentry != NULL; lentry = lentry->prev)
        {
          result = strcmp (lentry->locale, locale);

          if (result == 0)
            break;
          else if (result < 0)
            {
              lentry_before = lentry;
              lentry = NULL;
              break;
            }
        }

      if (G_LIKELY (lentry == NULL))
        {
          /* create new localized entry */
          lentry         = g_slice_new (LEntry);
          lentry->locale = g_string_chunk_insert (simple->string_chunk, locale);
          lentry->value  = g_string_chunk_insert (simple->string_chunk, value);

          if (G_UNLIKELY (entry->lfirst == NULL))
            {
              lentry->next = lentry->prev = NULL;
              entry->lfirst = entry->llast = lentry;
            }
          else if (lentry_before != NULL)
            {
              lentry->next = lentry_before->next;
              lentry->prev = lentry_before;
              if (G_UNLIKELY (lentry_before->next != NULL))
                lentry_before->next->prev = lentry;
              else
                entry->llast = lentry;
              lentry_before->next = lentry;
            }
          else
            {
              lentry->next = NULL;
              lentry->prev = entry->llast;
              entry->llast->next = lentry;
              entry->llast = lentry;
            }
        }
      else
        {
          /* overwrite value in existing localized entry */
          if (G_LIKELY (!str_is_equal (lentry->value, value)))
            lentry->value = g_string_chunk_insert (simple->string_chunk, value);
        }
    }

  return entry;
}



static gboolean
simple_parse_line (gchar  *line,
                   gchar **section,
                   gchar **key,
                   gchar **value,
                   gchar **locale)
{
  gchar *p, *q, *r, *s;

  p = line;

  *section = NULL;
  *locale  = NULL;
  *value   = NULL;
  *key     = NULL;

  while (g_ascii_isspace (*p))
    ++p;

  if (G_UNLIKELY (*p == '\0' || *p == '\n' || *p == '#'))
    return FALSE;

  if (*p == '[')
    {
      q = strrchr (++p, ']');
      if (G_LIKELY (q != NULL))
        {
          *section = p;
          *q = '\0';
          return TRUE;
        }
    }
  else
    {
      q = strchr (p + 1, '=');
      if (G_UNLIKELY (q == NULL))
        return FALSE;

      r = q + 1;

      for (--q; g_ascii_isspace (*q); --q)
        ;

      if (G_UNLIKELY (q == p))
        return FALSE;

      if (*q == ']')
        {
          for (s = q - 1; *s != '[' && s > p; --s)
            ;
          if (G_UNLIKELY (*s != '['))
            return FALSE;

          *key = p;
          *s = '\0';

          *locale = s + 1;
          *q = '\0';

          if (G_UNLIKELY (**key == '\0' || **locale == '\0'))
            return FALSE;
        }
      else
        {
          ++q;
          *key = p;
          *q = '\0';
        }

      while (g_ascii_isspace (*r))
        ++r;

      q = r + strlen (r);

      /* "\ " at the end of the string will not be removed */
      while (q > r && ((g_ascii_isspace (*(q-1)) && *(q-2) != '\\') || ((*(q-1)) == '\r')))
        --q;

      *value = r;
      *q = '\0';

      /* unescape \ , \n, \t, \r and \\ */
      for (p = r; *r != '\0'; )
        {
          if (G_UNLIKELY (*r == '\\'))
            {
              switch (*(r+1))
                {
                case ' ':
                  *p++ = ' ';
                  break;

                case 'n':
                  *p++ = '\n';
                  break;

                case 't':
                  *p++ = '\t';
                  break;

                case 'r':
                  *p++ = '\r';
                  break;

                case '\\':
                  *p++ = '\\';
                  break;

                default:
                  *p++ = '\\';
                  *p++ = *(r+1);
                  break;
                }

              r += 2;
            }
          else
            *p++ = *r++;
        }
      *p = '\0';

      return TRUE;
    }

  return FALSE;
}



static gchar*
simple_escape (gchar *buffer, gssize size, const gchar *string)
{
  const gchar *s;
  gchar       *p;

  /* escape all whitespace at the beginning of the string */
  for (p = buffer; p - buffer < size - 2 && *string == ' '; ++string)
    {
      *p++ = '\\';
      *p++ = ' ';
    }

  for (; p - buffer < size - 2 && *string != '\0'; ++string)
    switch (*string)
      {
      case ' ':
        /* check if any non whitespace characters follow */
        for (s = string + 1; g_ascii_isspace (*s); ++s)
          ;
        if (*s == '\0')
          {
            /* need to escape the space */
            *p++ = '\\';
            *p++ = ' ';
          }
        else
          {
            /* still non-whitespace, no need to escape */
            *p++ = ' ';
          }
        break;

      case '\n':
        *p++ = '\\';
        *p++ = 'n';
        break;

      case '\t':
        *p++ = '\\';
        *p++ = 't';
        break;

      case '\r':
        *p++ = '\\';
        *p++ = 'r';
        break;

      case '\\':
        *p++ = '\\';
        *p++ = '\\';
        break;

      default:
        *p++ = *string;
        break;
      }

  *p = '\0';
  return buffer;
}



static gboolean
simple_write (XfceRcSimple *simple, const gchar *filename)
{
  LEntry *lentry;
  Entry  *entry;
  Group  *group;
  gchar   buffer[LINE_MAX];
  FILE   *fp;

  fp = fopen (filename, "w");
  if (G_UNLIKELY (fp == NULL))
    {
      g_critical ("Unable to open file %s for writing: %s", filename, g_strerror (errno));
      return FALSE;
    }

  for (group = simple->gfirst; group != NULL; group = group->next)
    {
      /* don't store empty groups */
      if (group->efirst == NULL)
        continue;

      /* NULL_GROUP has no header */
      if (!str_is_equal (group->name, NULL_GROUP))
        fprintf (fp, "[%s]\n", group->name);

      for (entry = group->efirst; entry != NULL; entry = entry->next)
        {
          simple_escape (buffer, LINE_MAX, entry->value);
          fprintf (fp, "%s=%s\n", entry->key, buffer);

          for (lentry = entry->lfirst; lentry != NULL; lentry = lentry->next)
            {
              simple_escape (buffer, LINE_MAX, lentry->value);
              fprintf (fp, "%s[%s]=%s\n", entry->key, lentry->locale, buffer);
            }
        }

      fprintf (fp, "\n");
    }

  if (ferror (fp))
    {
      g_critical ("Unable to write to file %s: Unexpected internal error", filename);
      fclose (fp);
      unlink (filename);
      return FALSE;
    }

  fclose (fp);
  return TRUE;
}



static void
simple_entry_free (Entry *entry)
{
  /* release all lentries */
  g_slice_free_chain (LEntry, entry->lfirst, next);

  /* release the entry */
  g_slice_free (Entry, entry);
}



static void
simple_group_free (Group *group)
{
  Entry *entry;
  Entry *next;

  /* release all entries */
  for (entry = group->efirst; entry != NULL; entry = next)
    {
      /* determine the next entry */
      next = entry->next;

      /* release this entry */
      simple_entry_free (entry);
    }

  /* release the group */
  g_slice_free (Group, group);
}



XfceRcSimple*
_xfce_rc_simple_new (XfceRcSimple *shared,
                     const gchar  *filename,
                     gboolean      readonly)
{
  XfceRcSimple *simple;

  simple = g_new0 (XfceRcSimple, 1);

  _xfce_rc_init (XFCE_RC (simple));

  /* attach callbacks */
  simple->__parent__.close        = _xfce_rc_simple_close;
  simple->__parent__.get_groups   = _xfce_rc_simple_get_groups;
  simple->__parent__.get_entries  = _xfce_rc_simple_get_entries;
  simple->__parent__.delete_group = _xfce_rc_simple_delete_group;
  simple->__parent__.get_group    = _xfce_rc_simple_get_group;
  simple->__parent__.has_group    = _xfce_rc_simple_has_group;
  simple->__parent__.set_group    = _xfce_rc_simple_set_group;
  simple->__parent__.delete_entry = _xfce_rc_simple_delete_entry;
  simple->__parent__.has_entry    = _xfce_rc_simple_has_entry;
  simple->__parent__.read_entry   = _xfce_rc_simple_read_entry;

  if (!readonly)
    {
      simple->__parent__.flush       = _xfce_rc_simple_flush;
      simple->__parent__.rollback    = _xfce_rc_simple_rollback;
      simple->__parent__.is_dirty    = _xfce_rc_simple_is_dirty;
      simple->__parent__.is_readonly = _xfce_rc_simple_is_readonly;
      simple->__parent__.write_entry = _xfce_rc_simple_write_entry;
    }

  if (shared != NULL)
    {
      simple->shared_chunks = TRUE;
      simple->string_chunk  = shared->string_chunk;
    }
  else
    {
      simple->shared_chunks = FALSE;
      simple->string_chunk  = g_string_chunk_new (4096);
    }

  simple->filename = g_string_chunk_insert (simple->string_chunk, filename);
  simple->readonly = readonly;

  /* add NULL_GROUP */
  simple->group = simple_add_group (simple, NULL_GROUP);

  return simple;
}



gboolean
_xfce_rc_simple_parse (XfceRcSimple *simple)
{
  gboolean readonly;
  gchar    line[LINE_MAX];
  gchar   *section;
  gchar   *locale;
  gchar   *value;
  gchar   *key;
  FILE    *fp;

  _xfce_return_val_if_fail (simple != NULL, FALSE);
  _xfce_return_val_if_fail (simple->filename != NULL, FALSE);

  readonly = xfce_rc_is_readonly (XFCE_RC (simple));

  fp = fopen (simple->filename, "r");
  if (fp == NULL)
    return FALSE;

  while (fgets (line, LINE_MAX, fp) != NULL)
    {
      if (!simple_parse_line (line, &section, &key, &value, &locale))
        continue;

      if (section != NULL)
        {
          simple->group = simple_add_group (simple, section);
          continue;
        }

      if (locale == NULL)
        {
          simple_add_entry (simple, key, value, NULL);
          continue;
        }

      if (XFCE_RC (simple)->locale == NULL)
        continue;

      if (xfce_locale_match (XFCE_RC (simple)->locale, locale) > XFCE_LOCALE_NO_MATCH
          || !readonly)
        {
          simple_add_entry (simple, key, value, locale);
        }
    }

  fclose (fp);

  return TRUE;
}



void
_xfce_rc_simple_close (XfceRc *rc)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);
  Group        *group_next;
  Group        *group;

  /* release all memory allocated to the groups */
  for (group = simple->gfirst; group != NULL; group = group_next)
    {
      /* determine the next group */
      group_next = group->next;

      /* release this group */
      simple_group_free (group);
    }

  /* release the string chunk */
  if (!simple->shared_chunks)
    g_string_chunk_free (simple->string_chunk);
}



void
_xfce_rc_simple_flush (XfceRc *rc)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);
  gchar         tmp_path[PATH_MAX];

  if (G_UNLIKELY (!simple->dirty))
    return;

  g_snprintf (tmp_path, PATH_MAX, "%s.%d.tmp", simple->filename, (int)getpid ());
  if (simple_write (simple, tmp_path))
    {
      if (rename (tmp_path, simple->filename) < 0)
        {
          g_critical ("Unable to rename %s to %s: %s",
                tmp_path,
                simple->filename,
                g_strerror (errno));
          unlink (tmp_path);
        }
      else
        simple->dirty = FALSE;
    }
}



void
_xfce_rc_simple_rollback (XfceRc *rc)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);

  simple->dirty = FALSE;
}



gboolean
_xfce_rc_simple_is_dirty (const XfceRc *rc)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);

  return simple->dirty;
}



gboolean
_xfce_rc_simple_is_readonly (const XfceRc *rc)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);

  return simple->readonly;
}



const gchar*
_xfce_rc_simple_get_filename (const XfceRc *rc)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);

  return simple->filename;
}



gchar**
_xfce_rc_simple_get_groups (const XfceRc *rc)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);
  const Group        *group;
  gchar             **result;
  guint               size;
  guint               pos;

  result = g_new (gchar *, 11);
  size   = 10;
  pos    = 0;

  for (group = simple->gfirst; group != NULL; group = group->next)
    {
      if (pos == size)
        {
          size *= 2;
          result = g_realloc (result, (size + 1) * sizeof (*result));
        }
      result[pos] = g_strdup (group->name);
      ++pos;
    }

  result[pos] = NULL;
  return result;
}



gchar**
_xfce_rc_simple_get_entries (const XfceRc *rc,
                             const gchar  *name)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);
  const Group        *group;
  const Entry        *entry;
  gchar             **result;
  guint               size;
  guint               pos;

  if (name == NULL)
    name = NULL_GROUP;

  for (group = simple->gfirst; group != NULL; group = group->next)
    if (str_is_equal (group->name, name))
      break;

  if (group == NULL)
    return NULL;

  result = g_new (gchar *, 11);
  size   = 10;
  pos    = 0;

  for (entry = group->efirst; entry != NULL; entry = entry->next)
    {
      if (size == pos)
        {
          size *= 2;
          result = g_realloc (result, (size + 1) * sizeof (*result));
        }
      result[pos] = g_strdup (entry->key);
      ++pos;
    }

  result[pos] = NULL;

  return result;
}



void
_xfce_rc_simple_delete_group (XfceRc      *rc,
                              const gchar *name,
                              gboolean     global)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);
  Group        *group;
  Entry        *entry;
  Entry        *next;

  if (name == NULL)
    name = NULL_GROUP;

  for (group = simple->gfirst; group != NULL; group = group->next)
    {
      if (str_is_equal (group->name, name))
        {
          if (simple->group == group || str_is_equal (name, NULL_GROUP))
            {
              /* don't delete current group or the default group, just clear them */
              for (entry = group->efirst; entry != NULL; entry = next)
                {
                  next = entry->next;
                  simple_entry_free (entry);
                }
              group->efirst = group->elast = NULL;
            }
          else
            {
              /* unlink group from group list */
              if (group->prev != NULL)
                group->prev->next = group->next;
              else
                simple->gfirst = group->next;
              if (group->next != NULL)
                group->next->prev = group->prev;
              else
                simple->glast = group->prev;

              /* delete this group */
              simple_group_free (group);
            }

          simple->dirty = TRUE;
          break;
        }
    }
}



const gchar*
_xfce_rc_simple_get_group (const XfceRc *rc)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);

  if (str_is_equal (simple->group->name, NULL_GROUP))
    return NULL;
  else
    return simple->group->name;
}



gboolean
_xfce_rc_simple_has_group (const XfceRc *rc,
                           const gchar  *name)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);
  const Group        *group;

  /* the NULL group always exists */
  if (name == NULL)
    return TRUE;

  for (group = simple->gfirst; group != NULL; group = group->next)
    if (str_is_equal (group->name, name))
      break;

  return group != NULL;
}



void
_xfce_rc_simple_set_group (XfceRc      *rc,
                           const gchar *name)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);

  if (name == NULL)
    name = NULL_GROUP;

  if (!str_is_equal (simple->group->name, name))
    simple->group = simple_add_group (simple, name);
}



void
_xfce_rc_simple_delete_entry (XfceRc      *rc,
                              const gchar *key,
                              gboolean     global)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);
  Entry        *entry;

  for (entry = simple->group->efirst; entry != NULL; entry = entry->next)
    {
      if (str_is_equal (entry->key, key))
        {
          if (entry->prev != NULL)
            entry->prev->next = entry->next;
          else
            simple->group->efirst = entry->next;

          if (entry->next != NULL)
            entry->next->prev = entry->prev;
          else
            simple->group->elast = entry->prev;

          /* delete this entry */
          simple_entry_free (entry);

          simple->dirty = TRUE;
          break;
        }
    }
}



gboolean
_xfce_rc_simple_has_entry (const XfceRc *rc,
                           const gchar  *key)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);
  const Entry        *entry;

  for (entry = simple->group->efirst; entry != NULL; entry = entry->next)
    if (str_is_equal (entry->key, key))
      break;

  return entry != NULL;
}



const gchar*
_xfce_rc_simple_read_entry (const XfceRc *rc,
                            const gchar  *key,
                            gboolean      translated)
{
  const XfceRcSimple *simple = XFCE_RC_SIMPLE_CONST (rc);
  LEntry             *lentry;
  Entry              *entry;
  const gchar        *best_value;
  guint               best_match;
  guint               match;

  for (entry = simple->group->efirst; entry != NULL; entry = entry->next)
    if (str_is_equal (entry->key, key))
      break;

  if (G_LIKELY (entry != NULL))
    {
      /* check for localized entry (best fit!) */
      if (G_LIKELY (translated && rc->locale != NULL))
        {
          best_match = XFCE_LOCALE_NO_MATCH;
          best_value = NULL;

          for (lentry = entry->lfirst; lentry != NULL; lentry = lentry->next)
            {
              match = xfce_locale_match (rc->locale, lentry->locale);
              if (match == XFCE_LOCALE_FULL_MATCH)
                {
                  /* FULL MATCH */
                  return lentry->value;
                }
              else if (match > best_match)
                {
                  best_match = match;
                  best_value = lentry->value;
                }
            }

          if (best_value != NULL)
            return best_value;

          /* FALL-THROUGH */
        }

      return entry->value;
    }

  return NULL;
}



void
_xfce_rc_simple_write_entry (XfceRc      *rc,
                             const gchar *key,
                             const gchar *value)
{
  XfceRcSimple *simple = XFCE_RC_SIMPLE (rc);
  Entry        *result;

  result = simple_add_entry (simple, key, value, NULL);
  if (G_LIKELY (result != NULL))
    simple->dirty = TRUE;
}



#define __XFCE_RC_SIMPLE_C__
#include <libxfce4util/libxfce4util-aliasdef.c>
