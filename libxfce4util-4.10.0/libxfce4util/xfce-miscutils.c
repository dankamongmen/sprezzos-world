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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#ifdef HAVE_ERR_H
#include <err.h>
#endif
#include <errno.h>
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#elif defined (HAVE_VARARGS_H)
#include <varargs.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libxfce4util/libxfce4util.h>
#include <libxfce4util/libxfce4util-alias.h>

#define XFCE4DIR ".xfce4"

/* environment variable the user can set to change the path to
 * the users .xfce4 directory. If not set, the xfce_userdir defaults
 * to "$HOME/.xfce4".
 */
#define XFCE4HOME_ENVVAR "XFCE4HOME"

G_LOCK_DEFINE_STATIC(_lock);


static gchar *xfce_homedir = NULL; /* path to users home directory */
static gchar *xfce_userdir = NULL; /* path to users .xfce4 directory */



static void
internal_initialize(void)
{
  const gchar *dir;

  /* determine path to users home directory */
  dir = g_get_home_dir ();
  if (dir == NULL)
    {
#ifdef HAVE_ERR_H
      errx (EXIT_FAILURE, "Unable to determine users home directory");
#else
      fprintf (stderr, "%s: ", g_get_prgname ());
      fprintf (stderr, "Unable to determinte users home directory");
      fprintf (stderr, "\n");
      exit (EXIT_FAILURE);
#endif
    }
  else
    {
      xfce_homedir = g_strdup (dir);
    }

  /* get path to users .xfce4 directory */
  dir = g_getenv (XFCE4HOME_ENVVAR);
  if (dir != NULL)
    {
      xfce_userdir = g_strdup (dir);
    }
  else
    {
      xfce_userdir = g_build_filename (xfce_homedir, XFCE4DIR, NULL);
    }
}



static gchar*
internal_get_file_r (const gchar *dir,
                     gchar       *buffer,
                     gsize        len,
                     const gchar *format,
                     va_list      ap)
{
  gsize n;

  g_return_val_if_fail(buffer != NULL, NULL);
  g_return_val_if_fail(format != NULL, NULL);
  g_return_val_if_fail(len > 0, NULL);

  if ((n = g_strlcpy(buffer, dir, len)) >= len)
    return NULL;

  if ((n = g_strlcat(buffer, G_DIR_SEPARATOR_S, len)) >= len)
    return NULL;

  if ((gsize) g_vsnprintf(buffer + n, len - n, format, ap) >= len - n)
    return NULL;

  return buffer;
}



/**
 * xfce_version_string:
 *
 * Queries the version string of the installed Xfce desktop environment.
 *
 * Return value: the overall version information of the installed Xfce desktop.
 *
 * Since: 4.2
 */
const gchar*
xfce_version_string (void)
{
  return XFCE_VERSION_STRING;
}



/**
 * xfce_get_homedir:
 *
 * Similar to g_get_homedir() in functionality but will never return NULL.
 * While g_get_homedir() may return NULL under certain circumstances, this
 * function is garantied to never ever return NULL, but always return a
 * valid character pointer with the absolute path to the user's home directory.
 *
 * The returned string is owned by libxfce4util and must not be freed by
 * the caller.
 *
 * Return value: the path to the current user's home directory.
 **/
const gchar*
xfce_get_homedir (void)
{
  G_LOCK (_lock);
  if (!xfce_homedir)
    internal_initialize ();
  G_UNLOCK (_lock);

  return xfce_homedir;
}



/**
 * xfce_get_homefile_r:
 * @buffer  : pointer to a user provided destination buffer.
 * @length  : size of @buffer in bytes.
 * @format  : printf style format string.
 * @Varargs : the arguments to substitute in the output.
 *
 * Similar in functionality to #xfce_get_homefile, but uses a user
 * defined @buffer instead of allocating a new buffer.
 *
 * xfce_get_homefile_r uses safe string operations, that says, it garanties
 * that the resulting string is always zero terminated, as long as the
 * @length is greater than zero.
 *
 * Return value: pointer to @buffer.
 **/
gchar*
xfce_get_homefile_r (gchar *buffer, size_t len, const gchar *format, ...)
{
  gchar  *ptr;
  va_list ap;

  va_start (ap, format);
  ptr = internal_get_file_r (xfce_get_homedir (), buffer, len, format, ap);
  va_end (ap);

  return ptr;
}



/**
 * xfce_get_userdir:
 *
 * Safe way to retrieve the path to the user's ".xfce4" directory. The path
 * to the current user's ".xfce4" directory is either taken from the
 * environment variable XFCE4HOME if defined, or if unset, is gained by
 * concatenating the path to the user's home directory and the ".xfce4".
 * That says, it will, by default, return the path "$HOME/.xfce4", where
 * $HOME is replaced with the absolute path to the user's home directory.
 *
 * The returned string is managed by libxfce4util and must not be freed by
 * the caller.
 *
 * Return value: the path to the current user's ".xfce4" directory.
 */
const gchar*
xfce_get_userdir (void)
{
  G_LOCK(_lock);
  if (!xfce_userdir)
    {
      internal_initialize();

      /* verify that the directory exists or is created */
      if (!g_file_test (xfce_userdir, G_FILE_TEST_IS_DIR))
        xfce_mkdirhier (xfce_userdir, 0700, NULL);
    }
  G_UNLOCK(_lock);

  return xfce_userdir;
}



/**
 * xfce_get_userfile_r:
 * @buffer  : user provided destination buffer.
 * @length  : size of @buffer in bytes.
 * @format  : printf style format string.
 * @Varargs : arguments to substitute in the output.
 *
 * Return value: pointer to @buffer.
 **/
gchar*
xfce_get_userfile_r (gchar *buffer, size_t length, const gchar *format, ...)
{
  gchar  *ptr;
  va_list ap;

  va_start (ap, format);
  ptr = internal_get_file_r (xfce_get_userdir (), buffer, length, format, ap);
  va_end (ap);

  return ptr;
}



/**
 * xfce_strjoin:
 * @separator:
 * @strings:
 * @count:
 *
 * Joins the @count character strings pointed to by @strings using @separator
 * to a single string.
 *
 * Return value: the joined string. The string has to be freed by the caller
 *               using g_free() when no longer needed.
 **/
gchar*
xfce_strjoin (const gchar *separator,
              gchar      **strings,
              gint         count)
{
  gchar *result;
  gint   length;
  gint   n;

  g_return_val_if_fail (count > 0, NULL);
  g_return_val_if_fail (strings != NULL, NULL);

  for (length = 1, n = 0; n < count; n++)
    length += strlen (strings[n]);

  if (separator != NULL)
    length += (count - 1) * strlen (separator);

  result = g_new0 (gchar, length);

  for (n = 0; n < count; n++) {
    g_strlcat (result, strings[n], length);

    if (separator != NULL && n + 1 < count)
      g_strlcat (result, separator, length);
  }

  return result;
}



/**
 * xfce_gethostname:
 *
 * Portable way to query the hostname of the node running the process. This
 * function does not ever return %NULL, but always returns a string containing
 * the current node's hostname.
 *
 * Return value: the current node's hostname. The string has to be freed
 *               by the caller using g_free().
 **/
gchar*
xfce_gethostname (void)
{
#if defined(HAVE_GETHOSTNAME)
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif
  char hostname[MAXHOSTNAMELEN];

  if (gethostname (hostname, MAXHOSTNAMELEN) == 0)
    return g_strdup (hostname);
#elif defined(HAVE_SYS_UTSNAME_H)
  struct utsname name;

  if (uname (&name) == 0)
    return g_strdup (name.nodename);
#endif
  g_error ("Unable to determine your hostname: %s", g_strerror (errno));
  /* NOT REACHED */
  return NULL;
}



/**
 * xfce_putenv:
 * @string: Character string in the form "name=value".
 *
 * Portable replacement for the Unix putenv() library function. @string has
 * to have the form "name=value". Calling xfce_putenv() this way is equal to
 * calling xfce_setenv("name", "value", %TRUE).
 *
 * Return value: 0 if the operation was successful; otherwise the global
 *               variable errno is set to indicate the error and a value
 *               of -1 is returned.
 *
 * Since: 4.2
 **/
gint
xfce_putenv (const gchar *string)
{
#ifdef HAVE_BROKEN_PUTENV
  gchar* buffer;
  int result, sverrno;

  /*
   * We need to use plattform malloc() here, cause g_malloc() need not
   * to use malloc(), and we don't know about the evil interna of this
   * plattforms putenv() implementation.
   */
#ifdef HAVE_STRDUP
  if ((buffer = strdup (string)) != NULL)
    {
#else
  if ((buffer = malloc (strlen (string) + 1)) != NULL)
    {
      strcpy(buffer, string);
#endif
      if ((result = putenv (buffer)) < 0)
        {
          sverrno = errno;
          free (buffer);
          errno = sverrno;
        }
    }
  else
    {
      errno = ENOMEM;
      result = -1;
    }

    return result;
#else /* !HAVE_BROKEN_PUTENV */
    return putenv (string);
#endif /* !HAVE_BROKEN_PUTENV */
}



/**
 * xfce_setenv:
 * @name: the name of the environment variable to set, must not contain '='.
 * @value: the value to set the variable to.
 * @overwrite: whether to change the variable if it already exists.
 *
 * If the variable @name does not exists in the list of environment variables,
 * it is inserted with its value being set to @value. If the variable does
 * exist, then its value is only changed to @value if @overwrite is TRUE.
 *
 * On plattforms that provide a working native setenv() library call, this
 * functions is used, on all other plattforms setenv() is emulated using
 * xfce_putenv(). That says, xfce_setenv() is not subject to the limitations
 * that apply to some setenv() implementations and seem also to apply to
 * g_setenv() in Glib 2.4.
 *
 * Return value: 0 if the operation was successful; otherwise the global
 *               variable errno is set to indicate the error and a value
 *               of -1 is returned.
 *
 * Since: 4.2
 **/
gint
xfce_setenv (const gchar *name,
             const gchar *value,
             gboolean     overwrite)
{
  /* Plattforms with broken putenv() are unlikely to have a working setenv() */
#if !defined(HAVE_SETENV) || defined(HAVE_BROKEN_PUTENV)
  int result = 0;
  gchar *buf;

  if (g_getenv (name) == NULL || overwrite)
    {
      buf = g_strdup_printf ("%s=%s", name, value);
      result = xfce_putenv (buf);
      g_free (buf);
    }

  return result;
#else
  return setenv (name, value, overwrite);
#endif /* !HAVE_SETENV */
}



/**
 * xfce_unsetenv:
 * @name : the name of the environment variable to unset, must not contain '='.
 *
 * Deletes all instances of the variables @name from the list of environment
 * variables in the current process.
 *
 * Note that on some systems, the memory used for the variable and its value
 * can't be reclaimed. Furthermore, this function can't be guaranteed to
 * operate in a threadsafe way.
 *
 * Since: 4.2
 **/
void
xfce_unsetenv (const gchar *name)
{
#if defined(HAVE_UNSETENV)
  unsetenv (name);
#elif GLIB_CHECK_VERSION(2,4,0)
  g_unsetenv (name);
#else
  /* Adopted the GLib way of doing things, since my putenv (name + "=") does
   * not work properly on most plattforms :-/
   */
  extern char **environ;
  char **envp1;
  char **envp2;
  int    length;

  length = strlen (name);

  for (envp1 = envp2 = environ; *envp1 != NULL; ++envp1)
    if (strncmp (*envp1, name, length) != 0 || (*envp1)[length] != '=')
      *envp2++ = *envp1;

  *envp2 = NULL;
#endif
}



static inline gboolean
xfce_is_valid_tilde_prefix (const gchar *p)
{
  if (g_ascii_isspace (*p) /* thunar ~/music */
      || *p == '=' /* terminal --working-directory=~/ */
      || *p == '\'' || *p == '"') /* terminal --working-directory '~my music' */
    return TRUE;

  return FALSE;
}



/**
 * xfce_expand_variables:
 * @command : Input string or %NULL.
 * @envp    : Addition environment variables to take into account. These
 *            variables have higher priority than the ones in the process's
 *            environment.
 *
 * Expands shell like environment variables and tilde (~/ and ~user/ are both supported)
 * in @command.
 *
 * Return value: %NULL on error, else the string, which should be freed using
 *               g_free() when no longer needed.
 *
 * Since: 4.2
 **/
gchar *
xfce_expand_variables (const gchar *command,
                       gchar      **envp)
{
  GString        *buf;
  const gchar    *start;
  gchar          *variable;
  const gchar    *p;
  const gchar    *value;
  gchar         **ep;
  guint           len;
#ifdef HAVE_GETPWNAM
  struct passwd  *pw;
  gchar          *username;
#endif

  if (G_UNLIKELY (command == NULL))
    return NULL;

  buf = g_string_sized_new (strlen (command));

  for (p = command; *p != '\0'; ++p)
    {
      continue_without_increase:

      if (*p == '~'
          && (p == command
              || xfce_is_valid_tilde_prefix (p - 1)))
        {
          /* walk to the end of the string or to a directory separator */
          for (start = ++p; *p != '\0' && *p != G_DIR_SEPARATOR; ++p);

          if (G_LIKELY (start == p))
            {
              /* add the current user directory */
              buf = g_string_append (buf, xfce_get_homedir ());
            }
          else
            {
#ifdef HAVE_GETPWNAM
              username = g_strndup (start, p - start);
              pw = getpwnam (username);
              g_free (username);

              /* add the users' home directory if found, fallback to the
               * not-expanded string */
              if (pw != NULL && pw->pw_dir != NULL)
                buf = g_string_append (buf, pw->pw_dir);
              else
#endif
                buf = g_string_append_len (buf, start - 1, p - start + 1);
            }

          /* we are either at the end of the string or *p is a separator,
           * so continue to add it to the result buffer */
        }
      else if (*p == '$')
        {
          /* walk to the end of a valid variable name */
          for (start = ++p; *p != '\0' && (g_ascii_isalnum (*p) || *p == '_'); ++p);

          if (start < p)
            {
              value = NULL;
              len = p - start;

              /* lookup the variable in the environment supplied by the user */
              if (envp != NULL)
                {
                  /* format is NAME=VALUE */
                  for (ep = envp; *ep != NULL; ++ep)
                    if (strncmp (*ep, start, len) == 0
                        && (*ep)[len] == '=')
                      {
                        value = (*ep) + len + 1;
                        break;
                      }
                }

              /* fallback to the environment */
              if (value == NULL)
                {
                  variable = g_strndup (start, len);
                  value = g_getenv (variable);
                  g_free (variable);
                }

              if (G_LIKELY (value != NULL))
                {
                  buf = g_string_append (buf, value);
                }
              else
                {
                  /* the variable name was valid, but no value was
                   * found, insert nothing and continue */
                }

              /* *p is at the start of the charater after the variable,
               * so continue scanning without advancing the string offset
               * so two variables are replaced properly */
              goto continue_without_increase;
            }
          else
            {
              /* invalid variable format, add the
               * $ character and continue */
              --p;
            }
        }

      buf = g_string_append_c (buf, *p);
    }

  return g_string_free (buf, FALSE);
}



#define __XFCE_MISCUTILS_C__
#include <libxfce4util/libxfce4util-aliasdef.c>
