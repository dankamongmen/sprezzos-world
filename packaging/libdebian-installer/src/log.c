/*
 * log.c
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *               2003 Bastian Blank <waldi@debian.org>
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

#include <debian-installer/log.h>

#include <debian-installer/mem.h>
#include <debian-installer/slist.h>
#include <debian-installer/utils.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <unistd.h>

typedef struct di_log_handler_struct di_log_handler_struct;

/**
 * @addtogroup di_log
 */

/**
 * @internal
 * @brief Log handler info
 */
struct di_log_handler_struct
{
  unsigned int id;                                      /**< unique id */
  di_log_level_flags log_level;                         /**< flags */
  di_log_handler *log_func;                             /**< the handler */
  void *user_data;                                      /**< supplied data */
};

static di_slist handlers;

#define ALERT_LEVELS (DI_LOG_LEVEL_ERROR | DI_LOG_LEVEL_CRITICAL | DI_LOG_LEVEL_WARNING)

static void mklevel_prefix (char *level_prefix, di_log_level_flags log_level)
{
  char *prefix = NULL;
  char buf[16];

  switch (log_level & DI_LOG_LEVEL_MASK)
  {
    case DI_LOG_LEVEL_ERROR:
      prefix = "ERROR";
      break;
    case DI_LOG_LEVEL_CRITICAL:
      prefix = "CRITICAL";
      break;
    case DI_LOG_LEVEL_WARNING:
      prefix = "WARNING";
      break;
    case DI_LOG_LEVEL_MESSAGE:
      prefix = "Message";
      break;
    case DI_LOG_LEVEL_INFO:
      prefix = "INFO";
      break;
    case DI_LOG_LEVEL_DEBUG:
      prefix = "DEBUG";
      break;
    case DI_LOG_LEVEL_OUTPUT:
      return;
    default:
      if (log_level)
      {
        snprintf (buf, sizeof (buf), "LOG-%u", log_level & DI_LOG_LEVEL_MASK);
        prefix = buf;
      }
      else
        prefix = "LOG";
      break;
  }

  strcat (level_prefix, prefix);
  strcat (level_prefix, log_level & ALERT_LEVELS ? " **: " : ": ");
}

static void handler_default (di_log_level_flags log_level, const char *message, void *user_data)
{
  if (log_level != DI_LOG_LEVEL_DEBUG)
    di_log_handler_default (log_level, message, user_data);
}

void di_log_handler_default (di_log_level_flags log_level, const char *message, void *user_data __attribute__ ((unused)))
{
  char buf[1280];
  const char *progname = di_progname_get ();

  snprintf (buf, sizeof (buf), "(%s:%d): ", progname ? progname : "process", getpid ());
  mklevel_prefix (buf, log_level);
  strcat (buf, message);
  strcat (buf, "\n");

  fputs (buf, log_level & ALERT_LEVELS ? stderr : stdout);
}

void di_log_handler_syslog (di_log_level_flags log_level, const char *message, void *user_data)
{
  char buf[1280];
  static char ident_buf[128];
  static int log_open;
  int syslog_level;

  if (!log_open)
  {
    if (user_data)
      strncpy (ident_buf, user_data, sizeof (ident_buf));
    else
    {
      const char *progname = di_progname_get ();
      snprintf (ident_buf, sizeof (ident_buf), "%s[%d]", progname ? progname : "process", getpid ());
    }

    openlog (ident_buf, 0, LOG_USER);
    log_open = 1;
  }

  buf[0] = '\0';
  mklevel_prefix (buf, log_level);
  strcat (buf, message);
  strcat (buf, "\n");

  switch (log_level & DI_LOG_LEVEL_MASK)
  {
    case DI_LOG_LEVEL_ERROR:
      syslog_level = LOG_ALERT;
      break;
    case DI_LOG_LEVEL_CRITICAL:
      syslog_level = LOG_CRIT;
      break;
    case DI_LOG_LEVEL_WARNING:
      syslog_level = LOG_WARNING;
      break;
    case DI_LOG_LEVEL_MESSAGE:
      syslog_level = LOG_NOTICE;
      break;
    case DI_LOG_LEVEL_INFO:
    case DI_LOG_LEVEL_OUTPUT:
      syslog_level = LOG_INFO;
      break;
    default:
      syslog_level = LOG_DEBUG;
      break;
  }

  syslog (syslog_level, "%s", buf);
}

static di_log_handler *internal_di_log_get_handler (di_log_level_flags log_level, void **user_data)
{
  di_slist_node *node;

  for (node = handlers.head; node; node = node->next)
  {
    di_log_handler_struct *handler = node->data;

    if ((handler->log_level & log_level) == log_level)
    {
      *user_data = handler->user_data;
      return handler->log_func;
    }
  }

  return handler_default;
}

unsigned int di_log_set_handler (di_log_level_flags log_levels, di_log_handler *log_func, void *user_data)
{
  static unsigned int handler_id = 0;
  di_log_handler_struct *handler;

  handler = di_new (di_log_handler_struct, 1);

  handler->id = ++handler_id;
  handler->log_level = log_levels;
  handler->log_func = log_func;
  handler->user_data = user_data;

  di_slist_append (&handlers, handler);

  return handler_id;
}

void di_log (di_log_level_flags log_level, const char *format, ...)
{
  va_list args;

  va_start (args, format);
  di_vlog (log_level, format, args);
  va_end (args);
}

__asm__ (".symver di_log,di_log_real_4_0@LIBDI_4.0");

void di_vlog (di_log_level_flags log_level, const char *format, va_list args)
{
  char buf[1024];
  int fatal = log_level & DI_LOG_FATAL_MASK;
  di_log_handler *log_func;
  void *user_data=0;

  vsnprintf (buf, sizeof (buf), format, args);

  log_func = internal_di_log_get_handler (log_level, &user_data);

  log_func (log_level, buf, user_data);

  if (fatal)
    exit (1);
}

