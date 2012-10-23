/*
 * utils.c
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

#include <debian-installer/system/subarch.h>
#include <debian-installer/system/utils.h>

#include <debian-installer/log.h>
#include <debian-installer/utils.h>

#include <stdbool.h>
#include <unistd.h>

void di_system_init (const char *_progname)
{
  di_init (_progname);
  di_log_set_handler (DI_LOG_LEVEL_MASK, di_log_handler_syslog, NULL);
}

#ifndef DI_SYSTEM_SUBARCH_CAN_GUESS

/*
 * HACK: If there's a better way to do this, we should probably use that
 *       instead of this stub function for non armel archs
 */

const char *di_system_subarch_analyze_guess (void)
{
  return di_system_subarch_analyze();
}

#endif
