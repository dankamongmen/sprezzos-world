/*
 * Copyright (C) 2009-2012 Dr. Christoph L. Spiel
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifndef __TIFF_MESSAGE_H__
#define __TIFF_MESSAGE_H__

#include <stdarg.h>

/** Function that intercepts warnings from the TIFF library */
void tiff_warning(const char* module, const char* format, va_list arguments);

/** Function that intercepts errors from the TIFF library */
void tiff_error(const char* module, const char* format, va_list arguments);

#endif /* __TIFF_MESSAGE_H__ */

// Local Variables:
// mode: c++
// End:
