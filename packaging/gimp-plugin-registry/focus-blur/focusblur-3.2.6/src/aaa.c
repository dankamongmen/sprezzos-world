/* Focus Blur -- blur with focus plug-in.
 * Copyright (C) 2002-2008 Kyoichiro Suda
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include "aaa.h"

 
/*---- Prototypes ----*/

static gboolean focusblur_aaa_check     (guint8         *data,
                                         guint8         *min_ret,
                                         guint8         *max_ret);
static inline void
swap_uint8 (guint8 *a,
            guint8 *b)
{
  *a ^= *b;
  *b ^= *a;
  *a ^= *b;
}


/*---- Functions ----*/

void
focusblur_aaa_neighbor (guint8  *data,
                        guint8  *round_ret,
                        guint8  *dummy0,
                        gfloat  *dummy1)
{
  guint8 min, max;
  gfloat ratio;

  if (! focusblur_aaa_check (data, &min, &max))
    return;

  ratio = (gfloat) (data[4] - min) / (max - min);
  *round_ret = (ratio < 0.5f) ? min : max;
}


void
focusblur_aaa_separate (guint8  *data,
                        guint8  *min_ret,
                        guint8  *max_ret,
                        gfloat  *ratio_ret)
{
  if (! focusblur_aaa_check (data, min_ret, max_ret))
    return;

  *ratio_ret = (gfloat) (data[4] - *min_ret) / (*max_ret - *min_ret);
}


static gboolean
focusblur_aaa_check (guint8 *data,
                     guint8 *min_ret,
                     guint8 *max_ret)
{
  const gint aaa_ambiguous = 8; /* 6-10 is good */
  guint8     min,  max;
  guint8     gate_low, gate_high;
  gint       i;

  min = max = data[0];

  for (i = 1; i < 9; i ++)
    if (data[i] > max)
      max = data[i];
    else if (data[i] < min)
      min = data[i];

  gate_low  = min + aaa_ambiguous;
  gate_high = max - aaa_ambiguous;

  if (gate_low >= gate_high ||
      data[4] <= gate_low ||
      data[4] >= gate_high)
    return FALSE;

  if (data[1] > data[7])
    {
      swap_uint8 (&data[0], &data[6]);
      //swap_uint8 (&data[1], &data[7]);
      swap_uint8 (&data[2], &data[8]);
    }

  if (data[3] > data[5])
    {
      swap_uint8 (&data[0], &data[2]);
      //swap_uint8 (&data[3], &data[5]);
      swap_uint8 (&data[6], &data[8]);
    }

  if (data[0] > gate_low ||
      data[8] < gate_high)
    return FALSE;

  *min_ret = min;
  *max_ret = max;

  return TRUE;
}
