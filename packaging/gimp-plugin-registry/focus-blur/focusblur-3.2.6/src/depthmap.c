/* Focus Blur -- blur with focus plug-in.
 * Copyright (C) 2002-2008 Kyoichiro Suda
 *
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
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

#include <string.h>
#include <libgimp/gimp.h>

#include "libgimp/stdplugins-intl.h"

#include "focusblur.h"
#include "focusblurparam.h"
#include "depthmap.h"
#ifdef HAVE_FFTW3
#  include "fftblurbuffer.h"
#endif
#include "aaa.h"
 

typedef enum
{
  FBLUR_AAA_DISABLE,
  FBLUR_AAA_NEIGHBOR,
  FBLUR_AAA_SEPARATE,
} FblurAAAType;
 

/*---- Structure ----*/

typedef struct _FblurAAAData FblurAAAData;

struct _FblurDepthMap
{
  /* specified values */
  gint32                 depth_map_ID;
  gfloat                 focal_depth;
  /* pre-computed values */
  gint                   focal_depth_int;
  /* main data */
  gint                   width, height;
  guchar                *data;

  FblurAAAType           aaa_mode;
  struct _FblurAAAData {
    gfloat               val;
    guchar               depth;
  } *aaa;
};


/*---- Prototypes ----*/

static gboolean focusblur_depth_map_make        (FblurDepthMap  *depth_map);
static void     focusblur_depth_map_make_aaa    (FblurDepthMap  *depth_map,
                                                 gint            width,
                                                 gint            height);


/*---- Functions ----*/

gboolean
focusblur_depth_map_update (FblurDepthMap       **depth_map,
                            FblurFftBuffer       *fft,
                            FblurStoreParam      *store,
                            FblurQualityType      quality)
{
  FblurAAAType aaa_mode = FBLUR_AAA_DISABLE;

  if (! store->enable_depth_map)
    return TRUE;

  if (store->enable_depth_aaa)
    {
      switch (quality)
        {
        case FBLUR_QUALITY_BEST:
          aaa_mode = FBLUR_AAA_SEPARATE;
          break;

        case FBLUR_QUALITY_NORMAL:
        case FBLUR_QUALITY_LOW:
          aaa_mode = FBLUR_AAA_NEIGHBOR;
          break;

        case FBLUR_QUALITY_DEFECTIVE:
          aaa_mode = FBLUR_AAA_DISABLE;
          break;
        }

      if (aaa_mode != FBLUR_AAA_DISABLE)
        {
          gint width, height;
          if (*depth_map)
            {
              width = (*depth_map)->width;
              height = (*depth_map)->height;
            }
          else
            {
              width = gimp_drawable_width (store->depth_map_ID);
              height = gimp_drawable_height (store->depth_map_ID);
            }
          if (width < 3 ||
              height < 3)
            aaa_mode = FBLUR_AAA_DISABLE;
        }
    }

  if (*depth_map)
    {
      if ((*depth_map)->depth_map_ID == store->depth_map_ID &&
          aaa_mode == (*depth_map)->aaa_mode)
        {
          if (store->focal_depth == (*depth_map)->focal_depth)
            /* not changed */
            return TRUE;

          (*depth_map)->focal_depth = store->focal_depth;
          (*depth_map)->focal_depth_int =
            rintf (FBLUR_DEPTH_MAX * (*depth_map)->focal_depth / 100.0f);

          if (! gimp_drawable_has_alpha ((*depth_map)->depth_map_ID))
            /* do not need to update */
            return TRUE;
        }

      focusblur_depth_map_destroy (depth_map);
    }

#ifdef HAVE_FFTW3
  if (fft)
    focusblur_fft_buffer_invalidate_depth_map (fft);
#endif

  if (! gimp_drawable_is_valid (store->depth_map_ID))
    {
      gimp_message (_("Specified depth map is invalid."));
      return TRUE;
    }

  (*depth_map) = g_new0 (FblurDepthMap, 1);

  (*depth_map)->depth_map_ID = store->depth_map_ID;
  (*depth_map)->focal_depth = store->focal_depth;
  /* pre-computed values */
  (*depth_map)->focal_depth_int =
    rintf (FBLUR_DEPTH_MAX * (*depth_map)->focal_depth / 100.0f);
  (*depth_map)->aaa_mode = aaa_mode;

  if (! focusblur_depth_map_make (*depth_map))
    {
      focusblur_depth_map_destroy (depth_map);
      return FALSE;
    }

  return TRUE;
}

static gboolean
focusblur_depth_map_make (FblurDepthMap *depth_map)
{
  GimpDrawable  *drawable;
  GimpPixelRgn   pr;
  gpointer       p;
  guchar        *slp, *sp;
  guchar        *dlp, *dp;
  gint           x, y, w, h;
  gint           i, len;
  gint           focal;

  drawable = gimp_drawable_get (depth_map->depth_map_ID);

  if (! drawable)
    return FALSE;

  w = depth_map->width = drawable->width;
  h = depth_map->height = drawable->height;
  len = w * h;
  depth_map->data = g_new (guchar, len);
  if (! depth_map->data)
    return FALSE;

  if (depth_map->aaa_mode == FBLUR_AAA_SEPARATE)
    {
      depth_map->aaa = g_new0 (FblurAAAData, len);
      if (! depth_map->aaa)
        return FALSE;
    }

  gimp_pixel_rgn_init (&pr, drawable, 0, 0, w, h, FALSE, FALSE);
  p = gimp_pixel_rgns_register (1, &pr);

  switch (gimp_drawable_type (depth_map->depth_map_ID))
    {
    case GIMP_GRAY_IMAGE:
      g_assert (pr.bpp == 1);
      for (; p; p = gimp_pixel_rgns_process (p))
        for (y = pr.h, slp = pr.data,
               dlp = depth_map->data + pr.y * w + pr.x;
             y --; slp += pr.rowstride, dlp += w)
          for (x = pr.w, sp = slp, dp = dlp; x --; sp ++, dp ++)
            *dp = *sp;
      break;

    case GIMP_GRAYA_IMAGE:
      g_assert (pr.bpp == 2);
      focal = rintf (255 * depth_map->focal_depth / 100.0f);
      for (; p; p = gimp_pixel_rgns_process (p))
        for (y = pr.h, slp = pr.data,
               dlp = depth_map->data + pr.y * w + pr.x;
             y --; slp += pr.rowstride, dlp += w)
          for (x = pr.w, sp = slp, dp = dlp; x --; sp += 2, dp ++)
            *dp = (sp[0] * sp[1] + focal * (255 - sp[1])) / 255;
      break;

    case GIMP_RGB_IMAGE:
      g_assert (pr.bpp == 3);
      for (; p; p = gimp_pixel_rgns_process (p))
        for (y = pr.h, slp = pr.data,
               dlp = depth_map->data + pr.y * w + pr.x;
             y --; slp += pr.rowstride, dlp += w)
          for (x = pr.w, sp = slp, dp = dlp; x --; sp += 3, dp ++)
            *dp = (sp[0] + sp[1] + sp[2]) / 3;
      break;

    case GIMP_RGBA_IMAGE:
      g_assert (pr.bpp == 4);
      focal = rintf (255 * depth_map->focal_depth / 100.0f);
      for (; p; p = gimp_pixel_rgns_process (p))
        for (y = pr.h, slp = pr.data,
               dlp = depth_map->data + pr.y * w + pr.x;
             y --; slp += pr.rowstride, dlp += w)
          for (x = pr.w, sp = slp, dp = dlp; x --; sp += 4, dp ++)
            *dp = ((sp[0] + sp[1] + sp[2]) / 3 * sp[3]
                   + focal * (255 - sp[3])) / 255;
      break;

    default:
      g_assert_not_reached ();
    }

  gimp_drawable_detach (drawable);

  if (depth_map->aaa_mode != FBLUR_AAA_DISABLE)
    focusblur_depth_map_make_aaa (depth_map, w, h);

  /* hard coding FBLUR_DEPTH_MAX = 127 */
  for (i = 0; i < len; i ++)
    /* variable depth division
     *dp = FBLUR_DEPTH_MAX * (*dp & mask) / mask;
     or simply */
    depth_map->data[i] /= 2;
  if (depth_map->aaa)
    for (i = 0; i < len; i ++)
      depth_map->aaa[i].depth /= 2;

  return TRUE;
}


static void
focusblur_depth_map_make_aaa (FblurDepthMap     *depth_map,
                              gint               width,
                              gint               height)
{
  guint8         box[9];
  FblurAAAFunc   func;
  FblurAAAData  *ap;
  guint8        *bp;
  guint8        *line;
  guint8        *lp;
  guint8        *dp;
  gint           x, y;
  gint           bx, by;
  gint           lw;

  switch (depth_map->aaa_mode)
    {
    case FBLUR_AAA_NEIGHBOR:
      func = focusblur_aaa_neighbor;
      break;

    case FBLUR_AAA_SEPARATE:
      func = focusblur_aaa_separate;
      break;

    default:
      g_assert_not_reached ();
    }

  dp = depth_map->data;
  ap = depth_map->aaa;

  /* copy first line */
  lw = width + 2;
  line = g_malloc (lw * 3);
  memcpy (line + 1, depth_map->data, width);
  line[0] = line[1];
  line[lw -1] = line[lw - 2];
  memcpy (line + lw, line, lw);

  for (y = 0; y < height; y ++)
    {
      /* next line */
      if (y + 1 < height)
        {
          memcpy (line + lw + lw + 1, dp + width, width);
          line[lw * 2] = line[lw * 2 + 1];
          line[lw * 3 - 1] = line[lw * 3 - 2];
        }
      else
        memcpy (line + lw + lw, line + lw, lw);

      for (x = 0; x < width; x ++, dp ++)
        {
          /* copy to box */
          bp = box;
          lp = line + x;
          for (by = 0; by < 3; by ++, lp += lw)
            for (bx = 0; bx < 3; bx ++)
              *bp ++ = lp[bx];

          func (box, dp, &(ap->depth), &(ap->val));
        }

      /* move line */
      memcpy (line, line + lw, lw);
      memcpy (line + lw, line + 2 * lw, lw);
    }

  g_free (line);
}


void
focusblur_depth_map_destroy (FblurDepthMap      **depth_map)
{
  if (*depth_map)
    {
      if ((*depth_map)->data)
        g_free ((*depth_map)->data);

      if ((*depth_map)->aaa)
        g_free ((*depth_map)->aaa);

      g_free (*depth_map);
      *depth_map = NULL;
    }
}


gint
focusblur_depth_map_get_depth (FblurDepthMap *depth_map,
                               gint           x,
                               gint           y)
{
  gsize offset;
  gint  depth;

  if (x >= depth_map->width)
    x %= depth_map->width;
  else if (x < 0)
    x = depth_map->width + (x % depth_map->width);

  if (y >= depth_map->height)
    y %= depth_map->height;
  else if (y < 0)
    y = depth_map->height + (y % depth_map->height);

  offset = y * depth_map->width + x;
  depth = depth_map->data[offset];

  return depth;
}


gint
focusblur_depth_map_get_level (FblurDepthMap *depth_map,
                               gint           depth)
{
  gint  level;

  g_return_val_if_fail (depth_map != NULL, FBLUR_DEPTH_MAX);

  level = depth - depth_map->focal_depth_int;

  return level;
}


gint
focusblur_depth_map_focal_depth (FblurDepthMap  *depth_map)
{
  g_return_val_if_fail (depth_map != NULL, 0);

  return depth_map->focal_depth_int;
}


gboolean
focusblur_depth_map_has_aaa (FblurDepthMap      *depth_map)
{
  if (depth_map &&
      depth_map->width >= 3 &&
      depth_map->height >= 3 &&
      depth_map->aaa_mode == FBLUR_AAA_SEPARATE &&
      depth_map->aaa)

    return TRUE;

  else
    return FALSE;
}


gfloat
focusblur_depth_map_get_aaa (FblurDepthMap *depth_map,
                             gint           x,
                             gint           y,
                             gint          *depth_ret)
{
  gint offset;

  g_return_val_if_fail (depth_map != NULL, 0.0f);
  g_return_val_if_fail (depth_map->aaa != NULL, 0.0f);

  if (x >= depth_map->width)
    x %= depth_map->width;
  else if (x < 0)
    x = depth_map->width + (x % depth_map->width);

  if (y >= depth_map->height)
    y %= depth_map->height;
  else if (y < 0)
    y = depth_map->height + (y % depth_map->height);

  offset = y * depth_map->width + x;

  *depth_ret = depth_map->aaa[offset].depth;
  return depth_map->aaa[offset].val;
}
