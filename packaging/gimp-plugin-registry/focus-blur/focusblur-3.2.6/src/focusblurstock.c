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

#include <gtk/gtk.h>
#include <libgimp/gimp.h>

#include "libgimp/stdplugins-intl.h"

#include "pixmaps/pixbufs.h"
#include "focusblurstock.h"


/* Stock ID */

#define FBLUR_STOCK_MODEL_FLAT          "focusblur-model-flat"
#define FBLUR_STOCK_MODEL_SPHERICAL     "focusblur-model-spherical"
#define FBLUR_STOCK_MODEL_GAUSSIAN      "focusblur-model-gaussian"
#define FBLUR_STOCK_MODEL_RING          "focusblur-model-ring"
#define FBLUR_STOCK_MODEL_CONCAVE       "focusblur-model-concave"
#define FBLUR_STOCK_MODEL_BRUSH         "focusblur-model-brush"


/*---- Variables ----*/

static GtkIconFactory *fblur_stock_factory = NULL;

static const GtkStockItem fblur_stock_items[] =
{
  { FBLUR_STOCK_MODEL_FLAT,      N_("Flat"),      0, 0, GETTEXT_PACKAGE },
  { FBLUR_STOCK_MODEL_SPHERICAL, N_("Spherical"), 0, 0, GETTEXT_PACKAGE },
  { FBLUR_STOCK_MODEL_GAUSSIAN,  N_("Gaussian"),  0, 0, GETTEXT_PACKAGE },
  { FBLUR_STOCK_MODEL_RING,      N_("Ring"),      0, 0, GETTEXT_PACKAGE },
  { FBLUR_STOCK_MODEL_CONCAVE,   N_("Concave"),   0, 0, GETTEXT_PACKAGE },
  { FBLUR_STOCK_MODEL_BRUSH,     N_("Brush"),     0, 0, GETTEXT_PACKAGE },
};

const struct
{
  const gchar      *stock_id;
  const GdkPixdata *pixdata;
} fblur_stock_pixbufs[] =
{
  { FBLUR_STOCK_MODEL_FLAT,      &focusblur_model_flat          },
  { FBLUR_STOCK_MODEL_SPHERICAL, &focusblur_model_spherical     },
  { FBLUR_STOCK_MODEL_GAUSSIAN,  &focusblur_model_gaussian      },
  { FBLUR_STOCK_MODEL_RING,      &focusblur_model_ring          },
  { FBLUR_STOCK_MODEL_CONCAVE,   &focusblur_model_concave       },
};


/*---- Prototype ----*/

static GdkPixbuf* focusblur_pixbuf_new_from_gimp_brush (const gchar *name);


/*---- Function ----*/

void
focusblur_stock_init (void)
{
  GtkIconSet    *set;
  GdkPixbuf     *pixbuf;
  gint           i;

  if (fblur_stock_factory)
    return;

  fblur_stock_factory = gtk_icon_factory_new ();

  for (i = 0; i < G_N_ELEMENTS (fblur_stock_pixbufs); i++)
    {
      pixbuf =
        gdk_pixbuf_from_pixdata (fblur_stock_pixbufs[i].pixdata, TRUE, NULL);
      g_assert (pixbuf != NULL);

      set = gtk_icon_set_new_from_pixbuf (pixbuf);
      gtk_icon_factory_add
        (fblur_stock_factory, fblur_stock_pixbufs[i].stock_id, set);

      gtk_icon_set_unref (set);
      g_object_unref (pixbuf);
    }

  /* brush */
  focusblur_stock_update_modelbrush (NULL);

  gtk_icon_factory_add_default (fblur_stock_factory);
  gtk_stock_add_static (fblur_stock_items, G_N_ELEMENTS (fblur_stock_items));
}


void
focusblur_stock_update_modelbrush (const gchar *brush_name)
{
  GtkIconSet *set;
  GdkPixbuf  *pixbuf;

  g_return_if_fail (fblur_stock_factory != NULL);

  pixbuf = focusblur_pixbuf_new_from_gimp_brush (brush_name);
  set = gtk_icon_set_new_from_pixbuf (pixbuf);
  gtk_icon_factory_add (fblur_stock_factory, FBLUR_STOCK_MODEL_BRUSH, set);

  gtk_icon_set_unref (set);
  g_object_unref (pixbuf);
}


static GdkPixbuf*
focusblur_pixbuf_new_from_gimp_brush (const gchar *brush_name)
{
  GdkPixbuf     *pixbuf;
  guint8        *data;
  const gint     bpp = 3;
  gint           width, height, rowstride;
  gint           size;
  guint8        *dlp, *dp, *mp, *cp;
  gint           x, y;

  gboolean       ret;
  gint           mask_bpp, num_mask_bytes;
  gint           color_bpp, num_color_bytes;
  guint8        *mask_bytes, *color_bytes;

  if (! brush_name || ! brush_name[0])
    brush_name = gimp_context_get_brush ();

  ret = gimp_brush_get_pixels (brush_name, &width, &height,
                               &mask_bpp,  &num_mask_bytes,  &mask_bytes,
                               &color_bpp, &num_color_bytes, &color_bytes);
  if (! ret)
    return NULL;

  rowstride = (bpp * width + 3) & ~3;
  size = rowstride * height;
  dp = dlp = data = g_malloc0 (size);
  mp = mask_bytes;
  cp = color_bytes;

  if (color_bytes)
    {
      g_assert (mask_bpp == 1);
      g_assert (color_bpp == 3);

      for (y = 0; y < height; y ++, dlp += rowstride)
        for (x = 0, dp = dlp; x < width; x ++,
               dp += bpp, mp += mask_bpp, cp += color_bpp)
          {
            dp[0] = cp[0] * mp[0] / 255;
            dp[1] = cp[1] * mp[0] / 255;
            dp[2] = cp[2] * mp[0] / 255;
          }
    }
  else
    {
      g_assert (mask_bpp == 1);

      for (y = 0; y < height; y ++, dlp += rowstride)
        for (x = 0, dp = dlp; x < width; x ++, dp += bpp, mp += mask_bpp)
          dp[0] = dp[1] = dp[2] = mp[0];
    }

  g_free (mask_bytes);
  g_free (color_bytes);

  pixbuf = gdk_pixbuf_new_from_data (data,
                                     GDK_COLORSPACE_RGB, FALSE,
                                     8, width, height, rowstride,
                                     (GdkPixbufDestroyNotify) g_free, NULL);

  return pixbuf;
}
