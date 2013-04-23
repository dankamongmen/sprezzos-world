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

#include <glib-object.h>
#include <libgimpbase/gimpbasetypes.h>

#include "libgimp/stdplugins-intl.h"

#include "focusblurenums.h"


GType
focusblur_model_type_get_type (void)
{
  static GType type = 0;

  if (! type)
    {
      static const GEnumValue values[] =
        {
          { FBLUR_MODEL_FLAT,      "FBLUR_MODEL_FLAT",      "flat" },
          { FBLUR_MODEL_SPHERICAL, "FBLUR_MODEL_SPHERICAL", "spherical" },
          { FBLUR_MODEL_GAUSSIAN,  "FBLUR_MODEL_GAUSSIAN",  "gaussian" },
          { FBLUR_MODEL_RING,      "FBLUR_MODEL_RING",      "ring" },
          { FBLUR_MODEL_CONCAVE,   "FBLUR_MODEL_CONCAVE",   "concave" },
          { FBLUR_MODEL_BRUSH,     "FBLUR_MODEL_BRUSH",     "brush" },
          { 0, NULL, NULL }
        };
      static const GimpEnumDesc descs[] =
        {
          { FBLUR_MODEL_FLAT,      N_("Flat"),      NULL },
          { FBLUR_MODEL_SPHERICAL, N_("Spherical"), NULL },
          { FBLUR_MODEL_GAUSSIAN,  N_("Gaussian"),  NULL },
          { FBLUR_MODEL_RING,      N_("Ring"),      NULL },
          { FBLUR_MODEL_CONCAVE,   N_("Concave"),   NULL },
          { FBLUR_MODEL_BRUSH,     N_("Brush"),     NULL },
          { 0, NULL, NULL }
        };

      type = g_enum_register_static ("FblurModelType", values);
#ifdef ENABLE_NLS
      gimp_type_set_translation_domain (type, GETTEXT_PACKAGE);
#endif
      gimp_enum_set_value_descriptions (type, descs);
    }

  return type;
}


GType
focusblur_shine_type_get_type (void)
{
  static GType type = 0;

  if (! type)
    {
      static const GEnumValue values[] =
        {
          { FBLUR_SHINE_LUMINOSITY, "FBLUR_SHINE_LUMINOSITY", "luminosity" },
          { FBLUR_SHINE_SATURATION, "FBLUR_SHINE_SATURATION", "saturation" },
          { 0, NULL, NULL }
        };
      static const GimpEnumDesc descs[] =
        {
          { FBLUR_SHINE_LUMINOSITY, N_("Luminosity"), NULL },
          { FBLUR_SHINE_SATURATION, N_("Saturation"), NULL },
          { 0, NULL, NULL }
        };

      type = g_enum_register_static ("FblurShineType", values);
#ifdef ENABLE_NLS
      gimp_type_set_translation_domain (type, GETTEXT_PACKAGE);
#endif
      gimp_enum_set_value_descriptions (type, descs);
    }

  return type;
}


GType
focusblur_quality_type_get_type (void)
{
  static GType type = 0;

  if (! type)
    {
      static const GEnumValue values[] =
        {
          { FBLUR_QUALITY_BEST,      "FBLUR_QUALITY_BEST",      "best" },
          { FBLUR_QUALITY_NORMAL,    "FBLUR_QUALITY_NORMAL",    "normal" },
          { FBLUR_QUALITY_LOW,       "FBLUR_QUALITY_LOW",       "low" },
          { FBLUR_QUALITY_DEFECTIVE, "FBLUR_QUALITY_DEFECTIVE", "defective" },
          { 0, NULL, NULL }
        };
      static const GimpEnumDesc descs[] =
        {
          { FBLUR_QUALITY_BEST,      N_("Best"),      NULL },
          { FBLUR_QUALITY_NORMAL,    N_("Normal"),    NULL },
          { FBLUR_QUALITY_LOW,       N_("Low"),       NULL },
          { FBLUR_QUALITY_DEFECTIVE, N_("Defective"), NULL },
          { 0, NULL, NULL }
        };

      type = g_enum_register_static ("FblurQualityType", values);
#ifdef ENABLE_NLS
      gimp_type_set_translation_domain (type, GETTEXT_PACKAGE);
#endif
      gimp_enum_set_value_descriptions (type, descs);
    }

  return type;
}

