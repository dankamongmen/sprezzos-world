/* GStreamer
 * Copyright (C) 2008 Sebastian Dröge <sebastian.droege@collabora.co.uk>
 *
 * gst-codec-info.c: tool to print automatic codec installation info
 *                   for a given list of plugins
 *
 * Partially based on gst-inspect from gstreamer.
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */


#include <gst/gst.h>
#include <string.h>

static const gchar *virtual_packages[] = {
  "gstreamer0.10-audiosink",
  "gstreamer0.10-audiosource",
  "gstreamer0.10-videosink",
  "gstreamer0.10-videosource",
  "gstreamer0.10-visualization",
  NULL
};

static GList *elements = NULL;
static GList *uri_sources = NULL;
static GList *uri_sinks = NULL;
static GList *provides = NULL;
static GstCaps *encoders = NULL, *decoders = NULL;

static void
free_plugin_info (void)
{
  g_list_foreach (elements, (GFunc) g_free, NULL);
  g_list_foreach (uri_sources, (GFunc) g_free, NULL);
  g_list_foreach (uri_sinks, (GFunc) g_free, NULL);

  g_list_free (elements);
  g_list_free (uri_sources);
  g_list_free (uri_sinks);

  g_list_free (provides);

  gst_caps_unref (encoders);
  gst_caps_unref (decoders);
}

static void
print_plugin_info (void)
{
  GList *l;

  if (elements) {
    g_print ("gstreamer:Elements=");
    for (l = elements; l; l = l->next) {
      if (l->next)
        g_print ("%s, ", (gchar *) l->data);
      else
        g_print ("%s\n", (gchar *) l->data);
    }
  }

  if (provides) {
    g_print ("gstreamer:Provides=");
    for (l = provides; l; l = l->next) {
      if (l->next)
        g_print ("%s, ", (gchar *) l->data);
      else
        g_print ("%s\n", (gchar *) l->data);
    }
  }

  if (uri_sources) {
    g_print ("gstreamer:URISources=");
    for (l = uri_sources; l; l = l->next) {
      if (l->next)
        g_print ("%s, ", (gchar *) l->data);
      else
        g_print ("%s\n", (gchar *) l->data);
    }
  }

  if (uri_sinks) {
    g_print ("gstreamer:URISinks=");
    for (l = uri_sinks; l; l = l->next) {
      if (l->next)
        g_print ("%s, ", (gchar *) l->data);
      else
        g_print ("%s\n", (gchar *) l->data);
    }
  }

  if (!gst_caps_is_empty (encoders)) {
    gchar *caps = gst_caps_to_string (encoders);

    g_print ("gstreamer:Encoders=%s\n", caps);
    g_free (caps);
  }

  if (!gst_caps_is_empty (decoders)) {
    gchar *caps = gst_caps_to_string (decoders);

    g_print ("gstreamer:Decoders=%s\n", caps);
    g_free (caps);
  }
}

static void
remove_duplicates (GList * list, gboolean free)
{
  GList *l;
  gchar *previous;

  if (!list || !list->next)
    return;

  previous = list->data;
  l = list->next;

  while (l) {
    if (strcmp (l->data, previous) == 0) {
      GList *next = l->next;

      if (free)
        g_free (l->data);

      l = g_list_delete_link (l->prev, l);
      l = next;
    } else {
      previous = l->data;
      l = l->next;
    }
  }
}

static void
cleanup_plugin_info (void)
{
  if (encoders)
    gst_caps_do_simplify (encoders);

  if (decoders)
    gst_caps_do_simplify (decoders);

  elements = g_list_sort (elements, (GCompareFunc) strcmp);
  uri_sources = g_list_sort (uri_sources, (GCompareFunc) strcmp);
  uri_sinks = g_list_sort (uri_sinks, (GCompareFunc) strcmp);
  provides = g_list_sort (provides, (GCompareFunc) strcmp);

  remove_duplicates (elements, TRUE);
  remove_duplicates (uri_sources, TRUE);
  remove_duplicates (uri_sinks, TRUE);
  remove_duplicates (provides, FALSE);
}

static void
collect_uri_protocols (GstElementFactory * factory)
{
  gchar **protocols, **p;

  protocols = gst_element_factory_get_uri_protocols (factory);
  if (!protocols)
    return;

  switch (gst_element_factory_get_uri_type (factory)) {
    case GST_URI_SINK:
      for (p = protocols; *p; p++)
        uri_sinks = g_list_prepend (uri_sinks, g_strdup (*p));
      break;
    case GST_URI_SRC:
      for (p = protocols; *p; p++)
        uri_sources = g_list_prepend (uri_sources, g_strdup (*p));
      break;
  }
  g_strfreev (protocols);
}

static void
remove_min_max_fields (GstStructure * s)
{
  gint i, n;
  gboolean removed_field = FALSE;


  do {
    n = gst_structure_n_fields (s);
    removed_field = FALSE;
    for (i = 0; i < n; i++) {
      const gchar *field_name = gst_structure_nth_field_name (s, i);
      const GValue *field;

      field = gst_structure_get_value (s, field_name);

      if (GST_VALUE_HOLDS_INT_RANGE (field)) {
        gint min, max;

        min = gst_value_get_int_range_min (field);
        max = gst_value_get_int_range_max (field);

        if (min == 0 && max == G_MAXINT) {
          gst_structure_remove_field (s, field_name);
          removed_field = TRUE;
          break;
        }
      } else if (GST_VALUE_HOLDS_LIST (field)) {
        gint n2 = gst_value_list_get_size (field);

        if (n2 == 2) {
          const GValue *val1 = gst_value_list_get_value (field, 0);
          const GValue *val2 = gst_value_list_get_value (field, 1);

          if (G_VALUE_TYPE (val1) == G_TYPE_BOOLEAN
              && G_VALUE_TYPE (val2) == G_TYPE_BOOLEAN
              && ((g_value_get_boolean (val1) && !g_value_get_boolean (val2))
                  || (!g_value_get_boolean (val1)
                      && g_value_get_boolean (val2)))) {
            gst_structure_remove_field (s, field_name);
            removed_field = TRUE;
            break;
          }
        }
      } else if (GST_VALUE_HOLDS_ARRAY (field)) {
        gint n2 = gst_value_array_get_size (field);

        if (n2 == 2) {
          const GValue *val1 = gst_value_array_get_value (field, 0);
          const GValue *val2 = gst_value_array_get_value (field, 1);

          if (G_VALUE_TYPE (val1) == G_TYPE_BOOLEAN
              && G_VALUE_TYPE (val2) == G_TYPE_BOOLEAN
              && ((g_value_get_boolean (val1) && !g_value_get_boolean (val2))
                  || (!g_value_get_boolean (val1)
                      && g_value_get_boolean (val2)))) {
            gst_structure_remove_field (s, field_name);
            removed_field = TRUE;
            break;
          }
        }
      }
    }
  } while (removed_field);
}

static void
collect_codecs (GstElementFactory * factory)
{
  GstPadDirection direction;
  gboolean encoder;
  const gchar *klass;
  const GList *static_templates, *l;
  GstCaps *caps = NULL;
  gint i, n;

  klass = gst_element_factory_get_klass (factory);
  g_return_if_fail (klass);

  if (strstr (klass, "Demuxer") ||
      strstr (klass, "Decoder") ||
      strstr (klass, "Depay") || strstr (klass, "Parser")) {

    /* Ignore decoders with a less than marginal rank as they're
     * not autoplugged by playbin/decodebin */
    if (gst_plugin_feature_get_rank (GST_PLUGIN_FEATURE (factory)) <
        GST_RANK_MARGINAL)
      return;

    encoder = FALSE;
    direction = GST_PAD_SINK;
  } else if (strstr (klass, "Muxer") ||
      strstr (klass, "Encoder") || strstr (klass, "Pay")) {
    encoder = TRUE;
    direction = GST_PAD_SRC;
  } else if (strcmp (klass, "Sink/Audio") == 0) {
    provides = g_list_prepend (provides, (gchar *) virtual_packages[0]);
    return;
  } else if (strcmp (klass, "Source/Audio") == 0) {
    provides = g_list_prepend (provides, (gchar *) virtual_packages[1]);
    return;
  } else if (strcmp (klass, "Sink/Video") == 0) {
    provides = g_list_prepend (provides, (gchar *) virtual_packages[2]);
    return;
  } else if (strcmp (klass, "Source/Video") == 0) {
    provides = g_list_prepend (provides, (gchar *) virtual_packages[3]);
    return;
  } else if (strcmp (klass, "Visualization") == 0) {
    provides = g_list_prepend (provides, (gchar *) virtual_packages[4]);
    return;
  } else {
    return;
  }

  /* decoder/demuxer sink pads should always be static and there should only
   * be one, the same applies to encoders/muxers and source pads */
  static_templates = gst_element_factory_get_static_pad_templates (factory);
  for (l = static_templates; l; l = l->next) {
    GstStaticPadTemplate *tmpl = l->data;

    if (tmpl->direction == direction) {
      caps = gst_static_pad_template_get_caps (tmpl);
      break;
    }
  }

  if (caps == NULL) {
    g_printerr ("W: Couldn't find static pad template for '%s'\n",
        GST_PLUGIN_FEATURE_NAME (factory));
    return;
  }

  caps = gst_caps_make_writable (caps);
  n = gst_caps_get_size (caps);
  for (i = 0; i < n; i++) {
    GstStructure *s = gst_caps_get_structure (caps, i);

    /* make caps easier to interpret, remove common fields that are likely
     * to be irrelevant for determining the right plugin (ie. mostly fields
     * where template caps usually have the standard MIN - MAX range as value) */
    gst_structure_remove_field (s, "codec_data");
    gst_structure_remove_field (s, "palette_data");
    gst_structure_remove_field (s, "pixel-aspect-ratio");
    gst_structure_remove_field (s, "framerate");
    gst_structure_remove_field (s, "leaf_size");
    gst_structure_remove_field (s, "packet_size");
    gst_structure_remove_field (s, "block_align");
    gst_structure_remove_field (s, "metadata-interval");        /* icy caps */
    /* decoders/encoders almost always handle the usual width/height/channel/rate
     * range (and if we don't remove this then the app will have a much harder
     * time blacklisting formats it has unsuccessfully tried to install before) */
    gst_structure_remove_field (s, "width");
    gst_structure_remove_field (s, "depth");
    gst_structure_remove_field (s, "height");
    gst_structure_remove_field (s, "channels");
    gst_structure_remove_field (s, "rate");
    /* rtp fields */
    gst_structure_remove_field (s, "config");
    gst_structure_remove_field (s, "clock-rate");
    gst_structure_remove_field (s, "clock-base");
    gst_structure_remove_field (s, "maxps");
    gst_structure_remove_field (s, "seqnum-base");
    gst_structure_remove_field (s, "npt-start");
    gst_structure_remove_field (s, "npt-stop");
    gst_structure_remove_field (s, "play-speed");
    gst_structure_remove_field (s, "play-scale");
    gst_structure_remove_field (s, "dynamic_range");

    remove_min_max_fields (s);

    gst_caps_append_structure ((encoder) ? encoders : decoders,
        gst_structure_copy (s));
  }

  gst_caps_unref (caps);
}

static void
collect_plugin_info (GstPlugin * plugin)
{
  GList *features, *l;
  const gchar *plugin_name;

  plugin_name = gst_plugin_get_name (plugin);

  features = gst_registry_get_feature_list (gst_registry_get_default (),
      GST_TYPE_ELEMENT_FACTORY);

  for (l = features; l; l = l->next) {
    GstPluginFeature *feature = GST_PLUGIN_FEATURE (l->data);
    GstElementFactory *factory = GST_ELEMENT_FACTORY (feature);

    if (!g_str_equal (plugin_name, feature->plugin_name))
      continue;

    elements =
        g_list_prepend (elements,
        g_strdup (gst_plugin_feature_get_name (feature)));
    collect_uri_protocols (factory);
    collect_codecs (factory);
  }

  g_list_foreach (features, (GFunc) gst_object_unref, NULL);
  g_list_free (features);
}

int
main (int argc, char **argv)
{
  guint major, minor, micro, nano;
  gint i;

  if (!g_thread_supported ())
    g_thread_init (NULL);

  gst_init (NULL, NULL);

  gst_version (&major, &minor, &micro, &nano);

  if (argc == 1)
    return 0;

  encoders = gst_caps_new_empty ();
  decoders = gst_caps_new_empty ();

  for (i = 1; i < argc; i++) {
    GstPlugin *plugin = NULL;
    GError *error = NULL;

    if (argv[i] == NULL ||
        !g_file_test (argv[i], G_FILE_TEST_EXISTS) ||
        !g_str_has_suffix (argv[i], G_MODULE_SUFFIX)) {
      g_printerr ("W: '%s' is no valid plugin filename\n", argv[i]);
      continue;
    }

    plugin = gst_plugin_load_file (argv[i], &error);

    if (!plugin) {
      g_printerr ("W: Could not load '%s': %s\n", argv[i], error->message);
      g_error_free (error);
      continue;
    }

    collect_plugin_info (plugin);
  }

  if (elements)
    g_print ("gstreamer:Version=%u.%u\n", major, minor);

  cleanup_plugin_info ();
  print_plugin_info ();
  free_plugin_info ();

  return 0;
}
