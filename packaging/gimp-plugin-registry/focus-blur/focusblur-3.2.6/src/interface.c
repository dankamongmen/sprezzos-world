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

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#include "libgimp/stdplugins-intl.h"

#include "focusblur.h"
#include "focusblurparam.h"
#include "focusblurstock.h"
#include "gimpplugin.h"
#include "interface.h"
#include "render.h"


/*---- Prototypes ----*/

static GtkWidget* focusblur_notebook_append_vbox  (GtkNotebook  *notebook,
                                                   gchar        *label_text);
static GtkWidget* focusblur_notebook_append_table (GtkNotebook  *notebook,
                                                   gchar        *label_text,
                                                   gint          rows);

static void     focusblur_preview_invalidate    (GimpPreview    *gimppreview,
                                                 FblurParam     *param);
static void     focusblur_widget_pickup_focal_depth (FblurParam *param,
                                                     gint        x,
                                                     gint        y);
static void     focusblur_preview_button_press  (GtkWidget      *widget,
                                                 GdkEvent       *event,
                                                 FblurParam     *param);
static gboolean focusblur_map_constraint        (gint32          image_ID,
                                                 gint32          drawable_ID,
                                                 gpointer        data);
static void     focusblur_change_brush_name     (GObject        *object,
                                                 GParamSpec     *arg1,
                                                 gpointer        user_data);
static void     focusblur_update_enumcombobox_text (GimpEnumComboBox *combo,
                                                    gint              index,
                                                    const gchar      *text);
static void     focusblur_update_widget_brushmodel (GObject     *object,
                                                    GParamSpec  *arg1,
                                                    gpointer     user_data);


/*---- Funcs ----*/

gboolean
focusblur_dialog (FblurParam *param)
{
  GtkWidget     *dialog;
  GtkWidget     *main_vbox;
  GtkWidget     *preview;
  GtkWidget     *notebook;
  GtkWidget     *nb_vbox;
  GtkWidget     *vbox;
  GtkWidget     *hbox;
  GtkWidget     *label;
  GtkWidget     *button;
  GtkWidget     *spinbutton;
  GtkObject     *adj;
  GtkWidget     *combo_box;
  GtkWidget     *toggle;
  GtkWidget     *table;
  GtkListStore  *enum_store;
  gchar         *text;
  gint           row;
  gboolean       run;

  gimp_ui_init (PLUG_IN_BINARY, FALSE);
  focusblur_stock_init ();

  /* Main dialogue */

  dialog = gimp_dialog_new (_("Focus Blur"), PLUG_IN_BINARY,
                            NULL, 0,
                            gimp_standard_help_func, PLUG_IN_PROC,

                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            GTK_STOCK_OK,     GTK_RESPONSE_OK,

                            NULL);

  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);

  gimp_window_set_transient (GTK_WINDOW (dialog));
  gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);

  main_vbox = gtk_vbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), main_vbox);
  gtk_widget_show (main_vbox);

  /* Preview */

  preview = gimp_drawable_preview_new (param->drawable, NULL);
  gtk_box_pack_start (GTK_BOX (main_vbox), preview, FALSE, TRUE, 0);
  gtk_widget_show (preview);

  g_signal_connect (G_OBJECT (preview), "invalidated",
                    G_CALLBACK (focusblur_preview_invalidate),
                    param);
  g_signal_connect (G_OBJECT (gimp_preview_get_area (GIMP_PREVIEW (preview))),
                    "button_press_event",
                    G_CALLBACK (focusblur_preview_button_press),
                    param);


  /* Notebook to select options */

  notebook = gtk_notebook_new ();
  gtk_box_pack_start (GTK_BOX (main_vbox), notebook, TRUE, TRUE, 0);
  gtk_widget_show (notebook);
  /* bind to "Ctrl + Next" and "Ctrl + Prev" */
  g_signal_connect_swapped (G_OBJECT (dialog), "key-press-event",
                            G_CALLBACK (GTK_WIDGET_CLASS (G_OBJECT_GET_CLASS (notebook))->key_press_event),
                            notebook);


  /* Basic parameters */

  nb_vbox = focusblur_notebook_append_vbox (GTK_NOTEBOOK (notebook),
                                            _("_Basic"));

  /* Diffusion Radius */

  vbox = gtk_vbox_new (FALSE, 6);
  gtk_box_pack_start (GTK_BOX (nb_vbox), vbox, FALSE, FALSE, 0);
  gtk_widget_show (vbox);

  hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  gtk_widget_show (hbox);

  label = gtk_label_new_with_mnemonic (_("Diffusion Model and _Radius:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), label);
  gtk_widget_show (label);

  spinbutton = gimp_spin_button_new (&adj, param->store.model_radius,
                                     0.0, 100.0, 1.0, 10.0, 0.0, 1.0, 2);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), spinbutton);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.model_radius));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_box_pack_start (GTK_BOX (hbox), spinbutton, FALSE, FALSE, 0);
  gtk_widget_show (spinbutton);

  /* Diffusion Model */

  combo_box = gimp_enum_combo_box_new (FBLUR_TYPE_MODEL_TYPE);
  gimp_enum_combo_box_set_stock_prefix
    (GIMP_ENUM_COMBO_BOX (combo_box), "focusblur-model");
  /* change label */
  text = g_strdup_printf (_("Brush:%s"), param->brush_name);
  focusblur_update_enumcombobox_text (GIMP_ENUM_COMBO_BOX (combo_box),
                                      FBLUR_MODEL_BRUSH, text);
  g_free (text);

  gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo_box),
                              param->store.model_type,
                              G_CALLBACK (gimp_int_combo_box_get_active),
                              &(param->store.model_type));
  g_signal_connect_swapped (G_OBJECT (combo_box), "changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_box_pack_start (GTK_BOX (vbox), combo_box, FALSE, FALSE, 0);
  gtk_widget_show (combo_box);
  param->widgets[FBLUR_WIDGET_MODEL_TYPE] = combo_box;

  /* Shining radius */

  table = gtk_table_new (1, 2, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 6);
  gtk_box_pack_start (GTK_BOX (nb_vbox), table, FALSE, FALSE, 0);
  gtk_widget_show (table);

  spinbutton = gimp_spin_button_new (&adj, param->store.shine_radius,
                                     0.0, 100.0, 1.0, 10.0, 0.0, 1.0, 2);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.shine_radius));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_table_attach (GTK_TABLE (table), spinbutton, 1, 2, 2, 3,
                    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (spinbutton);

  label = gtk_label_new_with_mnemonic (_("Pea_k radius for Shining:"));
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), spinbutton);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 2, 3);
  gtk_widget_show (label);

  /* Depth Map */

  vbox = gtk_vbox_new (FALSE, 6);
  gtk_box_pack_start (GTK_BOX (nb_vbox), vbox, FALSE, FALSE, 0);
  gtk_widget_show (vbox);

  toggle = gtk_check_button_new_with_mnemonic (_("_Use Depth map:"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
                                param->store.enable_depth_map);
  g_signal_connect (G_OBJECT (toggle), "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &(param->store.enable_depth_map));
  g_signal_connect_swapped (G_OBJECT (toggle), "toggled",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_box_pack_start (GTK_BOX (vbox), toggle, FALSE, FALSE, 0);
  gtk_widget_show (toggle);

  combo_box = gimp_drawable_combo_box_new (focusblur_map_constraint, NULL);
  gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo_box),
                              param->store.depth_map_ID,
                              G_CALLBACK (gimp_int_combo_box_get_active),
                              &(param->store.depth_map_ID));
  g_signal_connect_swapped (G_OBJECT (combo_box), "changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_widget_set_sensitive (combo_box, param->store.enable_depth_map);
  g_object_set_data (G_OBJECT (toggle), "set_sensitive", combo_box);
  gtk_box_pack_start (GTK_BOX (vbox), combo_box, FALSE, FALSE, 0);

  gtk_widget_show (combo_box);

  /* Focal Depth */

  table = gtk_table_new (1, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 6);
  gtk_table_set_row_spacings (GTK_TABLE (table), 6);
  gtk_widget_set_sensitive (table, param->store.enable_depth_map);
  g_object_set_data (G_OBJECT (combo_box), "set_sensitive", table);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);
  gtk_widget_show (table);

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
                              _("Foc_al depth:"), 100, 0,
                              param->store.focal_depth,
                              0.0, 100.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.focal_depth));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  param->widgets[FBLUR_WIDGET_FOCAL_DEPTH] = GIMP_SCALE_ENTRY_SPINBUTTON (adj);


  /* Model parameters */

  table = focusblur_notebook_append_table (GTK_NOTEBOOK (notebook),
                                           _("_Model"), 5);
  row = 0;

  /* Brush select button */

  button = gimp_brush_select_button_new (NULL, param->brush_name, -1, -1, -1);
  gtk_table_attach (GTK_TABLE (table), button, 0, 3, row, row + 1,
                    GTK_FILL, GTK_FILL, 0, 0);
  g_signal_connect (G_OBJECT (button), "notify::brush-name",
                    G_CALLBACK (focusblur_change_brush_name),
                    &(param->brush_name));
  g_signal_connect (G_OBJECT (button), "notify::brush-name",
                    G_CALLBACK (focusblur_update_widget_brushmodel),
                    param->widgets[FBLUR_WIDGET_MODEL_TYPE]);
  g_signal_connect_swapped (G_OBJECT (button), "notify::brush-name",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_widget_show (button);
  row ++;

  /* Brush balance */

  toggle = gtk_check_button_new_with_mnemonic (_("Make a ba_lance of brush"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
                                param->store.enable_brush_balance);
  g_signal_connect (G_OBJECT (toggle), "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &(param->store.enable_brush_balance));
  g_signal_connect_swapped (G_OBJECT (toggle), "toggled",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_table_attach_defaults (GTK_TABLE (table), toggle, 0, 3, row, row + 1);
  row ++;
  gtk_widget_show (toggle);

  /* Model rotation */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("_Rotation:"), 100, 0,
                              param->store.model_rotate,
                              0.0, 360.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.model_rotate));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);

  /* Model filling */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("F_illing inside:"), 100, 0,
                              param->store.model_fill,
                              0.0, 100.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.model_fill));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);

  /* Model softness */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("Sof_tness:"), 100, 0,
                              param->store.model_softness,
                              0.0, 100.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.model_softness));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);


  /* Shine parameters */

  table = focusblur_notebook_append_table (GTK_NOTEBOOK (notebook),
                                           _("_Shine"), 4);
  row = 0;

  /* Shine type */

  combo_box = gimp_enum_combo_box_new (FBLUR_TYPE_SHINE_TYPE);
  gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo_box),
                              param->store.shine_type,
                              G_CALLBACK (gimp_int_combo_box_get_active),
                              &(param->store.shine_type));
  g_signal_connect_swapped (G_OBJECT (combo_box), "changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  label = gimp_table_attach_aligned (GTK_TABLE (table), 0, row ++,
                                     _("Sensing t_ype:"), 0.0, 0.5,
                                     combo_box, 2, FALSE);
  gtk_widget_show (combo_box);

  /* Shine Threshold */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("T_hreshold:"), 100, 0,
                              param->store.shine_threshold,
                              0.0, 100.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.shine_threshold));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);

  /* Shine level */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("_Level:"), 100, 0,
                              param->store.shine_level,
                              0.0, 100.0, 1.0, 10.0, 2, TRUE, 0, 0,
                              NULL, NULL);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.shine_level));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);

  /* Shine curve */

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, row ++,
                              _("C_urve:"), 100, 0,
                              param->store.shine_curve,
                              0.0, 10.0, 0.1, 1.0, 3, TRUE, 0, 0,
                              NULL, NULL);
  gimp_scale_entry_set_logarithmic (adj, TRUE);
  g_signal_connect (G_OBJECT (adj), "value-changed",
                    G_CALLBACK (gimp_float_adjustment_update),
                    &(param->store.shine_curve));
  g_signal_connect_swapped (G_OBJECT (adj), "value-changed",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);


  /* Depth parameters */

  table = focusblur_notebook_append_table (GTK_NOTEBOOK (notebook),
                                           _("_Depth"), 2);
  row = 0;

  /* Depth Precedence */

  toggle = gtk_check_button_new_with_mnemonic (_("Depth with p_recedence"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
                                param->store.enable_depth_precedence);
  g_signal_connect (G_OBJECT (toggle), "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &(param->store.enable_depth_precedence));
  g_signal_connect_swapped (G_OBJECT (toggle), "toggled",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_table_attach_defaults (GTK_TABLE (table), toggle, 0, 3, row, row + 1);
  row ++;
  gtk_widget_show (toggle);

  /* Anti-anti-alias */

  toggle = gtk_check_button_new_with_mnemonic (_("Evade _anti-alias"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
                                param->store.enable_depth_aaa);
  g_signal_connect (G_OBJECT (toggle), "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &(param->store.enable_depth_aaa));
  g_signal_connect_swapped (G_OBJECT (toggle), "toggled",
                            G_CALLBACK (gimp_preview_invalidate),
                            preview);
  gtk_table_attach_defaults (GTK_TABLE (table), toggle, 0, 3, row, row + 1);
  row ++;
  gtk_widget_show (toggle);


#if defined HAVE_FFTW3 || defined ENABLE_MP

  /* Recordable parameters */

  table = focusblur_notebook_append_table (GTK_NOTEBOOK (notebook),
                                           _("Pre_ferences"), 3);
  row = 0;

#  ifdef HAVE_FFTW3

  /* Rendering quality */

  enum_store = gimp_enum_store_new_with_range
    (FBLUR_TYPE_QUALITY_TYPE, FBLUR_QUALITY_BEST, FBLUR_QUALITY_LOW);
  combo_box =
    gimp_enum_combo_box_new_with_model (GIMP_ENUM_STORE (enum_store));
  g_object_unref (enum_store);
  gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo_box),
                              param->pref.quality,
                              G_CALLBACK (gimp_int_combo_box_get_active),
                              &(param->pref.quality));
  label = gimp_table_attach_aligned (GTK_TABLE (table), 0, row ++,
                                     _("Rendering _quality:"), 0.0, 0.5,
                                     combo_box, 2, FALSE);
  gtk_widget_show (combo_box);

  /* Preview quality */

  enum_store = gimp_enum_store_new_with_range
    (FBLUR_TYPE_QUALITY_TYPE, FBLUR_QUALITY_NORMAL, FBLUR_QUALITY_DEFECTIVE);
  combo_box =
    gimp_enum_combo_box_new_with_model (GIMP_ENUM_STORE (enum_store));
  g_object_unref (enum_store);
  gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo_box),
                              param->pref.quality_preview,
                              G_CALLBACK (gimp_int_combo_box_get_active),
                              &(param->pref.quality_preview));
  label = gimp_table_attach_aligned (GTK_TABLE (table), 0, row ++,
                                     _("Pre_view quality:"), 0.0, 0.5,
                                     combo_box, 2, FALSE);
  gtk_widget_show (combo_box);

#  endif /* HAVE_FFTW3 */

#  ifdef ENABLE_MP

  /* Disable multi-threads */

  if (row)
    gtk_table_set_row_spacing (GTK_TABLE (table), row - 1, 12);

  toggle = gtk_check_button_new_with_mnemonic (_("Dis_able support for multi-threads"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle),
                                param->pref.disable_mp);
  g_signal_connect (toggle, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &(param->pref.disable_mp));
  gtk_table_attach_defaults (GTK_TABLE (table), toggle, 0, 3, row, row + 1);
  row ++;
  gtk_widget_show (toggle);

#  endif /* ENABLE_MP */

#endif /* defined HAVE_FFTW3 || defined ENABLE_MP */


  /* get parameters */

  gtk_widget_show (dialog);
  run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);
  gtk_widget_destroy (dialog);

  return run;
}


static GtkWidget*
focusblur_notebook_append_vbox (GtkNotebook *notebook,
                                gchar       *label_text)
{
  GtkWidget *vbox;

  vbox = gtk_vbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 12);
  gtk_notebook_append_page (notebook, vbox,
                            gtk_label_new_with_mnemonic (label_text));
  gtk_widget_show (vbox);

  return vbox;
}


static GtkWidget*
focusblur_notebook_append_table (GtkNotebook *notebook,
                                 gchar       *label_text,
                                 gint         rows)
{
  GtkWidget *vbox;
  GtkWidget *table;

  vbox = focusblur_notebook_append_vbox (notebook, label_text);

  table = gtk_table_new (rows, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 6);
  gtk_table_set_row_spacings (GTK_TABLE (table), 6);

  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);
  gtk_widget_show (table);

  return table;
}


static void
focusblur_preview_invalidate (GimpPreview *gimppreview,
                              FblurParam  *param)
{
  focusblur_execute (param, gimppreview);
}


static void
focusblur_widget_pickup_focal_depth (FblurParam *param,
                                     gint        x,
                                     gint        y)
{
  GtkSpinButton         *spin;
  gfloat                 focal_depth;
  gint                   depth;
  guchar                *pixel;
  gint                   num_channels;

  g_return_if_fail (param->store.enable_depth_map);
  g_return_if_fail (x >= 0 && y >= 0);

  if (! gimp_drawable_is_valid (param->store.depth_map_ID))
    {
      gimp_message (_("Specified depth map is invalid."));
      return;
    }

  x %= gimp_drawable_width (param->store.depth_map_ID);
  y %= gimp_drawable_height (param->store.depth_map_ID);

  pixel = gimp_drawable_get_pixel
    (param->store.depth_map_ID, x, y, &num_channels);

  switch (gimp_drawable_type (param->store.depth_map_ID))
    {
    case GIMP_GRAYA_IMAGE:
      g_assert (num_channels == 2);
      if (! pixel[1])
        goto exit;

    case GIMP_GRAY_IMAGE:
      g_assert (num_channels >= 1);
      depth = FBLUR_DEPTH_MAX * pixel[0] / 255;
      break;

    case GIMP_RGBA_IMAGE:
      g_assert (num_channels == 4);
      if (! pixel[4])
        goto exit;

    case GIMP_RGB_IMAGE:
      g_assert (num_channels >= 3);
      depth = FBLUR_DEPTH_MAX * (pixel[0] + pixel[1] + pixel[2]) / (3 * 255);
      break;

    default:
      g_assert_not_reached ();
    }

  focal_depth = (100.0f / (gfloat) FBLUR_DEPTH_MAX) * depth;
  spin = GTK_SPIN_BUTTON (param->widgets[FBLUR_WIDGET_FOCAL_DEPTH]);
  gtk_spin_button_set_value (spin, focal_depth);

 exit:
  g_free (pixel);
}


static void
focusblur_preview_button_press (GtkWidget       *widget,
                                GdkEvent        *event,
                                FblurParam      *param)
{
  if (event->button.button == 2)
    {
      GimpPreviewArea *area;
      gint x, y;

      if (! param->store.enable_depth_map ||
          ! gimp_drawable_is_valid (param->store.depth_map_ID))
        return;

      area = GIMP_PREVIEW_AREA (widget);
      x = area->offset_x + event->button.x;
      y = area->offset_y + event->button.y;

      /* checks for padding */
      if (area->width < widget->allocation.width)
        {
          x -= (widget->allocation.width - area->width) / 2;
          if (x < 0 || x >= area->max_width)
            return;
        }
      if (area->height < widget->allocation.height)
        {
          y -= (widget->allocation.height - area->height) / 2;
          if (y < 0 || y >= area->max_height)
            return;
        }

      focusblur_widget_pickup_focal_depth (param, x, y);
      return;
    }
}


static gboolean
focusblur_map_constraint (gint32         image_ID,
                          gint32         drawable_ID,
                          gpointer       data)
{
  if (drawable_ID == -1)
    return FALSE;

  return (gimp_drawable_is_rgb (drawable_ID) ||
          gimp_drawable_is_gray (drawable_ID));
}


static void
focusblur_change_brush_name (GObject    *object,
                             GParamSpec *arg1,
                             gpointer   user_data)
{
  gchar **fblur_brush_name = (gchar **) user_data;
  gchar *gimp_brush_name = NULL;

  g_return_if_fail (GIMP_IS_BRUSH_SELECT_BUTTON (object));
  g_return_if_fail (fblur_brush_name != NULL);

  g_object_get (object, "brush-name", &gimp_brush_name, NULL);

  if (gimp_brush_name)
    {
      g_free (*fblur_brush_name);
      *fblur_brush_name = g_strdup (gimp_brush_name);
    }
}

static void
focusblur_update_enumcombobox_text (GimpEnumComboBox *combo_box,
                                    gint              index,
                                    const gchar      *text)
{
  GimpEnumStore         *model;
  GtkTreeIter            iter;
  gboolean               ret;

  g_object_get (GIMP_ENUM_COMBO_BOX (combo_box), "model", &model, NULL);

  ret = gimp_int_store_lookup_by_value (GTK_TREE_MODEL (model), index, &iter);

  if (ret)
    gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                        GIMP_INT_STORE_LABEL, text,
                        -1);
}


static void
focusblur_update_widget_brushmodel (GObject    *object,
                                    GParamSpec *arg1,
                                    gpointer    user_data)
{
  gchar *gimp_brush_name = NULL;
  gchar *text;

  g_return_if_fail (GIMP_IS_BRUSH_SELECT_BUTTON (object));
  g_return_if_fail (GIMP_IS_ENUM_COMBO_BOX (user_data));

  g_object_get (object, "brush-name", &gimp_brush_name, NULL);

  focusblur_stock_update_modelbrush (gimp_brush_name);

  text = g_strdup_printf (_("Brush:%s"), gimp_brush_name);
  focusblur_update_enumcombobox_text (GIMP_ENUM_COMBO_BOX (user_data),
                                      FBLUR_MODEL_BRUSH, text);
  g_free (text);
}
