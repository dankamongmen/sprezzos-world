/* Refocus plug-in
 * Copyright (C) 1999-2003 Ernst Lippe
 * 
 * Based on the Convolution Matrix plug-in by Lauri Alanko <la@iki.fi>
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
 *
 * Version $Id: refocus.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpcompat.h>
#include <gtk/gtk.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include "gimppreview.h"
#include "refocus.h"
#include "prevman.h"
#include "matrix.h"
#include "conv.h"
#include "bdclosure.h"
#include "util.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: refocus.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $";
#endif /* lint */

GimpDrawable *drawable;

/* Declare local functions. */
static void query (void);
static void run (char *name,
                 gint nparams,
                 GimpParam * param,
                 gint * nreturn_vals, GimpParam ** return_vals);
static gint dialog ();
static void doit (void);
static void check_config (void);

GimpPlugInInfo PLUG_IN_INFO = {
  NULL,                         /* init_proc */
  NULL,                         /* quit_proc */
  query,                        /* query_proc */
  run,                          /* run_proc */
};

gint bytes;
gint sx1, sy1, sx2, sy2;
gint run_flag = 0;
CMat *matrix = NULL;
gboolean matrix_needs_updating = TRUE;

typedef struct
{
  gint mat_width;
  gdouble radius, alpha, gamma, noise_factor;
}
config;

const config default_config = {
  5,
  1,
  0.0,
  0.5,
  0.01
};

config my_config;

struct
{
  GtkWidget *preview;
  GtkWidget *mat_width_entry;
  GtkWidget *radius_entry;
  GtkWidget *alpha_entry;
  GtkWidget *gamma_entry;
  GtkWidget *noise_entry;
  GtkWidget *ok_button;
  GtkWidget *update_preview_button;
}
my_widgets;


MAIN ()
     static void query ()
{
  static GimpParamDef args[] = {
    {GIMP_PDB_INT32, "run_mode", "Interactive, non-interactive"},
    {GIMP_PDB_IMAGE, "image", "Input image (unused)"},
    {GIMP_PDB_DRAWABLE, "drawable", "Input drawable"},
    {GIMP_PDB_INT32, "mat_size", "Size of matrix"},
    {GIMP_PDB_FLOAT, "radius", "Circle radius"},
    {GIMP_PDB_FLOAT, "gauss", "Parameter for Gaussian convolution"},
    {GIMP_PDB_FLOAT, "correlation", "Correlation"},
    {GIMP_PDB_FLOAT, "noise", "Noise to Signal ratio"},
  };
  static GimpParamDef *return_vals = NULL;
  static gint nargs = (gint) (sizeof (args) / sizeof (args[0]));
  static gint nreturn_vals = 0;

  gimp_install_procedure ("plug_in_refocus",
                          "Refocus with FIR Wiener Deconvolution",
                          "",
                          "Ernst Lippe",
                          "Ernst Lippe",
                          "1999",
                          "<Image>/Filters/Enhance/Refocus ...",
                          "RGB*, GRAY*",
                          GIMP_PLUGIN,
                          nargs, nreturn_vals, args, return_vals);
}

static void
run (char *name, gint n_params, GimpParam * param,
     gint * nreturn_vals, GimpParam ** return_vals)
{
  static GimpParam values[1];
  GimpRunModeType run_mode;
  GimpPDBStatusType status = GIMP_PDB_SUCCESS;

  (void) name;                  /* Shut up warnings about unused parameters. */
  /* Sleep so the debugger can attach to this proces */
  /* sleep(30); */
  *nreturn_vals = 1;
  *return_vals = values;

  run_mode = param[0].data.d_int32;

  /*  Get the specified drawable  */
  drawable = gimp_drawable_get (param[2].data.d_drawable);

  my_config = default_config;
  /* Although the convolution should work fine with a tiny cache
     it is made bigger to improve scrolling speed */
  gimp_tile_cache_ntiles (20);

  if (run_mode == GIMP_RUN_NONINTERACTIVE)
    {
      if (n_params != 8)
        status = GIMP_PDB_CALLING_ERROR;
      else
        {
          my_config.mat_width = param[3].data.d_int32;
          my_config.radius = param[4].data.d_float;
          my_config.alpha = param[5].data.d_float;
          my_config.gamma = param[6].data.d_float;
          my_config.noise_factor = param[7].data.d_float;
          check_config ();
        }
    }
  else
    {
      gimp_get_data ("plug_in_refocus", &my_config);

      if (run_mode == GIMP_RUN_INTERACTIVE)
        {
          /* Oh boy. We get to do a dialog box, because we can't really expect the
           * user to set us up with the right values using gdb.
           */
          check_config ();
          if (!dialog ())
            {
              /* The dialog was closed, or something similarly evil happened. */
              status = GIMP_PDB_EXECUTION_ERROR;
            }
        }
    }

  if (status == GIMP_PDB_SUCCESS)
    {

      /*  Make sure that the drawable is gray or RGB color  */
      if (gimp_drawable_is_rgb (drawable->drawable_id) ||
          gimp_drawable_is_gray (drawable->drawable_id))
        {
          doit ();

          if (run_mode != GIMP_RUN_NONINTERACTIVE)
            gimp_displays_flush ();
          if (run_mode == GIMP_RUN_INTERACTIVE)
            gimp_set_data ("plug_in_refocus", &my_config, sizeof (my_config));
        }
      else
        {
          status = GIMP_PDB_EXECUTION_ERROR;
        }
      gimp_drawable_detach (drawable);
    }

  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = status;
}

 /* Matrix computing  */
void
set_matrix_needs_updating (gboolean update_needed)
{
  matrix_needs_updating = update_needed;
  if (my_widgets.update_preview_button)
    {
      gtk_widget_set_sensitive (my_widgets.update_preview_button,
                                matrix_needs_updating);
    }
}

void
update_matrix (void)
 /* Recompute matrix if needed */
{
  CMat circle, gaussian, convolution;

  if (matrix_needs_updating)
    {
      if (matrix)
        {
          finish_c_mat (matrix);
        };
      make_gaussian_convolution (my_config.alpha, &gaussian,
                                 my_config.mat_width);
      make_circle_convolution (my_config.radius, &circle,
                               my_config.mat_width);
#ifdef RF_DEBUG
      fprintf (stderr, "mat_width=%d alpha=%f radius=%f gamma=%f noise=%f\n",
               my_config.mat_width, my_config.alpha, my_config.radius,
               my_config.gamma, my_config.noise_factor);
      fprintf (stderr, "Gauss\n");
      print_c_mat (stderr, &gaussian);
      fprintf (stderr, "Circle\n");
      print_c_mat (stderr, &circle);
#endif

      /* TODO: must use normal convolution */
      init_c_mat (&convolution, my_config.mat_width);
      convolve_star_mat (&convolution, &gaussian, &circle);
      matrix = compute_g_matrix (&convolution, my_config.mat_width,
                                 my_config.gamma,
                                 my_config.noise_factor, 0.0, TRUE);
      finish_c_mat (&convolution);
      finish_c_mat (&gaussian);
      finish_c_mat (&circle);
      set_matrix_needs_updating (FALSE);
    }
}


/***************************************************
 * GUI stuff
 */

static void
set_busy_cursor (GtkWidget * widget, gboolean busy_on)
{
  GdkCursor *cursor = (busy_on) ? gdk_cursor_new (GDK_WATCH) : NULL;

  gdk_window_set_cursor (GTK_WIDGET (widget)->window, cursor);
}

static void
redraw_all (void)
{
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.mat_width_entry),
                             my_config.mat_width);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.radius_entry),
                             my_config.radius);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.alpha_entry),
                             my_config.alpha);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.gamma_entry),
                             my_config.gamma);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.noise_entry),
                             my_config.noise_factor);
}

static void
close_callback (GtkWidget * widget, gpointer data)
{
  (void) widget;                /* Shut up warnings about unused parameters. */
  (void) data;
  gtk_main_quit ();
}

static void
ok_callback (GtkWidget * widget, gpointer data)
{
  (void) widget;                /* Shut up warnings about unused parameters. */
  run_flag = 1;
  gtk_widget_destroy (GTK_WIDGET (data));
}


/* Checks that the configuration is valid for the image type */
static void
check_config (void)
{
}


static void
defaults_callback (GtkWidget * widget, gpointer data)
{
  (void) widget;                /* Shut up warnings about unused parameters. */
  (void) data;
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.mat_width_entry),
                             default_config.mat_width);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.radius_entry),
                             default_config.radius);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.alpha_entry),
                             default_config.alpha);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.gamma_entry),
                             default_config.gamma);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (my_widgets.noise_entry),
                             default_config.noise_factor);
  check_config ();
  redraw_all ();
}

static void
update_callback (GtkWidget * widget, gpointer data)
{
  (void) widget;                /* Shut up warnings about unused parameters. */
  (void) data;
  set_busy_cursor (widget, TRUE);
  update_matrix ();
  gimp_preview_update (GIMP_PREVIEW (my_widgets.preview));
  set_busy_cursor (widget, FALSE);
}

static void
gdouble_entry_callback (GtkWidget * widget, gdouble * data)
{
  const gdouble epsilon = 1e-10;
  gdouble new_value = gtk_spin_button_get_value (GTK_SPIN_BUTTON (widget));
  if (fabs (new_value - *data) > epsilon)
    {
      *data = new_value;
      set_matrix_needs_updating (TRUE);
    }
}

static void
gint_entry_callback (GtkWidget * widget, gint * data)
{
  gint new_value =
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (widget));
  if (new_value != *data)
    {
      *data = new_value;
      set_matrix_needs_updating (TRUE);
    }
}

gboolean
preview_progress_update_fun (const gpointer data, double arg)
{
  gint event_id = GPOINTER_TO_INT (data);
  return (gimp_preview_progress_set_fraction
          (GIMP_PREVIEW (my_widgets.preview), event_id, arg));
}

static void
preview_callback (GtkWidget * widget, RefocusPreviewEvent * event, gpointer data)
{
  TileSource source;
  TileSink sink;
  gint row;
  guchar *buf;
  const gint im_width = event->image_width;
  const gint im_height = event->image_height;
  const gint image_x = event->image_x;
  const gint image_y = event->image_y;
  gboolean event_is_current = TRUE;
  BDClosure update_progress_closure;

  if (matrix)
    {
      update_matrix ();
      tile_source_init_from_drawable (&source, drawable, image_x, image_y,
                                      im_width, im_height);
      tile_sink_init_for_preview (&sink, drawable, image_x, image_y,
                                  im_width, im_height);
      gimp_preview_progress_set_fraction (GIMP_PREVIEW (my_widgets.preview),
                                          event->event_id, 0);
      bd_closure_init (&update_progress_closure,
                       preview_progress_update_fun,
                       GINT_TO_POINTER (event->event_id));

      convolve_image (&source, &sink, image_x, image_y,
                      im_width, im_height,
                      TB_BOUNDARY_MIRROR,
                      matrix, 2 * my_config.mat_width + 1,
                      &update_progress_closure);
      buf = g_new (guchar, im_width * drawable->bpp);
      for (row = 0; event_is_current && (row < im_height); row++)
        {
          tile_sink_get_row (&sink, buf, image_x, image_y + row, im_width);
          event_is_current =
            gimp_preview_draw_unscaled_row (GIMP_PREVIEW (my_widgets.preview),
                                            event->event_id,
                                            gimp_drawable_type (drawable->drawable_id),
                                            row, buf);
        };
      g_free (buf);
      tile_sink_free_buffers (&sink);
    }
}

static gint
dialog ()
{
  GtkWidget *dlg;
  GtkWidget *button;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *outbox;
  GtkWidget *vpaned;
  GtkWidget *empty_box;
  GtkWidget *table;
  GtkWidget *preview;
  gchar **argv;
  gint argc;

  argc = 1;
  argv = g_new (gchar *, 1);
  argv[0] = g_strdup ("refocus");

  gtk_init (&argc, &argv);
  gtk_rc_parse (gimp_gtkrc ());

  dlg = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dlg), "Refocus");
  gtk_window_set_position (GTK_WINDOW (dlg), GTK_WIN_POS_MOUSE);
  g_signal_connect (G_OBJECT (dlg), "destroy",
                    G_CALLBACK (close_callback), NULL);

  /*  Action area  */
  my_widgets.ok_button = button = gtk_button_new_with_label ("OK");
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  g_signal_connect (G_OBJECT (button), "clicked",
                    G_CALLBACK (ok_callback), G_OBJECT (dlg));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dlg)->action_area), button, TRUE,
                      TRUE, 0);
  gtk_widget_grab_default (button);
  gtk_widget_show (button);

  button = gtk_button_new_with_label ("Defaults");
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  g_signal_connect_object (G_OBJECT (button), "clicked",
                           G_CALLBACK (defaults_callback), G_OBJECT (dlg), 0);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dlg)->action_area), button, TRUE,
                      TRUE, 0);
  gtk_widget_show (button);

  my_widgets.update_preview_button = button =
    gtk_button_new_with_label ("Preview");
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  g_signal_connect_object (G_OBJECT (button), "clicked",
                           G_CALLBACK (update_callback), G_OBJECT (dlg), 0);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dlg)->action_area), button, TRUE,
                      TRUE, 0);
  gtk_widget_show (button);

  button = gtk_button_new_with_label ("Cancel");
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  g_signal_connect_object (G_OBJECT (button), "clicked",
                           G_CALLBACK (close_callback), G_OBJECT (dlg), 0);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dlg)->action_area), button, TRUE,
                      TRUE, 0);
  gtk_widget_show (button);


  /* Outbox */
  outbox = gtk_hpaned_new ();
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dlg)->vbox), outbox, TRUE, TRUE,
                      0);

  /* Outbox:Preview */
  vpaned = gtk_vpaned_new ();
  preview = my_widgets.preview = gimp_preview_new (drawable);
  g_signal_connect (G_OBJECT (preview), "update_preview",
                    G_CALLBACK (preview_callback), (gpointer) NULL);
  gtk_paned_pack1 (GTK_PANED (outbox), vpaned, TRUE, TRUE);
  gtk_paned_pack1 (GTK_PANED (vpaned), preview, TRUE, TRUE);
  empty_box = gtk_hbox_new (TRUE, 0);
  gtk_paned_pack2 (GTK_PANED (vpaned), empty_box, TRUE, TRUE);
  gtk_widget_show (empty_box);
  gtk_widget_show (vpaned);
  gtk_widget_show (preview);

  /* Outbox:Table */
  table = gtk_table_new (5, 2, FALSE);
  gtk_paned_pack2 (GTK_PANED (outbox), table, TRUE, TRUE);


  /* mat_width */

  label = gtk_label_new ("Matrix Size");
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 0, 1);
  gtk_widget_show (label);

  my_widgets.mat_width_entry = entry =
    gtk_spin_button_new_with_range (0.0, 25.0, 1.0);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (entry), 0);
  gtk_table_attach_defaults (GTK_TABLE (table), entry, 1, 2, 0, 1);
  g_signal_connect (G_OBJECT (entry), "changed",
                    G_CALLBACK (gint_entry_callback), &my_config.mat_width);
  gtk_widget_show (entry);

  /* Radius */

  label = gtk_label_new ("Radius");
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 1, 2);
  gtk_widget_show (label);

  my_widgets.radius_entry = entry =
    gtk_spin_button_new_with_range (0.0, 25.0, 0.1);
  gtk_table_attach_defaults (GTK_TABLE (table), entry, 1, 2, 1, 2);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (entry), 2);
  g_signal_connect (G_OBJECT (entry), "changed",
                    G_CALLBACK (gdouble_entry_callback), &my_config.radius);
  gtk_widget_show (entry);

  /* Alpha */

  label = gtk_label_new ("Gauss");
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 2, 3);
  gtk_widget_show (label);

  my_widgets.alpha_entry = entry =
    gtk_spin_button_new_with_range (0.0, 25.0, 0.1);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (entry), 2);
  gtk_table_attach_defaults (GTK_TABLE (table), entry, 1, 2, 2, 3);
  g_signal_connect (G_OBJECT (entry), "changed",
                    G_CALLBACK (gdouble_entry_callback), &my_config.alpha);
  gtk_widget_show (entry);

  /* gamma */

  label = gtk_label_new ("Correlation");
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 3, 4);
  gtk_widget_show (label);

  my_widgets.gamma_entry = entry =
    gtk_spin_button_new_with_range (0.0, 1.0, 0.05);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (entry), 3);
  gtk_table_attach_defaults (GTK_TABLE (table), entry, 1, 2, 3, 4);
  g_signal_connect (G_OBJECT (entry), "changed",
                    G_CALLBACK (gdouble_entry_callback), &my_config.gamma);
  gtk_widget_show (entry);

  /* noise ratio */

  label = gtk_label_new ("Noise");
  gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 4, 5);
  gtk_widget_show (label);

  my_widgets.noise_entry = entry =
    gtk_spin_button_new_with_range (0.0, 1.0, 0.01);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (entry), 5);

  gtk_table_attach_defaults (GTK_TABLE (table), entry, 1, 2, 4, 5);
  g_signal_connect (G_OBJECT (entry), "changed",
                    G_CALLBACK (gdouble_entry_callback),
                    &my_config.noise_factor);
  gtk_widget_show (entry);

  gtk_widget_show (table);
  gtk_widget_show (outbox);

  gtk_widget_show (dlg);
  redraw_all ();
  gimp_preview_update (GIMP_PREVIEW (preview));
  gtk_main ();
  gdk_flush ();
  return run_flag;
}

static gboolean
gimp_progress_update_fun (gpointer data, gdouble fraction)
{
  return (gimp_progress_update (fraction));
}


void
doit (void)
{
  TileSource source;
  TileSink sink;
  gint width, height;
  BDClosure update_progress_closure;

  bd_closure_init (&update_progress_closure, gimp_progress_update_fun, NULL);
  gimp_progress_init ("Computing matrix");
  update_matrix ();
  gimp_progress_init ("Applying convolution");
  gimp_drawable_mask_bounds (drawable->drawable_id, &sx1, &sy1, &sx2, &sy2);
  width = sx2 - sx1;
  height = sy2 - sy1;
  tile_source_init_from_drawable (&source, drawable, sx1, sy1, width, height);
  tile_sink_init_from_drawable (&sink, drawable, sx1, sy1, width, height);
  convolve_image (&source, &sink, sx1, sy1, width, height,
                  TB_BOUNDARY_MIRROR,
                  matrix, 2 * my_config.mat_width + 1,
                  &update_progress_closure);
  gimp_drawable_flush (drawable);
  gimp_drawable_merge_shadow (drawable->drawable_id, TRUE);
  gimp_drawable_update (drawable->drawable_id, sx1, sy1, width, height);
  g_free (matrix);
}
