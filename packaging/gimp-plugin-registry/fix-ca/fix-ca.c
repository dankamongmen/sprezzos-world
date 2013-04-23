/*
	fix-ca.c	Fix Chromatic Aberration Gimp Plug-In
	Copyright (c) 2006, 2007 Kriang Lerdsuwanakij
	email:		lerdsuwa@users.sourceforge.net

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#define _ISOC99_SOURCE
#include <string.h>
#include <math.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

/*#define DEBUG_TIME*/
#ifdef DEBUG_TIME
# include <sys/time.h>
# include <stdio.h>
#endif

/* No i18n for now */
#define _(x)	x
#define N_(x)	x

#define PROCEDURE_NAME	"Fix-CA"
#define DATA_KEY_VALS	"fix_ca"

/* Size controls in Fix CA dialog box */
#define SCALE_WIDTH	150
#define ENTRY_WIDTH	4

/* For row buffer management */
#define	SOURCE_ROWS	20
#define ROW_INVALID	-100
#define ITER_INITIAL	-100

/* Storage type */
typedef struct {
	gdouble  blue;
	gdouble  red;
	gboolean update_preview;
	GimpInterpolationType	interpolation;
	gdouble	 saturation;
	gdouble  x_blue;
	gdouble  x_red;
	gdouble  y_blue;
	gdouble  y_red;
} FixCaParams;

/* Global default */
static FixCaParams fix_ca_params_default = {
	0.0,
	0.0,
	TRUE,
	GIMP_INTERPOLATION_LINEAR,
	0.0,
	0.0,
	0.0,
	0.0,
	0.0
};

/* Local function prototypes */
static void	query (void);
static void	run (const gchar *name, gint nparams,
		     const GimpParam  *param, gint *nreturn_vals,
		     GimpParam **return_vals);
static void	fix_ca (GimpDrawable *drawable, FixCaParams *params);
static void	fix_ca_region (GimpDrawable *drawable, 
			       GimpPixelRgn *srcPTR, GimpPixelRgn *dstPTR,
			       gint bytes, FixCaParams *params,
			       gint x1, gint x2, gint y1, gint y2,
			       gboolean show_progress);
static gboolean	fix_ca_dialog (GimpDrawable *drawable, FixCaParams *params);
static void	preview_update (GimpPreview *preview, FixCaParams *params);
static inline int	round_nearest (gdouble d);
static inline int	absolute (gint i);
static inline int	clip (gdouble d);
static inline int	bilinear (gint xy, gint x1y, gint xy1, gint x1y1, gdouble dx, gdouble dy);
static inline double	cubic (gint xm1, gint j, gint xp1, gint xp2, gdouble dx);
static inline int	scale (gint i, gint size, gdouble scale_val, gdouble shift_val);
static inline double	scale_d (gint i, gint size, gdouble scale_val, gdouble shift_val);
static guchar *load_data (GimpPixelRgn *srcPTR,
			  guchar *src[SOURCE_ROWS], gint src_row[SOURCE_ROWS],
			  gint src_iter[SOURCE_ROWS], gint band_adj,
			  gint band_1, gint band_2, gint y, gint iter);
static void	fix_ca_help (const gchar *help_id, gpointer help_data);

GimpPlugInInfo PLUG_IN_INFO = {
	NULL,	/* init_proc  */
	NULL,	/* quit_proc  */
	query,	/* query_proc */
	run,	/* run_proc   */
};

MAIN ()

void	query (void)
{
	static GimpParamDef args[] = {
		{ GIMP_PDB_INT32, "run_mode", "Interactive, non-interactive" },
		{ GIMP_PDB_IMAGE, "image", "Input image" },
		{ GIMP_PDB_DRAWABLE, "drawable", "Input drawable" },
		{ GIMP_PDB_FLOAT, "blue", "Blue amount (lateral)" },
		{ GIMP_PDB_FLOAT, "red", "Red amount (lateral)" },
		{ GIMP_PDB_INT8, "interpolation", "Interpolation 0=None/1=Linear/2=Cubic" },
		{ GIMP_PDB_FLOAT, "x_blue", "Blue amount (x axis)" },
		{ GIMP_PDB_FLOAT, "x_red", "Red amount (x axis)" },
		{ GIMP_PDB_FLOAT, "y_blue", "Blue amount (y axis)" },
		{ GIMP_PDB_FLOAT, "y_red", "Red amount (y axis)" }
	};

	gimp_install_procedure (PROCEDURE_NAME,
				"Fix-CA Version 3.0.2",
				"Fix chromatic aberration caused by imperfect "
				"lens.  It works by shifting red and blue "
				"components of image pixels in the specified "
				"amounts.",
				"Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>",
				"Kriang Lerdsuwanakij",
				"2006, 2007",
				N_("Chromatic Aberration..."),
				"RGB*",
				GIMP_PLUGIN,
				G_N_ELEMENTS (args), 0,
				args, 0);

#if 0
				/* Need to decide about menu location */
	if (GIMP_CHECK_VERSION(2, 4, 0))
		gimp_plugin_menu_register (PROCEDURE_NAME, "<Image>/Colors");
	else
#endif
		gimp_plugin_menu_register (PROCEDURE_NAME, "<Image>/Filters/Colors");
}

void	run (const gchar *name, gint nparams,
	     const GimpParam *param, gint *nreturn_vals,
	     GimpParam **return_vals)
{
	static GimpParam values[1];
	GimpDrawable	*drawable;
	gint32		image_ID;
	GimpRunMode	run_mode;
	GimpPDBStatusType status = GIMP_PDB_SUCCESS;
	FixCaParams fix_ca_params;

	*nreturn_vals = 1;
	*return_vals  = values;
                      
	run_mode = param[0].data.d_int32;
	image_ID = param[1].data.d_int32;
	drawable = gimp_drawable_get (param[2].data.d_drawable);
	gimp_tile_cache_ntiles (2 * MAX (drawable->width  / gimp_tile_width () + 1 ,
					 drawable->height / gimp_tile_height () + 1));
                                     

	fix_ca_params.blue = fix_ca_params_default.blue;
	fix_ca_params.red = fix_ca_params_default.red;
	fix_ca_params.update_preview = fix_ca_params_default.update_preview;
	fix_ca_params.interpolation = fix_ca_params_default.interpolation;
	fix_ca_params.x_blue = fix_ca_params_default.x_blue;
	fix_ca_params.x_red = fix_ca_params_default.x_red;
	fix_ca_params.y_blue = fix_ca_params_default.y_blue;
	fix_ca_params.y_red = fix_ca_params_default.y_red;

	if (strcmp (name, PROCEDURE_NAME) == 0) {
		switch (run_mode) {
			case GIMP_RUN_NONINTERACTIVE:
				if (nparams < 5 || nparams > 10)
					status = GIMP_PDB_CALLING_ERROR;
				else {
					fix_ca_params.blue = param[3].data.d_float;
					fix_ca_params.red = param[4].data.d_float;
					if (nparams < 6)
						fix_ca_params.interpolation
							= GIMP_INTERPOLATION_NONE;
					else if (param[5].data.d_int8 > 2)
						status = GIMP_PDB_CALLING_ERROR;
					else
						fix_ca_params.interpolation
							= param[5].data.d_int8;

					if (nparams < 7)
						fix_ca_params.x_blue = 0;
					else
						fix_ca_params.x_blue = param[6].data.d_float;
					if (nparams < 8)
						fix_ca_params.x_red = 0;
					else
						fix_ca_params.x_red = param[7].data.d_float;
					if (nparams < 9)
						fix_ca_params.y_blue = 0;
					else
						fix_ca_params.y_blue = param[8].data.d_float;
					if (nparams < 10)
						fix_ca_params.y_red = 0;
					else
						fix_ca_params.y_red = param[9].data.d_float;
				}
				break;

			case GIMP_RUN_INTERACTIVE:
				gimp_get_data (DATA_KEY_VALS, &fix_ca_params);

				if (! fix_ca_dialog (drawable, &fix_ca_params))
					status = GIMP_PDB_CANCEL;
				break;

			case GIMP_RUN_WITH_LAST_VALS:
				gimp_get_data (DATA_KEY_VALS, &fix_ca_params);
				break;

			default:
				break;
		}
	}
	else
		status = GIMP_PDB_CALLING_ERROR;

	if (status == GIMP_PDB_SUCCESS) {
		fix_ca (drawable, &fix_ca_params);

		gimp_displays_flush ();

		if (run_mode == GIMP_RUN_INTERACTIVE)
			gimp_set_data (DATA_KEY_VALS, &fix_ca_params, sizeof (fix_ca_params));

		gimp_drawable_detach (drawable);
	}

	values[0].type = GIMP_PDB_STATUS;
	values[0].data.d_status = status;
}

void	fix_ca (GimpDrawable *drawable, FixCaParams *params)
{
	GimpPixelRgn srcPR, destPR;
	gint         x1, y1, x2, y2;

			/* Initialize pixel regions */
	gimp_pixel_rgn_init (&srcPR, drawable,
			     0, 0, drawable->width, drawable->height, FALSE, FALSE);
	gimp_pixel_rgn_init (&destPR, drawable,
			     0, 0, drawable->width, drawable->height, TRUE, TRUE);

			/* Get the input */
	gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);

	fix_ca_region (drawable, &srcPR, &destPR, drawable->bpp, params,
		       x1, x2, y1, y2, TRUE);

	gimp_drawable_flush (drawable);
	gimp_drawable_merge_shadow (drawable->drawable_id, TRUE);
	gimp_drawable_update (drawable->drawable_id, x1, y1, x2 - x1, y2 - y1);
}

gboolean	fix_ca_dialog (GimpDrawable *drawable, FixCaParams *params)
{
	GtkWidget *dialog;
	GtkWidget *main_vbox;
	GtkWidget *combo;
	GtkWidget *preview;
	GtkWidget *table;
	GtkWidget *frame;
	GtkObject *adj;
	gboolean   run;

	gimp_ui_init ("fix_ca", TRUE);

	dialog = gimp_dialog_new (_("Chromatic Aberration"), "fix_ca",
				  NULL, 0,
				  fix_ca_help, "plug-in-fix-ca",

				  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				  GTK_STOCK_OK,     GTK_RESPONSE_OK,

				  NULL);

	main_vbox = gtk_vbox_new (FALSE, 12);
	gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), main_vbox);
	gtk_widget_show (main_vbox);

	preview = gimp_drawable_preview_new (drawable,
					     &(params->update_preview));
	gtk_box_pack_start (GTK_BOX (main_vbox), preview, TRUE, TRUE, 0);
	gtk_widget_show (preview);

	g_signal_connect (preview, "invalidated",
			  G_CALLBACK (preview_update),
			  params);

	table = gtk_table_new (2, 2, FALSE);
	gtk_table_set_col_spacings (GTK_TABLE (table), 6);
	gtk_table_set_row_spacings (GTK_TABLE (table), 6);
	gtk_box_pack_start (GTK_BOX (main_vbox), table, FALSE, FALSE, 0);
  	gtk_widget_show (table);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
				    _("Preview _saturation:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->saturation, -100.0, 100.0, 1.0, 10.0, 0,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->saturation));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	combo = gimp_int_combo_box_new (_("None (Fastest)"),	GIMP_INTERPOLATION_NONE,
					_("Linear"),		GIMP_INTERPOLATION_LINEAR,
					_("Cubic (Best)"),	GIMP_INTERPOLATION_CUBIC,
					NULL);

	gimp_int_combo_box_connect (GIMP_INT_COMBO_BOX (combo),
				    params->interpolation,
				    G_CALLBACK (gimp_int_combo_box_get_active),
				    &params->interpolation);
	gimp_table_attach_aligned (GTK_TABLE (table), 0, 1,
				   _("_Interpolation:"), 0.0, 0.5,
				   combo, 2, FALSE);
	g_signal_connect_swapped (combo, "changed",
				  G_CALLBACK (gimp_preview_invalidate),
				  preview);


	frame = gimp_frame_new ("Lateral");
	gtk_box_pack_start (GTK_BOX (main_vbox), frame, FALSE, FALSE, 0);
	gtk_widget_show (frame);

	table = gtk_table_new (2, 2, FALSE);
	gtk_table_set_col_spacings (GTK_TABLE (table), 6);
	gtk_table_set_row_spacings (GTK_TABLE (table), 6);
	gtk_container_add (GTK_CONTAINER (frame), table);
  	gtk_widget_show (table);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
				    _("_Blue:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->blue, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->blue));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 1,
				    _("_Red:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->red, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->red));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	frame = gimp_frame_new ("X axis");
	gtk_box_pack_start (GTK_BOX (main_vbox), frame, FALSE, FALSE, 0);
	gtk_widget_show (frame);

	table = gtk_table_new (2, 2, FALSE);
	gtk_table_set_col_spacings (GTK_TABLE (table), 6);
	gtk_table_set_row_spacings (GTK_TABLE (table), 6);
	gtk_container_add (GTK_CONTAINER (frame), table);
  	gtk_widget_show (table);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
				    _("Blue:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->x_blue, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->x_blue));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 1,
				    _("Red:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->x_red, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->x_red));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	frame = gimp_frame_new ("Y axis");
	gtk_box_pack_start (GTK_BOX (main_vbox), frame, FALSE, FALSE, 0);
	gtk_widget_show (frame);

	table = gtk_table_new (2, 2, FALSE);
	gtk_table_set_col_spacings (GTK_TABLE (table), 6);
	gtk_table_set_row_spacings (GTK_TABLE (table), 6);
	gtk_container_add (GTK_CONTAINER (frame), table);
  	gtk_widget_show (table);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
				    _("Blue:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->y_blue, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->y_blue));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 1,
				    _("Red:"), SCALE_WIDTH, ENTRY_WIDTH,
				    params->y_red, -10.0, 10.0, 0.1, 0.5, 1,
				    TRUE, 0, 0,
				    NULL, NULL);

	g_signal_connect (adj, "value_changed",
			  G_CALLBACK (gimp_double_adjustment_update),
			  &(params->y_red));
	g_signal_connect_swapped (adj, "value_changed",
			  G_CALLBACK (gimp_preview_invalidate),
			  preview);

	gtk_widget_show (dialog);

	run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);

	gtk_widget_destroy (dialog);

	return run;
}

void	preview_update (GimpPreview *preview, FixCaParams *params)
{
	GimpDrawable *drawable;
	gint	x1, x2, y1, y2, width, height;
	gint	x, y;
	GimpPixelRgn srcPR, destPR;

	drawable = gimp_drawable_preview_get_drawable (GIMP_DRAWABLE_PREVIEW (preview));

	gimp_pixel_rgn_init (&srcPR, drawable,
			     0, 0, drawable->width, drawable->height, FALSE, FALSE);
	gimp_pixel_rgn_init (&destPR, drawable,
			     0, 0, drawable->width, drawable->height, TRUE, TRUE);

	gimp_preview_get_position (preview, &x, &y);
	gimp_preview_get_size (preview, &width, &height);

	x1 = x;
	y1 = y;
	x2 = x + width;
	y2 = y + height;

	fix_ca_region (drawable, &srcPR, &destPR, drawable->bpp,
		       params,
		       x1, x2, y1, y2,
		       FALSE);

	gimp_drawable_preview_draw_region (GIMP_DRAWABLE_PREVIEW (preview), &destPR);
}

/* Round to nearest integer. Only works correctly when d >= 0. 
   For d < 0, the expression should be return (int)(d - 0.5); */
int	round_nearest (gdouble d)
{
	return (int)(d + 0.5);
}

int	absolute (gint i)
{
	if (i >= 0)
		return i;
	else
		return -i;
}

int	scale (gint i, gint size, gdouble scale_val, gdouble shift_val)
{
	gdouble d = (i - size/2) * scale_val + size/2 - shift_val;
	gint j = round_nearest (d);
	if (j <= 0)
		return 0;
	else if (j >= size)
		return size-1;
	else
		return j;
}

double	scale_d (gint i, gint size, gdouble scale_val, gdouble shift_val)
{
	gdouble d = (i - size/2) * scale_val + size/2 - shift_val;
	if (d <= 0.0)
		return 0.0;
	else if (d >= size-1)
		return size-1;
	else
		return d;
}

guchar *load_data (GimpPixelRgn *srcPTR,
		   guchar *src[SOURCE_ROWS], gint src_row[SOURCE_ROWS],
		   gint src_iter[SOURCE_ROWS], gint band_adj,
		   gint band_1, gint band_2, gint y, gint iter)
{
	gint	i, diff, diff_max = -1, row_best = -1;
	int	iter_oldest;

	for (i = 0; i < SOURCE_ROWS; ++i) {
		if (src_row[i] == y) {
			src_iter[i] = iter;	/* Make sure to keep this row
						   during this iteration */
			return src[i];
		}
	}

		/* Find a row to replace */
	iter_oldest = INT_MAX;		/* Largest possible */
	for (i = 0; i < SOURCE_ROWS; ++i) {
		if (src_iter[i] < iter_oldest) {
			iter_oldest = src_iter[i];
			diff_max = absolute (y - src_row[i]);
			row_best = i;
		}
		else if (src_iter[i] == iter_oldest) {
			diff = absolute (y - src_row[i]);
			if (diff > diff_max) {
				diff_max = diff;
				row_best = i;
			}
		}
	}

	gimp_pixel_rgn_get_row (srcPTR, src[row_best]+band_adj, band_1, y, band_2-band_1+1);
	src_row[row_best] = y;
	src_iter[row_best] = iter;
	return src[row_best];
}

int	clip (gdouble d)
{
	gint	i = round_nearest (d);
	if (i <= 0)
		return 0;
	else if (i >= 255)
		return 255;
	else
		return i;
}

int	bilinear (gint xy, gint x1y, gint xy1, gint x1y1, gdouble dx, gdouble dy)
{
	double d = (1-dy) * (xy + dx * (x1y-xy))
		   + dy * (xy1 + dx * (x1y1-xy1));
	return clip (d);
}

double	cubic (gint xm1, gint x, gint xp1, gint xp2, gdouble dx)
{
	/* Catmull-Rom from Gimp gimpdrawable-transform.c */
	return ((( ( - xm1 + 3 * x - 3 * xp1 + xp2 ) * dx +
                 ( 2 * xm1 - 5 * x + 4 * xp1 - xp2 ) ) * dx +
                                ( - xm1 + xp1 ) ) * dx + (x + x) ) / 2.0;
}

void	fix_ca_region (GimpDrawable *drawable, 
		       GimpPixelRgn *srcPTR, GimpPixelRgn *dstPTR,
		       gint bytes, FixCaParams *params,
		       gint x1, gint x2, gint y1, gint y2,
		       gboolean show_progress)
{
	guchar  *src[SOURCE_ROWS];
	gint	src_row[SOURCE_ROWS];
	gint	src_iter[SOURCE_ROWS];
	gint	i;

	guchar	*dest;
	gint	x, y, b;
	gint	orig_width, orig_height, max_dim;
	gdouble	scale_blue, scale_red, scale_max;

	gdouble x_shift_max, x_shift_min;

	gint	band_1, band_2, band_adj;

#ifdef DEBUG_TIME
	double	sec;
	struct timeval tv1, tv2;
	gettimeofday (&tv1, NULL);
#endif

	if (show_progress)
		gimp_progress_init (_("Shifting pixel components..."));

	orig_width = srcPTR->w;
	orig_height = srcPTR->h;

			/* Allocate buffers for reading, writing */
	for (i = 0; i < SOURCE_ROWS; ++i) {
		src[i] = g_new (guchar, orig_width * bytes);
		src_row[i] = ROW_INVALID;	/* Invalid row */
		src_iter[i] = ITER_INITIAL;	/* Oldest iteration */
	}
	dest = g_new (guchar, (x2-x1) * bytes);

	if (orig_width > orig_height)
		max_dim = orig_width;
	else
		max_dim = orig_height;
			/* Scale to get source */
	scale_blue = max_dim / (max_dim + 2 * params->blue);
	scale_red = max_dim / (max_dim + 2 * params->red);

			/* Optimize by loading only parts of a row */
	if (scale_blue > scale_red)
		scale_max = scale_blue;
	else
		scale_max = scale_red;

	if (params->x_blue > params->x_red) {
		x_shift_min = params->x_red;
		x_shift_max = params->x_blue;
	}
	else {
		x_shift_min = params->x_blue;
		x_shift_max = params->x_red;
	}

					/* Horizontal band to load for each row */
	band_1 = scale (x1, orig_width, scale_max, x_shift_max);
	band_2 = scale (x2-1, orig_width, scale_max, x_shift_min);
	if (band_1 > x1)		/* Make sure green is also covered */
		band_1 = x1;
	if (band_2 < x2-1)
		band_2 = x2-1;

					/* Extra pixels needed for interpolation */
	if (params->interpolation != GIMP_INTERPOLATION_NONE) {
		if (band_1 > 0)
			--band_1;
		if (band_2 < orig_width-1)
			++band_2;
	}
					/* More pixels needed for cubic interpolation */
	if (params->interpolation == GIMP_INTERPOLATION_CUBIC) {
		if (band_1 > 0)
			--band_1;
		if (band_2 < orig_width-1)
			++band_2;
	}

	band_adj = band_1 * bytes;

	for (y = y1; y < y2; ++y) {
			/* Get current row, for green channel */
		guchar *ptr;
		ptr = load_data (srcPTR, src, src_row, src_iter,
				 band_adj, band_1, band_2, y, y);

		if (params->interpolation == GIMP_INTERPOLATION_NONE) {
			guchar	*ptr_blue, *ptr_red;
			gint	y_blue, y_red, x_blue, x_red;

				/* Get blue and red row */
			y_blue = scale (y, orig_height, scale_blue, params->y_blue);
			y_red = scale (y, orig_height, scale_red, params->y_red);
			ptr_blue = load_data (srcPTR, src, src_row, src_iter,
					      band_adj, band_1, band_2, y_blue, y);
			ptr_red = load_data (srcPTR, src, src_row, src_iter,
					     band_adj, band_1, band_2, y_red, y);

			for (x = x1; x < x2; ++x) {
					/* Green channel */
				dest[(x-x1)*bytes + 1] = ptr[x*bytes + 1];

					/* Blue and red channel */
				x_blue = scale (x, orig_width, scale_blue, params->x_blue);
				x_red = scale (x, orig_width, scale_red, params->x_red);

				dest[(x-x1)*bytes] = ptr_red[x_red*bytes];
				dest[(x-x1)*bytes + 2] = ptr_blue[x_blue*bytes + 2];

					/* Other channels if present */
				for (b = 3; b < bytes; ++b) {
					dest[(x-x1)*bytes + b] = ptr[x*bytes + b];
				}
			}
		}
		else if (params->interpolation == GIMP_INTERPOLATION_LINEAR) {
				/* Pointer to pixel data rows y, y+1 */
			guchar	*ptr_blue_1, *ptr_blue_2, *ptr_red_1, *ptr_red_2;
				/* Floating point row, fractional row */
			gdouble	y_blue_d, y_red_d, d_y_blue, d_y_red;
				/* Integer row y */
			gint	y_blue_1, y_red_1;
				/* Floating point column, fractional column */
			gdouble	x_blue_d, x_red_d, d_x_blue, d_x_red;
				/* Integer column x, x+1 */
			gint	x_blue_1, x_red_1, x_blue_2, x_red_2;

				/* Get blue and red row */
			y_blue_d = scale_d (y, orig_height, scale_blue, params->y_blue);
			y_red_d = scale_d (y, orig_height, scale_red, params->y_red);

				/* Integer and fractional row */
			y_blue_1 = floor (y_blue_d);
			y_red_1 = floor (y_red_d);
			d_y_blue = y_blue_d - y_blue_1;
			d_y_red = y_red_d - y_red_1;

				/* Load pixel data */
			ptr_blue_1 = load_data (srcPTR, src, src_row, src_iter,
						band_adj, band_1, band_2, y_blue_1, y);
			ptr_red_1 = load_data (srcPTR, src, src_row, src_iter,
					       band_adj, band_1, band_2, y_red_1, y);
			if (y_blue_1 == orig_height-1)
				ptr_blue_2 = ptr_blue_1;
			else
				ptr_blue_2 = load_data (srcPTR, src, src_row, src_iter,
							band_adj, band_1, band_2, y_blue_1+1, y);
			if (y_red_1 == orig_height-1)
				ptr_red_2 = ptr_red_1;
			else
				ptr_red_2 = load_data (srcPTR, src, src_row, src_iter,
						       band_adj, band_1, band_2, y_red_1+1, y);

			for (x = x1; x < x2; ++x) {
					/* Green channel */
				dest[(x-x1)*bytes + 1] = ptr[x*bytes + 1];

					/* Blue and red channel */
				x_blue_d = scale_d (x, orig_width, scale_blue, params->x_blue);
				x_red_d = scale_d (x, orig_width, scale_red, params->x_red);

					/* Integer and fractional column */
				x_blue_1 = floor (x_blue_d);
				x_red_1 = floor (x_red_d);
				d_x_blue = x_blue_d - x_blue_1;
				d_x_red = x_red_d - x_red_1;
				if (x_blue_1 == orig_width-1)
					x_blue_2 = x_blue_1;
				else
					x_blue_2 = x_blue_1 + 1;
				if (x_red_1 == orig_width-1)
					x_red_2 = x_red_1;
				else
					x_red_2 = x_red_1 + 1;

					/* Interpolation */
				dest[(x-x1)*bytes] = bilinear (ptr_red_1[x_red_1*bytes],
							       ptr_red_1[x_red_2*bytes],
							       ptr_red_2[x_red_1*bytes],
							       ptr_red_2[x_red_2*bytes],
							       d_x_red, d_y_red);
				dest[(x-x1)*bytes + 2] = bilinear (ptr_blue_1[x_blue_1*bytes+2],
								   ptr_blue_1[x_blue_2*bytes+2],
								   ptr_blue_2[x_blue_1*bytes+2],
								   ptr_blue_2[x_blue_2*bytes+2],
								   d_x_blue, d_y_blue);

					/* Other channels if present */
				for (b = 3; b < bytes; ++b) {
					dest[(x-x1)*bytes + b] = ptr[x*bytes + b];
				}
			}
		}
		else if (params->interpolation == GIMP_INTERPOLATION_CUBIC) {
				/* Pointer to pixel data rows y-1, y */
			guchar	*ptr_blue_1, *ptr_blue_2, *ptr_red_1, *ptr_red_2;
				/* Pointer to pixel data rows y+1, y+2 */
			guchar	*ptr_blue_3, *ptr_blue_4, *ptr_red_3, *ptr_red_4;
				/* Floating point row, fractional row */
			gdouble	y_blue_d, y_red_d, d_y_blue, d_y_red;
				/* Integer row y */
			gint	y_blue_2, y_red_2;
				/* Floating point column, fractional column */
			gdouble	x_blue_d, x_red_d, d_x_blue, d_x_red;
				/* Integer column x-1, x */
			gint	x_blue_1, x_red_1, x_blue_2, x_red_2;
				/* Integer column x+1, x+2 */
			gint	x_blue_3, x_red_3, x_blue_4, x_red_4;

				/* Get blue and red row */
			y_blue_d = scale_d (y, orig_height, scale_blue, params->y_blue);
			y_red_d = scale_d (y, orig_height, scale_red, params->y_red);

			y_blue_2 = floor (y_blue_d);
			y_red_2 = floor (y_red_d);
			d_y_blue = y_blue_d - y_blue_2;
			d_y_red = y_red_d - y_red_2;

				/* Row */
			ptr_blue_2 = load_data (srcPTR, src, src_row, src_iter,
						band_adj, band_1, band_2, y_blue_2, y);
			ptr_red_2 = load_data (srcPTR, src, src_row, src_iter,
					       band_adj, band_1, band_2, y_red_2, y);

				/* Row - 1 */
			if (y_blue_2 == 0)
				ptr_blue_1 = ptr_blue_2;
			else
				ptr_blue_1 = load_data (srcPTR, src, src_row, src_iter,
							band_adj, band_1, band_2, y_blue_2-1, y);
			if (y_red_2 == 0)
				ptr_red_1 = ptr_red_2;
			else
				ptr_red_1 = load_data (srcPTR, src, src_row, src_iter,
						       band_adj, band_1, band_2, y_red_2-1, y);

				/* Row + 1 */
			if (y_blue_2 == orig_height-1)
				ptr_blue_3 = ptr_blue_2;
			else
				ptr_blue_3 = load_data (srcPTR, src, src_row, src_iter,
							band_adj, band_1, band_2, y_blue_2+1, y);
			if (y_red_2 == orig_height-1)
				ptr_red_3 = ptr_red_2;
			else
				ptr_red_3 = load_data (srcPTR, src, src_row, src_iter,
						       band_adj, band_1, band_2, y_red_2+1, y);

				/* Row + 2 */
			if (y_blue_2 == orig_height-1)
				ptr_blue_4 = ptr_blue_2;
			else if (y_blue_2 == orig_height-2)
				ptr_blue_4 = ptr_blue_3;
			else
				ptr_blue_4 = load_data (srcPTR, src, src_row, src_iter,
							band_adj, band_1, band_2, y_blue_2+2, y);
			if (y_red_2 == orig_height-1)
				ptr_red_4 = ptr_red_2;
			else if (y_red_2 == orig_height-2)
				ptr_red_4 = ptr_red_3;
			else
				ptr_red_4 = load_data (srcPTR, src, src_row, src_iter,
						       band_adj, band_1, band_2, y_red_2+2, y);

			for (x = x1; x < x2; ++x) {
				double y1, y2, y3, y4;

					/* Green channel */
				dest[(x-x1)*bytes + 1] = ptr[x*bytes + 1];

					/* Blue and red channel */
				x_blue_d = scale_d (x, orig_width, scale_blue, params->x_blue);
				x_red_d = scale_d (x, orig_width, scale_red, params->x_red);

				x_blue_2 = floor (x_blue_d);
				x_red_2 = floor (x_red_d);
				d_x_blue = x_blue_d - x_blue_2;
				d_x_red = x_red_d - x_red_2;

					/* Column - 1 */
				if (x_blue_2 == 0)
					x_blue_1 = x_blue_2;
				else
					x_blue_1 = x_blue_2 - 1;
				if (x_red_2 == 0)
					x_red_1 = x_red_2;
				else
					x_red_1 = x_red_2 - 1;

					/* Column + 1 */
				if (x_blue_2 == orig_width-1)
					x_blue_3 = x_blue_2;
				else
					x_blue_3 = x_blue_2 + 1;
				if (x_red_2 == orig_width-1)
					x_red_3 = x_red_2;
				else
					x_red_3 = x_red_2 + 1;

					/* Column + 2 */
				if (x_blue_3 == orig_width-1)
					x_blue_4 = x_blue_3;
				else
					x_blue_4 = x_blue_3 + 1;
				if (x_red_3 == orig_width-1)
					x_red_4 = x_red_3;
				else
					x_red_4 = x_red_3 + 1;

				y1 = cubic (ptr_red_1[x_red_1*bytes],
					    ptr_red_1[x_red_2*bytes],
					    ptr_red_1[x_red_3*bytes],
					    ptr_red_1[x_red_4*bytes],
					    d_x_red);
				y2 = cubic (ptr_red_2[x_red_1*bytes],
					    ptr_red_2[x_red_2*bytes],
					    ptr_red_2[x_red_3*bytes],
					    ptr_red_2[x_red_4*bytes],
					    d_x_red);
				y3 = cubic (ptr_red_3[x_red_1*bytes],
					    ptr_red_3[x_red_2*bytes],
					    ptr_red_3[x_red_3*bytes],
					    ptr_red_3[x_red_4*bytes],
					    d_x_red);
				y4 = cubic (ptr_red_3[x_red_1*bytes],
					    ptr_red_3[x_red_2*bytes],
					    ptr_red_3[x_red_3*bytes],
					    ptr_red_3[x_red_4*bytes],
					    d_x_red);

				dest[(x-x1)*bytes] = clip (cubic (y1, y2, y3, y4, d_y_red));

				y1 = cubic (ptr_blue_1[x_blue_1*bytes+2],
					    ptr_blue_1[x_blue_2*bytes+2],
					    ptr_blue_1[x_blue_3*bytes+2],
					    ptr_blue_1[x_blue_4*bytes+2],
					    d_x_blue);
				y2 = cubic (ptr_blue_2[x_blue_1*bytes+2],
					    ptr_blue_2[x_blue_2*bytes+2],
					    ptr_blue_2[x_blue_3*bytes+2],
					    ptr_blue_2[x_blue_4*bytes+2],
					    d_x_blue);
				y3 = cubic (ptr_blue_3[x_blue_1*bytes+2],
					    ptr_blue_3[x_blue_2*bytes+2],
					    ptr_blue_3[x_blue_3*bytes+2],
					    ptr_blue_3[x_blue_4*bytes+2],
					    d_x_blue);
				y4 = cubic (ptr_blue_3[x_blue_1*bytes+2],
					    ptr_blue_3[x_blue_2*bytes+2],
					    ptr_blue_3[x_blue_3*bytes+2],
					    ptr_blue_3[x_blue_4*bytes+2],
					    d_x_blue);

				dest[(x-x1)*bytes + 2] = clip (cubic (y1, y2, y3, y4, d_y_blue));

					/* Other channels if present */
				for (b = 3; b < bytes; ++b) {
					dest[(x-x1)*bytes + b] = ptr[x*bytes + b];
				}
			}
		}

		if (!show_progress && params->saturation != 0.0) {
			gdouble	s_scale = 1+params->saturation/100;
			for (x = x1; x < x2; ++x) {
				int r = dest[(x-x1)*bytes];
				int g = dest[(x-x1)*bytes + 1];
				int b = dest[(x-x1)*bytes + 2];
				gimp_rgb_to_hsv_int (&r, &g, &b);
				g *= s_scale;
				if (g > 255)
					g = 255;
				dest[(x-x1)*bytes + 1] = g;
				gimp_hsv_to_rgb_int (&r, &g, &b);
				dest[(x-x1)*bytes] = r;
				dest[(x-x1)*bytes + 1] = g;
				dest[(x-x1)*bytes + 2] = b;
			}
		}

		gimp_pixel_rgn_set_row (dstPTR, dest, x1, y, x2-x1);

		if (show_progress && ((y-y1) % 8 == 0))
			gimp_progress_update ((gdouble) (y-y1) / (y2-y1));
	}

	if (show_progress)
		gimp_progress_update (0.0);

	for (i = 0; i < SOURCE_ROWS; ++i)
		g_free(src[i]);
	g_free (dest);

#ifdef DEBUG_TIME
	gettimeofday (&tv2, NULL);

	sec = tv2.tv_sec - tv1.tv_sec + (tv2.tv_usec - tv1.tv_usec)/1000000.0;
	printf ("Elapsed time: %.2f\n", sec);
#endif
}

void	fix_ca_help (const gchar *help_id, gpointer help_data)
{
	gimp_message ("Select the amount in pixels to shift for blue "
		      "and red components of image.  "
		      "Lateral chromatic aberration means there is no "
		      "aberration at the image center but it increases gradually "
		      "toward the edge of image.  "
		      "X axis and Y axis aberrations mean the amount of aberration "
		      "is the same throughout the image.\n\n"
		      "For lateral aberration, the number of pixel is the amount shifted "
		      "at the extreme edge of the image (width or height whatever is the larger), "
		      "and positive number means moving in outward "
		      "direction.\n\n"
		      "For X axis and Y axis, the number of pixel is the actual shift, "
		      "and positive number means moving rightward or upward.");
}
