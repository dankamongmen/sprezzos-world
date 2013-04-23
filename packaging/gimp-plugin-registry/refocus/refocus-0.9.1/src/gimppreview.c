/* GIMP Plugin Preview Widget                                                 
 * Copyright (C) 1998-1999 Shawn T. Amundson                
 * Modifications (C) 1999-2003 Ernst Lippe
 * 
 * Some of the checkerboard & image combination code is based of
 * code Copyright 1997 (C) Federico Mena Quintero and is used
 * here by permission.
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
 *
 * Version $Id: gimppreview.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#ifndef lint
static char vcid[] = "$Id: gimppreview.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $";
#endif /* lint */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include <glib-object.h>
#include "gimppreview.h"


static void gimp_preview_init (RefocusPreview * preview);
static void gimp_preview_class_init (RefocusPreviewClass * klass);

static void gimp_preview_plus_callback (GtkWidget * widget, gpointer data);
static void gimp_preview_minus_callback (GtkWidget * widget, gpointer data);
static gint gimp_preview_event (GtkWidget * widget, GdkEvent * event,
                                gpointer data);
static void gimp_preview_recompute_sizes (RefocusPreview * preview,
                                          gdouble newscale);
static void gimp_preview_update_preview (RefocusPreview * preview);

static void gimp_preview_image_set_size (RefocusPreview * preview, gint width,
                                         gint height);
static void gimp_preview_size_request (GtkWidget * widget,
                                       GtkRequisition * requisition);
static void gimp_preview_size_allocate (GtkWidget * widget,
                                        GtkAllocation * allocation);
static void gimp_preview_forall (GtkContainer * container,
                                 gboolean include_internals,
                                 GtkCallback callback,
                                 gpointer callback_data);
gboolean gimp_preview_update_preview_idle_fun (gpointer data);
void gimp_preview_schedule_update (RefocusPreview * preview);

#define PROGRESS_BAR_HEIGHT (10)
#define PREVIEW_SIZE (100)

enum
{
  UPDATE_PREVIEW,
  PREVIEW_CHANGED,
  LAST_SIGNAL
};

#define PREVIEW_SCALE_DEFAULT 5
#define PREVIEW_SCALE_LAST 20
static const gdouble preview_scale[PREVIEW_SCALE_LAST + 1] = {
  1 / 6.0, 1 / 5.0, 1 / 4.0,
  1 / 3.0, 1 / 2.0, 1.00,
  2.00, 3.00, 4.00,
  5.00, 6.00, 7.00,
  8.00, 9.00, 10.00,
  11.00, 12.00, 13.00,
  14.00, 15.00, 16.00
};


/* Constants for transparency checkerboard */

/* CHECK_SIZE must be power of 2 */
#define CHECK_SIZE (16)
#define CHECK_DARK  ((guchar) (1.3 / 3.0 * 255))
#define CHECK_LIGHT ((guchar) (1.8 / 3.0 * 255))

static guint gimp_preview_signals[LAST_SIGNAL] = { 0 };


/*
 * Apps which use a RefocusPreview widget should not be accessing the private
 * data!
 */
#define PREVIEW_DATA(preview) \
        ((RefocusPreviewData*)(GIMP_PREVIEW (preview)->private_data))

typedef struct
{
  gint scale_n;

  GtkWidget *label;
  GtkWidget *button_minus;
  GtkWidget *button_plus;
  GtkWidget *resize_box;
  GtkWidget *event_box;
  gboolean allow_scale;

  gboolean in_drag;             /* Is true while the preview is dragged     */

  gint drag_x;                  /* Mouse x-position when dragging started   */
  gint drag_y;                  /* Mouse y-position when dragging started   */

  gint orig_preview_x;          /* Value of preview_x when dragging started */
  gint orig_preview_y;          /* Value of preview_y when dragging started */

  gint current_event_id;        /* Is incremented when preview must be redrawn */
  gint last_processed_event_id; /* Most recent value of current_event_id for which a redraw was actually executed */
  guchar *preview_buffer_na[2]; /* Buffers for storing one row for preview */

  GtkWidget *image;
  GtkWidget *progress_bar;
}
RefocusPreviewData;


/* Convert coordinate in preview space to image coordinates */
#define p2i(preview_coordinate, scale) ((gint)((preview_coordinate) / (scale)))

gint
i2p (gint image_coordinate, gdouble scale)
/* Return maximum preview coordinate p such that
   p2i(p, scale) <= image_coordinate < p2i(p + 1,scale) */
{
  gint p = (gint) ((image_coordinate + 1) * scale) + 1;
  g_assert (p2i (p, scale) > image_coordinate);
  while (p2i (p, scale) > image_coordinate)
    {
      p--;
    };
  return (p);
}


GType
gimp_preview_get_type (void)
{
  static GType preview_type = 0;

  if (!preview_type)
    {
      GTypeInfo preview_info = {
        sizeof (RefocusPreviewClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) gimp_preview_class_init,
        (GClassFinalizeFunc) NULL,
        (gconstpointer) NULL,   /* class_data */
        sizeof (RefocusPreview),
        0,                      /* n_preallocs */
        (GInstanceInitFunc) gimp_preview_init,
        (GTypeValueTable *) NULL /* value_table */
      };

      preview_type =
        g_type_register_static (GTK_TYPE_CONTAINER, "RefocusPreview",
                                &preview_info, 0);
    }

  return preview_type;
}


/*
 * Initialization, which is done when the preview widget is first created
 * by GTK's internal mechanisms.
 */
static void
gimp_preview_class_init (RefocusPreviewClass * klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  GtkContainerClass *container_class = GTK_CONTAINER_CLASS (klass);

  gimp_preview_signals[UPDATE_PREVIEW] =
    g_signal_new ("update_preview",
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (RefocusPreviewClass, update_preview),
                  NULL,
                  NULL,
                  g_cclosure_marshal_VOID__POINTER,
                  G_TYPE_NONE, 1, G_TYPE_POINTER);

  gimp_preview_signals[PREVIEW_CHANGED] =
    g_signal_new ("preview_changed",
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (RefocusPreviewClass, preview_changed),
                  NULL, NULL, g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);

  klass->update_preview = NULL;
  klass->preview_changed = NULL;
  widget_class->size_request = gimp_preview_size_request;
  widget_class->size_allocate = gimp_preview_size_allocate;
  container_class->forall = gimp_preview_forall;
}


void
gimp_preview_set_scale_amount(RefocusPreview *preview, gdouble scale_amount)
{
  /*
   * If the caller wants to set the scale amount, let them do so.
   * Note that we only choose a scaling that is close to what the user wants, 
   * but not more than that.
   */
  gint j;

  if (scale_amount != 0.0)
    {
      /* Initialize it to the lowest setting, just in case. */
      PREVIEW_DATA (preview)->scale_n = 0;
      preview->scale = preview_scale[PREVIEW_DATA (preview)->scale_n];

      /*
       * If the request is larger than our largest value, just use the largest 
       * value
       */
      if ((gdouble) (scale_amount / 100.0) >
          preview_scale[PREVIEW_SCALE_LAST + 1])
        {
          PREVIEW_DATA (preview)->scale_n = PREVIEW_SCALE_LAST;
          preview->scale = preview_scale[PREVIEW_DATA (preview)->scale_n];
        }
      else
        {
          /* 
           * Check to see if the request fits somewhere inbetween the smallest and
           * largest requests.
           */
          for (j = 1; j < PREVIEW_SCALE_LAST + 1; j++)
            {
              if (((gdouble) (scale_amount / 100.0)) <
                  preview_scale[j])
                {
                  PREVIEW_DATA (preview)->scale_n = j - 1;
                  preview->scale =
                    preview_scale[PREVIEW_DATA (preview)->scale_n];
                  break;
                }
            }
        }
    }
  else
    {
      PREVIEW_DATA (preview)->scale_n = PREVIEW_SCALE_DEFAULT;
      preview->scale = preview_scale[PREVIEW_DATA (preview)->scale_n];
    }
}


/*
 * Initialization, which is done when the preview widget is first created
 * by GTK's internal mechanisms.
 */
static void
gimp_preview_init (RefocusPreview * preview)
{
  gchar buffer[10];

  g_assert (GIMP_IS_PREVIEW (preview));
  GTK_WIDGET_SET_FLAGS (preview, GTK_NO_WINDOW);
  GTK_CONTAINER (preview)->resize_mode = GTK_RESIZE_IMMEDIATE;

  preview->private_data = g_malloc0 (sizeof (RefocusPreviewData));


  PREVIEW_DATA (preview)->label = gtk_label_new ("");
  sprintf (buffer, "%d%%", (gint) (preview->scale * 100));
  gtk_label_set_text (GTK_LABEL (PREVIEW_DATA (preview)->label), buffer);

  PREVIEW_DATA (preview)->in_drag = FALSE;
  PREVIEW_DATA (preview)->drag_x = 0;
  PREVIEW_DATA (preview)->drag_y = 0;
  PREVIEW_DATA (preview)->orig_preview_x = 0;
  PREVIEW_DATA (preview)->orig_preview_y = 0;
  PREVIEW_DATA (preview)->current_event_id = 0;
  PREVIEW_DATA (preview)->last_processed_event_id = 0;

  PREVIEW_DATA (preview)->image = NULL;
  PREVIEW_DATA (preview)->progress_bar = NULL;
}

GtkWidget *
gimp_preview_new (GimpDrawable * drawable)
{
  /* Simplified front end that uses all the defaults. */
  return (gimp_preview_new_with_args (drawable, -1, 0.0, 1));
}

GtkWidget *
gimp_preview_new_with_args (GimpDrawable * drawable, gint cb_preview_size,
                            gdouble cb_scale_amount, gint cb_allow_scale)
{
  RefocusPreview *preview;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *event_box;
  gint preview_size, preview_width, preview_height;

  /* If the caller wants to set the preview window size, let them do so. */
  switch (cb_preview_size)
    {
      /*
       * The user wants a fixed size preview.  We'll calculate this
       * during the init routine.
       */
    case PREVIEW_FIXED_SIZE:
      preview_size = PREVIEW_FIXED_SIZE;
      break;

      /* Caller wants use to use the default preview window size. */
    case PREVIEW_DEFAULT_SIZE:
      preview_size = PREVIEW_SIZE;
      break;

      /* Caller wants to specify their own preview window size. */
    default:
      preview_size = cb_preview_size;
      break;
    }


  /* Now allocate the actual preview window. */
  preview = GIMP_PREVIEW (g_object_new (gimp_preview_get_type (), NULL));

  /* Set the scale amount. */
  gimp_preview_set_scale_amount(preview, cb_scale_amount);

  /* Save the drawable info. */
  preview->drawable = drawable;
  preview->drawable_has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);

  /* Calculate our preview size. */
  if (preview_size == PREVIEW_FIXED_SIZE)
    {
      preview_width = i2p (drawable->width, preview->scale);
      preview_height = i2p (drawable->height, preview->scale);

      GIMP_PREVIEW (preview)->width = preview_width;
      GIMP_PREVIEW (preview)->height = preview_height;
    }
  else
    {
      /* TODO: Cleanup this mess */
      preview->max_width = preview_width = preview_size;
      preview->max_height = preview_height = preview_size;
    }

  /* Now build our dialog. */

  event_box = gtk_event_box_new ();
  PREVIEW_DATA (preview)->image = gtk_image_new ();
  gtk_misc_set_padding (GTK_MISC (PREVIEW_DATA (preview)->image), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (PREVIEW_DATA (preview)->image), 0.5, 0.5);
  gtk_container_add (GTK_CONTAINER (event_box),
                     PREVIEW_DATA (preview)->image);
  gimp_preview_image_set_size (preview, preview_width, preview_height);
  frame = gtk_frame_new (NULL);
  PREVIEW_DATA (preview)->event_box = event_box;
  gtk_widget_set_parent (event_box, GTK_WIDGET (preview));

  if (preview_size != PREVIEW_FIXED_SIZE)
    {
      gtk_widget_set_events (event_box,
                             GDK_BUTTON_PRESS_MASK |
                             GDK_BUTTON_RELEASE_MASK |
                             GDK_POINTER_MOTION_HINT_MASK |
                             GDK_BUTTON_MOTION_MASK);
      g_signal_connect (G_OBJECT (event_box), "event",
                        G_CALLBACK (gimp_preview_event), (gpointer) preview);
    }

  PREVIEW_DATA (preview)->progress_bar = gtk_progress_bar_new ();
  gtk_widget_set_parent (PREVIEW_DATA (preview)->progress_bar,
                         GTK_WIDGET (preview));
  gtk_widget_set_size_request (PREVIEW_DATA (preview)->progress_bar, -1,
                               PROGRESS_BAR_HEIGHT);
  gtk_widget_show (PREVIEW_DATA (preview)->progress_bar);

  /*
   * If the caller wants to disable scaling, they need only set the 
   * cb_allow_scale argument to 1 (or higher).
   */
  if (cb_allow_scale)
    {
      PREVIEW_DATA (preview)->allow_scale = 1;

      hbox = gtk_hbox_new (FALSE, 0);
      PREVIEW_DATA (preview)->resize_box = hbox;
      gtk_container_set_resize_mode (GTK_CONTAINER (hbox),
                                     GTK_RESIZE_IMMEDIATE);
      gtk_container_set_border_width (GTK_CONTAINER (hbox), 4);
      gtk_widget_set_parent (hbox, GTK_WIDGET (preview));

      PREVIEW_DATA (preview)->button_minus = gtk_button_new_with_label ("-");
      PREVIEW_DATA (preview)->button_plus = gtk_button_new_with_label ("+");

      gtk_box_pack_start (GTK_BOX (hbox),
                          PREVIEW_DATA (preview)->button_minus, TRUE, FALSE,
                          0);
      gtk_box_pack_start (GTK_BOX (hbox), PREVIEW_DATA (preview)->label, TRUE,
                          FALSE, 0);
      gtk_box_pack_start (GTK_BOX (hbox), PREVIEW_DATA (preview)->button_plus,
                          TRUE, FALSE, 0);

      g_signal_connect (G_OBJECT (PREVIEW_DATA (preview)->button_minus),
                        "clicked",
                        G_CALLBACK (gimp_preview_minus_callback),
                        (gpointer) preview);
      g_signal_connect (G_OBJECT (PREVIEW_DATA (preview)->button_plus),
                        "clicked",
                        G_CALLBACK (gimp_preview_plus_callback),
                        (gpointer) preview);

      gtk_widget_show (PREVIEW_DATA (preview)->button_minus);
      gtk_widget_show (PREVIEW_DATA (preview)->label);
      gtk_widget_show (PREVIEW_DATA (preview)->button_plus);
      gtk_widget_show (hbox);
    }
  else
    {
      PREVIEW_DATA (preview)->allow_scale = 0;
    }

  gtk_widget_show (PREVIEW_DATA (preview)->image);
  gtk_widget_show (event_box);
  gtk_widget_show (frame);

  /*
   * Build our initial preview of the drawable data.
   */

  gimp_preview_update (preview);

  return GTK_WIDGET (preview);
}

static void
gimp_preview_size_request (GtkWidget * widget, GtkRequisition * requisition)
{
  RefocusPreview *preview = GIMP_PREVIEW (widget);
  GtkRequisition resize_box_requisition;

#ifdef PREVIEW_DEBUG
  printf ("Received size request request for %d x %d\n", requisition->width,
          requisition->height);
#endif
  gtk_widget_size_request (PREVIEW_DATA (preview)->resize_box,
                           &resize_box_requisition);
  requisition->width = MAX (preview->max_width, resize_box_requisition.width);
  requisition->height = preview->max_height +
    PROGRESS_BAR_HEIGHT + resize_box_requisition.height;
#ifdef PREVIEW_DEBUG
  printf ("size_request: resize_box requisition.height = %d\n",
          resize_box_requisition.height);
  printf ("max_height = %d\n", preview->max_height);
  printf ("Return size request request %d x %d\n", requisition->width,
          requisition->height);
#endif
}

static void
gimp_preview_size_allocate (GtkWidget * widget, GtkAllocation * allocation)
{
  RefocusPreview *preview = GIMP_PREVIEW (widget);
  GtkAllocation resize_box_allocation, progress_bar_allocation,
    event_box_allocation;
  GtkRequisition resize_box_requisition;
  gint resize_box_top, progress_bar_top;

#ifdef PREVIEW_DEBUG
  printf ("Received size allocate request request for (%d, %d) %d x %d\n",
          allocation->x, allocation->y, allocation->width,
          allocation->height);
#endif
  if (memcmp (&(widget->allocation), allocation, sizeof (GtkAllocation)))
    {
      widget->allocation = *allocation;
      gtk_widget_size_request (PREVIEW_DATA (preview)->resize_box,
                               &resize_box_requisition);
#ifdef PREVIEW_DEBUG
      printf ("size_allocate: resize_box requisition.height = %d\n",
              resize_box_requisition.height);
#endif
      resize_box_top =
        MAX (0, allocation->height - resize_box_requisition.height);
      resize_box_allocation.x = allocation->x;
      resize_box_allocation.y = allocation->y + resize_box_top;
      resize_box_allocation.width = allocation->width;
      resize_box_allocation.height = allocation->height - resize_box_top;

      progress_bar_top = MAX (0, resize_box_top - PROGRESS_BAR_HEIGHT);

      progress_bar_allocation.x = allocation->x;
      progress_bar_allocation.y = allocation->y + progress_bar_top;
      progress_bar_allocation.width = allocation->width;
      progress_bar_allocation.height = resize_box_top - progress_bar_top;

      event_box_allocation.x = allocation->x;
      event_box_allocation.y = allocation->y;
      event_box_allocation.width = allocation->width;
      event_box_allocation.height = progress_bar_top;

      gtk_widget_size_allocate (PREVIEW_DATA (preview)->resize_box,
                                &resize_box_allocation);
      gtk_widget_size_allocate (PREVIEW_DATA (preview)->progress_bar,
                                &progress_bar_allocation);
      gtk_widget_size_allocate (PREVIEW_DATA (preview)->event_box,
                                &event_box_allocation);
      if (GTK_WIDGET_MAPPED (widget))
        {
          gdk_window_invalidate_rect (widget->window, &widget->allocation,
                                      FALSE);
        };
      if ((event_box_allocation.width != preview->max_width) ||
          (event_box_allocation.height != preview->max_height))
        {
          preview->max_width = event_box_allocation.width;
          preview->max_height = event_box_allocation.height;
          gimp_preview_update (preview);
        }
    }
}

static void
gimp_preview_forall (GtkContainer * container,
                     gboolean include_internals,
                     GtkCallback callback, gpointer callback_data)
{
  RefocusPreview *preview = GIMP_PREVIEW (container);

  if (PREVIEW_DATA (preview)->resize_box)
    {
      (*callback) (PREVIEW_DATA (preview)->resize_box, callback_data);
    }
  if (PREVIEW_DATA (preview)->progress_bar)
    {
      (*callback) (PREVIEW_DATA (preview)->progress_bar, callback_data);
    }
  if (PREVIEW_DATA (preview)->event_box)
    {
      (*callback) (PREVIEW_DATA (preview)->event_box, callback_data);
    }
}


/*
 * Plug-ins call this to do an update of the preview area.
 */
void
gimp_preview_update (RefocusPreview * preview)
{
  gimp_preview_recompute_sizes (preview, preview->scale);
  gimp_preview_update_preview (preview);
}


/*
 * Change the scale amount upwards and redraw the display.
 * This can never be called if allow_scale was 0 when
 * gimp_preview_new_with_args() was called.
 */
static void
gimp_preview_plus_callback (GtkWidget * widget, gpointer data)
{
  RefocusPreview *preview;
  gchar buffer[10];
  gdouble new_scale;

  preview = GIMP_PREVIEW (data);
  if (PREVIEW_DATA (preview)->scale_n == PREVIEW_SCALE_LAST)
    return;

  PREVIEW_DATA (preview)->scale_n++;
  new_scale = preview_scale[PREVIEW_DATA (preview)->scale_n];
  sprintf (buffer, "%d%%", (gint) (new_scale * 100));
  gtk_label_set_text (GTK_LABEL (PREVIEW_DATA (preview)->label), buffer);
  /* This seems to be necessary to update the width of the label */
  gtk_container_resize_children (GTK_CONTAINER
                                 (PREVIEW_DATA (preview)->resize_box));

  if (PREVIEW_DATA (preview)->scale_n == PREVIEW_SCALE_LAST)
    gtk_widget_set_sensitive (widget, FALSE);

  if (PREVIEW_DATA (preview)->scale_n == 1)
    {
      gtk_widget_set_sensitive (PREVIEW_DATA (preview)->button_minus, TRUE);
    }

  gimp_preview_recompute_sizes (preview, new_scale);
  gimp_preview_update_preview (preview);
}


/*
 * Change the scale amount downwards and redraw the display.
 * This can never be called if allow_scale was 0 when
 * gimp_preview_new_with_args() was called.
 */
static void
gimp_preview_minus_callback (GtkWidget * widget, gpointer data)
{
  RefocusPreview *preview;
  gchar buffer[10];
  gdouble new_scale;

  preview = GIMP_PREVIEW (data);
  if (PREVIEW_DATA (preview)->scale_n == 0)
    return;

  PREVIEW_DATA (preview)->scale_n--;
  new_scale = preview_scale[PREVIEW_DATA (preview)->scale_n];
  sprintf (buffer, "%d%%", (gint) (new_scale * 100));
  gtk_label_set_text (GTK_LABEL (PREVIEW_DATA (preview)->label), buffer);
  /* This seems to be necessary to update the width of the label */
  gtk_container_resize_children (GTK_CONTAINER
                                 (PREVIEW_DATA (preview)->resize_box));

  if (PREVIEW_DATA (preview)->scale_n == 0)
    gtk_widget_set_sensitive (widget, FALSE);

  if (PREVIEW_DATA (preview)->scale_n == PREVIEW_SCALE_LAST - 1)
    gtk_widget_set_sensitive (PREVIEW_DATA (preview)->button_plus, TRUE);

  gimp_preview_recompute_sizes (preview, new_scale);
  gimp_preview_update_preview (preview);
}


/*
 * This function handles events from the mouse - dragging, button presses
 * and releases.  We only notice button 1 on the mouse to initiate and manage
 * dragging of the preview.
 */
static gint
gimp_preview_event (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  RefocusPreview *preview;
  GdkEventButton *button_event;
  gint x, y;
  gint dx, dy;

  preview = GIMP_PREVIEW (data);
  button_event = (GdkEventButton *) event;

  switch (event->type)
    {
    case GDK_BUTTON_PRESS:
      if (button_event->button == 1)
        {
          /* Make sure that results of previous update_events will not be
             shown in the preview */
          PREVIEW_DATA (preview)->current_event_id++;

          gtk_widget_get_pointer (widget, &x, &y);

          PREVIEW_DATA (preview)->in_drag = TRUE;
          PREVIEW_DATA (preview)->drag_x = x;
          PREVIEW_DATA (preview)->drag_y = y;

          PREVIEW_DATA (preview)->orig_preview_x = preview->preview_x;
          PREVIEW_DATA (preview)->orig_preview_y = preview->preview_y;

          gtk_grab_add (widget);

          gimp_preview_update_preview (preview);
        }
      break;

    case GDK_BUTTON_RELEASE:
      if (PREVIEW_DATA (preview)->in_drag && button_event->button == 1)
        {
          gtk_grab_remove (widget);
          PREVIEW_DATA (preview)->in_drag = FALSE;

          gimp_preview_update_preview (preview);
        }

      break;

    case GDK_MOTION_NOTIFY:
      if (PREVIEW_DATA (preview)->in_drag)
        {
          gtk_widget_get_pointer (widget, &x, &y);

          dx = x - PREVIEW_DATA (preview)->drag_x;
          dy = y - PREVIEW_DATA (preview)->drag_y;

          preview->preview_x =
            CLAMP (PREVIEW_DATA (preview)->orig_preview_x - dx,
                   0,
                   MAX (0,
                        i2p (preview->drawable->width - 1, preview->scale)
                        - preview->width));
          preview->preview_y =
            CLAMP (PREVIEW_DATA (preview)->orig_preview_y - dy,
                   0,
                   MAX (0,
                        i2p (preview->drawable->height - 1, preview->scale)
                        - preview->height));
          gimp_preview_update_preview (preview);
        }
      break;

    default:
      break;
    }

  return FALSE;
}


/* Recompute the size and location of the display.
 * new_scale is the new value of the scale. It must be supplied
 * as argument because otherwise we don't have enough information
 * to keep the same location in the image at the centre of the preview.
 * This function is also used for initializing the preview.
 */
static void
gimp_preview_recompute_sizes (RefocusPreview * preview, gdouble new_scale)
{

  /* The center of the preview in image coordinates.
   * We try to keep the center at the same position when changing scales
   */
  const gint im_center_x =
    p2i (preview->preview_x + preview->width / 2.0, preview->scale);
  const gint im_center_y =
    p2i (preview->preview_y + preview->height / 2.0, preview->scale);

  const gint image_width = i2p (preview->drawable->width - 1, new_scale) + 1;
  const gint image_height =
    i2p (preview->drawable->height - 1, new_scale) + 1;

  gimp_preview_image_set_size (preview, image_width, image_height);
  preview->scale = new_scale;
  preview->preview_x =
    CLAMP (i2p (im_center_x, new_scale) - preview->width / 2,
           0, MAX (0, image_width - preview->width));
  preview->preview_y =
    CLAMP (i2p (im_center_y, new_scale) - preview->height / 2,
           0, MAX (0, image_height - preview->height));

  /* Assert that bottom-right point of preview maps to a point in the drawable */
  g_assert (p2i (preview->width - 1, preview->scale) <
            preview->drawable->width);
  g_assert (p2i (preview->height - 1, preview->scale) <
            preview->drawable->height);
}

void
gimp_preview_generate_update_event (RefocusPreview * preview)
     /* Signal the user that the preview must be updated */
{
  const gdouble scale = preview->scale;
  const gint image_x = p2i (preview->preview_x, scale);
  const gint image_y = p2i (preview->preview_y, scale);

  const gint image_width =
    CLAMP (p2i (preview->preview_x + preview->width - 1, scale) + 1 - image_x,
           0,
           preview->drawable->width - image_x);

  const gint image_height =
    CLAMP (p2i (preview->preview_y + preview->height - 1, scale) + 1 -
           image_y,
           0,
           preview->drawable->height - image_y);

  RefocusPreviewEvent preview_event;

  preview_event.event_id = PREVIEW_DATA (preview)->current_event_id;
  preview_event.scale = preview->scale;
  preview_event.image_x = image_x;
  preview_event.image_y = image_y;
  preview_event.image_width = image_width;
  preview_event.image_height = image_height;
  preview_event.preview_x = preview->preview_x;
  preview_event.preview_y = preview->preview_y;
  preview_event.preview_width = preview->width;
  preview_event.preview_height = preview->height;
  preview_event.scaled_data = preview->buffer;

  g_signal_emit (GTK_OBJECT (preview),
                 gimp_preview_signals[UPDATE_PREVIEW], 0, &preview_event);
  gimp_preview_force_redraw (preview);
}

/* Update the preview. 
 * First scale the original image.
 * If we are currently dragging the preview is redrawn.
 * Otherwise an event is generated to signal the user that
 * the preview must be updated.
 * This function is the main performance bottleneck in the
 * PreviewWidget. Be careful with modifications!
 * This implementation uses explicit subscripting with loop variables.
 * This makes this version faster than a version that uses pointers
 * to step through source and destination!
 */
static void
gimp_preview_update_preview (RefocusPreview * preview)
{
  GimpPixelRgn region;
  guchar *image_data = NULL;

  const gint preview_x = preview->preview_x;
  const gint preview_y = preview->preview_y;
  const gint preview_width = preview->width;
  const gdouble scale = preview->scale;
  /* Coordinates within the image of top left pixel in preview */
  const gint image_x = p2i (preview->preview_x, scale);
  const gint image_y = p2i (preview->preview_y, scale);

  const gint image_width =
    CLAMP (p2i (preview->preview_x + preview->width - 1, scale) + 1 - image_x,
           0,
           preview->drawable->width - image_x);

  const gint image_height =
    CLAMP (p2i (preview->preview_y + preview->height - 1, scale) + 1 -
           image_y,
           0,
           preview->drawable->height - image_y);

  const gint width = CLAMP (preview->width,
                            0,
                            i2p (image_x + image_width - 1,
                                 preview->scale) + 1 - preview_x);
  const gint height = CLAMP (preview->height,
                             0,
                             i2p (image_y + image_height - 1,
                                  preview->scale) + 1 - preview_y);
  gint row;

  g_assert ((image_x >= 0) && (image_x < preview->drawable->width));
  g_assert ((image_y >= 0) && (image_y < preview->drawable->height));
  g_assert ((image_x + image_width - 1) < preview->drawable->width);
  g_assert ((image_y + image_height - 1) < preview->drawable->height);
  gimp_pixel_rgn_init (&region, preview->drawable,
                       image_x,
                       image_y, image_width, image_height, FALSE, FALSE);

  /* This should happen during initialization to speed things up on large images. */
  image_data = g_malloc0 (sizeof (guchar) * preview->drawable->bpp
                          * image_width * image_height);
  gimp_pixel_rgn_get_rect (&region, image_data,
                           image_x, image_y, image_width, image_height);


  /* Clear buffer */
  memset (preview->buffer, 0, preview->width * preview->height * 4);

  /* Scale and convert to RGBA */
  if (preview->drawable_has_alpha && (preview->drawable->bpp == 4))
    {
      gint dy;
      guchar *const dest_ptr = preview->buffer;
      const gint bpp = 4;

      /* For each row of the preview ... */
      for (dy = 0; dy < height; dy++)
        {
          /* Calculate: starting position in drawable */
          gint dx;
          /* y coordinate in image */
          const gint sy = p2i (dy + preview_y, scale);
          guchar *const dest_row_ptr = dest_ptr + dy * preview_width * 4;

          const guchar *const src_ptr =
            image_data + ((sy - image_y) * image_width - image_x) * bpp;

          for (dx = 0; dx < width; dx++)
            {
              /* x coordinate in image */
              const gint sx = p2i (dx + preview_x, scale);

              dest_row_ptr[dx * 4 + 0] = src_ptr[sx * bpp + 0];
              dest_row_ptr[dx * 4 + 1] = src_ptr[sx * bpp + 1];
              dest_row_ptr[dx * 4 + 2] = src_ptr[sx * bpp + 2];
              dest_row_ptr[dx * 4 + 3] = src_ptr[sx * bpp + 3];
            }
        }
    }
  else if (!preview->drawable_has_alpha && (preview->drawable->bpp == 3))
    {
      gint dy;
      guchar *const dest_ptr = preview->buffer;
      const gint bpp = 3;

      /* For each row of the preview ... */
      for (dy = 0; dy < height; dy++)
        {
          /* Calculate: starting position in drawable */
          gint dx;
          /* y coordinate in image */
          const gint sy = p2i (dy + preview_y, scale);
          guchar *const dest_row_ptr = dest_ptr + dy * preview_width * 4;

          const guchar *const src_ptr =
            image_data + ((sy - image_y) * image_width - image_x) * bpp;

          for (dx = 0; dx < width; dx++)
            {
              /* x coordinate in image */
              const gint sx = p2i (dx + preview_x, scale);

              dest_row_ptr[dx * 4 + 0] = src_ptr[sx * bpp + 0];
              dest_row_ptr[dx * 4 + 1] = src_ptr[sx * bpp + 1];
              dest_row_ptr[dx * 4 + 2] = src_ptr[sx * bpp + 2];
              dest_row_ptr[dx * 4 + 3] = 255;
            }
        }

    }
  else if (preview->drawable_has_alpha && (preview->drawable->bpp == 2))
    {
      gint dy;
      guchar *const dest_ptr = preview->buffer;
      const gint bpp = 2;

      for (dy = 0; dy < height; dy++)
        {
          /* Calculate: starting position in drawable */
          gint dx;
          /* y coordinate in image */
          const gint sy = p2i (dy + preview_y, scale);
          guchar *const dest_row_ptr = dest_ptr + dy * preview_width * 4;

          const guchar *const src_ptr =
            image_data + ((sy - image_y) * image_width - image_x) * bpp;

          for (dx = 0; dx < width; dx++)
            {
              /* x coordinate in image */
              const gint sx = p2i (dx + preview_x, scale);

              dest_row_ptr[dx * 4 + 0] =
                dest_row_ptr[dx * 4 + 1] =
                dest_row_ptr[dx * 4 + 2] = src_ptr[sx * bpp + 0];
              dest_row_ptr[dx * 4 + 3] = src_ptr[sx * bpp + 1];
            }
        }
    }
  else if (!preview->drawable_has_alpha && (preview->drawable->bpp == 1))
    {
      gint dy;
      guchar *const dest_ptr = preview->buffer;
      const gint bpp = 1;

      /* For each row of the preview ... */
      for (dy = 0; dy < height; dy++)
        {
          /* Calculate: starting position in drawable */
          gint dx;
          /* y coordinate in image */
          const gint sy = p2i (dy + preview_y, scale);
          guchar *const dest_row_ptr = dest_ptr + dy * preview_width * 4;

          const guchar *const src_ptr =
            image_data + ((sy - image_y) * image_width - image_x) * bpp;

          for (dx = 0; dx < width; dx++)
            {
              /* x coordinate in image */
              const gint sx = p2i (dx + preview_x, scale);

              dest_row_ptr[dx * 4 + 0] =
                dest_row_ptr[dx * 4 + 1] =
                dest_row_ptr[dx * 4 + 2] = src_ptr[sx * bpp + 0];
              dest_row_ptr[dx * 4 + 3] = 255;
            }
        }
    }


  /* Now redraw the preview */

  for (row = 0; row < preview->height; row++)
    {
      gimp_preview_draw_row (preview,
                             PREVIEW_DATA (preview)->current_event_id,
                             GIMP_RGBA_IMAGE, row,
                             &(preview->buffer[row * preview->width * 4]));
    }
  if (!PREVIEW_DATA (preview)->in_drag)
    {
      /* Not preview->in_drag */
      /* Signal the user that the preview must be updated */
      gimp_preview_schedule_update (preview);
    }

  gimp_preview_force_redraw (preview);

  g_free (image_data);
}


void
gimp_preview_force_redraw (RefocusPreview * preview)
{
  gtk_widget_queue_draw (GTK_WIDGET (PREVIEW_DATA (preview)->image));
}


/* This function can be called from the glib idle loop */
gboolean
gimp_preview_update_preview_idle_fun (gpointer data)
{
  RefocusPreview *preview = GIMP_PREVIEW (data);
  gint event_id = PREVIEW_DATA (preview)->current_event_id;

#ifdef PREVIEW_DEBUG
  printf ("Idle_fun: event_id = %d done = %d\n",
          PREVIEW_DATA (preview)->current_event_id,
          PREVIEW_DATA (preview)->last_processed_event_id);
#endif
  if ((PREVIEW_DATA (preview)->last_processed_event_id < event_id) &&
      !PREVIEW_DATA (preview)->in_drag)
    {
      PREVIEW_DATA (preview)->last_processed_event_id =
        MAX (PREVIEW_DATA (preview)->last_processed_event_id, event_id);
      gimp_preview_generate_update_event (preview);
    }
  return (FALSE);
}

void
gimp_preview_schedule_update (RefocusPreview * preview)
{
  PREVIEW_DATA (preview)->current_event_id++;

#ifdef PREVIEW_DEBUG
  printf ("Scheduled update %d\n", PREVIEW_DATA (preview)->current_event_id);
#endif
  g_idle_add_full (G_PRIORITY_LOW,
                   gimp_preview_update_preview_idle_fun, preview, NULL);
}

void
gimp_preview_pixbuf_draw_row (GdkPixbuf * pixbuf, guchar * data,
                              gint x, gint y, gint w)
{
  const gint n_chars_per_pixel =
    gdk_pixbuf_get_n_channels (pixbuf) *
    (gdk_pixbuf_get_bits_per_sample (pixbuf) / 8);
  guchar *dest;

  g_assert ((x >= 0) && (x < gdk_pixbuf_get_width (pixbuf)));
  g_assert ((w >= 0) && ((x + w) <= gdk_pixbuf_get_width (pixbuf)));
  g_assert ((y >= 0) && (y < gdk_pixbuf_get_height (pixbuf)));

  dest =
    gdk_pixbuf_get_pixels (pixbuf) + y * gdk_pixbuf_get_rowstride (pixbuf) +
    x * n_chars_per_pixel;
  memcpy (dest, data, w * n_chars_per_pixel);
}

void
gimp_preview_image_draw_row (GtkWidget * image, guchar * data,
                             gint x, gint y, gint w)
{
  GdkPixbuf *pixbuf;

  pixbuf = gtk_image_get_pixbuf (GTK_IMAGE (image));
  gimp_preview_pixbuf_draw_row (pixbuf, data, x, y, w);
}

void
gimp_preview_image_set_size (RefocusPreview * preview, gint width, gint height)
{
  const gint real_width = MIN (preview->max_width, width);
  const gint real_height = MIN (preview->max_height, height);

  if ((preview->width != real_width) || (preview->height != real_height))
    {
      GdkPixbuf *pixbuf;

      pixbuf =
        gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8, real_width,
                        real_height);
      gtk_image_set_from_pixbuf (GTK_IMAGE (PREVIEW_DATA (preview)->image),
                                 pixbuf);
      g_object_unref (pixbuf);
      preview->width = real_width;
      preview->height = real_height;

      /* Recalculate storage. */
      g_free (PREVIEW_DATA (preview)->preview_buffer_na[0]);
      g_free (PREVIEW_DATA (preview)->preview_buffer_na[1]);
      g_free (preview->buffer);

      PREVIEW_DATA (preview)->preview_buffer_na[0] = g_malloc (sizeof (guchar)
                                                               * real_width
                                                               * 3);
      PREVIEW_DATA (preview)->preview_buffer_na[1] = g_malloc (sizeof (guchar)
                                                               * real_width
                                                               * 3);
      /* The buffer that holds scaled image data */
      preview->buffer = g_malloc (sizeof (guchar)
                                  * (real_width + 16)
                                  * (real_height + 16) * 4);
    }
}

/**
 * gimp_preview_draw_row:
 * @preview: the #RefocusPreview
 * @event_id: event_id that was sent with the #RefocusPreviewEvent.
 * @type: the format of the data (e.g. %GIMP_RGBA_IMAGE).
 * @row:the relative number of the row within the preview.
 * The top row of the preview is number 0.
 * @data: pixels for the preview. It must have a length of
 * event->preview_width pixels.
 *
 * Draw one scaled row of data in the preview.
 * This function handles conversion to RGB and checkerboarding of
 * transparent areas.
 *
 * Return value: Returns FALSE when the event-id is not current.
 * This means that this request and all subsequent requests
 * with the same event-id will be ignored by the preview.
 **/
gboolean
gimp_preview_draw_row (RefocusPreview * preview, const gint event_id,
                       GimpImageType type, gint row,
                       const guchar * const data)
{
  /* This implementation uses explicit subscripting with loop variables.
   * This makes this version faster than a version that uses pointers
   * to step through source and destination!
   */
  /* When the event-id is not current return FALSE to indicate
   * the plug-in that it can stop with updates for this event.
   */
  const gboolean return_status =
    ((event_id == -1)
     || (event_id == PREVIEW_DATA (preview)->current_event_id));

  if (return_status)
    {
      const gint width = preview->width;
      guchar *buf_start = PREVIEW_DATA (preview)->preview_buffer_na[0];
      const gint check_offset =
        ((preview->preview_y + row) & CHECK_SIZE) ?
        preview->preview_x : preview->preview_x + CHECK_SIZE;
      gint j;

      if ((row < 0) || (row >= preview->height))
        return (return_status);

      switch (type)
        {
        case GIMP_RGBA_IMAGE:
          {
            guchar *dest_ptr = buf_start;
            const guchar *src_ptr = data;

            for (j = width - 1; j >= 0; j--)
              {
                const guchar check = ((j + check_offset) & CHECK_SIZE) ?
                  CHECK_LIGHT : CHECK_DARK;

                dest_ptr[3 * j] =
                  check +
                  (((src_ptr[4 * j + 0] - check) * src_ptr[4 * j + 3]) >> 8);
                dest_ptr[3 * j + 1] =
                  check +
                  (((src_ptr[4 * j + 1] - check) * src_ptr[4 * j + 3]) >> 8);
                dest_ptr[3 * j + 2] =
                  check +
                  (((src_ptr[4 * j + 2] - check) * src_ptr[4 * j + 3]) >> 8);
              }
          }
          break;
        case GIMP_RGB_IMAGE:
          {
            guchar *dest_ptr = buf_start;
            const guchar *src_ptr = data;

            /* Yes, you're right. We could use memcpy here */
            for (j = width - 1; j >= 0; j--)
              {
                dest_ptr[3 * j] = src_ptr[3 * j];
                dest_ptr[3 * j + 1] = src_ptr[3 * j + 1];
                dest_ptr[3 * j + 2] = src_ptr[3 * j + 2];
              }
          }
          break;
        case GIMP_GRAYA_IMAGE:
          {
            guchar *dest_ptr = buf_start;
            const guchar *src_ptr = data;

            for (j = width - 1; j >= 0; j--)
              {
                const guchar check = ((j + check_offset) & CHECK_SIZE) ?
                  CHECK_LIGHT : CHECK_DARK;

                dest_ptr[3 * j] =
                  dest_ptr[3 * j + 1] =
                  dest_ptr[3 * j + 2] =
                  check +
                  (((src_ptr[2 * j] - check) * src_ptr[2 * j + 1]) >> 8);
              }
          }
          break;
        case GIMP_GRAY_IMAGE:
          {
            guchar *dest_ptr = buf_start;
            const guchar *src_ptr = data;

            for (j = width - 1; j >= 0; j--)
              {

                dest_ptr[3 * j] =
                  dest_ptr[3 * j + 1] = dest_ptr[3 * j + 2] = src_ptr[j];
              }
          }
          break;
        default:
          /* Type argument is wrong */
          g_assert_not_reached ();
          break;
        }

      gimp_preview_image_draw_row (PREVIEW_DATA (preview)->image, buf_start,
                                   0, row, width);
    }
  return (return_status);
}

/**
 * gimp_preview_draw_unscaled_row:
 * @preview: the #RefocusPreview
 * @event_id: event_id that was sent with the #RefocusPreviewEvent.
 * @type: the format of the data (e.g. %GIMP_RGBA_IMAGE).
 * @row:row is the relative position of the row w.r.t. preview_event->image_y.
 * The top row has number 0.
 * @data: pixels for the preview. The first pixel in @data has x-coordinate
 * preview_event->image_x in the image. @data must have a length of
 * preview_event->image_width pixels.
 *
 * Draw one unscaled row of data in the preview.
 * This function handles scaling, conversion to RGB and checkerboarding of
 * transparent areas.
 * A nice feature is that this function will draw several lines of
 * the preview when scale > 1.
 *
 * Return value: Returns FALSE when the event-id is not current.
 * This means that this request and all subsequent requests
 * with the same event-id will be ignored by the preview.
 **/
gboolean
gimp_preview_draw_unscaled_row (RefocusPreview * preview, const gint event_id,
                                GimpImageType type, const gint row,
                                const guchar * const data)
{
  /*
     There is a small complication with transparent data when scale > 1
     that is caused by the checkerboarding. Since there are two different
     checkerboard patterns we may have to compute two different versions
     for the preview.
     This implementation uses explicit subscripting with loop variables.
     This makes this version faster than a version that uses pointers
     to step through source and destination!
   */

  /* When the event-id is not current return FALSE to indicate
   * the plug-in that it can stop with updates for this event.
   */
  const gboolean return_status =
    ((event_id == -1)
     || (event_id == PREVIEW_DATA (preview)->current_event_id));

  if (return_status)
    {
      const gint width = preview->width;
      guchar *buf_start[2];
      /* Indicate whether we must compute the odd and/or the even version 
         for checkerboarding transparent areas */
      gboolean need_check_idx[2] = { TRUE, TRUE };
      gint check_idx;
      const gdouble scale = preview->scale;
      const gint image_x = p2i (preview->preview_x, scale);
      const gint image_y = p2i (preview->preview_y, scale);

      /* Compute the range of the lines in the preview that we can draw */
      const gint y_start = CLAMP (i2p (image_y + row - 1, scale) + 1,
                                  preview->preview_y,
                                  preview->preview_y + preview->height - 1);
      const gint y_end = CLAMP (i2p (image_y + row, scale) + 1,
                                preview->preview_y,
                                preview->preview_y + preview->height);

      const gint preview_x = preview->preview_x;
      gint x, y;

      if (y_start >= y_end)
        {
          /* This row is not used in the preview */
          return (return_status);
        }

      if ((type == GIMP_RGBA_IMAGE) || (type == GIMP_GRAYA_IMAGE))
        {
          /* This checkerboard version must always be computed */
          const gint first_check_idx =
            ((y_start - preview->preview_y) / CHECK_SIZE) & 1;

          /* See if we must also compute the other checkerboard version */
          if (y_start + CHECK_SIZE < y_end)
            need_check_idx[1 - first_check_idx] = FALSE;
          buf_start[0] = PREVIEW_DATA (preview)->preview_buffer_na[0];
          buf_start[1] = PREVIEW_DATA (preview)->preview_buffer_na[1];
        }
      else
        {
          buf_start[0] = buf_start[1] =
            PREVIEW_DATA (preview)->preview_buffer_na[0];
        }

      /* Scale and checkerboard the image data */
      switch (type)
        {
        case GIMP_RGBA_IMAGE:
          for (check_idx = 0; check_idx < 2; check_idx++)
            {
              buf_start[check_idx] =
                PREVIEW_DATA (preview)->preview_buffer_na[check_idx];
              if (need_check_idx[check_idx])
                {
                  guchar *const dest_ptr = buf_start[check_idx];
                  const gint check_offset =
                    preview->preview_x + CHECK_SIZE * (1 - check_idx);

                  for (x = width - 1; x >= 0; x--)
                    {
                      const guchar check = ((x + check_offset) & CHECK_SIZE) ?
                        CHECK_LIGHT : CHECK_DARK;
                      const guchar *const src_ptr = data +
                        4 * (p2i (preview_x + x, scale) - image_x);

                      dest_ptr[3 * x + 0] =
                        check + (((src_ptr[0] - check) * src_ptr[3]) >> 8);
                      dest_ptr[3 * x + 1] =
                        check + (((src_ptr[1] - check) * src_ptr[3]) >> 8);
                      dest_ptr[3 * x + 2] =
                        check + (((src_ptr[2] - check) * src_ptr[3]) >> 8);
                    }
                }
            }
          break;
        case GIMP_RGB_IMAGE:
          {
            guchar *const dest_ptr = buf_start[0];

            for (x = width - 1; x >= 0; x--)
              {
                const guchar *const src_ptr = data +
                  3 * (p2i (preview_x + x, scale) - image_x);

                dest_ptr[3 * x + 0] = src_ptr[0];
                dest_ptr[3 * x + 1] = src_ptr[1];
                dest_ptr[3 * x + 2] = src_ptr[2];
              }
          }
          break;
        case GIMP_GRAYA_IMAGE:
          for (check_idx = 0; check_idx < 2; check_idx++)
            {
              buf_start[check_idx] =
                PREVIEW_DATA (preview)->preview_buffer_na[check_idx];
              if (need_check_idx[check_idx])
                {
                  guchar *const dest_ptr = buf_start[check_idx];
                  const gint check_offset =
                    preview->preview_x + CHECK_SIZE * (1 - check_idx);

                  for (x = width - 1; x >= 0; x--)
                    {
                      const guchar check = ((x + check_offset) & CHECK_SIZE) ?
                        CHECK_LIGHT : CHECK_DARK;
                      const guchar *const src_ptr = data +
                        2 * (p2i (preview_x + x, scale) - image_x);

                      dest_ptr[3 * x + 0] =
                        dest_ptr[3 * x + 1] =
                        dest_ptr[3 * x + 2] =
                        check + (((src_ptr[0] - check) * src_ptr[1]) >> 8);
                    }
                }
            }
          break;
        case GIMP_GRAY_IMAGE:
          {
            guchar *const dest_ptr = buf_start[0];

            for (x = width - 1; x >= 0; x--)
              {
                const guchar *const src_ptr = data +
                  p2i (preview_x + x, scale) - image_x;

                dest_ptr[3 * x + 0] =
                  dest_ptr[3 * x + 1] = dest_ptr[3 * x + 2] = src_ptr[0];
              }
          }
          break;
        default:
          /* Type argument is wrong */
          g_assert_not_reached ();
          break;
        }

      /* Now draw the scaled and checkerboarded rows into the pixmap */
      for (y = y_start; y < y_end; y++)
        {
          const gint check_idx = (y / CHECK_SIZE) & 1;

          gimp_preview_image_draw_row (PREVIEW_DATA (preview)->image,
                                       buf_start[check_idx],
                                       0, y - preview->preview_y, width);

        }
      /* Perhaps this is a bit expensive when used internally */
      gimp_preview_force_redraw (preview);
    }
  return (return_status);
}

/**
 * gimp_preview_progress_set_fraction:
 * @preview: the #RefocusPreview.
 * @event_id: event_id that was sent with the #RefocusPreviewEvent.
 * @fraction: the fraction completed.
 *
 * Set the progress bar of the preview to @fraction completed.
 *
 * Return value: Returns FALSE when the event-id is not current.
 * This means that this request and all subsequent requests
 * with the same event-id will be ignored by the preview.
 **/
gboolean
gimp_preview_progress_set_fraction (RefocusPreview * preview,
                                    const gint event_id, double fraction)
{
  const gboolean return_status =
    ((event_id == -1)
     || (event_id == PREVIEW_DATA (preview)->current_event_id));

  if (return_status)
    {
      if (PREVIEW_DATA (preview)->progress_bar)
        {
          gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR
                                         (PREVIEW_DATA (preview)->
                                          progress_bar), fraction);
          /* The following is absolutely necessary, otherwise Gtk waits
             with updating the display until the plug-in is ready with
             rendering
           */
          while (gtk_events_pending ())
            {
              gtk_main_iteration_do (FALSE);
            }
        }
    }
  return (return_status);
}
