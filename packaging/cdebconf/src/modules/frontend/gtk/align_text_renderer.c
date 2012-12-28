/*****************************************************************************
 *
 * cdebconf - An implementation of the Debian Configuration Management
 *            System
 *
 * cdebconf is (c) 2000-2007 Randolph Chung and others under the following
 * license.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *****************************************************************************/

/** @file align_text_renderer.c
 * GtkCellRenderer for text aligned with tab stops
 */

#include "align_text_renderer.h"

#include "cdebconf_gtk.h"

enum {
    PROP_0,

    PROP_TEXT
};

static gpointer cdebconf_gtk_align_text_renderer_parent_class;

static void cdebconf_gtk_align_text_renderer_init(AlignTextRenderer * renderer)
{
    gtk_cell_renderer_set_alignment(GTK_CELL_RENDERER(renderer), 0.0, 0.5);
    gtk_cell_renderer_set_padding(GTK_CELL_RENDERER(renderer), 2, 2);
    renderer->tab_array = NULL;
    renderer->align_center_glyph = 0;
    renderer->align_right_glyph = 0;
}

static void align_text_renderer_finalize(GObject * object)
{
    AlignTextRenderer * renderer = ALIGN_TEXT_RENDERER(object);

    if (NULL != renderer->text) {
        g_free(renderer->text);
    }
    if (NULL != renderer->tab_array) {
        pango_tab_array_free(renderer->tab_array);
    }

    (* G_OBJECT_CLASS(
        cdebconf_gtk_align_text_renderer_parent_class)->finalize)(object);
}

static void align_text_renderer_get_property(
    GObject * object, guint param_id, GValue * value,
    GParamSpec * param_spec)
{
    AlignTextRenderer * renderer = ALIGN_TEXT_RENDERER(object);

    switch (param_id) {
        case PROP_TEXT:
            g_value_set_string(value, renderer->text);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, param_spec);
            break;
    }
}

static void align_text_renderer_set_property(
    GObject * object, guint param_id, const GValue * value,
    GParamSpec * param_spec)
{
    AlignTextRenderer * renderer = ALIGN_TEXT_RENDERER(object);

    switch (param_id) {
        case PROP_TEXT:
            if (NULL != renderer->text) {
                g_free(renderer->text);
            }
            renderer->text = g_strdup(g_value_get_string(value));
            g_object_notify(object, "text");
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, param_spec);
            break;
    }
}

void cdebconf_gtk_align_text_renderer_set_tab_array(
    AlignTextRenderer * renderer, PangoTabArray * tab_array)
{
    if (NULL != renderer->tab_array) {
        pango_tab_array_free(renderer->tab_array);
    }
    renderer->tab_array = pango_tab_array_copy(tab_array);
}

GtkCellRenderer * cdebconf_gtk_align_text_renderer_new(void)
{
    return g_object_new(TYPE_ALIGN_TEXT_RENDERER, NULL);
}

static void handle_right_alignment(AlignTextRenderer * renderer,
                                   PangoLayout * layout,
                                   const GdkRectangle * cell_area)
{
    PangoLayoutIter * iter;
    PangoLayoutRun * run;
    PangoGlyphInfo * current;
    PangoGlyphInfo * last_spacer;
    enum {
        ALIGN_LEFT = 0,
        ALIGN_CENTER,
        ALIGN_RIGHT
    } align_mode;
    PangoRectangle rect;
    int i;

    last_spacer = NULL;
    align_mode = ALIGN_LEFT;
    iter = pango_layout_get_iter(layout);
    do {
        if (NULL != (run = pango_layout_iter_get_run(iter))) {
            for (i = 0; i < run->glyphs->num_glyphs; i++) {
                current = &run->glyphs->glyphs[i];
                if (PANGO_GLYPH_EMPTY == current->glyph) {
                    if (NULL != last_spacer) {
                        if (ALIGN_CENTER == align_mode) {
                            last_spacer->geometry.width +=
                                (current->geometry.width
                                 - last_spacer->geometry.width
                                 - COLUMN_SPACING) / 2;
                            current->geometry.width =
                                COLUMN_SPACING + last_spacer->geometry.width;
                        } else if (ALIGN_RIGHT == align_mode) {
                            last_spacer->geometry.width +=
                                current->geometry.width - COLUMN_SPACING;
                            current->geometry.width = COLUMN_SPACING;
                        }
                        last_spacer = NULL;
                        align_mode = ALIGN_LEFT;
                    }
                } else if (renderer->align_center_glyph == current->glyph) {
                    align_mode = ALIGN_CENTER;
                    current->glyph = PANGO_GLYPH_EMPTY;
                    last_spacer = current;
                } else if (renderer->align_right_glyph == current->glyph) {
                    align_mode = ALIGN_RIGHT;
                    current->glyph = PANGO_GLYPH_EMPTY;
                    last_spacer = current;
                }
            }
        }
    } while (pango_layout_iter_next_run(iter));
    if (NULL != cell_area && NULL != last_spacer && ALIGN_LEFT != align_mode) {
        pango_layout_get_pixel_extents(layout, NULL, &rect);
        if (ALIGN_CENTER == align_mode) {
            last_spacer->geometry.width +=
                PANGO_SCALE * ((cell_area->width - rect.width) / 2);
        } else if (ALIGN_RIGHT == align_mode) {
            last_spacer->geometry.width +=
                PANGO_SCALE * (cell_area->width - rect.width);
        }
    }
    pango_layout_iter_free(iter);
}

static PangoLayout * get_layout(AlignTextRenderer * renderer,
                                GtkWidget * widget,
                                const GdkRectangle * cell_area)
{
    PangoLayout * layout;

    layout = gtk_widget_create_pango_layout(widget, renderer->text);
    pango_layout_set_width(layout, -1);
    pango_layout_set_wrap(layout, PANGO_WRAP_CHAR);
    pango_layout_set_tabs(layout, renderer->tab_array);
    handle_right_alignment(renderer, layout, cell_area);
    return layout;
}

static void align_text_renderer_get_size(
    GtkCellRenderer * cell, GtkWidget * widget, GdkRectangle * cell_area,
    gint * x_offset, gint * y_offset, gint * width, gint * height)
{
    AlignTextRenderer * renderer = ALIGN_TEXT_RENDERER(cell);
    PangoLayout * layout;
    PangoRectangle rect;
    gint xpad;
    gint ypad;
    gfloat xalign;
    gfloat yalign;

    layout = get_layout(renderer, widget, cell_area);
    pango_layout_get_pixel_extents(layout, NULL, &rect);
    gtk_cell_renderer_get_padding(cell, &xpad, &ypad);
    gtk_cell_renderer_get_alignment(cell, &xalign, &yalign);
    if (NULL != height) {
        *height = ypad * 2 + rect.height;
    }
    if (NULL != width) {
        *width = xpad * 2 + rect.x + rect.width;
    }

    if (NULL != cell_area) {
        if (NULL != x_offset) {
            *x_offset = cell_area->width - rect.x - rect.width -
                        (2 * xpad);
            if (GTK_TEXT_DIR_RTL == gtk_widget_get_direction(widget)) {
                *x_offset *= 1.0 - xalign;
            } else {
                *x_offset *= xalign;
            }
        }
        if (NULL != y_offset) {
            *y_offset = cell_area->height - rect.height - (2 * ypad);
            *y_offset = MAX(*y_offset * yalign, 0);
        }
    }
}

static GtkStateType get_state(GtkCellRenderer * cell, GtkWidget * widget,
                              GtkCellRendererState flags)
{
    if (!gtk_cell_renderer_get_sensitive(cell)) {
        return GTK_STATE_INSENSITIVE;
    }
    if (GTK_CELL_RENDERER_SELECTED == (flags & GTK_CELL_RENDERER_SELECTED)) {
        return gtk_widget_has_focus(widget) ?
                   GTK_STATE_SELECTED : GTK_STATE_ACTIVE;
    }
    if (GTK_CELL_RENDERER_PRELIT == (flags & GTK_CELL_RENDERER_PRELIT) &&
        GTK_STATE_PRELIGHT == gtk_widget_get_state(widget)) {
        return GTK_STATE_PRELIGHT;
    }
    if (GTK_STATE_INSENSITIVE == gtk_widget_get_state(widget)) {
        return GTK_STATE_INSENSITIVE;
    }
    return GTK_STATE_NORMAL;
}

static PangoGlyph find_glyph(GtkWidget * widget, const gchar * str)
{
    PangoLayout * layout;
    PangoLayoutIter * iter;
    PangoLayoutRun * run;
    PangoGlyph glyph = PANGO_GET_UNKNOWN_GLYPH(PANGO_GLYPH_EMPTY);

    layout = gtk_widget_create_pango_layout(widget, str);
    iter = pango_layout_get_iter(layout);
    if (NULL != (run = pango_layout_iter_get_run(iter))) {
        if (1 >= run->glyphs->num_glyphs) {
            glyph = run->glyphs->glyphs[0].glyph;
        }
    }
    pango_layout_iter_free(iter);
    return glyph;
}

static void find_renderer_glyphs(AlignTextRenderer * renderer, GtkWidget * widget)
{
    if (0 == renderer->align_center_glyph) {
        renderer->align_center_glyph = find_glyph(widget, ALIGN_CENTER_STRING);
    }
    if (0 == renderer->align_right_glyph) {
        renderer->align_right_glyph = find_glyph(widget, ALIGN_RIGHT_STRING);
    }
}

static void align_text_renderer_render(
    GtkCellRenderer * cell, GdkWindow * window, GtkWidget * widget,
    GdkRectangle * background_area, GdkRectangle * cell_area,
    GdkRectangle * expose_area, guint flags)
{
    AlignTextRenderer * renderer = ALIGN_TEXT_RENDERER(cell);
    PangoLayout * layout;
    GtkStateType state;
    gint x_offset = 0;
    gint y_offset = 0;
    gint x_padding = 0;
    gint y_padding = 0;

    find_renderer_glyphs(renderer, widget);
    layout = get_layout(renderer, widget, cell_area);
    align_text_renderer_get_size(cell, widget, cell_area, &x_offset,
                                 &y_offset, NULL, NULL);

    state = get_state(cell, widget, flags);
    gtk_cell_renderer_get_padding(cell, &x_padding, &y_padding);
    gtk_paint_layout(gtk_widget_get_style(widget), window, state, TRUE, expose_area,
                     widget, "align_text_renderer",
                     cell_area->x + x_offset + x_padding,
                     cell_area->y + y_offset + y_padding,
                     layout);
}

static void cdebconf_gtk_align_text_renderer_class_init(
    AlignTextRendererClass * klass)
{
    GObjectClass * object_class = G_OBJECT_CLASS(klass);
    GtkCellRendererClass * cell_class = GTK_CELL_RENDERER_CLASS(klass);

    cdebconf_gtk_align_text_renderer_parent_class =
        g_type_class_peek_parent (klass);
    object_class->finalize = align_text_renderer_finalize;
    object_class->get_property = align_text_renderer_get_property;
    object_class->set_property = align_text_renderer_set_property;

    cell_class->get_size = align_text_renderer_get_size;
    cell_class->render = align_text_renderer_render;

    klass->set_tab_array = cdebconf_gtk_align_text_renderer_set_tab_array;

    g_object_class_install_property(
        object_class, PROP_TEXT,
        g_param_spec_string("text", "Text", "Text to render", NULL,
        G_PARAM_READWRITE));
}

GType cdebconf_gtk_align_text_renderer_get_type(void)
{
    static const GTypeInfo align_text_renderer_info = {
        sizeof (AlignTextRendererClass),
        NULL,                                                     /* base_init */
        NULL,                                                     /* base_finalize */
        (GClassInitFunc) cdebconf_gtk_align_text_renderer_class_init,
        NULL,                                                     /* class_finalize */
        NULL,                                                     /* class_data */
        sizeof (AlignTextRenderer),
        0,                                                        /* n_preallocs */
        (GInstanceInitFunc) cdebconf_gtk_align_text_renderer_init,
        NULL
    };
    static GType align_text_renderer_type = 0;

    if (align_text_renderer_type) {
        return align_text_renderer_type;
    }

    /* Derive from GtkCellRenderer */
    align_text_renderer_type = g_type_register_static(
        GTK_TYPE_CELL_RENDERER, "AlignTextRenderer",
        &align_text_renderer_info, 0);
    return align_text_renderer_type;
}

/* vim: et sw=4 si
 */
