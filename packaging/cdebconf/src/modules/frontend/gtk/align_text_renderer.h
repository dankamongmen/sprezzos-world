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

/** @file align_text_renderer.h
 * GtkCellRenderer for text aligned with tab stops (header)
 */

#ifndef _ALIGN_TEXT_RENDERER_H_
#define _ALIGN_TEXT_RENDERER_H_

#include <gtk/gtk.h>

#define ALIGN_CENTER_STRING "\xe2\x80\x84"
#define ALIGN_CENTER_GLYPH  ((PangoGlyph) 0xa8e)
#define ALIGN_RIGHT_STRING  "\xe2\x80\x85"
#define ALIGN_RIGHT_GLYPH   ((PangoGlyph) 0xa8f)

#define TYPE_ALIGN_TEXT_RENDERER (cdebconf_gtk_align_text_renderer_get_type())

#define ALIGN_TEXT_RENDERER(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST( \
        (obj), TYPE_ALIGN_TEXT_RENDERER, AlignTextRenderer))
#define ALIGN_TEXT_RENDERER_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), \
                             TYPE_ALIGN_TEXT_RENDERER, \
			     AlignTextRendererClass))
#define IS_TEXT_ALIGN_RENDERER(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE((obj), TYPE_TEXT_ALIGN_RENDERER))
#define IS_TEXT_ALIGN_RENDERER_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE((klass), TYPE_TEXT_ALIGN_RENDERER))
#define TEXT_ALIGN_RENDERER_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), \
                               TYPE_ALIGN_TEXT_RENDERER, \
                               AlignTextRendererClass))

typedef struct _AlignTextRenderer AlignTextRenderer;
typedef struct _AlignTextRendererClass AlignTextRendererClass;

struct _AlignTextRenderer {
    GtkCellRenderer parent;

    gchar * text;
    PangoTabArray * tab_array;
    PangoGlyph align_center_glyph;
    PangoGlyph align_right_glyph;
};

struct _AlignTextRendererClass {
    GtkCellRendererClass parent_class;
    void (* set_tab_array)(AlignTextRenderer * renderer,
                           PangoTabArray * tab_array);
};

GType cdebconf_gtk_align_text_renderer_get_type(void);
GtkCellRenderer * cdebconf_gtk_align_text_renderer_new(void);
void cdebconf_gtk_align_text_renderer_set_tab_array(
    AlignTextRenderer * renderer, PangoTabArray * tab_array);

#endif /* ! _ALIGN_TEXT_RENDERER_H_ */

/* vim: et sw=4 si
 */
