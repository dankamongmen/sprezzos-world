topdir=../../src

INCLUDES = -I$(topdir)/include

gl_includedir = $(includedir)/GL/
gl_include_HEADERS = $(topdir)/include/GL/glui.h

lib_LTLIBRARIES = libglui.la

libglui_la_SOURCES = $(topdir)/quaternion.cpp \
	$(topdir)/glui_translation.cpp $(topdir)/glui_checkbox.cpp \
	$(topdir)/glui_rollout.cpp $(topdir)/glui_statictext.cpp \
	$(topdir)/glui_panel.cpp $(topdir)/glui_list.cpp \
	$(topdir)/glui_button.cpp $(topdir)/glui_separator.cpp \
	$(topdir)/arcball.cpp $(topdir)/glui_control.cpp \
	$(topdir)/glui_add_controls.cpp $(topdir)/glui_bitmap_img_data.cpp \
	$(topdir)/glui_window.cpp $(topdir)/glui_scrollbar.cpp \
	$(topdir)/glui_bitmaps.cpp $(topdir)/glui.cpp \
	$(topdir)/glui_textbox.cpp $(topdir)/glui_filebrowser.cpp \
	$(topdir)/glui_rotation.cpp $(topdir)/glui_node.cpp \
	$(topdir)/glui_treepanel.cpp $(topdir)/algebra3.cpp \
	$(topdir)/glui_edittext.cpp $(topdir)/glui_spinner.cpp \
	$(topdir)/glui_commandline.cpp $(topdir)/glui_mouse_iaction.cpp \
	$(topdir)/glui_listbox.cpp $(topdir)/glui_string.cpp \
	$(topdir)/glui_tree.cpp $(topdir)/glui_column.cpp \
	$(topdir)/glui_radio.cpp

libglui_la_LDFLAGS = -version-info 38:0:36
libglui_la_LIBADD = -lGL -lglut -lGLU

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = glui.pc
