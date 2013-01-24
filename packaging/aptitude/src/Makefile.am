MAINTAINERCLEANFILES=Makefile.in

if BUILD_GTK
  MAYBE_GTK = gtk
  MAYBE_LIBGTK = gtk/libgtk.a gtk/toplevel/libgtk-toplevel.a
endif

if BUILD_QT
  MAYBE_QT = qt
  MAYBE_LIBQT = qt/libqt.a
endif

SUBDIRS=generic $(MAYBE_GTK) $(MAYBE_QT) cmdline mine

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

bin_PROGRAMS=aptitude

LDADD=@LIBINTL@ $(MAYBE_LIBGTK) $(MAYBE_LIBQT) cmdline/libcmdline.a mine/libcmine.a \
	generic/apt/libgeneric-apt.a generic/util/libgeneric-util.a \
	generic/apt/matching/libgeneric-matching.a \
	generic/controllers/libgeneric-controllers.a \
	generic/problemresolver/libgeneric-problemresolver.a \
	generic/views/libgeneric-views.a

aptitude_SOURCES= 	\
	aptitude.h	\
	main.cc		\
	desc_render.cc   \
	desc_render.h    \
	apt_config_treeitems.cc \
	apt_config_treeitems.h  \
	apt_config_widgets.cc \
	apt_config_widgets.h  \
	apt_info_tree.cc\
	apt_info_tree.h\
	apt_options.cc	\
	apt_options.h	\
	apt_undo_tree.cc \
	apt_undo_tree.h \
	broken_indicator.cc\
	broken_indicator.h\
	defaults.cc	\
	defaults.h	\
	dep_item.cc	\
	dep_item.h	\
	download_item.h	\
	download_item.cc\
	download_list.h \
	download_list.cc \
	download_thread.h\
	download_thread.cc\
	edit_pkg_hier.h	\
	edit_pkg_hier.cc\
	load_config.cc	\
	load_config.h	\
	load_grouppolicy.cc\
	load_grouppolicy.h \
	load_pkgview.cc	\
	load_pkgview.h	\
	load_sortpolicy.cc\
	load_sortpolicy.h\
	loggers.cc\
	loggers.h\
	menu_redirect.cc\
	menu_redirect.h\
	menu_text_layout.cc \
	menu_text_layout.h  \
	menu_tree.cc\
	menu_tree.h\
	pkg_columnizer.cc\
	pkg_columnizer.h\
	pkg_info_screen.cc\
	pkg_info_screen.h \
	pkg_item.cc	\
	pkg_item.h	\
	pkg_item_with_subtree.h \
	pkg_grouppolicy.h\
	pkg_grouppolicy.cc\
	pkg_sortpolicy.h\
	pkg_sortpolicy.cc\
	pkg_node.cc	\
	pkg_node.h	\
	pkg_subtree.cc	\
	pkg_subtree.h	\
	pkg_tree.cc	\
	pkg_tree.h	\
	pkg_ver_item.cc	\
	pkg_ver_item.h	\
	pkg_view.cc	\
	pkg_view.h	\
	progress.cc	\
	progress.h	\
	reason_fragment.cc\
	reason_fragment.h\
	safe_slot_event.h\
	solution_dialog.cc\
	solution_dialog.h\
	solution_fragment.cc\
	solution_fragment.h\
	solution_item.cc\
	solution_item.h\
	solution_screen.cc\
	solution_screen.h\
	trust.h		\
	trust.cc	\
	ui.cc		\
	ui.h		\
	ui_download_manager.cc \
	ui_download_manager.h  \
	view_changelog.cc \
	view_changelog.h
