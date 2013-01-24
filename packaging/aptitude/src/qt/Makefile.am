include moc.mk

INCLUDES=-I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(top_srcdir)/intl

noinst_LIBRARIES=libqt.a

pkgdatadir=$(datadir)/$(PACKAGE)

libqt_a_MOC = \
   widgets/changes_preview_tab.moc \
   widgets/package_info_tab.moc \
   widgets/packages_tab.moc \
   widgets/perform_changes_tab.moc \
   widgets/resolver_tab.moc \
   widgets/status_widget.moc \
   widgets/tab.moc \
   widgets/tab_widget.moc \
   windows/main_window.moc \
   tabs_manager.mocc

libqt_a_SOURCES= \
   widgets/changes_preview_tab.cc \
   widgets/changes_preview_tab.h \
   widgets/package_info_tab.cc \
   widgets/package_info_tab.h \
   widgets/packages_tab.cc \
   widgets/packages_tab.h \
   widgets/perform_changes_tab.cc \
   widgets/perform_changes_tab.h \
   widgets/resolver_tab.cc \
   widgets/resolver_tab.h \
   widgets/status_widget.cc \
   widgets/status_widget.h \
   widgets/tab.cc \
   widgets/tab.h \
   widgets/tab_widget.cc \
   widgets/tab_widget.h \
   windows/main_window.cc \
   windows/main_window.h \
   package.cc \
   package.h \
   package_pool.cc \
   package_pool.h \
   qt_main.cc \
   qt_main.h \
   tabs_manager.cc \
   tabs_manager.h \
   version.cc \
   version.h

BUILT_SOURCES = $(addprefix $(srcdir)/,$(libqt_a_MOC))
