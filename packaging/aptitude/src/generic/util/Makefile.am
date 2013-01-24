MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

SUBDIRS = mocks

noinst_LIBRARIES = libgeneric-util.a
libgeneric_util_a_SOURCES = \
	compare3.h \
	dense_setset.h \
	dirent_safe.h \
	dynamic_list.h \
	dynamic_list_collection.h \
	dynamic_list_impl.h \
	dynamic_set.h \
	dynamic_set_impl.h \
	dynamic_set_transform.h \
	dynamic_set_union.h \
	enumerator.h \
	enumerator_transform.h \
	file_cache.cc \
	file_cache.h \
	immlist.h \
	immset.h \
	job_queue_thread.h \
	logging.cc \
	logging.h \
	maybe.h \
	mut_fun.h \
	parsers.h \
        post_thunk.h        \
	progress_info.cc \
	progress_info.h \
	refcounted_base.cc \
	refcounted_base.h \
	refcounted_wrapper.h \
	safe_slot.h \
	setset.h \
	sqlite.cc \
	sqlite.h \
	temp.cc \
	temp.h \
	throttle.cc \
	throttle.h \
	undo.cc \
	undo.h \
	util.cc \
	util.h