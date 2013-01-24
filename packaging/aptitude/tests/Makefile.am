MAINTAINERCLEANFILES = Makefile.in

INCLUDES = -I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(srcdir)
BOOST_TEST_LDFLAGS = @BOOST_UNIT_TEST_LIBS@
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK -DSRCDIR=\"$(srcdir)\"
LDADD = $(top_builddir)/src/loggers.o			       \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/cmdline/libcmdline.a \
$(top_builddir)/src/generic/apt/libgeneric-apt.a	       \
$(top_builddir)/src/generic/controllers/libgeneric-controllers.a \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/generic/apt/libgeneric-apt.a	       \
$(top_builddir)/src/generic/problemresolver/libgeneric-problemresolver.a   \
$(top_builddir)/src/cmdline/mocks/libcmdline-mocks.a \
$(top_builddir)/src/cmdline/libcmdline.a \
$(top_builddir)/src/generic/util/libgeneric-util.a \
$(top_builddir)/src/generic/views/libgeneric-views.a \
$(top_builddir)/src/generic/views/mocks/libgeneric-views-mocks.a \
-lcppunit \
$(BOOST_TEST_LDFLAGS) $(GMOCK_LDFLAGS)

check_PROGRAMS = gtest_test cppunit_test boost_test gtest_test

noinst_PROGRAMS = interactive_set_test

TESTS = gtest_test cppunit_test boost_test gtest_test

EXTRA_DIST = file_caches

interactive_set_test_SOURCES = interactive_set_test.cc

test_choice.o test_choice_set.o test_resolver.o: $(top_srcdir)/src/generic/problemresolver/*.h
test_promotion_set.o test_resolver_costs.o test_resolver_hints.o: $(top_srcdir)/src/generic/problemresolver/*.h

# Build a local copy of gmock if necessary.
if BUILD_LOCAL_GMOCK
noinst_LIBRARIES = libgmock.a

GMOCK_LDFLAGS = $(builddir)/libgmock.a
nodist_libgmock_a_SOURCES = /usr/src/gtest/src/gtest-all.cc /usr/src/gmock/src/gmock-all.cc
libgmock_a_CPPFLAGS = -I/usr/src/gtest -I/usr/src/gmock
else
GMOCK_LDFLAGS = -lgmock -lgtest
endif

# Note: test_apt_universe is not built by default because it takes way
# too long.  Of course, ideally this would be done in a less ad-hoc
# way...
cppunit_test_SOURCES = \
	cppunit_test_main.cc \
	test_choice.cc \
	test_choice_set.cc \
	test_config_pusher.cc \
	test_dense_setset.cc \
	test_incremental_expression.cc \
	test_matching.cc \
	test_misc.cc \
	test_parsers.cc \
	test_promotion_set.cc \
	test_resolver.cc \
	test_resolver_costs.cc \
	test_resolver_hints.cc \
	test_setset.cc \
	test_tags.cc \
	test_temp.cc \
	test_wtree.cc

boost_test_SOURCES = \
	boost_test_main.cc \
	test_dynamic_list.cc \
	test_dynamic_set.cc \
	test_enumerator.cc \
	test_file_cache.cc \
	test_logging.cc \
	test_search_input_controller.cc \
	test_sqlite.cc

gtest_test_SOURCES = \
	gtest_test_main.cc \
	test_cmdline_download_progress_display.cc \
	test_cmdline_download_status_display.cc \
	test_cmdline_progress_display.cc \
	test_cmdline_search_progress.cc \
	test_logging.cc \
	test_teletype_mock.cc \
	test_terminal_mock.cc \
	test_transient_message.cc
