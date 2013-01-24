INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src

EXTRA_DIST=model.tex test1.txt test3.txt test4.txt resolver_undo.h

noinst_LIBRARIES=libgeneric-problemresolver.a

noinst_PROGRAMS=test

test_LDADD = $(top_builddir)/src/generic/util/libgeneric-util.a libgeneric-problemresolver.a

libgeneric_problemresolver_a_SOURCES = \
	choice.h choice_indexed_map.h choice_set.h \
	cost.cc cost.h \
	cost_limits.cc cost_limits.h \
	dump_universe.h \
	exceptions.h \
	dummy_universe.cc dummy_universe.h \
	incremental_expression.cc incremental_expression.h \
	problemresolver.h \
	promotion_set.h sanity_check_universe.h \
	search_graph.h solution.h

test_SOURCES=test.cc
