AM_CPPFLAGS = $(EXTRACPPFLAGS)
AM_CFLAGS = $(EXTRACFLAGS)
AM_CXXFLAGS = $(EXTRACXXFLAGS)
AM_LDFLAGS = $(EXTRALDFLAGS)

SUBDIRS = layer_selection win32helpers

bin_PROGRAMS = enblend enfuse

enblend_SOURCES = anneal.h assemble.h blend.h bounds.h \
                  common.h enblend.h enblend.cc fixmath.h \
                  global.h gpu.cc gpu.h graphcut.h \
                  maskcommon.h masktypedefs.h mask.h postoptimizer.h \
                  nearest.h numerictraits.h openmp.h path.h pyramid.h \
                  error_message.h error_message.cc \
                  filenameparse.h filenameparse.cc \
                  filespec.h filespec.cc \
                  self_test.h self_test.cc \
                  tiff_message.h tiff_message.cc \
                  minimizer.h
enblend_LDFLAGS = $(AM_LDFLAGS) $(OPENGL_CFLAGS)
enblend_LDADD = layer_selection/liblayersel.a \
                $(GSL_LIBS) $(OPENGL_LIBS) @EXTRA_LIBS@
enblend_CXXFLAGS = $(AM_CXXFLAGS) $(GSL_CFLAGS) $(OPENGL_CFLAGS) \
                   -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 \
                   -I${top_srcdir}/include -I${top_srcdir}/src/layer_selection

enfuse_SOURCES = assemble.h blend.h bounds.h common.h \
                 enfuse.h enfuse.cc fixmath.h \
                 global.h mga.h numerictraits.h openmp.h pyramid.h \
                 error_message.h error_message.cc \
                 filenameparse.h filenameparse.cc \
                 filespec.h filespec.cc \
                 self_test.h self_test.cc \
                 tiff_message.h tiff_message.cc \
                 minimizer.h
enfuse_LDFLAGS = $(AM_LDFLAGS)
enfuse_LDADD = layer_selection/liblayersel.a \
               $(GSL_LIBS) @EXTRA_LIBS@
enfuse_CXXFLAGS = $(AM_CXXFLAGS) $(GSL_CFLAGS) \
                  -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 \
                  -I${top_srcdir}/include -I${top_srcdir}/src/layer_selection

EXTRA_DIST = enblend.1 enfuse.1 \
             gen_sig DefaultSig.pm Sig.pm \
             CMakeLists.txt

DISTCLEANFILES = enblend.1 enfuse.1


# Generated sources

BUILT_SOURCES = signature.h
CLEANFILES = signature.h

signature.h: $(srcdir)/gen_sig $(srcdir)/DefaultSig.pm $(srcdir)/Sig.pm
	@ $(PERL) -I$(srcdir) $< --extra=$(VERSION) > $@


# Documentation

man_MANS = enblend.1 enfuse.1
#dist_man_MANS = enblend.1 enfuse.1

.PHONY: man
man: enblend.1 enfuse.1


enblend.1: enblend
	$(HELP2MAN) --output=$@ ./enblend

enfuse.1: enfuse
	$(HELP2MAN) --output=$@ ./enfuse
