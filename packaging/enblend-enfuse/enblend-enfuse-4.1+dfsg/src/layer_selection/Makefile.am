AM_CPPFLAGS = $(EXTRACPPFLAGS)
AM_CFLAGS = $(EXTRACFLAGS)
AM_CXXFLAGS = $(EXTRACXXFLAGS)
AM_LDFLAGS = $(EXTRALDFLAGS)

noinst_LIBRARIES = liblayersel.a

liblayersel_a_SOURCES = info.h info.cc \
                        layer_selection.h layer_selection.cc \
                        selector.h selector.cc

liblayersel_a_CXXFLAGS = $(AM_CXXFLAGS) \
                         -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 \
                         -I${top_srcdir}/include -I${top_srcdir}/src -I${top_srcdir}/src/layer_selection
