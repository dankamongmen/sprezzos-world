if USE_PROTOBUF

PROTOBUF_LIB = -lprotobuf
PROTOBUF_DEFINES = \
	-DUSE_PROTOBUF=1

compizconfig.pb.cc: compizconfig.proto
	protoc --cpp_out=. compizconfig.proto

compizconfig.pb.h: compizconfig.pb.cc

compiz.cpp: compizconfig.pb.h Makefile

nodist_libcompizconfig_la_SOURCES = \
	compizconfig.pb.cc

clean-local:
	rm -f compizconfig.pb.cc compizconfig.pb.h

endif

EXTRA_DIST = compizconfig.proto

INCLUDES =			               \
	@COMPIZ_CFLAGS@                        \
	@LIBX11_CFLAGS@                        \
	@LIBXML2_CFLAGS@                       \
	-I$(top_srcdir)/include	               \
	-I$(top_srcdir)/src                    \
	-DPLUGINDIR=\"$(PLUGINDIR)\"           \
	-DMETADATADIR=\"$(METADATADIR)\"       \
	-DLIBDIR=\"$(libdir)\"                 \
	$(PROTOBUF_DEFINES)                    \
	-DSYSCONFDIR=\"$(sysconfdir)\"


libcompizconfig_la_SOURCES = \
	main.c 		\
	lists.c 	\
	compiz.cpp 	\
	config.c 	\
	iniparser.c 	\
	ini.c 		\
	bindings.c 	\
	filewatch.c 	\
	ccs-private.h	\
	iniparser.h

libcompizconfig_la_LIBADD = -ldl @LIBXML2_LIBS@ @LIBX11_LIBS@ $(PROTOBUF_LIB)

lib_LTLIBRARIES=libcompizconfig.la

