## Process this file with automake to produce Makefile.in

libexecdir = $(GIMP_PLUGIN_DIR)/plug-ins

libexec_PROGRAMS = focusblur

focusblur_SOURCES = \
	aaa.c			\
	aaa.h			\
	brush.c			\
	brush.h			\
	depthmap.c		\
	depthmap.h		\
	diffusion.c		\
	diffusion.h		\
	focusblur.h		\
	focusblurenums.c	\
	focusblurenums.h	\
	focusblurparam.c	\
	focusblurparam.h	\
	focusblurrc.c		\
	focusblurrc.h		\
	focusblurstock.c	\
	focusblurstock.h	\
	focusblurtypes.h	\
	gimpplugin.c		\
	gimpplugin.h		\
	interface.c		\
	interface.h		\
	render.c		\
	render.h		\
	shine.c			\
	shine.h			\
	source.c		\
	source.h

if HAVE_FFTW3
focusblur_SOURCES += \
	fftblur.c		\
	fftblur.h		\
	fftblurbuffer.c		\
	fftblurbuffer.h		\
	fftblurproc.c		\
	fftblurproc.h
endif

if PLATFORM_WIN32
mwindows = -mwindows
endif

AM_LDFLAGS = $(mwindows)

INCLUDES = \
	$(GTHREAD_CFLAGS)	\
	$(GIMP_CFLAGS)		\
	$(FFTW3_CFLAGS)		\
	-I$(includedir)

LDADD = \
	$(GTHREAD_LIBS)		\
	$(GIMP_LIBS)		\
	$(FFTW3_LIBS)

focusblurstock.$(OBJEXT): $(top_srcdir)/pixmaps/pixbufs.h
$(top_srcdir)/pixmaps/pixbufs.h:
	$(MAKE) -C $(top_srcdir)/pixmaps pixbufs.h

install-user: $(libexec_PROGRAMS)
	@if test -d $(GIMP_USER_PLUGIN_DIR) ; then \
	  $(INSTALL_PROGRAM) $(libexec_PROGRAMS) $(GIMP_USER_PLUGIN_DIR) ; \
	fi

uninstall-user:
	@if test -d $(GIMP_USER_PLUGIN_DIR) ; then \
	  for i in $(libexec_PROGRAMS) ; do \
	    echo " $(RM) '$(GIMP_USER_PLUGIN_DIR)/$$i'"; \
	    $(RM) "$(GIMP_USER_PLUGIN_DIR)/$$i" ; \
	  done ; \
	fi
