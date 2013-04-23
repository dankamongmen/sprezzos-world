BUILT_SOURCES = pixbufs.h

icons = \
	focusblur-model-flat.png \
	focusblur-model-spherical.png \
	focusblur-model-gaussian.png \
	focusblur-model-ring.png \
	focusblur-model-concave.png

EXTRA_DIST = $(icons)

pixbufs.h: $(icons)
	{ echo '#include <gdk-pixbuf/gdk-pixdata.h>' ; \
	$(GDK_PIXBUF_CSOURCE) --struct --rle --build-list \
	$$(for i in $(icons) ; do \
		echo $$i | sed -e 's;\..*;;' -e 's;-;_;g' ; \
		echo -n $$i" " ; \
	done) | \
	sed -e '/pixel_data:/a(guint8 *)' ; \
	} > $@
