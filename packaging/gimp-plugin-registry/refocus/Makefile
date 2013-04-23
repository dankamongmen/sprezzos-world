
CC=gcc

build: build-stamp
build-stamp:
	set -e; cd build; $(CC) *.c $(CFLAGS) `pkg-config --cflags gimp-2.0 gtk+-2.0` \
			        -I/usr/include/atlas \
				-L/usr/lib/libblas -L/usr/lib/lapack -lblas -llapack -lm \
	                        $(LDFLAGS) \
	                        `pkg-config --libs gimp-2.0 gtk+-2.0`  \
	                        -o refocus
	touch build-stamp

clean:
	rm -f build-stamp
	rm -f build/*.o build/refocus

install:
	install -m 755 build/refocus $(DESTDIR)$(PLUGINBINDIR)
	install -m 755 -d $(DESTDIR)$(DOCDIR)
	install -m 644 src/doc/doc.html $(DESTDIR)$(DOCDIR)
	install -m 644 src/doc/doc.pdf $(DESTDIR)$(DOCDIR)

.PHONY: clean
