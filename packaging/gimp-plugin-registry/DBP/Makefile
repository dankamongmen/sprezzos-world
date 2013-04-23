

build: build-stamp
build-stamp:
	if dpkg --compare-versions $(GIMPVER) '>=' 2.3; then \
	    make -C src CPPFLAGS="$(CFLAGS)" NODEPS="" dbp  ;\
	else \
	    make -C src CPPFLAGS="$(CFLAGS)" dbp ;\
	fi
	touch build-stamp

clean:
	rm -f build-stamp
	-make -C src clean

install:
	install -m 755 src/dbp $(DESTDIR)$(PLUGINBINDIR)
	mkdir -p $(DESTDIR)$(DOCDIR)
	install -m 644 src/dbp.html src/dbp-main.png src/dbp-fileSelector.png $(DESTDIR)$(DOCDIR)


.PHONY: clean
