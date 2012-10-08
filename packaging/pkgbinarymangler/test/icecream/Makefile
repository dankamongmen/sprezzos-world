default:

install:
	# install mo files
	for DOMAIN in vanilla chocolate; do \
	    for f in po-$$DOMAIN/*.po; do \
	        D=$$DESTDIR/usr/share/locale/`basename $$f .po`/LC_MESSAGES/; \
	        install -d -m 755 $$D; \
	        msgfmt -o $$D/$$DOMAIN.mo $$f; \
	    done; \
	done

	install -D -m 600 foo%.png $$DESTDIR/usr/share/icecream/foo%.png
	ln -s foo%.png $$DESTDIR/usr/share/icecream/foolink.png
	install -D -m 644 notapng.png $$DESTDIR/usr/share/icecream/notapng.png

	install -d $$DESTDIR/usr/share/applications/ $$DESTDIR/usr/lib/bonobo/servers/ $$DESTDIR/usr/share/polkit-1/actions/ $$DESTDIR/usr/share/gconf/schemas
	install -m 644 data/*.desktop $$DESTDIR/usr/share/applications/
	install -m 644 data/*.server $$DESTDIR/usr/lib/bonobo/servers/
	install -m 644 data/*.policy $$DESTDIR/usr/share/polkit-1/actions/
	install -m 644 data/*.schemas $$DESTDIR/usr/share/gconf/schemas

clean:
	rm -f test

.PHONY: clean
