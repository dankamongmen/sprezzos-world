all::
	docbook-to-man apt-listchanges.sgml > apt-listchanges.1
	docbook-to-man apt-listchanges.es.sgml > apt-listchanges.es.1
	docbook-to-man apt-listchanges.fr.sgml > apt-listchanges.fr.1

install:: all
	# modules
	install -d $(DESTDIR)/usr/share/apt-listchanges
	install -m 644 apt-listchanges/* $(DESTDIR)/usr/share/apt-listchanges
	# config
	install -d $(DESTDIR)/etc/apt/apt.conf.d
	install -m 644 debian/apt.conf $(DESTDIR)/etc/apt/apt.conf.d/20listchanges
	# exe
	install -d $(DESTDIR)/usr/bin
	install -m 755 apt-listchanges.py $(DESTDIR)/usr/bin/apt-listchanges
	# man pages
	for man in apt-listchanges*.1; do \
	    lang=`echo $$man | sed -e 's/apt-listchanges\.*// ; s/\.*1//'`; \
	    install -d $(DESTDIR)/usr/share/man/$$lang/man1;                \
	    install -m 644 $$man $(DESTDIR)/usr/share/man/$$lang/man1/apt-listchanges.1; \
	done

clean::
	rm -f apt-listchanges*.1
	rm -f apt-listchanges/*.pyc

all install clean update-po::
	$(MAKE) -C po $@
