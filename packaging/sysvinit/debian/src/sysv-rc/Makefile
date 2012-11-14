DESTDIR =
sysconfdir = /etc
prefix     = /usr
docdir     = $(prefix)/share/doc/sysv-rc
bindir     = $(prefix)/bin
sbindir    = $(prefix)/sbin

INSTALL      = install -m755 -o root -g root
INSTALL_DATA = install -m644 -o root -g root

all:

install:
	$(INSTALL) -d $(DESTDIR)$(docdir)/.
	$(INSTALL) -d $(DESTDIR)/var/lib/insserv
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/.
	$(INSTALL_DATA) doc/* $(DESTDIR)$(docdir)/.
	cp -af etc/* $(DESTDIR)$(sysconfdir)
	find $(DESTDIR)$(sysconfdir) -type d -name .svn -print0 |xargs -r0 rm -r

	for N in 2 3 4 5 ; do \
		$(INSTALL) -d $(DESTDIR)$(sysconfdir)/rc$${N}.d ; \
		$(INSTALL_DATA) rc2-5.d-README \
			$(DESTDIR)$(sysconfdir)/rc$${N}.d/README ; \
	done
	chmod 755 $(DESTDIR)$(sysconfdir)/init.d/[a-z]*
	chmod 644 $(DESTDIR)$(sysconfdir)/init.d/README
	chmod -R go=u-w $(DESTDIR)$(sysconfdir)
	chown -R root:root $(DESTDIR)$(sysconfdir)

	$(INSTALL) -d $(DESTDIR)/usr/share/man/man8/.
	$(INSTALL_DATA) man8/*.8 $(DESTDIR)/usr/share/man/man8
#	$(INSTALL) -d $(DESTDIR)/usr/share/man/ja/man8/.
#	$(INSTALL_DATA) man8/ja/*.8 $(DESTDIR)/usr/share/man/ja/man8
#	$(INSTALL) -d $(DESTDIR)/usr/share/man/fr.UTF-8/man8/.
#	$(INSTALL_DATA) man8/fr.UTF-8/*.8 $(DESTDIR)/usr/share/man/fr.UTF-8/man8
#	$(INSTALL) -d $(tmp)/usr/share/man/es/man8/.
#	$(INSTALL_DATA) man8/es/*.8 $(DESTDIR)/usr/share/man/es/man8

	$(INSTALL) -d $(DESTDIR)$(sbindir)/.
	$(INSTALL) sbin/invoke-rc.d $(DESTDIR)$(sbindir)
	$(INSTALL) sbin/update-rc.d $(DESTDIR)$(sbindir)

	$(INSTALL) -d $(DESTDIR)/usr/share/sysv-rc/.
	$(INSTALL) saveconfig $(DESTDIR)/usr/share/sysv-rc/saveconfig
