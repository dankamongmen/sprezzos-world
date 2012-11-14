DESTDIR =
sysconfdir = /etc
prefix     = /usr
docdir     = $(prefix)/share/doc/initscripts
sbindir    = /sbin

INSTALL      = install -m755 -o root -g root
INSTALL_DATA = install -m644 -o root -g root

all:

install:
	$(INSTALL) -d $(DESTDIR)/lib/init/.
	$(INSTALL) -d $(DESTDIR)/run/.
	$(INSTALL) -d $(DESTDIR)/var/lib/initscripts/.
	$(INSTALL) -d $(DESTDIR)/var/lib/urandom/.
	$(INSTALL) -d $(DESTDIR)/var/log/fsck/.
	$(INSTALL) -d $(DESTDIR)$(docdir)/.
	$(INSTALL_DATA) doc/* $(DESTDIR)$(docdir)

	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/.
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/default
	cp -afv etc/* $(DESTDIR)$(sysconfdir)
	find $(DESTDIR)$(sysconfdir) -type d -name .svn -print0 |xargs -r0 rm -r

	cp -afv lib/init/* $(DESTDIR)/lib/init
	find $(DESTDIR)/lib -type d -name .svn  -print0 |xargs -r0 rm -r
	chmod 755 $(DESTDIR)$(sysconfdir)/init.d/[a-z]*
	chmod 755 $(DESTDIR)$(sysconfdir)/network/if-up.d/[a-z]*
	chmod 644 $(DESTDIR)/lib/init/*.sh
	chmod 644 $(DESTDIR)$(sysconfdir)/init.d/skeleton
	chmod -R g-w $(DESTDIR)
	chown -R root:root $(DESTDIR)

	$(INSTALL) -d $(DESTDIR)$(sbindir)/.
	$(INSTALL) sbin/fsck.nfs $(DESTDIR)$(sbindir)/fsck.nfs

	$(INSTALL) -d $(DESTDIR)/usr/share/man/man8
	$(INSTALL_DATA) man/fsck.nfs.8 \
		$(DESTDIR)/usr/share/man/man8/fsck.nfs.8

	$(INSTALL) -d $(DESTDIR)/usr/share/man/man5
	$(INSTALL_DATA) man/*.5 $(DESTDIR)/usr/share/man/man5/.
