DESTDIR =
sysconfdir = /etc

INSTALL      = install -m755 -o root -g root

all:

install:
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/.
	cp -afv etc/* $(DESTDIR)$(sysconfdir)
	find $(DESTDIR)$(sysconfdir) -type d -name .svn -print0 |xargs -r0 rm -r

	chmod 755 $(DESTDIR)$(sysconfdir)/init.d/[a-z]*
	chmod -R g-w $(DESTDIR)
	chown -R root:root $(DESTDIR)
