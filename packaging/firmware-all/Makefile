FIRMWAREDIR = /lib/firmware

all:

install:
	install -d $(DESTDIR)/$(FIRMWARE) $(DESTDIR)/usr/share $(DESTDIR)/bin
	install -d $(DESTDIR)/sbin
	cp -rv etc/* $(DESTDIR)/etc
	cp -rv lib/firmware/* $(DESTDIR)/$(FIRMWARE)
	cp -rv usr/sbin/* $(DESTDIR)/sbin
	cp -rv usr/share/* $(DESTDIR)/usr/share
