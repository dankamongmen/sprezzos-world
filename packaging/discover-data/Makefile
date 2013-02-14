# $Progeny$

prefix = /usr
datadir = $(prefix)/share
execdir = $(datadir)/tools
hwlistsdir = $(datadir)/discover

DESTDIR =

MAJOR=2
DATE=$(shell date +%Y.%m.%d)
VERSION=$(MAJOR).$(DATE)
DISTNAME=discover-data-$(VERSION)

#############################################################
# Kernel values used when generating the discover v1 data set
#############################################################
# Woody = 2.4.27, Sarge = n/a, Etch = n/a
KVERSION_24 = 2.4.27

# Woody = ?, Sarge = 2.6.8, Etch = 2.6.18
KVERSION_26 = 2.6.21

# Woody = 4.2.1 (XFree86), Sarge = 4.3.0 (XFree86), Etch = 7.1 (X.org)
XVER = 7.3
#############################################################

discover_data_EXEC = lst2xml merge-lst-to-xml mkshorthwlist reduce-xml xml2lst \
	discover-updater.pl trim26lst
hwlists_DATA = ata-busclass.xml \
               ata-device.xml \
               ata-vendor.xml \
               pci-busclass.xml \
               pci-device.xml \
               pci-device-deb.xml \
               pci-vendor.xml \
               pcmcia-busclass.xml \
               pcmcia-device.xml \
               pcmcia-vendor.xml \
               scsi-busclass.xml \
               scsi-device.xml \
               scsi-vendor.xml \
               usb-busclass.xml \
               usb-device.xml \
               usb-vendor.xml
GENERATED = list.xml
EXTRA_DIST = Makefile Makefile.update merged_vendor_list.xls \
	ChangeLog README list.xml.in di-kernel-list \
	merge-device-xml pci-ids2xml xml-device-sort.xsl
all: $(discover_data_EXEC) $(hwlists_DATA) $(GENERATED)

list.xml: list.xml.in
	sed s,@HWLISTSDIR@,$(hwlistsdir),g < list.xml.in > list.xml

shortxmllists: $(hwlists_DATA)
	./reduce-xml --class-spec=linux:2.4 --class-spec=linux:2.6 \
		--module-list=./di-kernel-list \
		-i pci-vendor.xml -i pci-busclass.xml -i pci-device.xml \
		-o pci-device-short.xml
	./reduce-xml --class-spec=linux:2.4 --class-spec=linux:2.6 \
		--module-list=./di-kernel-list \
		-i usb-vendor.xml -i usb-busclass.xml -i usb-device.xml \
		-o usb-device-short.xml
	./reduce-xml --class-spec=linux:2.4 --class-spec=linux:2.6 \
		--module-list=./di-kernel-list \
		-i pci-vendor.xml -i pci-busclass.xml -i pci-device.xml \
		-o pci-vendor-short.xml --output-type=vendor
	./reduce-xml --class-spec=linux:2.4 --class-spec=linux:2.6 \
		--module-list=./di-kernel-list \
		-i usb-vendor.xml -i usb-busclass.xml -i usb-device.xml \
		-o usb-vendor-short.xml --output-type=vendor
	./reduce-xml --class-spec=linux:2.4 --class-spec=linux:2.6 \
		--module-list=./di-kernel-list \
		-i scsi-vendor.xml -i scsi-busclass.xml -i scsi-device.xml \
		-o scsi-vendor-short.xml --output-type=vendor

pci.lst: Makefile pci-busclass-v1.xml pci-vendor.xml pci-device.xml
	./xml2lst --previous-file=pci.lst \
		--data-version=linux/module/name:$(KVERSION_24) \
		--data-version=xfree86/server/name:$(XVER) \
		--data-version=xfree86/server/device/driver:$(XVER) \
		-opci-new.lst \
		pci-busclass-v1.xml pci-vendor.xml pci-device.xml
	rm pci.lst
	mv pci-new.lst pci.lst

pci-26.lst: Makefile pci-busclass-v1.xml pci-vendor.xml pci-device.xml
	./xml2lst --previous-file=pci-26.lst \
		--data-version=linux/module/name:$(KVERSION_26) \
		--data-version=xfree86/server/name:2 \
		--data-version=xfree86/server/device/driver:2 \
		-opci-26-new.lst \
		pci-busclass-v1.xml pci-vendor.xml pci-device.xml
# Need to skip video entries, to make sure discover --xdriver find the
# entry in pci.lst
	./trim26lst < pci-26-new.lst > pci-26.lst
	rm pci-26-new.lst

pcmcia.lst: Makefile pcmcia-busclass-v1.xml pcmcia-vendor.xml pcmcia-device.xml
	./xml2lst --previous-file=pcmcia.lst \
		--data-version=linux/module/name:$(KVERSION_24) \
		--data-version=xfree86/server/name:$(XVER) \
		--data-version=xfree86/server/device/driver:$(XVER) \
		-opcmcia-new.lst \
		pcmcia-busclass-v1.xml pcmcia-vendor.xml pcmcia-device.xml
	rm pcmcia.lst
	mv pcmcia-new.lst pcmcia.lst

pcmcia-26.lst: Makefile pcmcia-busclass-v1.xml pcmcia-vendor.xml pcmcia-device.xml
	./xml2lst --previous-file=pcmcia-26.lst \
		--data-version=linux/module/name:$(KVERSION_26) \
		--data-version=xfree86/server/name:2 \
		--data-version=xfree86/server/device/driver:2 \
		-opcmcia-26-new.lst \
		pcmcia-busclass-v1.xml pcmcia-vendor.xml pcmcia-device.xml
	./trim26lst < pcmcia-26-new.lst > pcmcia-26.lst
	rm pcmcia-26-new.lst

usb.lst: Makefile usb-busclass-v1.xml usb-vendor.xml usb-device.xml
	./xml2lst --previous-file=usb.lst \
		--data-version=linux/module/name:$(KVERSION_24) \
		--data-version=xfree86/server/name:$(XVER) \
		--data-version=xfree86/server/device/driver:$(XVER) \
		-ousb-new.lst \
		usb-busclass-v1.xml usb-vendor.xml usb-device.xml
	rm usb.lst
	mv usb-new.lst usb.lst

usb-26.lst: Makefile usb-busclass-v1.xml usb-vendor.xml usb-device.xml
	./xml2lst --previous-file=usb-26.lst \
		--data-version=linux/module/name:$(KVERSION_26) \
		--data-version=xfree86/server/name:2 \
		--data-version=xfree86/server/device/driver:2 \
		-ousb-26-new.lst \
		usb-busclass-v1.xml usb-vendor.xml usb-device.xml
	./trim26lst < usb-26-new.lst > usb-26.lst
	rm usb-26-new.lst

install: $(hwlists_DATA) $(GENERATED)
	install -m 755 -d $(DESTDIR)$(hwlistsdir)
	install -m 644 $(hwlists_DATA) $(GENERATED) $(DESTDIR)$(hwlistsdir)
	install -m 755 -d $(DESTDIR)$(execdir)
	install -m 755 $(discover_data_EXEC) $(DESTDIR)$(execdir)

uninstall:
	for i in $(hwlists_DATA) $(GENERATED); do rm $(DESTDIR)$(hwlistsdir)/$$i; done
	for i in $(discover_data_EXEC); do rm $(DESTDIR)$(execdir)/$$i; done
	-rmdir $(DESTDIR)$(hwlistsdir)
	-rmdir $(DESTDIR)$(execdir)

dist: clean
	rm -rf $(DISTNAME)
	mkdir $(DISTNAME)
	cp $(hwlists_DATA) \
		$(discover_data_EXEC) $(EXTRA_DIST) $(DISTNAME)
	tar cf - $(DISTNAME) | gzip -9 > $(DISTNAME).tar.gz
	md5sum $(DISTNAME).tar.gz > $(DISTNAME).tar.gz.md5sum
	rm -rf $(DISTNAME)

clean::
	rm -f *.xml-short
	rm -f list.xml
	rm -f *~

distclean: clean

.PHONY: all install uninstall dist
