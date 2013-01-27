DOMAIN=debian-tasks
TASKDESC=$(DOMAIN).desc
TASKDIR=/usr/share/tasksel
DESCDIR=tasks
DESCPO=$(DESCDIR)/po
VERSION=$(shell expr "`dpkg-parsechangelog 2>/dev/null |grep Version:`" : '.*Version: \(.*\)' | cut -d - -f 1)
LANGS=ar bg bn bs ca cs cy da de dz el eo es et eu fa fi fr gl gu he hi hr hu hy id it ja km ko lt lv mg mk nb ne nl nn pa pl pt_BR pt ro ru sk sl sq sv ta te th tl tr uk vi wo zh_CN zh_TW
LANGS_DESC=ar bg bn bs ca cs cy da de dz el eo es et et eu fi fr gl gu he hi hr hu id it ja km ko lt lv mg mk nb ne nl nn pa pl pt_BR pt ro ru sk sl sq sv te th tl tr uk vi wo zh_CN zh_TW
LOCALEDIR=$(DESTDIR)/usr/share/locale

all: $(TASKDESC) $(DESCPO)/build_stamp po/build_stamp

$(TASKDESC): makedesc.pl $(DESCDIR)/[a-z]??*
	./doincludes.pl $(DESCDIR)
	./makedesc.pl $(DESCDIR) $(TASKDESC)

%.o: %.c
	$(COMPILE) $<

po/build_stamp:
	$(MAKE) -C po LANGS="$(LANGS)"

updatepo:
	$(MAKE) -C po update LANGS="$(LANGS)"

$(DESCPO)/build_stamp:
	$(MAKE) -C $(DESCPO) LANGS="$(LANGS_DESC)"

updatetaskspo:
	$(MAKE) -C $(DESCPO) update LANGS="$(LANGS_DESC)"

install:
	install -d $(DESTDIR)/usr/bin \
		$(DESTDIR)/usr/lib/tasksel/tests \
		$(DESTDIR)/usr/lib/tasksel/packages \
		$(DESTDIR)/usr/share/man/man8
	install -m 755 tasksel.pl $(DESTDIR)/usr/bin/tasksel
	install -m 755 tasksel-debconf $(DESTDIR)/usr/lib/tasksel/
	install -m 755 tests/new-install $(DESTDIR)/usr/lib/tasksel/tests/
	install -m 755 tests/debconf $(DESTDIR)/usr/lib/tasksel/tests/
	install -m 755 tests/lang $(DESTDIR)/usr/lib/tasksel/tests/
	install -m 755 packages/list $(DESTDIR)/usr/lib/tasksel/packages/
	pod2man --section=8 --center "Debian specific manpage" --release $(VERSION) tasksel.pod | gzip -9c > $(DESTDIR)/usr/share/man/man8/tasksel.8.gz
	for lang in $(LANGS); do \
		[ ! -d $(LOCALEDIR)/$$lang/LC_MESSAGES/ ] && mkdir -p $(LOCALEDIR)/$$lang/LC_MESSAGES/; \
		install -m 644 po/$$lang.mo $(LOCALEDIR)/$$lang/LC_MESSAGES/tasksel.mo; \
	done

install-data:
	install -d $(DESTDIR)$(TASKDIR)/descs \
		$(DESTDIR)/usr/lib/tasksel/tests
	install -m 0644 $(TASKDESC) $(DESTDIR)$(TASKDIR)/descs
	for test in tests/*; do \
		[ "$$test" = "tests/new-install" ] && continue; \
		[ "$$test" = "tests/debconf" ] && continue; \
		[ "$$test" = "tests/lang" ] && continue; \
		install -m 755 $$test $(DESTDIR)/usr/lib/tasksel/tests/; \
	done
	for package in packages/*; do \
		[ "$$package" = "packages/list" ] && continue; \
		install -m 755 $$package $(DESTDIR)/usr/lib/tasksel/packages/; \
	done
	for lang in $(LANGS_DESC); do \
		[ ! -d $(LOCALEDIR)/$$lang/LC_MESSAGES/ ] && mkdir -p $(LOCALEDIR)/$$lang/LC_MESSAGES/; \
		install -m 644 tasks/po/$$lang.mo $(LOCALEDIR)/$$lang/LC_MESSAGES/$(DOMAIN).mo; \
	done

clean:
	rm -f $(TASKDESC) *~
	$(MAKE) -C po clean
	$(MAKE) -C $(DESCPO) clean
