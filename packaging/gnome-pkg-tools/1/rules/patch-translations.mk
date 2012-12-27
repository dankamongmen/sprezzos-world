# Helper rules to translate new strings introduced in patches
# The relevant patches must be listed by hand in debian/po-up/POTFILES.in

clean::
	rm -f debian/stamp-patch-translations
	rm -f po/patched.pot
	-mv po.saved/*.po po/
	if [ -d po.saved ]; then rmdir po.saved; fi

infiles = $(shell sed 's/^\[.*\]//' debian/po-up/POTFILES.in)

update-po::
	cd debian && for i in $(infiles); do \
		if [ $${i##*.} = patch ]; then \
			mv $$i $$i.saved ; \
			sed -n 's/^\+//p' $$i.saved > $$i ; \
		fi; done
	cd debian/po-up && intltool-update --pot -g patches
	cd debian && for i in $(infiles); do \
		if [ -f $$i.saved ]; then \
			mv $$i.saved $$i ; \
		fi; done
	cd debian/po-up && for i in *.po; do \
		if [ -f $$i ]; then \
			intltool-update -g patches --dist $${i%.po}; \
		fi; done

post-patches:: debian/stamp-patch-translations
debian/stamp-patch-translations:
	mkdir -p po.saved
	cd po && intltool-update --pot -g patched
	cd debian/po-up && for i in *.po; do \
		if [ -f $$i ]; then \
			cp -p ../../po/$$i ../../po.saved/ ; \
			msgmerge --backup=none -U -C $$i ../../po/$$i ../../po/patched.pot ; \
		fi; done
	touch $@
