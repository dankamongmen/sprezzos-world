.PHONY: python-gevent
python-gevent:$(PYTHONGEVENT)_$(ARCH).deb
$(PYTHONGEVENT): $(SPREZZ)/python-gevent/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gevent-$(python-gevent_UPVER).tar.gz $(TARARGS) $@

.PHONY: python-greenlet
python-greenlet:$(PYTHONGREENLET)_$(ARCH).deb
$(PYTHONGREENLET): $(SPREZZ)/python-greenlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf python-greenlet_$(python-greenlet_UPVER).orig.tar.gz $(TARARGS) $@

