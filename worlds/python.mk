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

.PHONY: markupsafe
markupsafe:$(MARKUPSAFE)_$(ARCH).deb
$(MARKUPSAFE): $(SPREZZ)/markupsafe/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MarkupSafe-$(markupsafe_UPVER).tar.gz $(TARARGS) $@

.PHONY: pythoncard
pythoncard:$(PYTHONCARD)_$(ARCH).deb
$(PYTHONCARD): $(SPREZZ)/pythoncard/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf PythonCard-$(pythoncard_UPVER).tar.gz $(TARARGS) $@

.PHONY: rdflib
rdflib:$(RDFLIB)_$(ARCH).deb
$(RDFLIB): $(SPREZZ)/rdflib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rdflib_$(rdflib_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-feedvalidator
python-feedvalidator:$(PYTHONFEEDVALIDATOR)_$(ARCH).deb
$(PYTHONFEEDVALIDATOR): $(SPREZZ)/python-feedvalidator/debian/changelog
	svn checkout http://feedvalidator.googlecode.com/svn/trunk/feedvalidator $@
	rm -rf $@/python
	tar cJf python-feedvalidator-$(python-feedvalidator_UPVER).tar.xz $@ --exclude-vcs
	ln -sf python-feedvalidator-$(python-feedvalidator_UPVER).tar.xz python-feedvalidator_$(python-feedvalidator_UPVER).orig.tar.xz
	cp -r $(<D) $@

.PHONY: pyatspi
pyatspi:$(PYATSPI)_$(ARCH).deb
$(PYATSPI): $(SPREZZ)/pyatspi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pyatspi-$(pyatspi_UPVER).tar.xz $(TARARGS) $@

.PHONY: mygpoclient
mygpoclient:$(MYGPOCLIENT)_$(ARCH).deb
$(MYGPOCLIENT): $(SPREZZ)/mygpoclient/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mygpoclient-$(mygpoclient_UPVER).tar.gz $(TARARGS) $@

.PHONY: seascope
seascope:$(SEASCOPE)_$(ARCH).deb
$(SEASCOPE): $(SPREZZ)/seascope/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf seascope_$(seascope_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pymad
pymad:$(PYMAD)_$(ARCH).deb
$(PYMAD): $(SPREZZ)/pymad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pymad_$(pymad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pyacoustid
pyacoustid:$(PYACOUSTID)_$(ARCH).deb
$(PYACOUSTID): $(SPREZZ)/pyacoustid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pyacoustid_$(pyacoustid_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: audioread
audioread:$(AUDIOREAD)_$(ARCH).deb
$(AUDIOREAD): $(SPREZZ)/audioread/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf audioread_$(audioread_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-netfilter
python-netfilter:$(pythonnetfilter)_$(ARCH).deb
$(pythonnetfilter): $(SPREZZ)/python-netfilter/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-netfilter_$(python-netfilter_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nose
nose:$(nose)_$(ARCH).deb
$(nose): $(SPREZZ)/nose/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nose_$(nose_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: musicbrainzngs
musicbrainzngs:$(MUSICBRAINZNGS)_$(ARCH).deb
$(MUSICBRAINZNGS): $(SPREZZ)/musicbrainzngs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf musicbrainzngs_$(musicbrainzngs_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mutagen
mutagen:$(MUTAGEN)_$(ARCH).deb
$(MUTAGEN): $(SPREZZ)/mutagen/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mutagen_$(mutagen_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-publicsuffix
python-publicsuffix:$(PYTHONPUBLICSUFFIX)_$(ARCH).deb
$(PYTHONPUBLICSUFFIX): $(SPREZZ)/python-publicsuffix/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-publicsuffix_$(python-publicsuffix_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: isodate
isodate:$(ISODATE)_$(ARCH).deb
$(ISODATE): $(SPREZZ)/isodate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf isodate_$(isodate_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cython
cython:$(CYTHON)_$(ARCH).deb
$(CYTHON): $(SPREZZ)/cython/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cython_$(cython_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-argcomplete
python-argcomplete:$(PYTHONARGCOMPLETE)_$(ARCH).deb
$(PYTHONARGCOMPLETE): $(SPREZZ)/python-argcomplete/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-argcomplete_$(python-argcomplete_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-wsgilog
python-wsgilog:$(PYTHONWSGILOG)_$(ARCH).deb
$(PYTHONWSGILOG): $(SPREZZ)/python-wsgilog/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-wsgilog_$(python-wsgilog_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-dvdvideo
python-dvdvideo:$(PYTHONDVDVIDEO)_$(ARCH).deb
$(PYTHONDVDVIDEO): $(SPREZZ)/python-dvdvideo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-dvdvideo_$(python-dvdvideo_UPVER).orig.tar.gz $(TARARGS) $@

