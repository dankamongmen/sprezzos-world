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
python-netfilter:$(PYTHONNETFILTER)_$(ARCH).deb
$(PYTHONNETFILTER): $(SPREZZ)/python-netfilter/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-netfilter_$(python-netfilter_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nose
nose:$(NOSE)_$(ARCH).deb
$(NOSE): $(SPREZZ)/nose/debian/changelog
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

.PHONY: epydoc
epydoc:$(EPYDOC)_$(ARCH).deb
$(EPYDOC): $(SPREZZ)/epydoc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf epydoc_$(epydoc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: scour
scour:$(SCOUR)_$(ARCH).deb
$(SCOUR): $(SPREZZ)/scour/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf scour_$(scour_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-markdown
python-markdown:$(PYTHONMARKDOWN)_$(ARCH).deb
$(PYTHONMARKDOWN): $(SPREZZ)/python-markdown/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-markdown_$(python-markdown_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mako
mako:$(MAKO)_$(ARCH).deb
$(MAKO): $(SPREZZ)/mako/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mako_$(mako_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pygobject-2
pygobject-2:$(PYGOBJECT2)_$(ARCH).deb
$(PYGOBJECT2): $(SPREZZ)/pygobject-2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pygobject-2_$(pygobject-2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-soappy
python-soappy:$(PYTHONSOAPPY)_$(ARCH).deb
$(PYTHONSOAPPY): $(SPREZZ)/python-soappy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-soappy_$(python-soappy_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gnome-python
gnome-python:$(GNOMEPYTHON)_$(ARCH).deb
$(GNOMEPYTHON): $(SPREZZ)/gnome-python/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnome-python_$(gnome-python_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: lxml
lxml:$(LXML)_$(ARCH).deb
$(LXML): $(SPREZZ)/lxml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lxml_$(lxml_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pyppd
pyppd:$(PYPPD)_$(ARCH).deb
$(PYPPD): $(SPREZZ)/pyppd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pyppd_$(pyppd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-cups
python-cups:$(PYTHONCUPS)_$(ARCH).deb
$(PYTHONCUPS): $(SPREZZ)/python-cups/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf python-cups_$(python-cups_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: pysmbc
pysmbc:$(PYSMBC)_$(ARCH).deb
$(PYSMBC): $(SPREZZ)/pysmbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pysmbc_$(pysmbc_UPVER).orig.tar.bz2 $(TARARGS) $@

