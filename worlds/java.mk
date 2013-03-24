.PHONY: rhino
rhino:$(RHINO)_$(ARCH).deb
$(RHINO): $(SPREZZ)/rhino/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf rhino_$(rhino_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: jansi
jansi:$(JANSI)_$(ARCH).deb
$(JANSI): $(SPREZZ)/jansi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jansi-$(jansi_UPVER).tar.gz $(TARARGS) $@

.PHONY: junit
junit:$(JUNIT)_$(ARCH).deb
$(JUNIT): $(SPREZZ)/junit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf junit-$(junit_UPVER).tar.gz $(TARARGS) $@

.PHONY: ant
ant:$(ANT)_$(ARCH).deb
$(ANT): $(SPREZZ)/ant/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf apache-ant-$(ant_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: fastjar
fastjar:$(FASTJAR)_$(ARCH).deb
$(FASTJAR): $(SPREZZ)/fastjar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fastjar-$(fastjar_UPVER).tar.gz $(TARARGS) $@

.PHONY: libphonenumber
libphonenumber:$(LIBPHONENUMBER)_$(ARCH).deb
$(LIBPHONENUMBER): $(SPREZZ)/libphonenumber/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack

.PHONY: bsh
bsh:$(bsh)_$(ARCH).deb
$(bsh): $(SPREZZ)/bsh/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf bsh_$(bsh_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lucene-solr
lucene-solr:$(LUCENESOLR)_$(ARCH).deb
$(LUCENESOLR): $(SPREZZ)/lucene-solr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lucene-solr_$(lucene-solr_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libjgraphx-java
libjgraphx-java:$(libjgraphxjava)_$(ARCH).deb
$(libjgraphxjava): $(SPREZZ)/libjgraphx-java/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libjgraphx-java_$(libjgraphx-java_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libflexdock-java
libflexdock-java:$(LIBFLEXDOCKJAVA)_$(ARCH).deb
$(LIBFLEXDOCKJAVA): $(SPREZZ)/libflexdock-java/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libflexdock-java_$(libflexdock-java_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libjlatexmath-java
libjlatexmath-java:$(LIBJLATEXMATHJAVA)_$(ARCH).deb
$(LIBJLATEXMATHJAVA): $(SPREZZ)/libjlatexmath-java/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libjlatexmath-java_$(libjlatexmath-java_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: scirenderer
scirenderer:$(SCIRENDERER)_$(ARCH).deb
$(SCIRENDERER): $(SPREZZ)/scirenderer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scirenderer_$(scirenderer_UPVER).orig.tar.gz $(TARARGS) $@

