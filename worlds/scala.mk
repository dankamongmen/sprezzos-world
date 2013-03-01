.PHONY: scala
scala:$(SCALA)_$(ARCH).deb
$(SCALA): $(SPREZZ)/scala/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scala-$(scala_UPVER).tar.gz $(TARARGS) $@

