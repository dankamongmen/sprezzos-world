.PHONY: erlang
erlang:$(ERLANG)_$(ARCH).deb
$(ERLANG): $(SPREZZ)/erlang/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf erlang_$(erlang_UPVER).orig.tar.gz $(TARARGS) $@

