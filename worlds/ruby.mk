.PHONY: ruby1.8
ruby1.8:$(RUBY1.8)_$(ARCH).deb
$(RUBY1.8): $(SPREZZ)/ruby1.8/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby1.8_$(ruby1.8_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby1.9.1
ruby1.9.1:$(RUBY1.9.1)_$(ARCH).deb
$(RUBY1.9.1): $(SPREZZ)/ruby1.9.1/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby1.9.1_$(ruby1.9.1_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-levenshtein
ruby-levenshtein:$(RUBYLEVENSHTEIN)_$(ARCH).deb
$(RUBYLEVENSHTEIN): $(SPREZZ)/ruby-levenshtein/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-levenshtein_$(ruby-levenshtein_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-xpath
ruby-xpath:$(RUBYXPATH)_$(ARCH).deb
$(RUBYXPATH): $(SPREZZ)/ruby-xpath/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-xpath_$(ruby-xpath_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: rake
rake:$(RAKE)_$(ARCH).deb
$(RAKE): $(SPREZZ)/rake/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rake_$(rake_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-minitest
ruby-minitest:$(RUBYMINITEST)_$(ARCH).deb
$(RUBYMINITEST): $(SPREZZ)/ruby-minitest/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-minitest_$(ruby-minitest_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: rubygems
rubygems:$(RUBYGEMS)_$(ARCH).deb
$(RUBYGEMS): $(SPREZZ)/rubygems/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rubygems_$(rubygems_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-flexmock
ruby-flexmock:$(RUBYFLEXMOCK)_$(ARCH).deb
$(RUBYFLEXMOCK): $(SPREZZ)/ruby-flexmock/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-flexmock_$(ruby-flexmock_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-session
ruby-session:$(RUBYSESSION)_$(ARCH).deb
$(RUBYSESSION): $(SPREZZ)/ruby-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-session_$(ruby-session_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-rspec
ruby-rspec:$(RUBYRSPEC)_$(ARCH).deb
$(RUBYRSPEC): $(SPREZZ)/ruby-rspec/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-rspec_$(ruby-rspec_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-nokogiri
ruby-nokogiri:$(RUBYNOKOGIRI)_$(ARCH).deb
$(RUBYNOKOGIRI): $(SPREZZ)/ruby-nokogiri/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-nokogiri_$(ruby-nokogiri_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-ox
ruby-ox:$(RUBYOX)_$(ARCH).deb
$(RUBYOX): $(SPREZZ)/ruby-ox/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-ox_$(ruby-ox_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-ref
ruby-ref:$(RUBYREF)_$(ARCH).deb
$(RUBYREF): $(SPREZZ)/ruby-ref/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-ref_$(ruby-ref_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-bluefeather
ruby-bluefeather:$(RUBYBLUEFEATHER)_$(ARCH).deb
$(RUBYBLUEFEATHER): $(SPREZZ)/ruby-bluefeather/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-bluefeather_$(ruby-bluefeather_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-configurate
ruby-configurate:$(RUBYCONFIGURATE)_$(ARCH).deb
$(RUBYCONFIGURATE): $(SPREZZ)/ruby-configurate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-configurate_$(ruby-configurate_UPVER).orig.tar.gz $(TARARGS) $@

