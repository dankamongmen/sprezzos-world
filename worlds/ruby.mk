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

.PHONY: ruby-riot
ruby-riot:$(RUBYRIOT)_$(ARCH).deb
$(RUBYRIOT): $(SPREZZ)/ruby-riot/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-riot_$(ruby-riot_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-multipart-parser
ruby-multipart-parser:$(RUBYMULTIPARTPARSER)_$(ARCH).deb
$(RUBYMULTIPARTPARSER): $(SPREZZ)/ruby-multipart-parser/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-multipart-parser_$(ruby-multipart-parser_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-rack-mobile-detect
ruby-rack-mobile-detect:$(RUBYRACKMOBILEDETECT)_$(ARCH).deb
$(RUBYRACKMOBILEDETECT): $(SPREZZ)/ruby-rack-mobile-detect/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-rack-mobile-detect_$(ruby-rack-mobile-detect_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-rack-openid
ruby-rack-openid:$(RUBYRACKOPENID)_$(ARCH).deb
$(RUBYRACKOPENID): $(SPREZZ)/ruby-rack-openid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-rack-openid_$(ruby-rack-openid_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-rr
ruby-rr:$(RUBYRR)_$(ARCH).deb
$(RUBYRR): $(SPREZZ)/ruby-rr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-rr_$(ruby-rr_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-shoulda
ruby-shoulda:$(RUBYSHOULDA)_$(ARCH).deb
$(RUBYSHOULDA): $(SPREZZ)/ruby-shoulda/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-shoulda_$(ruby-shoulda_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-rack
ruby-rack:$(RUBYRACK)_$(ARCH).deb
$(RUBYRACK): $(SPREZZ)/ruby-rack/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-rack_$(ruby-rack_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-bacon
ruby-bacon:$(RUBYBACON)_$(ARCH).deb
$(RUBYBACON): $(SPREZZ)/ruby-bacon/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-bacon_$(ruby-bacon_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-memcache-client
ruby-memcache-client:$(RUBYMEMCACHECLIENT)_$(ARCH).deb
$(RUBYMEMCACHECLIENT): $(SPREZZ)/ruby-memcache-client/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-memcache-client_$(ruby-memcache-client_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: thin
thin:$(THIN)_$(ARCH).deb
$(THIN): $(SPREZZ)/thin/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf thin_$(thin_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-eventmachine
ruby-eventmachine:$(RUBYEVENTMACHINE)_$(ARCH).deb
$(RUBYEVENTMACHINE): $(SPREZZ)/ruby-eventmachine/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-eventmachine_$(ruby-eventmachine_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-daemons
ruby-daemons:$(RUBYDAEMONS)_$(ARCH).deb
$(RUBYDAEMONS): $(SPREZZ)/ruby-daemons/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-daemons_$(ruby-daemons_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ruby-spoon
ruby-spoon:$(RUBYSPOON)_$(ARCH).deb
$(RUBYSPOON): $(SPREZZ)/ruby-spoon/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ruby-spoon_$(ruby-spoon_UPVER).orig.tar.gz $(TARARGS) $@

