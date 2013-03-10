.PHONY: cpphs
cpphs:$(CPPHS)_$(ARCH).deb
$(CPPHS): $(SPREZZ)/cpphs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpphs-$(cpphs_UPVER).tar.gz $(TARARGS) $@

.PHONY: ghc
ghc:$(GHC)_$(ARCH).deb
$(GHC): $(SPREZZ)/ghc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ghc-$(ghc_UPVER)-src.tar.bz2 $(TARARGS) $@

.PHONY: haskell-aes
haskell-aes:$(HASKELLAES)_$(ARCH).deb
$(HASKELLAES): $(SPREZZ)/haskell-aes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-aes-$(haskell-aes_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-ansi-terminal
haskell-ansi-terminal:$(HASKELLANSITERMINAL)_$(ARCH).deb
$(HASKELLANSITERMINAL): $(SPREZZ)/haskell-ansi-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ansi-terminal-$(haskell-ansi-terminal_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-ansi-wl-pprint
haskell-ansi-wl-pprint:$(HASKELLANSIWLPPRINT)_$(ARCH).deb
$(HASKELLANSIWLPPRINT): $(SPREZZ)/haskell-ansi-wl-pprint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ansi-wl-pprint-$(haskell-ansi-wl-pprint_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-asn1-data
haskell-asn1-data:$(HASKELLASN1DATA)_$(ARCH).deb
$(HASKELLASN1DATA): $(SPREZZ)/haskell-asn1-data/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf asn1-data-$(haskell-asn1-data_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec
haskell-attoparsec:$(HASKELLATTOPARSEC)_$(ARCH).deb
$(HASKELLATTOPARSEC): $(SPREZZ)/haskell-attoparsec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-$(haskell-attoparsec_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec-conduit
haskell-attoparsec-conduit:$(HASKELLATTOPARSECCONDUIT)_$(ARCH).deb
$(HASKELLATTOPARSECCONDUIT): $(SPREZZ)/haskell-attoparsec-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-conduit-$(haskell-attoparsec-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec-enumerator
haskell-attoparsec-enumerator:$(HASKELLATTOPARSECENUMERATOR)_$(ARCH).deb
$(HASKELLATTOPARSECENUMERATOR): $(SPREZZ)/haskell-attoparsec-enumerator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-enumerator-$(haskell-attoparsec-enumerator_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-base-unicode-symbols
haskell-base-unicode-symbols:$(HASKELLBASEUNICODESYMBOLS)_$(ARCH).deb
$(HASKELLBASEUNICODESYMBOLS): $(SPREZZ)/haskell-base-unicode-symbols/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base-unicode-symbols-$(haskell-base-unicode-symbols_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-base64-bytestring
haskell-base64-bytestring:$(HASKELLBASE64BYTESTRING)_$(ARCH).deb
$(HASKELLBASE64BYTESTRING): $(SPREZZ)/haskell-base64-bytestring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base64-bytestring-$(haskell-base64-bytestring_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-builder
haskell-blaze-builder:$(HASKELLBLAZEBUILDER)_$(ARCH).deb
$(HASKELLBLAZEBUILDER): $(SPREZZ)/haskell-blaze-builder/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-builder-$(haskell-blaze-builder_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-html
haskell-blaze-html:$(HASKELLBLAZEHTML)_$(ARCH).deb
$(HASKELLBLAZEHTML): $(SPREZZ)/haskell-blaze-html/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-html-$(haskell-blaze-html_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-markup
haskell-blaze-markup:$(HASKELLBLAZEMARKUP)_$(ARCH).deb
$(HASKELLBLAZEMARKUP): $(SPREZZ)/haskell-blaze-markup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-markup-$(haskell-blaze-markup_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-bloomfilter
haskell-bloomfilter:$(HASKELLBLOOMFILTER)_$(ARCH).deb
$(HASKELLBLOOMFILTER): $(SPREZZ)/haskell-bloomfilter/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bloomfilter-$(haskell-bloomfilter_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-case-insensitive
haskell-case-insensitive:$(HASKELLCASEINSENSITIVE)_$(ARCH).deb
$(HASKELLCASEINSENSITIVE): $(SPREZZ)/haskell-case-insensitive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf case-insensitive-$(haskell-case-insensitive_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cereal
haskell-cereal:$(HASKELLCEREAL)_$(ARCH).deb
$(HASKELLCEREAL): $(SPREZZ)/haskell-cereal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cereal-$(haskell-cereal_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-certificate
haskell-certificate:$(HASKELLCERTIFICATE)_$(ARCH).deb
$(HASKELLCERTIFICATE): $(SPREZZ)/haskell-certificate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf certificate-$(haskell-certificate_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-clientsession
haskell-clientsession:$(HASKELLCLIENTSESSION)_$(ARCH).deb
$(HASKELLCLIENTSESSION): $(SPREZZ)/haskell-clientsession/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clientsession-$(haskell-clientsession_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-conduit
haskell-conduit:$(HASKELLCONDUIT)_$(ARCH).deb
$(HASKELLCONDUIT): $(SPREZZ)/haskell-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conduit-$(haskell-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cookie
haskell-cookie:$(HASKELLCOOKIE)_$(ARCH).deb
$(HASKELLCOOKIE): $(SPREZZ)/haskell-cookie/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cookie-$(haskell-cookie_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cprng-aes
haskell-cprng-aes:$(HASKELLCPRNGAES)_$(ARCH).deb
$(HASKELLCPRNGAES): $(SPREZZ)/haskell-cprng-aes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cprng-aes-$(haskell-cprng-aes_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto
haskell-crypto:$(HASKELLCRYPTO)_$(ARCH).deb
$(HASKELLCRYPTO): $(SPREZZ)/haskell-crypto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypto-$(haskell-crypto_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cpu
haskell-cpu:$(HASKELLCPU)_$(ARCH).deb
$(HASKELLCPU): $(SPREZZ)/haskell-cpu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpu-$(haskell-cpu_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cryptocipher
haskell-cryptocipher:$(HASKELLCRYPTOCIPHER)_$(ARCH).deb
$(HASKELLCRYPTOCIPHER): $(SPREZZ)/haskell-cryptocipher/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cryptocipher-$(haskell-cryptocipher_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cryptohash
haskell-cryptohash:$(HASKELLCRYPTOHASH)_$(ARCH).deb
$(HASKELLCRYPTOHASH): $(SPREZZ)/haskell-cryptohash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cryptohash-$(haskell-cryptohash_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-api
haskell-crypto-api:$(HASKELLCRYPTOAPI)_$(ARCH).deb
$(HASKELLCRYPTOAPI): $(SPREZZ)/haskell-crypto-api/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-api-$(haskell-crypto-api_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-numbers
haskell-crypto-numbers:$(HASKELLCRYPTONUMBERS)_$(ARCH).deb
$(HASKELLCRYPTONUMBERS): $(SPREZZ)/haskell-crypto-numbers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-numbers-$(haskell-crypto-numbers_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-pubkey
haskell-crypto-pubkey:$(HASKELLCRYPTOPUBKEY)_$(ARCH).deb
$(HASKELLCRYPTOPUBKEY): $(SPREZZ)/haskell-crypto-pubkey/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-pubkey-$(haskell-crypto-pubkey_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-pubkey-types
haskell-crypto-pubkey-types:$(HASKELLCRYPTOPUBKEYTYPES)_$(ARCH).deb
$(HASKELLCRYPTOPUBKEYTYPES): $(SPREZZ)/haskell-crypto-pubkey-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-pubkey-types-$(haskell-crypto-pubkey-types_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-random-api
haskell-crypto-random-api:$(HASKELLCRYPTORANDOMAPI)_$(ARCH).deb
$(HASKELLCRYPTORANDOMAPI): $(SPREZZ)/haskell-crypto-random-api/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-random-api-$(haskell-crypto-random-api_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-data-default
haskell-data-default:$(HASKELLDATADEFAULT)_$(ARCH).deb
$(HASKELLDATADEFAULT): $(SPREZZ)/haskell-data-default/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-default-$(haskell-data-default_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-dataenc
haskell-dataenc:$(HASKELLDATAENC)_$(ARCH).deb
$(HASKELLDATAENC): $(SPREZZ)/haskell-dataenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dataenc-$(haskell-dataenc_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-dlist
haskell-dlist:$(HASKELLDLIST)_$(ARCH).deb
$(HASKELLDLIST): $(SPREZZ)/haskell-dlist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dlist-$(haskell-dlist_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-doctest
haskell-doctest:$(HASKELLDOCTEST)_$(ARCH).deb
$(HASKELLDOCTEST): $(SPREZZ)/haskell-doctest/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf doctest-$(haskell-doctest_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-edit-distance
haskell-edit-distance:$(HASKELLEDITDISTANCE)_$(ARCH).deb
$(HASKELLEDITDISTANCE): $(SPREZZ)/haskell-edit-distance/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf edit-distance-$(haskell-edit-distance_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-entropy
haskell-entropy:$(HASKELLENTROPY)_$(ARCH).deb
$(HASKELLENTROPY): $(SPREZZ)/haskell-entropy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf entropy-$(haskell-entropy_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-enumerator
haskell-enumerator:$(HASKELLENUMERATOR)_$(ARCH).deb
$(HASKELLENUMERATOR): $(SPREZZ)/haskell-enumerator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf enumerator-$(haskell-enumerator_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-extensible-exceptions
haskell-extensible-exceptions:$(HASKELLEXTENSIBLEEXCEPTIONS)_$(ARCH).deb
$(HASKELLEXTENSIBLEEXCEPTIONS): $(SPREZZ)/haskell-extensible-exceptions/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf extensible-exceptions-$(haskell-extensible-exceptions_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-failure
haskell-failure:$(HASKELLFAILURE)_$(ARCH).deb
$(HASKELLFAILURE): $(SPREZZ)/haskell-failure/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf failure-$(haskell-failure_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-fast-logger
haskell-fast-logger:$(HASKELLFASTLOGGER)_$(ARCH).deb
$(HASKELLFASTLOGGER): $(SPREZZ)/haskell-fast-logger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fast-logger-$(haskell-fast-logger_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-ghc-paths
haskell-ghc-paths:$(HASKELLGHCPATHS)_$(ARCH).deb
$(HASKELLGHCPATHS): $(SPREZZ)/haskell-ghc-paths/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-paths-$(haskell-ghc-paths_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-gsasl
haskell-gsasl:$(HASKELLGSASL)_$(ARCH).deb
$(HASKELLGSASL): $(SPREZZ)/haskell-gsasl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gsasl-$(haskell-gsasl_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hamlet
haskell-hamlet:$(HASKELLHAMLET)_$(ARCH).deb
$(HASKELLHAMLET): $(SPREZZ)/haskell-hamlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hamlet-$(haskell-hamlet_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hashable
haskell-hashable:$(HASKELLHASHABLE)_$(ARCH).deb
$(HASKELLHASHABLE): $(SPREZZ)/haskell-hashable/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hashable-$(haskell-hashable_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hinotify
haskell-hinotify:$(HASKELLHINOTIFY)_$(ARCH).deb
$(HASKELLHINOTIFY): $(SPREZZ)/haskell-hinotify/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hinotify-$(haskell-hinotify_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hostname
haskell-hostname:$(HASKELLHOSTNAME)_$(ARCH).deb
$(HASKELLHOSTNAME): $(SPREZZ)/haskell-hostname/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hostname-$(haskell-hostname_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hsh
haskell-hsh:$(HASKELLHSH)_$(ARCH).deb
$(HASKELLHSH): $(SPREZZ)/haskell-hsh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HSH-$(haskell-hsh_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hslogger
haskell-hslogger:$(HASKELLHSLOGGER)_$(ARCH).deb
$(HASKELLHSLOGGER): $(SPREZZ)/haskell-hslogger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hslogger-$(haskell-hslogger_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hslogger
haskell-hspec:$(HASKELLHSPEC)_$(ARCH).deb
$(HASKELLHSPEC): $(SPREZZ)/haskell-hspec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hspec-$(haskell-hspec_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hspec-expectations
haskell-hspec-expectations:$(HASKELLHSPECEXPECTATIONS)_$(ARCH).deb
$(HASKELLHSPECEXPECTATIONS): $(SPREZZ)/haskell-hspec-expectations/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hspec-expectations-$(haskell-hspec-expectations_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-http
haskell-http:$(HASKELLHTTP)_$(ARCH).deb
$(HASKELLHTTP): $(SPREZZ)/haskell-http/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HTTP-$(haskell-http_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-http-conduit
haskell-http-conduit:$(HASKELLHTTPCONDUIT)_$(ARCH).deb
$(HASKELLHTTPCONDUIT): $(SPREZZ)/haskell-http-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf http-conduit-$(haskell-http-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-http-types
haskell-http-types:$(HASKELLHTTPTYPES)_$(ARCH).deb
$(HASKELLHTTPTYPES): $(SPREZZ)/haskell-http-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf http-types-$(haskell-http-types_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hunit
haskell-hunit:$(HASKELLHUNIT)_$(ARCH).deb
$(HASKELLHUNIT): $(SPREZZ)/haskell-hunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HUnit-$(haskell-hunit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hxt
haskell-hxt:$(HASKELLHXT)_$(ARCH).deb
$(HASKELLHXT): $(SPREZZ)/haskell-hxt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-$(haskell-hxt_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-json
haskell-json:$(HASKELLJSON)_$(ARCH).deb
$(HASKELLJSON): $(SPREZZ)/haskell-json/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf json-$(haskell-json_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-largeword
haskell-largeword:$(HASKELLLARGEWORD)_$(ARCH).deb
$(HASKELLLARGEWORD): $(SPREZZ)/haskell-largeword/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf largeword-$(haskell-largeword_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-lens
haskell-lens:$(HASKELLLENS)_$(ARCH).deb
$(HASKELLLENS): $(SPREZZ)/haskell-lens/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lens-$(haskell-lens_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-libxml-sax
haskell-libxml-sax:$(HASKELLLIBXMLSAX)_$(ARCH).deb
$(HASKELLLIBXMLSAX): $(SPREZZ)/haskell-libxml-sax/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-sax-$(haskell-libxml-sax_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-lifted-base
haskell-lifted-base:$(HASKELLLIFTEDBASE)_$(ARCH).deb
$(HASKELLLIFTEDBASE): $(SPREZZ)/haskell-lifted-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lifted-base-$(haskell-lifted-base_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-missingh
haskell-missingh:$(HASKELLMISSINGH)_$(ARCH).deb
$(HASKELLMISSINGH): $(SPREZZ)/haskell-missingh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MissingH-$(haskell-missingh_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-control
haskell-monad-control:$(HASKELLMONADCONTROL)_$(ARCH).deb
$(HASKELLMONADCONTROL): $(SPREZZ)/haskell-monad-control/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-control-$(haskell-monad-control_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-monads-tf
haskell-monads-tf:$(HASKELLMONADSTF)_$(ARCH).deb
$(HASKELLMONADSTF): $(SPREZZ)/haskell-monads-tf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monads-tf-$(haskell-monads-tf_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-mtl
haskell-mtl:$(HASKELLMTL)_$(ARCH).deb
$(HASKELLMTL): $(SPREZZ)/haskell-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mtl-$(haskell-mtl_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-nats
haskell-nats:$(HASKELLNATS)_$(ARCH).deb
$(HASKELLNATS): $(SPREZZ)/haskell-nats/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nats-$(haskell-nats_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-network
haskell-network:$(HASKELLNETWORK)_$(ARCH).deb
$(HASKELLNETWORK): $(SPREZZ)/haskell-network/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-$(haskell-network_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-network-protocol-xmpp
haskell-network-protocol-xmpp:$(HASKELLNETWORKPROTOCOLXMPP)_$(ARCH).deb
$(HASKELLNETWORKPROTOCOLXMPP): $(SPREZZ)/haskell-network-protocol-xmpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-protocol-xmpp-$(haskell-network-protocol-xmpp_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-parallel
haskell-parallel:$(HASKELLPARALLEL)_$(ARCH).deb
$(HASKELLPARALLEL): $(SPREZZ)/haskell-parallel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parallel-$(haskell-parallel_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-parsec
haskell-parsec:$(HASKELLPARSEC)_$(ARCH).deb
$(HASKELLPARSEC): $(SPREZZ)/haskell-parsec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parsec-$(haskell-parsec_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pem
haskell-pem:$(HASKELLPEM)_$(ARCH).deb
$(HASKELLPEM): $(SPREZZ)/haskell-pem/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pem-$(haskell-pem_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-primitive
haskell-primitive:$(HASKELLPRIMITIVE)_$(ARCH).deb
$(HASKELLPRIMITIVE): $(SPREZZ)/haskell-primitive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf primitive-$(haskell-primitive_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck
haskell-quickcheck:$(HASKELLQUICKCHECK)_$(ARCH).deb
$(HASKELLQUICKCHECK): $(SPREZZ)/haskell-quickcheck/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf QuickCheck-$(haskell-quickcheck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-random
haskell-random:$(HASKELLRANDOM)_$(ARCH).deb
$(HASKELLRANDOM): $(SPREZZ)/haskell-random/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf random-$(haskell-random_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-rc4
haskell-rc4:$(HASKELLRC4)_$(ARCH).deb
$(HASKELLRC4): $(SPREZZ)/haskell-rc4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-rc4-$(haskell-rc4_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-regex-base
haskell-regex-base:$(HASKELLREGEXBASE)_$(ARCH).deb
$(HASKELLREGEXBASE): $(SPREZZ)/haskell-regex-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-base-$(haskell-regex-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-compat
haskell-regex-compat:$(HASKELLREGEXCOMPAT)_$(ARCH).deb
$(HASKELLREGEXCOMPAT): $(SPREZZ)/haskell-regex-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-compat-$(haskell-regex-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-posix
haskell-regex-posix:$(HASKELLREGEXPOSIX)_$(ARCH).deb
$(HASKELLREGEXPOSIX): $(SPREZZ)/haskell-regex-posix/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-posix-$(haskell-regex-posix_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-resourcet
haskell-resourcet:$(HASKELLRESOURCET)_$(ARCH).deb
$(HASKELLRESOURCET): $(SPREZZ)/haskell-resourcet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf resourcet-$(haskell-resourcet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-semigroups
haskell-semigroups:$(HASKELLSEMIGROUPS)_$(ARCH).deb
$(HASKELLSEMIGROUPS): $(SPREZZ)/haskell-semigroups/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf semigroups-$(haskell-semigroups_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sha
haskell-sha:$(HASKELLSHA)_$(ARCH).deb
$(HASKELLSHA): $(SPREZZ)/haskell-sha/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SHA-$(haskell-sha_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-setenv
haskell-setenv:$(HASKELLSETENV)_$(ARCH).deb
$(HASKELLSETENV): $(SPREZZ)/haskell-setenv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf setenv-$(haskell-setenv_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare
haskell-shakespeare:$(HASKELLSHAKESPEARE)_$(ARCH).deb
$(HASKELLSHAKESPEARE): $(SPREZZ)/haskell-shakespeare/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-$(haskell-shakespeare_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-silently
haskell-silently:$(HASKELLSILENTLY)_$(ARCH).deb
$(HASKELLSILENTLY): $(SPREZZ)/haskell-silently/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf silently-$(haskell-silently_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-skein
haskell-skein:$(HASKELLSKEIN)_$(ARCH).deb
$(HASKELLSKEIN): $(SPREZZ)/haskell-skein/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf skein-$(haskell-skein_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-socks
haskell-socks:$(HASKELLSOCKS)_$(ARCH).deb
$(HASKELLSOCKS): $(SPREZZ)/haskell-socks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf socks-$(haskell-socks_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-stm
haskell-stm:$(HASKELLSTM)_$(ARCH).deb
$(HASKELLSTM): $(SPREZZ)/haskell-stm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stm-$(haskell-stm_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-syb
haskell-syb:$(HASKELLSYB)_$(ARCH).deb
$(HASKELLSYB): $(SPREZZ)/haskell-syb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syb-$(haskell-syb_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-system-filepath
haskell-system-filepath:$(HASKELLSYSTEMFILEPATH)_$(ARCH).deb
$(HASKELLSYSTEMFILEPATH): $(SPREZZ)/haskell-system-filepath/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf system-filepath-$(haskell-system-filepath_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tagged
haskell-tagged:$(HASKELLTAGGED)_$(ARCH).deb
$(HASKELLTAGGED): $(SPREZZ)/haskell-tagged/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tagged-$(haskell-tagged_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework
haskell-test-framework:$(HASKELLTESTFRAMEWORK)_$(ARCH).deb
$(HASKELLTESTFRAMEWORK): $(SPREZZ)/haskell-test-framework/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-$(haskell-test-framework_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-hunit
haskell-test-framework-hunit:$(HASKELLTESTFRAMEWORKHUNIT)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKHUNIT): $(SPREZZ)/haskell-test-framework-hunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-hunit-$(haskell-test-framework-hunit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-quickcheck2
haskell-test-framework-quickcheck2:$(HASKELLTESTFRAMEWORKQUICKCHECK2)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKQUICKCHECK2): $(SPREZZ)/haskell-test-framework-quickcheck2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-quickcheck2-$(haskell-test-framework-quickcheck2_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-testpack
haskell-testpack:$(HASKELLTESTPACK)_$(ARCH).deb
$(HASKELLTESTPACK): $(SPREZZ)/haskell-testpack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf testpack-$(haskell-testpack_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-text
haskell-text:$(HASKELLTEXT)_$(ARCH).deb
$(HASKELLTEXT): $(SPREZZ)/haskell-text/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf text-$(haskell-text_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tls
haskell-tls:$(HASKELLTLS)_$(ARCH).deb
$(HASKELLTLS): $(SPREZZ)/haskell-tls/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tls-$(haskell-tls_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tls-extra
haskell-tls-extra:$(HASKELLTLSEXTRA)_$(ARCH).deb
$(HASKELLTLSEXTRA): $(SPREZZ)/haskell-tls-extra/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tls-extra-$(haskell-tls-extra_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-transformers
haskell-transformers:$(HASKELLTRANSFORMERS)_$(ARCH).deb
$(HASKELLTRANSFORMERS): $(SPREZZ)/haskell-transformers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf transformers-$(haskell-transformers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-transformers-base
haskell-transformers-base:$(HASKELLTRANSFORMERSBASE)_$(ARCH).deb
$(HASKELLTRANSFORMERSBASE): $(SPREZZ)/haskell-transformers-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf transformers-base-$(haskell-transformers-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-transformers-compat
haskell-transformers-compat:$(HASKELLTRANSFORMERSCOMPAT)_$(ARCH).deb
$(HASKELLTRANSFORMERSCOMPAT): $(SPREZZ)/haskell-transformers-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf transformers-compat-$(haskell-transformers-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unordered-containers
haskell-unordered-containers:$(HASKELLUNORDEREDCONTAINERS)_$(ARCH).deb
$(HASKELLUNORDEREDCONTAINERS): $(SPREZZ)/haskell-unordered-containers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unordered-containers-$(haskell-unordered-containers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vault
haskell-vault:$(HASKELLVAULT)_$(ARCH).deb
$(HASKELLVAULT): $(SPREZZ)/haskell-vault/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vault-$(haskell-vault_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vector
haskell-vector:$(HASKELLVECTOR)_$(ARCH).deb
$(HASKELLVECTOR): $(SPREZZ)/haskell-vector/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vector-$(haskell-vector_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-void
haskell-void:$(HASKELLVOID)_$(ARCH).deb
$(HASKELLVOID): $(SPREZZ)/haskell-void/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf void-$(haskell-void_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai
haskell-wai:$(HASKELLWAI)_$(ARCH).deb
$(HASKELLWAI): $(SPREZZ)/haskell-wai/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-$(haskell-wai_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-logger
haskell-wai-logger:$(HASKELLWAILOGGER)_$(ARCH).deb
$(HASKELLWAILOGGER): $(SPREZZ)/haskell-wai-logger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-logger-$(haskell-wai-logger_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml
haskell-xml:$(HASKELLXML)_$(ARCH).deb
$(HASKELLXML): $(SPREZZ)/haskell-xml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-$(haskell-xml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-conduit
haskell-xml-conduit:$(HASKELLXMLCONDUIT)_$(ARCH).deb
$(HASKELLXMLCONDUIT): $(SPREZZ)/haskell-xml-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-conduit-$(haskell-xml-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-hamlet
haskell-xml-hamlet:$(HASKELLXMLHAMLET)_$(ARCH).deb
$(HASKELLXMLHAMLET): $(SPREZZ)/haskell-xml-hamlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-hamlet-$(haskell-xml-hamlet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-types
haskell-xml-types:$(HASKELLXMLTYPES)_$(ARCH).deb
$(HASKELLXMLTYPES): $(SPREZZ)/haskell-xml-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-types-$(haskell-xml-types_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib
haskell-zlib:$(HASKELLZLIB)_$(ARCH).deb
$(HASKELLZLIB): $(SPREZZ)/haskell-zlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-$(haskell-zlib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib-bindings
haskell-zlib-bindings:$(HASKELLZLIBBINDINGS)_$(ARCH).deb
$(HASKELLZLIBBINDINGS): $(SPREZZ)/haskell-zlib-bindings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-bindings-$(haskell-zlib-bindings_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib-conduit
haskell-zlib-conduit:$(HASKELLZLIBCONDUIT)_$(ARCH).deb
$(HASKELLZLIBCONDUIT): $(SPREZZ)/haskell-zlib-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-conduit-$(haskell-zlib-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: hugs98
hugs98:$(HUGS98)_$(ARCH).deb
$(HUGS98): $(SPREZZ)/hugs98/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hugs98-$(hugs98_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-warp
haskell-warp:$(HASKELLWARP)_$(ARCH).deb
$(HASKELLWARP): $(SPREZZ)/haskell-warp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf warp-$(haskell-warp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dbus
haskell-dbus:$(HASKELLDBUS)_$(ARCH).deb
$(HASKELLDBUS): $(SPREZZ)/haskell-dbus/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-$(haskell-dbus_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod
haskell-yesod:$(HASKELLYESOD)_$(ARCH).deb
$(HASKELLYESOD): $(SPREZZ)/haskell-yesod/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-$(haskell-yesod_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-static
haskell-yesod-static:$(HASKELLYESODSTATIC)_$(ARCH).deb
$(HASKELLYESODSTATIC): $(SPREZZ)/haskell-yesod-static/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-static-$(haskell-yesod-static_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-blaze-textual
haskell-blaze-textual:$(HASKELLBLAZETEXTUAL)_$(ARCH).deb
$(HASKELLBLAZETEXTUAL): $(SPREZZ)/haskell-blaze-textual/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-textual-$(haskell-blaze-textual_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-boolean
haskell-boolean:$(HASKELLBOOLEAN)_$(ARCH).deb
$(HASKELLBOOLEAN): $(SPREZZ)/haskell-boolean/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Boolean-$(haskell-boolean_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-brainfuck
haskell-brainfuck:$(HASKELLBRAINFUCK)_$(ARCH).deb
$(HASKELLBRAINFUCK): $(SPREZZ)/haskell-brainfuck/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf brainfuck-$(haskell-brainfuck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bytestring-lexing
haskell-bytestring-lexing:$(HASKELLBYTESTRINGLEXING)_$(ARCH).deb
$(HASKELLBYTESTRINGLEXING): $(SPREZZ)/haskell-bytestring-lexing/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bytestring-lexing-$(haskell-bytestring-lexing_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bytestring-nums
haskell-bytestring-nums:$(HASKELLBYTESTRINGNUMS)_$(ARCH).deb
$(HASKELLBYTESTRINGNUMS): $(SPREZZ)/haskell-bytestring-nums/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bytestring-nums-$(haskell-bytestring-nums_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bytestring-show
haskell-bytestring-show:$(HASKELLBYTESTRINGSHOW)_$(ARCH).deb
$(HASKELLBYTESTRINGSHOW): $(SPREZZ)/haskell-bytestring-show/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bytestring-show-$(haskell-bytestring-show_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bzlib
haskell-bzlib:$(HASKELLBZLIB)_$(ARCH).deb
$(HASKELLBZLIB): $(SPREZZ)/haskell-bzlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bzlib-$(haskell-bzlib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cabal-file-th
haskell-cabal-file-th:$(HASKELLCABALFILETH)_$(ARCH).deb
$(HASKELLCABALFILETH): $(SPREZZ)/haskell-cabal-file-th/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cabal-file-th-$(haskell-cabal-file-th_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cairo
haskell-cairo:$(HASKELLCAIRO)_$(ARCH).deb
$(HASKELLCAIRO): $(SPREZZ)/haskell-cairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cairo-$(haskell-cairo_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-categories
haskell-categories:$(HASKELLCATEGORIES)_$(ARCH).deb
$(HASKELLCATEGORIES): $(SPREZZ)/haskell-categories/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf categories-$(haskell-categories_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cautious-file
haskell-cautious-file:$(HASKELLCAUTIOUSFILE)_$(ARCH).deb
$(HASKELLCAUTIOUSFILE): $(SPREZZ)/haskell-cautious-file/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cautious-file-$(haskell-cautious-file_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cgi
haskell-cgi:$(HASKELLCGI)_$(ARCH).deb
$(HASKELLCGI): $(SPREZZ)/haskell-cgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cgi-$(haskell-cgi_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-chart
haskell-chart:$(HASKELLCHART)_$(ARCH).deb
$(HASKELLCHART): $(SPREZZ)/haskell-chart/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Chart-$(haskell-chart_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-chell
haskell-chell:$(HASKELLCHELL)_$(ARCH).deb
$(HASKELLCHELL): $(SPREZZ)/haskell-chell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf chell_$(haskell-chell_UPVER).tar.xz $(TARARGS) $@

.PHONY: haskell-citeproc-hs
haskell-citeproc-hs:$(HASKELLCITEPROCHS)_$(ARCH).deb
$(HASKELLCITEPROCHS): $(SPREZZ)/haskell-citeproc-hs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf citeproc-hs-$(haskell-citeproc-hs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-clock
haskell-clock:$(HASKELLCLOCK)_$(ARCH).deb
$(HASKELLCLOCK): $(SPREZZ)/haskell-clock/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clock-$(haskell-clock_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-clocked
haskell-clocked:$(HASKELLCLOCKED)_$(ARCH).deb
$(HASKELLCLOCKED): $(SPREZZ)/haskell-clocked/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clocked-$(haskell-clocked_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cmdargs
haskell-cmdargs:$(HASKELLCMDARGS)_$(ARCH).deb
$(HASKELLCMDARGS): $(SPREZZ)/haskell-cmdargs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cmdargs-$(haskell-cmdargs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-colour
haskell-colour:$(HASKELLCOLOUR)_$(ARCH).deb
$(HASKELLCOLOUR): $(SPREZZ)/haskell-colour/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf colour-$(haskell-colour_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-comonad
haskell-comonad:$(HASKELLCOMONAD)_$(ARCH).deb
$(HASKELLCOMONAD): $(SPREZZ)/haskell-comonad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf comonad-$(haskell-comonad_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-comonads-fd
haskell-comonads-fd:$(HASKELLCOMONADSFD)_$(ARCH).deb
$(HASKELLCOMONADSFD): $(SPREZZ)/haskell-comonads-fd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf comonads-fd-$(haskell-comonads-fd_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-comonad-transformers
haskell-comonad-transformers:$(HASKELLCOMONADTRANSFORMERS)_$(ARCH).deb
$(HASKELLCOMONADTRANSFORMERS): $(SPREZZ)/haskell-comonad-transformers/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf comonad-transformers-$(haskell-comonad-transformers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cond
haskell-cond:$(HASKELLCOND)_$(ARCH).deb
$(HASKELLCOND): $(SPREZZ)/haskell-cond/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cond-$(haskell-cond_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-configfile
haskell-configfile:$(HASKELLCONFIGFILE)_$(ARCH).deb
$(HASKELLCONFIGFILE): $(SPREZZ)/haskell-configfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ConfigFile-$(haskell-configfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-arrows
haskell-arrows:$(HASKELLARROWS)_$(ARCH).deb
$(HASKELLARROWS): $(SPREZZ)/haskell-arrows/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf arrows-$(haskell-arrows_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-async
haskell-async:$(HASKELLASYNC)_$(ARCH).deb
$(HASKELLASYNC): $(SPREZZ)/haskell-async/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf async-$(haskell-async_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-attempt
haskell-attempt:$(HASKELLATTEMPT)_$(ARCH).deb
$(HASKELLATTEMPT): $(SPREZZ)/haskell-attempt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attempt-$(haskell-attempt_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-authenticate
haskell-authenticate:$(HASKELLAUTHENTICATE)_$(ARCH).deb
$(HASKELLAUTHENTICATE): $(SPREZZ)/haskell-authenticate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf authenticate-$(haskell-authenticate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-authenticate-oauth
haskell-authenticate-oauth:$(HASKELLAUTHENTICATEOAUTH)_$(ARCH).deb
$(HASKELLAUTHENTICATEOAUTH): $(SPREZZ)/haskell-authenticate-oauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf authenticate-oauth-$(haskell-authenticate-oauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-base16-bytestring
haskell-base16-bytestring:$(HASKELLBASE16BYTESTRING)_$(ARCH).deb
$(HASKELLBASE16BYTESTRING): $(SPREZZ)/haskell-base16-bytestring/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base16-bytestring-$(haskell-base16-bytestring_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bifunctors
haskell-bifunctors:$(HASKELLBIFUNCTORS)_$(ARCH).deb
$(HASKELLBIFUNCTORS): $(SPREZZ)/haskell-bifunctors/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bifunctors-$(haskell-bifunctors_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-binary-communicator
haskell-binary-communicator:$(HASKELLBINARYCOMMUNICATOR)_$(ARCH).deb
$(HASKELLBINARYCOMMUNICATOR): $(SPREZZ)/haskell-binary-communicator/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf binary-communicator-$(haskell-binary-communicator_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-binary-shared
haskell-binary-shared:$(HASKELLBINARYSHARED)_$(ARCH).deb
$(HASKELLBINARYSHARED): $(SPREZZ)/haskell-binary-shared/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf binary-shared-$(haskell-binary-shared_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bindings-dsl
haskell-bindings-dsl:$(HASKELLBINDINGSDSL)_$(ARCH).deb
$(HASKELLBINDINGSDSL): $(SPREZZ)/haskell-bindings-dsl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bindings-DSL-$(haskell-bindings-dsl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bindings-gpgme
haskell-bindings-gpgme:$(HASKELLBINDINGSGPGME)_$(ARCH).deb
$(HASKELLBINDINGSGPGME): $(SPREZZ)/haskell-bindings-gpgme/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bindings-gpgme-$(haskell-bindings-gpgme_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bindings-libzip
haskell-bindings-libzip:$(HASKELLBINDINGSLIBZIP)_$(ARCH).deb
$(HASKELLBINDINGSLIBZIP): $(SPREZZ)/haskell-bindings-libzip/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bindings-libzip-$(haskell-bindings-libzip_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bindings-sane
haskell-bindings-sane:$(HASKELLBINDINGSSANE)_$(ARCH).deb
$(HASKELLBINDINGSSANE): $(SPREZZ)/haskell-bindings-sane/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bindings-sane-$(haskell-bindings-sane_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bitarray
haskell-bitarray:$(HASKELLBITARRAY)_$(ARCH).deb
$(HASKELLBITARRAY): $(SPREZZ)/haskell-bitarray/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bitarray-$(haskell-bitarray_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-agda
haskell-agda:$(HASKELLAGDA)_$(ARCH).deb
$(HASKELLAGDA): $(SPREZZ)/haskell-agda/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf agda-$(haskell-agda_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-alut
haskell-alut:$(HASKELLALUT)_$(ARCH).deb
$(HASKELLALUT): $(SPREZZ)/haskell-alut/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ALUT-$(haskell-alut_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-binary
haskell-binary:$(HASKELLBINARY)_$(ARCH).deb
$(HASKELLBINARY): $(SPREZZ)/haskell-binary/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf binary-$(haskell-binary_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-convertible
haskell-convertible:$(HASKELLCONVERTIBLE)_$(ARCH).deb
$(HASKELLCONVERTIBLE): $(SPREZZ)/haskell-convertible/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf convertible-$(haskell-convertible_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-csv
haskell-csv:$(HASKELLCSV)_$(ARCH).deb
$(HASKELLCSV): $(SPREZZ)/haskell-csv/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf csv-$(haskell-csv_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-curl
haskell-curl:$(HASKELLCURL)_$(ARCH).deb
$(HASKELLCURL): $(SPREZZ)/haskell-curl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf curl-$(haskell-curl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-accessor
haskell-data-accessor:$(HASKELLDATAACCESSOR)_$(ARCH).deb
$(HASKELLDATAACCESSOR): $(SPREZZ)/haskell-data-accessor/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-accessor-$(haskell-data-accessor_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-datetime
haskell-datetime:$(HASKELLDATETIME)_$(ARCH).deb
$(HASKELLDATETIME): $(SPREZZ)/haskell-datetime/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf datetime-$(haskell-datetime_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-debian
haskell-debian:$(HASKELLDEBIAN)_$(ARCH).deb
$(HASKELLDEBIAN): $(SPREZZ)/haskell-debian/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf debian-$(haskell-debian_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-deepseq
haskell-deepseq:$(HASKELLDEEPSEQ)_$(ARCH).deb
$(HASKELLDEEPSEQ): $(SPREZZ)/haskell-deepseq/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf deepseq-$(haskell-deepseq_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-diagrams
haskell-diagrams:$(HASKELLDIAGRAMS)_$(ARCH).deb
$(HASKELLDIAGRAMS): $(SPREZZ)/haskell-diagrams/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diagrams-$(haskell-diagrams_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-diff
haskell-diff:$(HASKELLDIFF)_$(ARCH).deb
$(HASKELLDIFF): $(SPREZZ)/haskell-diff/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Diff-$(haskell-diff_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-digest
haskell-digest:$(HASKELLDIGEST)_$(ARCH).deb
$(HASKELLDIGEST): $(SPREZZ)/haskell-digest/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf digest-$(haskell-digest_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-edison-api
haskell-edison-api:$(HASKELLEDISONAPI)_$(ARCH).deb
$(HASKELLEDISONAPI): $(SPREZZ)/haskell-edison-api/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf EdisonAPI-$(haskell-edison-api_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-edison-core
haskell-edison-core:$(HASKELLEDISONCORE)_$(ARCH).deb
$(HASKELLEDISONCORE): $(SPREZZ)/haskell-edison-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf EdisonCore-$(haskell-edison-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-editline
haskell-editline:$(HASKELLEDITLINE)_$(ARCH).deb
$(HASKELLEDITLINE): $(SPREZZ)/haskell-editline/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf editline-$(haskell-editline_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-erf
haskell-erf:$(HASKELLERF)_$(ARCH).deb
$(HASKELLERF): $(SPREZZ)/haskell-erf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf erf-$(haskell-erf_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-event-list
haskell-event-list:$(HASKELLEVENTLIST)_$(ARCH).deb
$(HASKELLEVENTLIST): $(SPREZZ)/haskell-event-list/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf event-list-$(haskell-event-list_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-explicit-exception
haskell-explicit-exception:$(HASKELLEXPLICITEXCEPTION)_$(ARCH).deb
$(HASKELLEXPLICITEXCEPTION): $(SPREZZ)/haskell-explicit-exception/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf explicit-exception-$(haskell-explicit-exception_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-fastcgi
haskell-fastcgi:$(HASKELLFASTCGI)_$(ARCH).deb
$(HASKELLFASTCGI): $(SPREZZ)/haskell-fastcgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fastcgi-$(haskell-fastcgi_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-feed
haskell-feed:$(HASKELLFEED)_$(ARCH).deb
$(HASKELLFEED): $(SPREZZ)/haskell-feed/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf feed-$(haskell-feed_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-filemanip
haskell-filemanip:$(HASKELLFILEMANIP)_$(ARCH).deb
$(HASKELLFILEMANIP): $(SPREZZ)/haskell-filemanip/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf filemanip-$(haskell-filemanip_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-filestore
haskell-filestore:$(HASKELLFILESTORE)_$(ARCH).deb
$(HASKELLFILESTORE): $(SPREZZ)/haskell-filestore/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf filestore-$(haskell-filestore_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ftphs
haskell-ftphs:$(HASKELLFTPHS)_$(ARCH).deb
$(HASKELLFTPHS): $(SPREZZ)/haskell-ftphs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ftphs-$(haskell-ftphs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gconf
haskell-gconf:$(HASKELLGCONF)_$(ARCH).deb
$(HASKELLGCONF): $(SPREZZ)/haskell-gconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gconf-$(haskell-gconf_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ghc-events
haskell-ghc-events:$(HASKELLGHCEVENTS)_$(ARCH).deb
$(HASKELLGHCEVENTS): $(SPREZZ)/haskell-ghc-events/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-events-$(haskell-ghc-events_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ghc-mtl
haskell-ghc-mtl:$(HASKELLGHCMTL)_$(ARCH).deb
$(HASKELLGHCMTL): $(SPREZZ)/haskell-ghc-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-mtl-$(haskell-ghc-mtl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gio
haskell-gio:$(HASKELLGIO)_$(ARCH).deb
$(HASKELLGIO): $(SPREZZ)/haskell-gio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gio-$(haskell-gio_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gitit
haskell-gitit:$(HASKELLGITIT)_$(ARCH).deb
$(HASKELLGITIT): $(SPREZZ)/haskell-gitit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gitit-$(haskell-gitit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-glfw
haskell-glfw:$(HASKELLGLFW)_$(ARCH).deb
$(HASKELLGLFW): $(SPREZZ)/haskell-glfw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GLFW-$(haskell-glfw_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gstreamer
haskell-gstreamer:$(HASKELLGSTREAMER)_$(ARCH).deb
$(HASKELLGSTREAMER): $(SPREZZ)/haskell-gstreamer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gstreamer-$(haskell-gstreamer_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gtksourceview2
haskell-gtksourceview2:$(HASKELLGTKSOURCEVIEW2)_$(ARCH).deb
$(HASKELLGTKSOURCEVIEW2): $(SPREZZ)/haskell-gtksourceview2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtksourceview2-$(haskell-gtksourceview2_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haddock
haskell-haddock:$(HASKELLHADDOCK)_$(ARCH).deb
$(HASKELLHADDOCK): $(SPREZZ)/haskell-haddock/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haddock-$(haskell-haddock_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-happstack
haskell-happstack:$(HASKELLHAPPSTACK)_$(ARCH).deb
$(HASKELLHAPPSTACK): $(SPREZZ)/haskell-happstack/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf happstack-$(haskell-happstack_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-happstack-server
haskell-happstack-server:$(HASKELLHAPPSTACKSERVER)_$(ARCH).deb
$(HASKELLHAPPSTACKSERVER): $(SPREZZ)/haskell-happstack-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf happstack-server-$(haskell-happstack-server_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-harp
haskell-harp:$(HASKELLHARP)_$(ARCH).deb
$(HASKELLHARP): $(SPREZZ)/haskell-harp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf harp-$(haskell-harp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hashed-storage
haskell-hashed-storage:$(HASKELLHASHEDSTORAGE)_$(ARCH).deb
$(HASKELLHASHEDSTORAGE): $(SPREZZ)/haskell-hashed-storage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hashed-storage-$(haskell-hashed-storage_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskeline
haskell-haskeline:$(HASKELLHASKELINE)_$(ARCH).deb
$(HASKELLHASKELINE): $(SPREZZ)/haskell-haskeline/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskeline-$(haskell-haskeline_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb
haskell-haskelldb:$(HASKELLHASKELLDB)_$(ARCH).deb
$(HASKELLHASKELLDB): $(SPREZZ)/haskell-haskelldb/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-$(haskell-haskelldb_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc
haskell-haskelldb-hdbc:$(HASKELLHASKELLDBHDBC)_$(ARCH).deb
$(HASKELLHASKELLDBHDBC): $(SPREZZ)/haskell-haskelldb-hdbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-$(haskell-haskelldb-hdbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-odbc
haskell-haskelldb-hdbc-odbc:$(HASKELLHASKELLDBHDBCODBC)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCODBC): $(SPREZZ)/haskell-haskelldb-hdbc-odbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-odbc-$(haskell-haskelldb-hdbc-odbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-postgresql
haskell-haskelldb-hdbc-postgresql:$(HASKELLHASKELLDBHDBCPOSTGRESQL)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCPOSTGRESQL): $(SPREZZ)/haskell-haskelldb-hdbc-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-postgresql-$(haskell-haskelldb-hdbc-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-sqlite3
haskell-haskelldb-hdbc-sqlite3:$(HASKELLHASKELLDBHDBCSQLITE3)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCSQLITE3): $(SPREZZ)/haskell-haskelldb-hdbc-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-sqlite3-$(haskell-haskelldb-hdbc-sqlite3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-lexer
haskell-lexer:$(HASKELLLEXER)_$(ARCH).deb
$(HASKELLLEXER): $(SPREZZ)/haskell-lexer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-lexer-$(haskell-lexer_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskell-src
haskell-haskell-src:$(HASKELLHASKELLSRC)_$(ARCH).deb
$(HASKELLHASKELLSRC): $(SPREZZ)/haskell-haskell-src/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-src-$(haskell-haskell-src_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskore
haskell-haskore:$(HASKELLHASKORE)_$(ARCH).deb
$(HASKELLHASKORE): $(SPREZZ)/haskell-haskore/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskore-$(haskell-haskore_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haxml
haskell-haxml:$(HASKELLHAXML)_$(ARCH).deb
$(HASKELLHAXML): $(SPREZZ)/haskell-haxml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haxml-$(haskell-haxml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haxr
haskell-haxr:$(HASKELLHAXR)_$(ARCH).deb
$(HASKELLHAXR): $(SPREZZ)/haskell-haxr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haxr-$(haskell-haxr_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc
haskell-hdbc:$(HASKELLHDBC)_$(ARCH).deb
$(HASKELLHDBC): $(SPREZZ)/haskell-hdbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-$(haskell-hdbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-odbc
haskell-hdbc-odbc:$(HASKELLHDBCODBC)_$(ARCH).deb
$(HASKELLHDBCODBC): $(SPREZZ)/haskell-hdbc-odbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-odbc-$(haskell-hdbc-odbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-postgresql
haskell-hdbc-postgresql:$(HASKELLHDBCPOSTGRESQL)_$(ARCH).deb
$(HASKELLHDBCPOSTGRESQL): $(SPREZZ)/haskell-hdbc-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-postgresql-$(haskell-hdbc-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-sqlite3
haskell-hdbc-sqlite3:$(HASKELLHDBCSQLITE3)_$(ARCH).deb
$(HASKELLHDBCSQLITE3): $(SPREZZ)/haskell-hdbc-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-sqlite3-$(haskell-hdbc-sqlite3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-highlighting-kate
haskell-highlighting-kate:$(HASKELLHIGHLIGHTINGKATE)_$(ARCH).deb
$(HASKELLHIGHLIGHTINGKATE): $(SPREZZ)/haskell-highlighting-kate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf highlighting-kate-$(haskell-highlighting-kate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hint
haskell-hint:$(HASKELLHINT)_$(ARCH).deb
$(HASKELLHINT): $(SPREZZ)/haskell-hint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hint-$(haskell-hint_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hjavascript
haskell-hjavascript:$(HASKELLHJAVASCRIPT)_$(ARCH).deb
$(HASKELLHJAVASCRIPT): $(SPREZZ)/haskell-hjavascript/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HJavaScript-$(haskell-hjavascript_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hjscript
haskell-hjscript:$(HASKELLHJSCRIPT)_$(ARCH).deb
$(HASKELLHJSCRIPT): $(SPREZZ)/haskell-hjscript/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HJScript-$(haskell-hjscript_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hoauth
haskell-hoauth:$(HASKELLHOAUTH)_$(ARCH).deb
$(HASKELLHOAUTH): $(SPREZZ)/haskell-hoauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hoauth-$(haskell-hoauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hscolour
haskell-hscolour:$(HASKELLHSCOLOUR)_$(ARCH).deb
$(HASKELLHSCOLOUR): $(SPREZZ)/haskell-hscolour/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hscolour-$(haskell-hscolour_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hscurses
haskell-hscurses:$(HASKELLHSCURSES)_$(ARCH).deb
$(HASKELLHSCURSES): $(SPREZZ)/haskell-hscurses/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hscurses-$(haskell-hscurses_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsemail
haskell-hsemail:$(HASKELLHSEMAIL)_$(ARCH).deb
$(HASKELLHSEMAIL): $(SPREZZ)/haskell-hsemail/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsemail-$(haskell-hsemail_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsp
haskell-hsp:$(HASKELLHSP)_$(ARCH).deb
$(HASKELLHSP): $(SPREZZ)/haskell-hsp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsp-$(haskell-hsp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsql
haskell-hsql:$(HASKELLHSQL)_$(ARCH).deb
$(HASKELLHSQL): $(SPREZZ)/haskell-hsql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsql-$(haskell-hsql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsql-mysql
haskell-hsql-mysql:$(HASKELLHSQLMYSQL)_$(ARCH).deb
$(HASKELLHSQLMYSQL): $(SPREZZ)/haskell-hsql-mysql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsql-mysql-$(haskell-hsql-mysql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsql-odbc
haskell-hsql-odbc:$(HASKELLHSQLODBC)_$(ARCH).deb
$(HASKELLHSQLODBC): $(SPREZZ)/haskell-hsql-odbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsql-odbc-$(haskell-hsql-odbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsql-postgresql
haskell-hsql-postgresql:$(HASKELLHSQLPOSTGRESQL)_$(ARCH).deb
$(HASKELLHSQLPOSTGRESQL): $(SPREZZ)/haskell-hsql-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsql-postgresql-$(haskell-hsql-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsql-sqlite3
haskell-hsql-sqlite3:$(HASKELLHSQLSQLITE3)_$(ARCH).deb
$(HASKELLHSQLSQLITE3): $(SPREZZ)/haskell-hsql-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsql-sqlite3-$(haskell-hsql-sqlite3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hstringtemplate
haskell-hstringtemplate:$(HASKELLHSTRINGTEMPLATE)_$(ARCH).deb
$(HASKELLHSTRINGTEMPLATE): $(SPREZZ)/haskell-hstringtemplate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HStringTemplate-$(haskell-hstringtemplate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsx
haskell-hsx:$(HASKELLHSX)_$(ARCH).deb
$(HASKELLHSX): $(SPREZZ)/haskell-hsx/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsx-$(haskell-hsx_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-html
haskell-html:$(HASKELLHTML)_$(ARCH).deb
$(HASKELLHTML): $(SPREZZ)/haskell-html/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf html-$(haskell-html_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-irc
haskell-irc:$(HASKELLIRC)_$(ARCH).deb
$(HASKELLIRC): $(SPREZZ)/haskell-irc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf irc-$(haskell-irc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-language-c
haskell-language-c:$(HASKELLLANGUAGEC)_$(ARCH).deb
$(HASKELLLANGUAGEC): $(SPREZZ)/haskell-language-c/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf language-c-$(haskell-language-c_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-lazysmallcheck
haskell-lazysmallcheck:$(HASKELLLAZYSMALLCHECK)_$(ARCH).deb
$(HASKELLLAZYSMALLCHECK): $(SPREZZ)/haskell-lazysmallcheck/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lazysmallcheck-$(haskell-lazysmallcheck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ldap
haskell-ldap:$(HASKELLLDAP)_$(ARCH).deb
$(HASKELLLDAP): $(SPREZZ)/haskell-ldap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ldap-$(haskell-ldap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-leksah-server
haskell-leksah-server:$(HASKELLLEKSAHSERVER)_$(ARCH).deb
$(HASKELLLEKSAHSERVER): $(SPREZZ)/haskell-leksah-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf leksah-server-$(haskell-leksah-server_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-llvm
haskell-llvm:$(HASKELLLLVM)_$(ARCH).deb
$(HASKELLLLVM): $(SPREZZ)/haskell-llvm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf llvm-$(haskell-llvm_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ltk
haskell-ltk:$(HASKELLLTK)_$(ARCH).deb
$(HASKELLLTK): $(SPREZZ)/haskell-ltk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ltk-$(haskell-ltk_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-magic
haskell-magic:$(HASKELLMAGIC)_$(ARCH).deb
$(HASKELLMAGIC): $(SPREZZ)/haskell-magic/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf magic-$(haskell-magic_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-markov-chain
haskell-markov-chain:$(HASKELLMARKOVCHAIN)_$(ARCH).deb
$(HASKELLMARKOVCHAIN): $(SPREZZ)/haskell-markov-chain/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf markov-chain-$(haskell-markov-chain_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-maybet
haskell-maybet:$(HASKELLMAYBET)_$(ARCH).deb
$(HASKELLMAYBET): $(SPREZZ)/haskell-maybet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf maybet-$(haskell-maybet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-midi
haskell-midi:$(HASKELLMIDI)_$(ARCH).deb
$(HASKELLMIDI): $(SPREZZ)/haskell-midi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf midi-$(haskell-midi_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mmap
haskell-mmap:$(HASKELLMMAP)_$(ARCH).deb
$(HASKELLMMAP): $(SPREZZ)/haskell-mmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mmap-$(haskell-mmap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monadcatchio-mtl
haskell-monadcatchio-mtl:$(HASKELLMONADCATCHIOMTL)_$(ARCH).deb
$(HASKELLMONADCATCHIOMTL): $(SPREZZ)/haskell-monadcatchio-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MonadCatchIO-mtl-$(haskell-monadcatchio-mtl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monoid-transformer
haskell-monoid-transformer:$(HASKELLMONOIDTRANSFORMER)_$(ARCH).deb
$(HASKELLMONOIDTRANSFORMER): $(SPREZZ)/haskell-monoid-transformer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monoid-transformer-$(haskell-monoid-transformer_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mwc-random
haskell-mwc-random:$(HASKELLMWCRANDOM)_$(ARCH).deb
$(HASKELLMWCRANDOM): $(SPREZZ)/haskell-mwc-random/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mwc-random-$(haskell-mwc-random_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-non-negative
haskell-non-negative:$(HASKELLNONNEGATIVE)_$(ARCH).deb
$(HASKELLNONNEGATIVE): $(SPREZZ)/haskell-non-negative/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf non-negative-$(haskell-non-negative_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-openal
haskell-openal:$(HASKELLOPENAL)_$(ARCH).deb
$(HASKELLOPENAL): $(SPREZZ)/haskell-openal/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf OpenAL-$(haskell-openal_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-opengl
haskell-opengl:$(HASKELLOPENGL)_$(ARCH).deb
$(HASKELLOPENGL): $(SPREZZ)/haskell-opengl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf OpenGL-$(haskell-opengl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pandoc
haskell-pandoc:$(HASKELLPANDOC)_$(ARCH).deb
$(HASKELLPANDOC): $(SPREZZ)/haskell-pandoc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pandoc-$(haskell-pandoc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pango
haskell-pango:$(HASKELLPANGO)_$(ARCH).deb
$(HASKELLPANGO): $(SPREZZ)/haskell-pango/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pango-$(haskell-pango_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-parsec2
haskell-parsec2:$(HASKELLPARSEC2)_$(ARCH).deb
$(HASKELLPARSEC2): $(SPREZZ)/haskell-parsec2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parsec-$(haskell-parsec2_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-polyparse
haskell-polyparse:$(HASKELLPOLYPARSE)_$(ARCH).deb
$(HASKELLPOLYPARSE): $(SPREZZ)/haskell-polyparse/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf polyparse-$(haskell-polyparse_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pretty-show
haskell-pretty-show:$(HASKELLPRETTYSHOW)_$(ARCH).deb
$(HASKELLPRETTYSHOW): $(SPREZZ)/haskell-pretty-show/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pretty-show-$(haskell-pretty-show_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck1
haskell-quickcheck1:$(HASKELLQUICKCHECK1)_$(ARCH).deb
$(HASKELLQUICKCHECK1): $(SPREZZ)/haskell-quickcheck1/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf QuickCheck-$(haskell-quickcheck1_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck2
haskell-quickcheck2:$(HASKELLQUICKCHECK2)_$(ARCH).deb
$(HASKELLQUICKCHECK2): $(SPREZZ)/haskell-quickcheck2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf quickcheck2-$(haskell-quickcheck2_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-recaptcha
haskell-recaptcha:$(HASKELLRECAPTCHA)_$(ARCH).deb
$(HASKELLRECAPTCHA): $(SPREZZ)/haskell-recaptcha/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf recaptcha-$(haskell-recaptcha_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-tdfa
haskell-regex-tdfa:$(HASKELLREGEXTDFA)_$(ARCH).deb
$(HASKELLREGEXTDFA): $(SPREZZ)/haskell-regex-tdfa/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-tdfa-$(haskell-regex-tdfa_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-tdfa-utf8
haskell-regex-tdfa-utf8:$(HASKELLREGEXTDFAUTF8)_$(ARCH).deb
$(HASKELLREGEXTDFAUTF8): $(SPREZZ)/haskell-regex-tdfa-utf8/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-tdfa-utf8-$(haskell-regex-tdfa-utf8_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-safe
haskell-safe:$(HASKELLSAFE)_$(ARCH).deb
$(HASKELLSAFE): $(SPREZZ)/haskell-safe/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf safe-$(haskell-safe_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sdl
haskell-sdl:$(HASKELLSDL)_$(ARCH).deb
$(HASKELLSDL): $(SPREZZ)/haskell-sdl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-$(haskell-sdl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sdl-gfx
haskell-sdl-gfx:$(HASKELLSDLGFX)_$(ARCH).deb
$(HASKELLSDLGFX): $(SPREZZ)/haskell-sdl-gfx/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-gfx-$(haskell-sdl-gfx_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sdl-image
haskell-sdl-image:$(HASKELLSDLIMAGE)_$(ARCH).deb
$(HASKELLSDLIMAGE): $(SPREZZ)/haskell-sdl-image/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-image-$(haskell-sdl-image_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sdl-mixer
haskell-sdl-mixer:$(HASKELLSDLMIXER)_$(ARCH).deb
$(HASKELLSDLMIXER): $(SPREZZ)/haskell-sdl-mixer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-mixer-$(haskell-sdl-mixer_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sdl-ttf
haskell-sdl-ttf:$(HASKELLSDLTTF)_$(ARCH).deb
$(HASKELLSDLTTF): $(SPREZZ)/haskell-sdl-ttf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-ttf-$(haskell-sdl-ttf_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sendfile
haskell-sendfile:$(HASKELLSENDFILE)_$(ARCH).deb
$(HASKELLSENDFILE): $(SPREZZ)/haskell-sendfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sendfile-$(haskell-sendfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-smtpclient
haskell-smtpclient:$(HASKELLSMTPCLIENT)_$(ARCH).deb
$(HASKELLSMTPCLIENT): $(SPREZZ)/haskell-smtpclient/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SMTPClient-$(haskell-smtpclient_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-split
haskell-split:$(HASKELLSPLIT)_$(ARCH).deb
$(HASKELLSPLIT): $(SPREZZ)/haskell-split/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf split-$(haskell-split_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-src-exts
haskell-src-exts:$(HASKELLSRCEXTS)_$(ARCH).deb
$(HASKELLSRCEXTS): $(SPREZZ)/haskell-src-exts/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-src-exts-$(haskell-src-exts_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-statistics
haskell-statistics:$(HASKELLSTATISTICS)_$(ARCH).deb
$(HASKELLSTATISTICS): $(SPREZZ)/haskell-statistics/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf statistics-$(haskell-statistics_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-stream
haskell-stream:$(HASKELLSTREAM)_$(ARCH).deb
$(HASKELLSTREAM): $(SPREZZ)/haskell-stream/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Stream-$(haskell-stream_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-strict-concurrency
haskell-strict-concurrency:$(HASKELLSTRICTCONCURRENCY)_$(ARCH).deb
$(HASKELLSTRICTCONCURRENCY): $(SPREZZ)/haskell-strict-concurrency/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf strict-concurrency-$(haskell-strict-concurrency_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-svgcairo
haskell-svgcairo:$(HASKELLSVGCAIRO)_$(ARCH).deb
$(HASKELLSVGCAIRO): $(SPREZZ)/haskell-svgcairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf svgcairo-$(haskell-svgcairo_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-syb-with-class
haskell-syb-with-class:$(HASKELLSYBWITHCLASS)_$(ARCH).deb
$(HASKELLSYBWITHCLASS): $(SPREZZ)/haskell-syb-with-class/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syb-with-class-$(haskell-syb-with-class_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-syb-with-class-instances-text
haskell-syb-with-class-instances-text:$(HASKELLSYBWITHCLASSINSTANCESTEXT)_$(ARCH).deb
$(HASKELLSYBWITHCLASSINSTANCESTEXT): $(SPREZZ)/haskell-syb-with-class-instances-text/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syb-with-class-instances-text-$(haskell-syb-with-class-instances-text_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tagsoup
haskell-tagsoup:$(HASKELLTAGSOUP)_$(ARCH).deb
$(HASKELLTAGSOUP): $(SPREZZ)/haskell-tagsoup/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tagsoup-$(haskell-tagsoup_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tar
haskell-tar:$(HASKELLTAR)_$(ARCH).deb
$(HASKELLTAR): $(SPREZZ)/haskell-tar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tar-$(haskell-tar_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-terminfo
haskell-terminfo:$(HASKELLTERMINFO)_$(ARCH).deb
$(HASKELLTERMINFO): $(SPREZZ)/haskell-terminfo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf terminfo-$(haskell-terminfo_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-texmath
haskell-texmath:$(HASKELLTEXMATH)_$(ARCH).deb
$(HASKELLTEXMATH): $(SPREZZ)/haskell-texmath/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf texmath-$(haskell-texmath_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tokyocabinet
haskell-tokyocabinet:$(HASKELLTOKYOCABINET)_$(ARCH).deb
$(HASKELLTOKYOCABINET): $(SPREZZ)/haskell-tokyocabinet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tokyocabinet-$(haskell-tokyocabinet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-type-level
haskell-type-level:$(HASKELLTYPELEVEL)_$(ARCH).deb
$(HASKELLTYPELEVEL): $(SPREZZ)/haskell-type-level/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf type-level-$(haskell-type-level_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-uniplate
haskell-uniplate:$(HASKELLUNIPLATE)_$(ARCH).deb
$(HASKELLUNIPLATE): $(SPREZZ)/haskell-uniplate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uniplate-$(haskell-uniplate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unix-compat
haskell-unix-compat:$(HASKELLUNIXCOMPAT)_$(ARCH).deb
$(HASKELLUNIXCOMPAT): $(SPREZZ)/haskell-unix-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unix-compat-$(haskell-unix-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unixutils
haskell-unixutils:$(HASKELLUNIXUTILS)_$(ARCH).deb
$(HASKELLUNIXUTILS): $(SPREZZ)/haskell-unixutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Unixutils-$(haskell-unixutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-url
haskell-url:$(HASKELLURL)_$(ARCH).deb
$(HASKELLURL): $(SPREZZ)/haskell-url/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf url-$(haskell-url_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-utility-ht
haskell-utility-ht:$(HASKELLUTILITYHT)_$(ARCH).deb
$(HASKELLUTILITYHT): $(SPREZZ)/haskell-utility-ht/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf utility-ht-$(haskell-utility-ht_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-uulib
haskell-uulib:$(HASKELLUULIB)_$(ARCH).deb
$(HASKELLUULIB): $(SPREZZ)/haskell-uulib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uulib-$(haskell-uulib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vector-algorithms
haskell-vector-algorithms:$(HASKELLVECTORALGORITHMS)_$(ARCH).deb
$(HASKELLVECTORALGORITHMS): $(SPREZZ)/haskell-vector-algorithms/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vector-algorithms-$(haskell-vector-algorithms_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vte
haskell-vte:$(HASKELLVTE)_$(ARCH).deb
$(HASKELLVTE): $(SPREZZ)/haskell-vte/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vte-$(haskell-vte_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vty
haskell-vty:$(HASKELLVTY)_$(ARCH).deb
$(HASKELLVTY): $(SPREZZ)/haskell-vty/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vty-$(haskell-vty_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-webkit
haskell-webkit:$(HASKELLWEBKIT)_$(ARCH).deb
$(HASKELLWEBKIT): $(SPREZZ)/haskell-webkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf webkit-$(haskell-webkit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-x11
haskell-x11:$(HASKELLX11)_$(ARCH).deb
$(HASKELLX11): $(SPREZZ)/haskell-x11/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf X11-$(haskell-x11_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-x11-xft
haskell-x11-xft:$(HASKELLX11XFT)_$(ARCH).deb
$(HASKELLX11XFT): $(SPREZZ)/haskell-x11-xft/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf X11-xft-$(haskell-x11-xft_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xhtml
haskell-xhtml:$(HASKELLXHTML)_$(ARCH).deb
$(HASKELLXHTML): $(SPREZZ)/haskell-xhtml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xhtml-$(haskell-xhtml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xmonad-contrib
haskell-xmonad-contrib:$(HASKELLXMONADCONTRIB)_$(ARCH).deb
$(HASKELLXMONADCONTRIB): $(SPREZZ)/haskell-xmonad-contrib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmonad-contrib-$(haskell-xmonad-contrib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xmonad
haskell-xmonad:$(HASKELLXMONAD)_$(ARCH).deb
$(HASKELLXMONAD): $(SPREZZ)/haskell-xmonad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmonad-$(haskell-xmonad_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zip-archive
haskell-zip-archive:$(HASKELLZIPARCHIVE)_$(ARCH).deb
$(HASKELLZIPARCHIVE): $(SPREZZ)/haskell-zip-archive/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zip-archive-$(haskell-zip-archive_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-acid-state
haskell-acid-state:$(HASKELLACIDSTATE)_$(ARCH).deb
$(HASKELLACIDSTATE): $(SPREZZ)/haskell-acid-state/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acid-state-$(haskell-acid-state_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-active
haskell-active:$(HASKELLACTIVE)_$(ARCH).deb
$(HASKELLACTIVE): $(SPREZZ)/haskell-active/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf active-$(haskell-active_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-adjunctions
haskell-adjunctions:$(HASKELLADJUNCTIONS)_$(ARCH).deb
$(HASKELLADJUNCTIONS): $(SPREZZ)/haskell-adjunctions/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf adjunctions-$(haskell-adjunctions_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-aeson
haskell-aeson:$(HASKELLAESON)_$(ARCH).deb
$(HASKELLAESON): $(SPREZZ)/haskell-aeson/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf aeson-$(haskell-aeson_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-agda
haskell-agda:$(HASKELLAGDA)_$(ARCH).deb
$(HASKELLAGDA): $(SPREZZ)/haskell-agda/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf agda-$(haskell-agda_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-algebra
haskell-algebra:$(HASKELLALGEBRA)_$(ARCH).deb
$(HASKELLALGEBRA): $(SPREZZ)/haskell-algebra/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf algebra-$(haskell-algebra_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ami
haskell-ami:$(HASKELLAMI)_$(ARCH).deb
$(HASKELLAMI): $(SPREZZ)/haskell-ami/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf AMi-$(haskell-ami_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-augeas
haskell-augeas:$(HASKELLAUGEAS)_$(ARCH).deb
$(HASKELLAUGEAS): $(SPREZZ)/haskell-augeas/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf augeas-$(haskell-augeas_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-blaze-builder-enumerator
haskell-blaze-builder-enumerator:$(HASKELLBLAZEBUILDERENUMERATOR)_$(ARCH).deb
$(HASKELLBLAZEBUILDERENUMERATOR): $(SPREZZ)/haskell-blaze-builder-enumerator/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-builder-enumerator-$(haskell-blaze-builder-enumerator_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-boomerang
haskell-boomerang:$(HASKELLBOOMERANG)_$(ARCH).deb
$(HASKELLBOOMERANG): $(SPREZZ)/haskell-boomerang/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf boomerang-$(haskell-boomerang_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-bytestring-mmap
haskell-bytestring-mmap:$(HASKELLBYTESTRINGMMAP)_$(ARCH).deb
$(HASKELLBYTESTRINGMMAP): $(SPREZZ)/haskell-bytestring-mmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bytestring-mmap-$(haskell-bytestring-mmap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cereal-conduit
haskell-cereal-conduit:$(HASKELLCEREALCONDUIT)_$(ARCH).deb
$(HASKELLCEREALCONDUIT): $(SPREZZ)/haskell-cereal-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cereal-conduit-$(haskell-cereal-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cipher-aes
haskell-cipher-aes:$(HASKELLCIPHERAES)_$(ARCH).deb
$(HASKELLCIPHERAES): $(SPREZZ)/haskell-cipher-aes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-aes-$(haskell-cipher-aes_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cipher-rc4
haskell-cipher-rc4:$(HASKELLCIPHERRC4)_$(ARCH).deb
$(HASKELLCIPHERRC4): $(SPREZZ)/haskell-cipher-rc4/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-rc4-$(haskell-cipher-rc4_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-configurator
haskell-configurator:$(HASKELLCONFIGURATOR)_$(ARCH).deb
$(HASKELLCONFIGURATOR): $(SPREZZ)/haskell-configurator/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf configurator-$(haskell-configurator_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-contravariant
haskell-contravariant:$(HASKELLCONTRAVARIANT)_$(ARCH).deb
$(HASKELLCONTRAVARIANT): $(SPREZZ)/haskell-contravariant/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf contravariant-$(haskell-contravariant_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cpphs
haskell-cpphs:$(HASKELLCPPHS)_$(ARCH).deb
$(HASKELLCPPHS): $(SPREZZ)/haskell-cpphs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpphs-$(haskell-cpphs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-criterion
haskell-criterion:$(HASKELLCRITERION)_$(ARCH).deb
$(HASKELLCRITERION): $(SPREZZ)/haskell-criterion/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf criterion-$(haskell-criterion_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-crypto-conduit
haskell-crypto-conduit:$(HASKELLCRYPTOCONDUIT)_$(ARCH).deb
$(HASKELLCRYPTOCONDUIT): $(SPREZZ)/haskell-crypto-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-conduit-$(haskell-crypto-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-css-text
haskell-css-text:$(HASKELLCSSTEXT)_$(ARCH).deb
$(HASKELLCSSTEXT): $(SPREZZ)/haskell-css-text/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf css-text-$(haskell-css-text_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-csv-conduit
haskell-csv-conduit:$(HASKELLCSVCONDUIT)_$(ARCH).deb
$(HASKELLCSVCONDUIT): $(SPREZZ)/haskell-csv-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf csv-conduit-$(haskell-csv-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-darcs
haskell-darcs:$(HASKELLDARCS)_$(ARCH).deb
$(HASKELLDARCS): $(SPREZZ)/haskell-darcs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf darcs-$(haskell-darcs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-accessor-mtl
haskell-data-accessor-mtl:$(HASKELLDATAACCESSORMTL)_$(ARCH).deb
$(HASKELLDATAACCESSORMTL): $(SPREZZ)/haskell-data-accessor-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-accessor-mtl-$(haskell-data-accessor-mtl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-accessor-template
haskell-data-accessor-template:$(HASKELLDATAACCESSORTEMPLATE)_$(ARCH).deb
$(HASKELLDATAACCESSORTEMPLATE): $(SPREZZ)/haskell-data-accessor-template/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-accessor-template-$(haskell-data-accessor-template_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-binary-ieee754
haskell-data-binary-ieee754:$(HASKELLDATABINARYIEEE754)_$(ARCH).deb
$(HASKELLDATABINARYIEEE754): $(SPREZZ)/haskell-data-binary-ieee754/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-binary-ieee754-$(haskell-data-binary-ieee754_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-inttrie
haskell-data-inttrie:$(HASKELLDATAINTTRIE)_$(ARCH).deb
$(HASKELLDATAINTTRIE): $(SPREZZ)/haskell-data-inttrie/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-inttrie-$(haskell-data-inttrie_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-lens
haskell-data-lens:$(HASKELLDATALENS)_$(ARCH).deb
$(HASKELLDATALENS): $(SPREZZ)/haskell-data-lens/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-lens-$(haskell-data-lens_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-data-memocombinators
haskell-data-memocombinators:$(HASKELLDATAMEMOCOMBINATORS)_$(ARCH).deb
$(HASKELLDATAMEMOCOMBINATORS): $(SPREZZ)/haskell-data-memocombinators/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-memocombinators-$(haskell-data-memocombinators_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-date-cache
haskell-date-cache:$(HASKELLDATECACHE)_$(ARCH).deb
$(HASKELLDATECACHE): $(SPREZZ)/haskell-date-cache/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf date-cache-$(haskell-date-cache_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-diagrams-cairo
haskell-diagrams-cairo:$(HASKELLDIAGRAMSCAIRO)_$(ARCH).deb
$(HASKELLDIAGRAMSCAIRO): $(SPREZZ)/haskell-diagrams-cairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diagrams-cairo-$(haskell-diagrams-cairo_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-diagrams-core
haskell-diagrams-core:$(HASKELLDIAGRAMSCORE)_$(ARCH).deb
$(HASKELLDIAGRAMSCORE): $(SPREZZ)/haskell-diagrams-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diagrams-core-$(haskell-diagrams-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-diagrams-lib
haskell-diagrams-lib:$(HASKELLDIAGRAMSLIB)_$(ARCH).deb
$(HASKELLDIAGRAMSLIB): $(SPREZZ)/haskell-diagrams-lib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diagrams-lib-$(haskell-diagrams-lib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dimensional
haskell-dimensional:$(HASKELLDIMENSIONAL)_$(ARCH).deb
$(HASKELLDIMENSIONAL): $(SPREZZ)/haskell-dimensional/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dimensional-$(haskell-dimensional_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-directory-tree
haskell-directory-tree:$(HASKELLDIRECTORYTREE)_$(ARCH).deb
$(HASKELLDIRECTORYTREE): $(SPREZZ)/haskell-directory-tree/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf directory-tree-$(haskell-directory-tree_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-distributive
haskell-distributive:$(HASKELLDISTRIBUTIVE)_$(ARCH).deb
$(HASKELLDISTRIBUTIVE): $(SPREZZ)/haskell-distributive/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf distributive-$(haskell-distributive_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-download-curl
haskell-download-curl:$(HASKELLDOWNLOADCURL)_$(ARCH).deb
$(HASKELLDOWNLOADCURL): $(SPREZZ)/haskell-download-curl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf download-curl-$(haskell-download-curl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dpkg
haskell-dpkg:$(HASKELLDPKG)_$(ARCH).deb
$(HASKELLDPKG): $(SPREZZ)/haskell-dpkg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dpkg-$(haskell-dpkg_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dyre
haskell-dyre:$(HASKELLDYRE)_$(ARCH).deb
$(HASKELLDYRE): $(SPREZZ)/haskell-dyre/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dyre-$(haskell-dyre_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ekg
haskell-ekg:$(HASKELLEKG)_$(ARCH).deb
$(HASKELLEKG): $(SPREZZ)/haskell-ekg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ekg-$(haskell-ekg_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-email-validate
haskell-email-validate:$(HASKELLEMAILVALIDATE)_$(ARCH).deb
$(HASKELLEMAILVALIDATE): $(SPREZZ)/haskell-email-validate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf email-validate-$(haskell-email-validate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-exception-transformers
haskell-exception-transformers:$(HASKELLEXCEPTIONTRANSFORMERS)_$(ARCH).deb
$(HASKELLEXCEPTIONTRANSFORMERS): $(SPREZZ)/haskell-exception-transformers/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf exception-transformers-$(haskell-exception-transformers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-executable-path
haskell-executable-path:$(HASKELLEXECUTABLEPATH)_$(ARCH).deb
$(HASKELLEXECUTABLEPATH): $(SPREZZ)/haskell-executable-path/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf executable-path-$(haskell-executable-path_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-fclabels
haskell-fclabels:$(HASKELLFCLABELS)_$(ARCH).deb
$(HASKELLFCLABELS): $(SPREZZ)/haskell-fclabels/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fclabels-$(haskell-fclabels_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-fgl
haskell-fgl:$(HASKELLFGL)_$(ARCH).deb
$(HASKELLFGL): $(SPREZZ)/haskell-fgl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fgl-$(haskell-fgl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-file-embed
haskell-file-embed:$(HASKELLFILEEMBED)_$(ARCH).deb
$(HASKELLFILEEMBED): $(SPREZZ)/haskell-file-embed/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf file-embed-$(haskell-file-embed_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-file-location
haskell-file-location:$(HASKELLFILELOCATION)_$(ARCH).deb
$(HASKELLFILELOCATION): $(SPREZZ)/haskell-file-location/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf file-location-$(haskell-file-location_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-filesystem-conduit
haskell-filesystem-conduit:$(HASKELLFILESYSTEMCONDUIT)_$(ARCH).deb
$(HASKELLFILESYSTEMCONDUIT): $(SPREZZ)/haskell-filesystem-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf filesystem-conduit-$(haskell-filesystem-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-free
haskell-free:$(HASKELLFREE)_$(ARCH).deb
$(HASKELLFREE): $(SPREZZ)/haskell-free/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf free-$(haskell-free_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ftphs
haskell-ftphs:$(HASKELLFTPHS)_$(ARCH).deb
$(HASKELLFTPHS): $(SPREZZ)/haskell-ftphs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ftphs-$(haskell-ftphs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gd
haskell-gd:$(HASKELLGD)_$(ARCH).deb
$(HASKELLGD): $(SPREZZ)/haskell-gd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gd-$(haskell-gd_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ghc-syb-utils
haskell-ghc-syb-utils:$(HASKELLGHCSYBUTILS)_$(ARCH).deb
$(HASKELLGHCSYBUTILS): $(SPREZZ)/haskell-ghc-syb-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-syb-utils-$(haskell-ghc-syb-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-github
haskell-github:$(HASKELLGITHUB)_$(ARCH).deb
$(HASKELLGITHUB): $(SPREZZ)/haskell-github/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf github-$(haskell-github_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gitit
haskell-gitit:$(HASKELLGITIT)_$(ARCH).deb
$(HASKELLGITIT): $(SPREZZ)/haskell-gitit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gitit-$(haskell-gitit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-glade
haskell-glade:$(HASKELLGLADE)_$(ARCH).deb
$(HASKELLGLADE): $(SPREZZ)/haskell-glade/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glade-$(haskell-glade_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-glib
haskell-glib:$(HASKELLGLIB)_$(ARCH).deb
$(HASKELLGLIB): $(SPREZZ)/haskell-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glib-$(haskell-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-glut
haskell-glut:$(HASKELLGLUT)_$(ARCH).deb
$(HASKELLGLUT): $(SPREZZ)/haskell-glut/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GLUT-$(haskell-glut_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gtk
haskell-gtk:$(HASKELLGTK)_$(ARCH).deb
$(HASKELLGTK): $(SPREZZ)/haskell-gtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtk-$(haskell-gtk_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gtkglext
haskell-gtkglext:$(HASKELLGTKGLEXT)_$(ARCH).deb
$(HASKELLGTKGLEXT): $(SPREZZ)/haskell-gtkglext/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkglext-$(haskell-gtkglext_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hakyll
haskell-hakyll:$(HASKELLHAKYLL)_$(ARCH).deb
$(HASKELLHAKYLL): $(SPREZZ)/haskell-hakyll/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hakyll-$(haskell-hakyll_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hashmap
haskell-hashmap:$(HASKELLHASHMAP)_$(ARCH).deb
$(HASKELLHASHMAP): $(SPREZZ)/haskell-hashmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hashmap-$(haskell-hashmap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hashtables
haskell-hashtables:$(HASKELLHASHTABLES)_$(ARCH).deb
$(HASKELLHASHTABLES): $(SPREZZ)/haskell-hashtables/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hashtables-$(haskell-hashtables_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb
haskell-haskelldb:$(HASKELLHASKELLDB)_$(ARCH).deb
$(HASKELLHASKELLDB): $(SPREZZ)/haskell-haskelldb/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-$(haskell-haskelldb_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc
haskell-haskelldb-hdbc:$(HASKELLHASKELLDBHDBC)_$(ARCH).deb
$(HASKELLHASKELLDBHDBC): $(SPREZZ)/haskell-haskelldb-hdbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-$(haskell-haskelldb-hdbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-odbc
haskell-haskelldb-hdbc-odbc:$(HASKELLHASKELLDBHDBCODBC)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCODBC): $(SPREZZ)/haskell-haskelldb-hdbc-odbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-odbc-$(haskell-haskelldb-hdbc-odbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-postgresql
haskell-haskelldb-hdbc-postgresql:$(HASKELLHASKELLDBHDBCPOSTGRESQL)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCPOSTGRESQL): $(SPREZZ)/haskell-haskelldb-hdbc-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-postgresql-$(haskell-haskelldb-hdbc-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskelldb-hdbc-sqlite3
haskell-haskelldb-hdbc-sqlite3:$(HASKELLHASKELLDBHDBCSQLITE3)_$(ARCH).deb
$(HASKELLHASKELLDBHDBCSQLITE3): $(SPREZZ)/haskell-haskelldb-hdbc-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskelldb-hdbc-sqlite3-$(haskell-haskelldb-hdbc-sqlite3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haskell-lexer
haskell-haskell-lexer:$(HASKELLHASKELLLEXER)_$(ARCH).deb
$(HASKELLHASKELLLEXER): $(SPREZZ)/haskell-haskell-lexer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-lexer-$(haskell-haskell-lexer_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hastache
haskell-hastache:$(HASKELLHASTACHE)_$(ARCH).deb
$(HASKELLHASTACHE): $(SPREZZ)/haskell-hastache/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hastache-$(haskell-hastache_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-haxml
haskell-haxml:$(HASKELLHAXML)_$(ARCH).deb
$(HASKELLHAXML): $(SPREZZ)/haskell-haxml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haxml-$(haskell-haxml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hbro-contrib
haskell-hbro-contrib:$(HASKELLHBROCONTRIB)_$(ARCH).deb
$(HASKELLHBROCONTRIB): $(SPREZZ)/haskell-hbro-contrib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hbro-contrib-$(haskell-hbro-contrib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hbro
haskell-hbro:$(HASKELLHBRO)_$(ARCH).deb
$(HASKELLHBRO): $(SPREZZ)/haskell-hbro/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hbro-$(haskell-hbro_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hcard
haskell-hcard:$(HASKELLHCARD)_$(ARCH).deb
$(HASKELLHCARD): $(SPREZZ)/haskell-hcard/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hcard-$(haskell-hcard_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hcwiid
haskell-hcwiid:$(HASKELLHCWIID)_$(ARCH).deb
$(HASKELLHCWIID): $(SPREZZ)/haskell-hcwiid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hcwiid-$(haskell-hcwiid_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc
haskell-hdbc:$(HASKELLHDBC)_$(ARCH).deb
$(HASKELLHDBC): $(SPREZZ)/haskell-hdbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-$(haskell-hdbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-odbc
haskell-hdbc-odbc:$(HASKELLHDBCODBC)_$(ARCH).deb
$(HASKELLHDBCODBC): $(SPREZZ)/haskell-hdbc-odbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-odbc-$(haskell-hdbc-odbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-postgresql
haskell-hdbc-postgresql:$(HASKELLHDBCPOSTGRESQL)_$(ARCH).deb
$(HASKELLHDBCPOSTGRESQL): $(SPREZZ)/haskell-hdbc-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-postgresql-$(haskell-hdbc-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hdbc-sqlite3
haskell-hdbc-sqlite3:$(HASKELLHDBCSQLITE3)_$(ARCH).deb
$(HASKELLHDBCSQLITE3): $(SPREZZ)/haskell-hdbc-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-sqlite3-$(haskell-hdbc-sqlite3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hfuse
haskell-hfuse:$(HASKELLHFUSE)_$(ARCH).deb
$(HASKELLHFUSE): $(SPREZZ)/haskell-hfuse/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HFuse-$(haskell-hfuse_UPVER).tar.gz $(TARARGS) $@

.PHONY: highlighting-kate
highlighting-kate:$(HIGHLIGHTINGKATE)_$(ARCH).deb
$(HIGHLIGHTINGKATE): $(SPREZZ)/highlighting-kate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf highlighting-kate-$(highlighting-kate_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hipmunk
haskell-hipmunk:$(HASKELLHIPMUNK)_$(ARCH).deb
$(HASKELLHIPMUNK): $(SPREZZ)/haskell-hipmunk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Hipmunk-$(haskell-hipmunk_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hjsmin
haskell-hjsmin:$(HASKELLHJSMIN)_$(ARCH).deb
$(HASKELLHJSMIN): $(SPREZZ)/haskell-hjsmin/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hjsmin-$(haskell-hjsmin_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hledger
haskell-hledger:$(HASKELLHLEDGER)_$(ARCH).deb
$(HASKELLHLEDGER): $(SPREZZ)/haskell-hledger/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hledger-$(haskell-hledger_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hledger-lib
haskell-hledger-lib:$(HASKELLHLEDGERLIB)_$(ARCH).deb
$(HASKELLHLEDGERLIB): $(SPREZZ)/haskell-hledger-lib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hledger-lib-$(haskell-hledger-lib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hlint
haskell-hlint:$(HASKELLHLINT)_$(ARCH).deb
$(HASKELLHLINT): $(SPREZZ)/haskell-hlint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hlint-$(haskell-hlint_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hoauth
haskell-hoauth:$(HASKELLHOAUTH)_$(ARCH).deb
$(HASKELLHOAUTH): $(SPREZZ)/haskell-hoauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hoauth-$(haskell-hoauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hopenpgp
haskell-hopenpgp:$(HASKELLHOPENPGP)_$(ARCH).deb
$(HASKELLHOPENPGP): $(SPREZZ)/haskell-hopenpgp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hopenpgp-$(haskell-hopenpgp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hs-bibutils
haskell-hs-bibutils:$(HASKELLHSBIBUTILS)_$(ARCH).deb
$(HASKELLHSBIBUTILS): $(SPREZZ)/haskell-hs-bibutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hs-bibutils-$(haskell-hs-bibutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hscolour
haskell-hscolour:$(HASKELLHSCOLOUR)_$(ARCH).deb
$(HASKELLHSCOLOUR): $(SPREZZ)/haskell-hscolour/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hscolour-$(haskell-hscolour_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hsmagick
haskell-hsmagick:$(HASKELLHSMAGICK)_$(ARCH).deb
$(HASKELLHSMAGICK): $(SPREZZ)/haskell-hsmagick/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hsmagick-$(haskell-hsmagick_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hssyck
haskell-hssyck:$(HASKELLHSSYCK)_$(ARCH).deb
$(HASKELLHSSYCK): $(SPREZZ)/haskell-hssyck/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HsSyck-$(haskell-hssyck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-html-conduit
haskell-html-conduit:$(HASKELLHTMLCONDUIT)_$(ARCH).deb
$(HASKELLHTMLCONDUIT): $(SPREZZ)/haskell-html-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf html-conduit-$(haskell-html-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-http-date
haskell-http-date:$(HASKELLHTTPDATE)_$(ARCH).deb
$(HASKELLHTTPDATE): $(SPREZZ)/haskell-http-date/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf http-date-$(haskell-http-date_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-cache
haskell-hxt-cache:$(HASKELLHXTCACHE)_$(ARCH).deb
$(HASKELLHXTCACHE): $(SPREZZ)/haskell-hxt-cache/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-cache-$(haskell-hxt-cache_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-curl
haskell-hxt-curl:$(HASKELLHXTCURL)_$(ARCH).deb
$(HASKELLHXTCURL): $(SPREZZ)/haskell-hxt-curl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-curl-$(haskell-hxt-curl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-http
haskell-hxt-http:$(HASKELLHXTHTTP)_$(ARCH).deb
$(HASKELLHXTHTTP): $(SPREZZ)/haskell-hxt-http/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-http-$(haskell-hxt-http_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-relaxng
haskell-hxt-relaxng:$(HASKELLHXTRELAXNG)_$(ARCH).deb
$(HASKELLHXTRELAXNG): $(SPREZZ)/haskell-hxt-relaxng/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-relaxng-$(haskell-hxt-relaxng_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-tagsoup
haskell-hxt-tagsoup:$(HASKELLHXTTAGSOUP)_$(ARCH).deb
$(HASKELLHXTTAGSOUP): $(SPREZZ)/haskell-hxt-tagsoup/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-tagsoup-$(haskell-hxt-tagsoup_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-xpath
haskell-hxt-xpath:$(HASKELLHXTXPATH)_$(ARCH).deb
$(HASKELLHXTXPATH): $(SPREZZ)/haskell-hxt-xpath/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-xpath-$(haskell-hxt-xpath_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-xslt
haskell-hxt-xslt:$(HASKELLHXTXSLT)_$(ARCH).deb
$(HASKELLHXTXSLT): $(SPREZZ)/haskell-hxt-xslt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-xslt-$(haskell-hxt-xslt_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-iconv
haskell-iconv:$(HASKELLICONV)_$(ARCH).deb
$(HASKELLICONV): $(SPREZZ)/haskell-iconv/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf iconv-$(haskell-iconv_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ieee754
haskell-ieee754:$(HASKELLIEEE754)_$(ARCH).deb
$(HASKELLIEEE754): $(SPREZZ)/haskell-ieee754/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ieee754-$(haskell-ieee754_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-io-choice
haskell-io-choice:$(HASKELLIOCHOICE)_$(ARCH).deb
$(HASKELLIOCHOICE): $(SPREZZ)/haskell-io-choice/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf io-choice-$(haskell-io-choice_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-iospec
haskell-iospec:$(HASKELLIOSPEC)_$(ARCH).deb
$(HASKELLIOSPEC): $(SPREZZ)/haskell-iospec/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IOSpec-$(haskell-iospec_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-io-storage
haskell-io-storage:$(HASKELLIOSTORAGE)_$(ARCH).deb
$(HASKELLIOSTORAGE): $(SPREZZ)/haskell-io-storage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf io-storage-$(haskell-io-storage_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-iteratee
haskell-iteratee:$(HASKELLITERATEE)_$(ARCH).deb
$(HASKELLITERATEE): $(SPREZZ)/haskell-iteratee/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf iteratee-$(haskell-iteratee_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ixset
haskell-ixset:$(HASKELLIXSET)_$(ARCH).deb
$(HASKELLIXSET): $(SPREZZ)/haskell-ixset/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ixset-$(haskell-ixset_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-keys
haskell-keys:$(HASKELLKEYS)_$(ARCH).deb
$(HASKELLKEYS): $(SPREZZ)/haskell-keys/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf keys-$(haskell-keys_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-knob
haskell-knob:$(HASKELLKNOB)_$(ARCH).deb
$(HASKELLKNOB): $(SPREZZ)/haskell-knob/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf knob-$(haskell-knob_UPVER).tar.xz $(TARARGS) $@

.PHONY: haskell-lambdabot-utils
haskell-lambdabot-utils:$(HASKELLLAMBDABOTUTILS)_$(ARCH).deb
$(HASKELLLAMBDABOTUTILS): $(SPREZZ)/haskell-lambdabot-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lambdabot-utils-$(haskell-lambdabot-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-language-haskell-extract
haskell-language-haskell-extract:$(HASKELLLANGUAGEHASKELLEXTRACT)_$(ARCH).deb
$(HASKELLLANGUAGEHASKELLEXTRACT): $(SPREZZ)/haskell-language-haskell-extract/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf language-haskell-extract-$(haskell-language-haskell-extract_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-language-javascript
haskell-language-javascript:$(HASKELLLANGUAGEJAVASCRIPT)_$(ARCH).deb
$(HASKELLLANGUAGEJAVASCRIPT): $(SPREZZ)/haskell-language-javascript/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf language-javascript-$(haskell-language-javascript_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ldap
haskell-ldap:$(HASKELLLDAP)_$(ARCH).deb
$(HASKELLLDAP): $(SPREZZ)/haskell-ldap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ldap-$(haskell-ldap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-libtagc
haskell-libtagc:$(HASKELLLIBTAGC)_$(ARCH).deb
$(HASKELLLIBTAGC): $(SPREZZ)/haskell-libtagc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtagc-$(haskell-libtagc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-libzip
haskell-libzip:$(HASKELLLIBZIP)_$(ARCH).deb
$(HASKELLLIBZIP): $(SPREZZ)/haskell-libzip/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf LibZip-$(haskell-libzip_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-listlike
haskell-listlike:$(HASKELLLISTLIKE)_$(ARCH).deb
$(HASKELLLISTLIKE): $(SPREZZ)/haskell-listlike/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf listlike-$(haskell-listlike_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-llvm-base
haskell-llvm-base:$(HASKELLLLVMBASE)_$(ARCH).deb
$(HASKELLLLVMBASE): $(SPREZZ)/haskell-llvm-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf llvm-base-$(haskell-llvm-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-logict
haskell-logict:$(HASKELLLOGICT)_$(ARCH).deb
$(HASKELLLOGICT): $(SPREZZ)/haskell-logict/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf logict-$(haskell-logict_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-maccatcher
haskell-maccatcher:$(HASKELLMACCATCHER)_$(ARCH).deb
$(HASKELLMACCATCHER): $(SPREZZ)/haskell-maccatcher/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf maccatcher-$(haskell-maccatcher_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-magic
haskell-magic:$(HASKELLMAGIC)_$(ARCH).deb
$(HASKELLMAGIC): $(SPREZZ)/haskell-magic/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf magic-$(haskell-magic_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-math-functions
haskell-math-functions:$(HASKELLMATHFUNCTIONS)_$(ARCH).deb
$(HASKELLMATHFUNCTIONS): $(SPREZZ)/haskell-math-functions/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf math-functions-$(haskell-math-functions_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-maths
haskell-maths:$(HASKELLMATHS)_$(ARCH).deb
$(HASKELLMATHS): $(SPREZZ)/haskell-maths/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf foo-$(haskell-maths_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mbox
haskell-mbox:$(HASKELLMBOX)_$(ARCH).deb
$(HASKELLMBOX): $(SPREZZ)/haskell-mbox/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mbox-$(haskell-mbox_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-memotrie
haskell-memotrie:$(HASKELLMEMOTRIE)_$(ARCH).deb
$(HASKELLMEMOTRIE): $(SPREZZ)/haskell-memotrie/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MemoTrie-$(haskell-memotrie_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mersenne-random
haskell-mersenne-random:$(HASKELLMERSENNERANDOM)_$(ARCH).deb
$(HASKELLMERSENNERANDOM): $(SPREZZ)/haskell-mersenne-random/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mersenne-random-$(haskell-mersenne-random_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mime-mail
haskell-mime-mail:$(HASKELLMIMEMAIL)_$(ARCH).deb
$(HASKELLMIMEMAIL): $(SPREZZ)/haskell-mime-mail/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mime-mail-$(haskell-mime-mail_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mime-types
haskell-mime-types:$(HASKELLMIMETYPES)_$(ARCH).deb
$(HASKELLMIMETYPES): $(SPREZZ)/haskell-mime-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mime-types-$(haskell-mime-types_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monadcatchio-transformers
haskell-monadcatchio-transformers:$(HASKELLMONADCATCHIOTRANSFORMERS)_$(ARCH).deb
$(HASKELLMONADCATCHIOTRANSFORMERS): $(SPREZZ)/haskell-monadcatchio-transformers/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MonadCatchIO-transformers-$(haskell-monadcatchio-transformers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monadcryptorandom
haskell-monadcryptorandom:$(HASKELLMONADCRYPTORANDOM)_$(ARCH).deb
$(HASKELLMONADCRYPTORANDOM): $(SPREZZ)/haskell-monadcryptorandom/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monadcryptorandom-$(haskell-monadcryptorandom_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-loops
haskell-monad-loops:$(HASKELLMONADLOOPS)_$(ARCH).deb
$(HASKELLMONADLOOPS): $(SPREZZ)/haskell-monad-loops/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-loops-$(haskell-monad-loops_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-par
haskell-monad-par:$(HASKELLMONADPAR)_$(ARCH).deb
$(HASKELLMONADPAR): $(SPREZZ)/haskell-monad-par/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-par-$(haskell-monad-par_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monadrandom
haskell-monadrandom:$(HASKELLMONADRANDOM)_$(ARCH).deb
$(HASKELLMONADRANDOM): $(SPREZZ)/haskell-monadrandom/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MonadRandom-$(haskell-monadrandom_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-mtlparse
haskell-mtlparse:$(HASKELLMTLPARSE)_$(ARCH).deb
$(HASKELLMTLPARSE): $(SPREZZ)/haskell-mtlparse/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mtlparse-$(haskell-mtlparse_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-murmur-hash
haskell-murmur-hash:$(HASKELLMURMURHASH)_$(ARCH).deb
$(HASKELLMURMURHASH): $(SPREZZ)/haskell-murmur-hash/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf murmur-hash-$(haskell-murmur-hash_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ncurses
haskell-ncurses:$(HASKELLNCURSES)_$(ARCH).deb
$(HASKELLNCURSES): $(SPREZZ)/haskell-ncurses/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf haskell-ncurses_$(haskell-ncurses_UPVER).tar.xz $(TARARGS) $@

.PHONY: haskell-netwire
haskell-netwire:$(HASKELLNETWIRE)_$(ARCH).deb
$(HASKELLNETWIRE): $(SPREZZ)/haskell-netwire/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf netwire-$(haskell-netwire_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-network-conduit
haskell-network-conduit:$(HASKELLNETWORKCONDUIT)_$(ARCH).deb
$(HASKELLNETWORKCONDUIT): $(SPREZZ)/haskell-network-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-conduit-$(haskell-network-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-newtype
haskell-newtype:$(HASKELLNEWTYPE)_$(ARCH).deb
$(HASKELLNEWTYPE): $(SPREZZ)/haskell-newtype/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf newtype-$(haskell-newtype_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-numbers
haskell-numbers:$(HASKELLNUMBERS)_$(ARCH).deb
$(HASKELLNUMBERS): $(SPREZZ)/haskell-numbers/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf numbers-$(haskell-numbers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-numeric-quest
haskell-numeric-quest:$(HASKELLNUMERICQUEST)_$(ARCH).deb
$(HASKELLNUMERICQUEST): $(SPREZZ)/haskell-numeric-quest/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf numeric-quest-$(haskell-numeric-quest_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-numinstances
haskell-numinstances:$(HASKELLNUMINSTANCES)_$(ARCH).deb
$(HASKELLNUMINSTANCES): $(SPREZZ)/haskell-numinstances/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf numinstances-$(haskell-numinstances_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-numtype
haskell-numtype:$(HASKELLNUMTYPE)_$(ARCH).deb
$(HASKELLNUMTYPE): $(SPREZZ)/haskell-numtype/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf numtype-$(haskell-numtype_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-oeis
haskell-oeis:$(HASKELLOEIS)_$(ARCH).deb
$(HASKELLOEIS): $(SPREZZ)/haskell-oeis/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf oeis-$(haskell-oeis_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-openpgp-asciiarmor
haskell-openpgp-asciiarmor:$(HASKELLOPENPGPASCIIARMOR)_$(ARCH).deb
$(HASKELLOPENPGPASCIIARMOR): $(SPREZZ)/haskell-openpgp-asciiarmor/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openpgp-asciiarmor-$(haskell-openpgp-asciiarmor_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-openpgp
haskell-openpgp:$(HASKELLOPENPGP)_$(ARCH).deb
$(HASKELLOPENPGP): $(SPREZZ)/haskell-openpgp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openpgp-$(haskell-openpgp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-options
haskell-options:$(HASKELLOPTIONS)_$(ARCH).deb
$(HASKELLOPTIONS): $(SPREZZ)/haskell-options/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf haskell-options_$(haskell-options_UPVER).tar.xz $(TARARGS) $@

.PHONY: haskell-pandoc
haskell-pandoc:$(HASKELLPANDOC)_$(ARCH).deb
$(HASKELLPANDOC): $(SPREZZ)/haskell-pandoc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pandoc-$(haskell-pandoc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pandoc-types
haskell-pandoc-types:$(HASKELLPANDOCTYPES)_$(ARCH).deb
$(HASKELLPANDOCTYPES): $(SPREZZ)/haskell-pandoc-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pandoc-types-$(haskell-pandoc-types_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-parseargs
haskell-parseargs:$(HASKELLPARSEARGS)_$(ARCH).deb
$(HASKELLPARSEARGS): $(SPREZZ)/haskell-parseargs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parseargs-$(haskell-parseargs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pastis
haskell-pastis:$(HASKELLPASTIS)_$(ARCH).deb
$(HASKELLPASTIS): $(SPREZZ)/haskell-pastis/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pastis-$(haskell-pastis_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-path-pieces
haskell-path-pieces:$(HASKELLPATHPIECES)_$(ARCH).deb
$(HASKELLPATHPIECES): $(SPREZZ)/haskell-path-pieces/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf path-pieces-$(haskell-path-pieces_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-patience
haskell-patience:$(HASKELLPATIENCE)_$(ARCH).deb
$(HASKELLPATIENCE): $(SPREZZ)/haskell-patience/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf patience-$(haskell-patience_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pcap
haskell-pcap:$(HASKELLPCAP)_$(ARCH).deb
$(HASKELLPCAP): $(SPREZZ)/haskell-pcap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pcap-$(haskell-pcap_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-persistent
haskell-persistent:$(HASKELLPERSISTENT)_$(ARCH).deb
$(HASKELLPERSISTENT): $(SPREZZ)/haskell-persistent/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf persistent-$(haskell-persistent_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-persistent-postgresql
haskell-persistent-postgresql:$(HASKELLPERSISTENTPOSTGRESQL)_$(ARCH).deb
$(HASKELLPERSISTENTPOSTGRESQL): $(SPREZZ)/haskell-persistent-postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf persistent-postgresql-$(haskell-persistent-postgresql_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-persistent-sqlite
haskell-persistent-sqlite:$(HASKELLPERSISTENTSQLITE)_$(ARCH).deb
$(HASKELLPERSISTENTSQLITE): $(SPREZZ)/haskell-persistent-sqlite/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf persistent-sqlite-$(haskell-persistent-sqlite_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-persistent-template
haskell-persistent-template:$(HASKELLPERSISTENTTEMPLATE)_$(ARCH).deb
$(HASKELLPERSISTENTTEMPLATE): $(SPREZZ)/haskell-persistent-template/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf persistent-template-$(haskell-persistent-template_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pool-conduit
haskell-pool-conduit:$(HASKELLPOOLCONDUIT)_$(ARCH).deb
$(HASKELLPOOLCONDUIT): $(SPREZZ)/haskell-pool-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pool-conduit-$(haskell-pool-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-postgresql-libpq
haskell-postgresql-libpq:$(HASKELLPOSTGRESQLLIBPQ)_$(ARCH).deb
$(HASKELLPOSTGRESQLLIBPQ): $(SPREZZ)/haskell-postgresql-libpq/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf postgresql-libpq-$(haskell-postgresql-libpq_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-postgresql-simple
haskell-postgresql-simple:$(HASKELLPOSTGRESQLSIMPLE)_$(ARCH).deb
$(HASKELLPOSTGRESQLSIMPLE): $(SPREZZ)/haskell-postgresql-simple/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf postgresql-simple-$(haskell-postgresql-simple_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-primes
haskell-primes:$(HASKELLPRIMES)_$(ARCH).deb
$(HASKELLPRIMES): $(SPREZZ)/haskell-primes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf primes-$(haskell-primes_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-psqueue
haskell-psqueue:$(HASKELLPSQUEUE)_$(ARCH).deb
$(HASKELLPSQUEUE): $(SPREZZ)/haskell-psqueue/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf PSQueue-$(haskell-psqueue_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-puremd5
haskell-puremd5:$(HASKELLPUREMD5)_$(ARCH).deb
$(HASKELLPUREMD5): $(SPREZZ)/haskell-puremd5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pureMD5-$(haskell-puremd5_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pwstore-fast
haskell-pwstore-fast:$(HASKELLPWSTOREFAST)_$(ARCH).deb
$(HASKELLPWSTOREFAST): $(SPREZZ)/haskell-pwstore-fast/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pwstore-fast-$(haskell-pwstore-fast_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck2
haskell-quickcheck2:$(HASKELLQUICKCHECK2)_$(ARCH).deb
$(HASKELLQUICKCHECK2): $(SPREZZ)/haskell-quickcheck2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf quickcheck2-$(haskell-quickcheck2_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck-instances
haskell-quickcheck-instances:$(HASKELLQUICKCHECKINSTANCES)_$(ARCH).deb
$(HASKELLQUICKCHECKINSTANCES): $(SPREZZ)/haskell-quickcheck-instances/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf quickcheck-instances-$(haskell-quickcheck-instances_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-random-shuffle
haskell-random-shuffle:$(HASKELLRANDOMSHUFFLE)_$(ARCH).deb
$(HASKELLRANDOMSHUFFLE): $(SPREZZ)/haskell-random-shuffle/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf random-shuffle-$(haskell-random-shuffle_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ranged-sets
haskell-ranged-sets:$(HASKELLRANGEDSETS)_$(ARCH).deb
$(HASKELLRANGEDSETS): $(SPREZZ)/haskell-ranged-sets/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Ranged-sets-$(haskell-ranged-sets_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ranges
haskell-ranges:$(HASKELLRANGES)_$(ARCH).deb
$(HASKELLRANGES): $(SPREZZ)/haskell-ranges/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ranges-$(haskell-ranges_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-reactive-banana
haskell-reactive-banana:$(HASKELLREACTIVEBANANA)_$(ARCH).deb
$(HASKELLREACTIVEBANANA): $(SPREZZ)/haskell-reactive-banana/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf reactive-banana-$(haskell-reactive-banana_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-readline
haskell-readline:$(HASKELLREADLINE)_$(ARCH).deb
$(HASKELLREADLINE): $(SPREZZ)/haskell-readline/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf readline-$(haskell-readline_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-pcre
haskell-regex-pcre:$(HASKELLREGEXPCRE)_$(ARCH).deb
$(HASKELLREGEXPCRE): $(SPREZZ)/haskell-regex-pcre/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-pcre-$(haskell-regex-pcre_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regexpr
haskell-regexpr:$(HASKELLREGEXPR)_$(ARCH).deb
$(HASKELLREGEXPR): $(SPREZZ)/haskell-regexpr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regexpr-$(haskell-regexpr_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-representable-functors
haskell-representable-functors:$(HASKELLREPRESENTABLEFUNCTORS)_$(ARCH).deb
$(HASKELLREPRESENTABLEFUNCTORS): $(SPREZZ)/haskell-representable-functors/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf representable-functors-$(haskell-representable-functors_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-representable-tries
haskell-representable-tries:$(HASKELLREPRESENTABLETRIES)_$(ARCH).deb
$(HASKELLREPRESENTABLETRIES): $(SPREZZ)/haskell-representable-tries/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf representable-tries-$(haskell-representable-tries_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-resource-pool
haskell-resource-pool:$(HASKELLRESOURCEPOOL)_$(ARCH).deb
$(HASKELLRESOURCEPOOL): $(SPREZZ)/haskell-resource-pool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf resource-pool-$(haskell-resource-pool_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-rsa
haskell-rsa:$(HASKELLRSA)_$(ARCH).deb
$(HASKELLRSA): $(SPREZZ)/haskell-rsa/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf RSA-$(haskell-rsa_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-safecopy
haskell-safecopy:$(HASKELLSAFECOPY)_$(ARCH).deb
$(HASKELLSAFECOPY): $(SPREZZ)/haskell-safecopy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf safecopy-$(haskell-safecopy_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-semigroupoids
haskell-semigroupoids:$(HASKELLSEMIGROUPOIDS)_$(ARCH).deb
$(HASKELLSEMIGROUPOIDS): $(SPREZZ)/haskell-semigroupoids/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf semigroupoids-$(haskell-semigroupoids_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-sfml-audio
haskell-sfml-audio:$(HASKELLSFMLAUDIO)_$(ARCH).deb
$(HASKELLSFMLAUDIO): $(SPREZZ)/haskell-sfml-audio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sfml-audio-$(haskell-sfml-audio_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare-css
haskell-shakespeare-css:$(HASKELLSHAKESPEARECSS)_$(ARCH).deb
$(HASKELLSHAKESPEARECSS): $(SPREZZ)/haskell-shakespeare-css/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-css-$(haskell-shakespeare-css_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare-i18n
haskell-shakespeare-i18n:$(HASKELLSHAKESPEAREI18N)_$(ARCH).deb
$(HASKELLSHAKESPEAREI18N): $(SPREZZ)/haskell-shakespeare-i18n/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-i18n-$(haskell-shakespeare-i18n_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare-js
haskell-shakespeare-js:$(HASKELLSHAKESPEAREJS)_$(ARCH).deb
$(HASKELLSHAKESPEAREJS): $(SPREZZ)/haskell-shakespeare-js/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-js-$(haskell-shakespeare-js_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare-text
haskell-shakespeare-text:$(HASKELLSHAKESPEARETEXT)_$(ARCH).deb
$(HASKELLSHAKESPEARETEXT): $(SPREZZ)/haskell-shakespeare-text/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-text-$(haskell-shakespeare-text_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shellac
haskell-shellac:$(HASKELLSHELLAC)_$(ARCH).deb
$(HASKELLSHELLAC): $(SPREZZ)/haskell-shellac/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Shellac-$(haskell-shellac_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-show
haskell-show:$(HASKELLSHOW)_$(ARCH).deb
$(HASKELLSHOW): $(SPREZZ)/haskell-show/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf show-$(haskell-show_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-simpleea
haskell-simpleea:$(HASKELLSIMPLEEA)_$(ARCH).deb
$(HASKELLSIMPLEEA): $(SPREZZ)/haskell-simpleea/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf simpleea-$(haskell-simpleea_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-simpleirc
haskell-simpleirc:$(HASKELLSIMPLEIRC)_$(ARCH).deb
$(HASKELLSIMPLEIRC): $(SPREZZ)/haskell-simpleirc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf simpleirc-$(haskell-simpleirc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-simple-sendfile
haskell-simple-sendfile:$(HASKELLSIMPLESENDFILE)_$(ARCH).deb
$(HASKELLSIMPLESENDFILE): $(SPREZZ)/haskell-simple-sendfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf simple-sendfile-$(haskell-simple-sendfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-smallcheck
haskell-smallcheck:$(HASKELLSMALLCHECK)_$(ARCH).deb
$(HASKELLSMALLCHECK): $(SPREZZ)/haskell-smallcheck/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf smallcheck-$(haskell-smallcheck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-snap-core
haskell-snap-core:$(HASKELLSNAPCORE)_$(ARCH).deb
$(HASKELLSNAPCORE): $(SPREZZ)/haskell-snap-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf snap-core-$(haskell-snap-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-snap-server
haskell-snap-server:$(HASKELLSNAPSERVER)_$(ARCH).deb
$(HASKELLSNAPSERVER): $(SPREZZ)/haskell-snap-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf snap-server-$(haskell-snap-server_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-statevar
haskell-statevar:$(HASKELLSTATEVAR)_$(ARCH).deb
$(HASKELLSTATEVAR): $(SPREZZ)/haskell-statevar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf StateVar-$(haskell-statevar_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-static-hash
haskell-static-hash:$(HASKELLSTATICHASH)_$(ARCH).deb
$(HASKELLSTATICHASH): $(SPREZZ)/haskell-static-hash/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf static-hash-$(haskell-static-hash_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-strict
haskell-strict:$(HASKELLSTRICT)_$(ARCH).deb
$(HASKELLSTRICT): $(SPREZZ)/haskell-strict/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf strict-$(haskell-strict_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-stringsearch
haskell-stringsearch:$(HASKELLSTRINGSEARCH)_$(ARCH).deb
$(HASKELLSTRINGSEARCH): $(SPREZZ)/haskell-stringsearch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stringsearch-$(haskell-stringsearch_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-strptime
haskell-strptime:$(HASKELLSTRPTIME)_$(ARCH).deb
$(HASKELLSTRPTIME): $(SPREZZ)/haskell-strptime/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf strptime-$(haskell-strptime_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-system-fileio
haskell-system-fileio:$(HASKELLSYSTEMFILEIO)_$(ARCH).deb
$(HASKELLSYSTEMFILEIO): $(SPREZZ)/haskell-system-fileio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf system-fileio-$(haskell-system-fileio_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tagstream-conduit
haskell-tagstream-conduit:$(HASKELLTAGSTREAMCONDUIT)_$(ARCH).deb
$(HASKELLTAGSTREAMCONDUIT): $(SPREZZ)/haskell-tagstream-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tagstream-conduit-$(haskell-tagstream-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-template
haskell-template:$(HASKELLTEMPLATE)_$(ARCH).deb
$(HASKELLTEMPLATE): $(SPREZZ)/haskell-template/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf template-$(haskell-template_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-temporary
haskell-temporary:$(HASKELLTEMPORARY)_$(ARCH).deb
$(HASKELLTEMPORARY): $(SPREZZ)/haskell-temporary/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf temporary-$(haskell-temporary_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-th
haskell-test-framework-th:$(HASKELLTESTFRAMEWORKTH)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKTH): $(SPREZZ)/haskell-test-framework-th/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-th-$(haskell-test-framework-th_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-th-prime
haskell-test-framework-th-prime:$(HASKELLTESTFRAMEWORKTHPRIME)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKTHPRIME): $(SPREZZ)/haskell-test-framework-th-prime/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-th-prime-$(haskell-test-framework-th-prime_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-text-icu
haskell-text-icu:$(HASKELLTEXTICU)_$(ARCH).deb
$(HASKELLTEXTICU): $(SPREZZ)/haskell-text-icu/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf text-icu-$(haskell-text-icu_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tinyurl
haskell-tinyurl:$(HASKELLTINYURL)_$(ARCH).deb
$(HASKELLTINYURL): $(SPREZZ)/haskell-tinyurl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf TinyURL-$(haskell-tinyurl_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tokyocabinet
haskell-tokyocabinet:$(HASKELLTOKYOCABINET)_$(ARCH).deb
$(HASKELLTOKYOCABINET): $(SPREZZ)/haskell-tokyocabinet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tokyocabinet-$(haskell-tokyocabinet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unix-bytestring
haskell-unix-bytestring:$(HASKELLUNIXBYTESTRING)_$(ARCH).deb
$(HASKELLUNIXBYTESTRING): $(SPREZZ)/haskell-unix-bytestring/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unix-bytestring-$(haskell-unix-bytestring_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unix-time
haskell-unix-time:$(HASKELLUNIXTIME)_$(ARCH).deb
$(HASKELLUNIXTIME): $(SPREZZ)/haskell-unix-time/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unix-time-$(haskell-unix-time_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-unlambda
haskell-unlambda:$(HASKELLUNLAMBDA)_$(ARCH).deb
$(HASKELLUNLAMBDA): $(SPREZZ)/haskell-unlambda/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unlambda-$(haskell-unlambda_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-uri
haskell-uri:$(HASKELLURI)_$(ARCH).deb
$(HASKELLURI): $(SPREZZ)/haskell-uri/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uri-$(haskell-uri_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-utf8-light
haskell-utf8-light:$(HASKELLUTF8LIGHT)_$(ARCH).deb
$(HASKELLUTF8LIGHT): $(SPREZZ)/haskell-utf8-light/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf utf8-light-$(haskell-utf8-light_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-uuagc-cabal
haskell-uuagc-cabal:$(HASKELLUUAGCCABAL)_$(ARCH).deb
$(HASKELLUUAGCCABAL): $(SPREZZ)/haskell-uuagc-cabal/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uuagc-cabal-$(haskell-uuagc-cabal_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-uuid
haskell-uuid:$(HASKELLUUID)_$(ARCH).deb
$(HASKELLUUID): $(SPREZZ)/haskell-uuid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uuid-$(haskell-uuid_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vector-space
haskell-vector-space:$(HASKELLVECTORSPACE)_$(ARCH).deb
$(HASKELLVECTORSPACE): $(SPREZZ)/haskell-vector-space/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vector-space-$(haskell-vector-space_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vector-space-points
haskell-vector-space-points:$(HASKELLVECTORSPACEPOINTS)_$(ARCH).deb
$(HASKELLVECTORSPACEPOINTS): $(SPREZZ)/haskell-vector-space-points/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vector-space-points-$(haskell-vector-space-points_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-app-file-cgi
haskell-wai-app-file-cgi:$(HASKELLWAIAPPFILECGI)_$(ARCH).deb
$(HASKELLWAIAPPFILECGI): $(SPREZZ)/haskell-wai-app-file-cgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-app-file-cgi-$(haskell-wai-app-file-cgi_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-app-static
haskell-wai-app-static:$(HASKELLWAIAPPSTATIC)_$(ARCH).deb
$(HASKELLWAIAPPSTATIC): $(SPREZZ)/haskell-wai-app-static/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-app-static-$(haskell-wai-app-static_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-eventsource
haskell-wai-eventsource:$(HASKELLWAIEVENTSOURCE)_$(ARCH).deb
$(HASKELLWAIEVENTSOURCE): $(SPREZZ)/haskell-wai-eventsource/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-eventsource-$(haskell-wai-eventsource_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-extra
haskell-wai-extra:$(HASKELLWAIEXTRA)_$(ARCH).deb
$(HASKELLWAIEXTRA): $(SPREZZ)/haskell-wai-extra/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-extra-$(haskell-wai-extra_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-logger-prefork
haskell-wai-logger-prefork:$(HASKELLWAILOGGERPREFORK)_$(ARCH).deb
$(HASKELLWAILOGGERPREFORK): $(SPREZZ)/haskell-wai-logger-prefork/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-logger-prefork-$(haskell-wai-logger-prefork_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-test
haskell-wai-test:$(HASKELLWAITEST)_$(ARCH).deb
$(HASKELLWAITEST): $(SPREZZ)/haskell-wai-test/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-test-$(haskell-wai-test_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-warp-tls
haskell-warp-tls:$(HASKELLWARPTLS)_$(ARCH).deb
$(HASKELLWARPTLS): $(SPREZZ)/haskell-warp-tls/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf warp-tls-$(haskell-warp-tls_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wash
haskell-wash:$(HASKELLWASH)_$(ARCH).deb
$(HASKELLWASH): $(SPREZZ)/haskell-wash/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wash-$(haskell-wash_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-web-routes
haskell-web-routes:$(HASKELLWEBROUTES)_$(ARCH).deb
$(HASKELLWEBROUTES): $(SPREZZ)/haskell-web-routes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf web-routes-$(haskell-web-routes_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-weighted-regexp
haskell-weighted-regexp:$(HASKELLWEIGHTEDREGEXP)_$(ARCH).deb
$(HASKELLWEIGHTEDREGEXP): $(SPREZZ)/haskell-weighted-regexp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf weighted-regexp-$(haskell-weighted-regexp_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xdg-basedir
haskell-xdg-basedir:$(HASKELLXDGBASEDIR)_$(ARCH).deb
$(HASKELLXDGBASEDIR): $(SPREZZ)/haskell-xdg-basedir/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xdg-basedir-$(haskell-xdg-basedir_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml2html
haskell-xml2html:$(HASKELLXML2HTML)_$(ARCH).deb
$(HASKELLXML2HTML): $(SPREZZ)/haskell-xml2html/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml2html-$(haskell-xml2html_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xmonad-contrib
haskell-xmonad-contrib:$(HASKELLXMONADCONTRIB)_$(ARCH).deb
$(HASKELLXMONADCONTRIB): $(SPREZZ)/haskell-xmonad-contrib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmonad-contrib-$(haskell-xmonad-contrib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xmonad
haskell-xmonad:$(HASKELLXMONAD)_$(ARCH).deb
$(HASKELLXMONAD): $(SPREZZ)/haskell-xmonad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmonad-$(haskell-xmonad_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xss-sanitize
haskell-xss-sanitize:$(HASKELLXSSSANITIZE)_$(ARCH).deb
$(HASKELLXSSSANITIZE): $(SPREZZ)/haskell-xss-sanitize/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xss-sanitize-$(haskell-xss-sanitize_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yaml
haskell-yaml:$(HASKELLYAML)_$(ARCH).deb
$(HASKELLYAML): $(SPREZZ)/haskell-yaml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yaml-$(haskell-yaml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yaml-light
haskell-yaml-light:$(HASKELLYAMLLIGHT)_$(ARCH).deb
$(HASKELLYAMLLIGHT): $(SPREZZ)/haskell-yaml-light/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yaml-light-$(haskell-yaml-light_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-auth
haskell-yesod-auth:$(HASKELLYESODAUTH)_$(ARCH).deb
$(HASKELLYESODAUTH): $(SPREZZ)/haskell-yesod-auth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-auth-$(haskell-yesod-auth_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-auth-oauth
haskell-yesod-auth-oauth:$(HASKELLYESODAUTHOAUTH)_$(ARCH).deb
$(HASKELLYESODAUTHOAUTH): $(SPREZZ)/haskell-yesod-auth-oauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-auth-oauth-$(haskell-yesod-auth-oauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-core
haskell-yesod-core:$(HASKELLYESODCORE)_$(ARCH).deb
$(HASKELLYESODCORE): $(SPREZZ)/haskell-yesod-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-core-$(haskell-yesod-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-default
haskell-yesod-default:$(HASKELLYESODDEFAULT)_$(ARCH).deb
$(HASKELLYESODDEFAULT): $(SPREZZ)/haskell-yesod-default/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-default-$(haskell-yesod-default_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-form
haskell-yesod-form:$(HASKELLYESODFORM)_$(ARCH).deb
$(HASKELLYESODFORM): $(SPREZZ)/haskell-yesod-form/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-form-$(haskell-yesod-form_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-json
haskell-yesod-json:$(HASKELLYESODJSON)_$(ARCH).deb
$(HASKELLYESODJSON): $(SPREZZ)/haskell-yesod-json/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-json-$(haskell-yesod-json_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-markdown
haskell-yesod-markdown:$(HASKELLYESODMARKDOWN)_$(ARCH).deb
$(HASKELLYESODMARKDOWN): $(SPREZZ)/haskell-yesod-markdown/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-markdown-$(haskell-yesod-markdown_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-persistent
haskell-yesod-persistent:$(HASKELLYESODPERSISTENT)_$(ARCH).deb
$(HASKELLYESODPERSISTENT): $(SPREZZ)/haskell-yesod-persistent/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-persistent-$(haskell-yesod-persistent_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-routes
haskell-yesod-routes:$(HASKELLYESODROUTES)_$(ARCH).deb
$(HASKELLYESODROUTES): $(SPREZZ)/haskell-yesod-routes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-routes-$(haskell-yesod-routes_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-yesod-test
haskell-yesod-test:$(HASKELLYESODTEST)_$(ARCH).deb
$(HASKELLYESODTEST): $(SPREZZ)/haskell-yesod-test/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yesod-test-$(haskell-yesod-test_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zeromq-haskell
haskell-zeromq-haskell:$(HASKELLZEROMQHASKELL)_$(ARCH).deb
$(HASKELLZEROMQHASKELL): $(SPREZZ)/haskell-zeromq-haskell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zeromq-haskell-$(haskell-zeromq-haskell_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib-enum
haskell-zlib-enum:$(HASKELLZLIBENUM)_$(ARCH).deb
$(HASKELLZLIBENUM): $(SPREZZ)/haskell-zlib-enum/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-enum-$(haskell-zlib-enum_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-utf8-string
haskell-utf8-string:$(HASKELLUTF8STRING)_$(ARCH).deb
$(HASKELLUTF8STRING): $(SPREZZ)/haskell-utf8-string/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf utf8-string-$(haskell-utf8-string_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pcre-light
haskell-pcre-light:$(HASKELLPCRELIGHT)_$(ARCH).deb
$(HASKELLPCRELIGHT): $(SPREZZ)/haskell-pcre-light/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pcre-light-$(haskell-pcre-light_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-chasingbottoms
haskell-chasingbottoms:$(HASKELLCHASINGBOTTOMS)_$(ARCH).deb
$(HASKELLCHASINGBOTTOMS): $(SPREZZ)/haskell-chasingbottoms/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ChasingBottoms-$(haskell-chasingbottoms_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gluraw
haskell-gluraw:$(HASKELLGLURAW)_$(ARCH).deb
$(HASKELLGLURAW): $(SPREZZ)/haskell-gluraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GLURaw-$(haskell-gluraw_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-openglraw
haskell-openglraw:$(HASKELLOPENGLRAW)_$(ARCH).deb
$(HASKELLOPENGLRAW): $(SPREZZ)/haskell-openglraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf OpenGLRaw-$(haskell-openglraw_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-charproperties
haskell-hxt-charproperties:$(HASKELLHXTCHARPROPERTIES)_$(ARCH).deb
$(HASKELLHXTCHARPROPERTIES): $(SPREZZ)/haskell-hxt-charproperties/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-charproperties-$(haskell-hxt-charproperties_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-regex-xmlschema
haskell-hxt-regex-xmlschema:$(HASKELLHXTREGEXXMLSCHEMA)_$(ARCH).deb
$(HASKELLHXTREGEXXMLSCHEMA): $(SPREZZ)/haskell-hxt-regex-xmlschema/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-regex-xmlschema-$(haskell-hxt-regex-xmlschema_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hxt-unicode
haskell-hxt-unicode:$(HASKELLHXTUNICODE)_$(ARCH).deb
$(HASKELLHXTUNICODE): $(SPREZZ)/haskell-hxt-unicode/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-unicode-$(haskell-hxt-unicode_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-byteorder
haskell-byteorder:$(HASKELLBYTEORDER)_$(ARCH).deb
$(HASKELLBYTEORDER): $(SPREZZ)/haskell-byteorder/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf byteorder-$(haskell-byteorder_UPVER).tar.gz $(TARARGS) $@

.PHONY: happy
happy:$(HAPPY)_$(ARCH).deb
$(HAPPY): $(SPREZZ)/happy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf happy-$(happy_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hspec-discover
haskell-hspec-discover:$(HASKELLHSPECDISCOVER)_$(ARCH).deb
$(HASKELLHSPECDISCOVER): $(SPREZZ)/haskell-hspec-discover/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hspec-discover-$(haskell-hspec-discover_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-cabal-install
haskell-cabal-install:$(HASKELLCABALINSTALL)_$(ARCH).deb
$(HASKELLCABALINSTALL): $(SPREZZ)/haskell-cabal-install/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cabal-install-$(haskell-cabal-install_UPVER).tar.gz $(TARARGS) $@

.PHONY: haxml
haxml:$(HAXML)_$(ARCH).deb
$(HAXML): $(SPREZZ)/haxml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HaXml-$(haxml_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-string-qq
haskell-string-qq:$(HASKELLSTRINGQQ)_$(ARCH).deb
$(HASKELLSTRINGQQ): $(SPREZZ)/haskell-string-qq/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf string-qq-$(haskell-string-qq_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tensor
haskell-tensor:$(HASKELLTENSOR)_$(ARCH).deb
$(HASKELLTENSOR): $(SPREZZ)/haskell-tensor/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Tensor-$(haskell-tensor_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-objectname
haskell-objectname:$(HASKELLOBJECTNAME)_$(ARCH).deb
$(HASKELLOBJECTNAME): $(SPREZZ)/haskell-objectname/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ObjectName-$(haskell-objectname_UPVER).tar.gz $(TARARGS) $@

.PHONY: hscolour
hscolour:$(HSCOLOUR)_$(ARCH).deb
$(HSCOLOUR): $(SPREZZ)/hscolour/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hscolour-$(hscolour_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-threads
haskell-threads:$(HASKELLTHREADS)_$(ARCH).deb
$(HASKELLTHREADS): $(SPREZZ)/haskell-threads/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf threads-$(haskell-threads_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-time-compat
haskell-time-compat:$(HASKELLTIMECOMPAT)_$(ARCH).deb
$(HASKELLTIMECOMPAT): $(SPREZZ)/haskell-time-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf time-compat-$(haskell-time-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monoid-extras
haskell-monoid-extras:$(HASKELLMONOIDEXTRAS)_$(ARCH).deb
$(HASKELLMONOIDEXTRAS): $(SPREZZ)/haskell-monoid-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monoid-extras-$(haskell-monoid-extras_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dual-tree
haskell-dual-tree:$(HASKELLDUALTREE)_$(ARCH).deb
$(HASKELLDUALTREE): $(SPREZZ)/haskell-dual-tree/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dual-tree-$(haskell-dual-tree_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-logger
haskell-monad-logger:$(HASKELLMONADLOGGER)_$(ARCH).deb
$(HASKELLMONADLOGGER): $(SPREZZ)/haskell-monad-logger/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-logger-$(haskell-monad-logger_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gnutls
haskell-gnutls:$(HASKELLGNUTLS)_$(ARCH).deb
$(HASKELLGNUTLS): $(SPREZZ)/haskell-gnutls/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnutls-$(haskell-gnutls_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-abstract-deque
haskell-abstract-deque:$(HASKELLABSTRACTDEQUE)_$(ARCH).deb
$(HASKELLABSTRACTDEQUE): $(SPREZZ)/haskell-abstract-deque/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf abstract-deque-$(haskell-abstract-deque_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-par-extras
haskell-monad-par-extras:$(HASKELLMONADPAREXTRAS)_$(ARCH).deb
$(HASKELLMONADPAREXTRAS): $(SPREZZ)/haskell-monad-par-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-par-extras-$(haskell-monad-par-extras_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-abstract-par
haskell-abstract-par:$(HASKELLABSTRACTPAR)_$(ARCH).deb
$(HASKELLABSTRACTPAR): $(SPREZZ)/haskell-abstract-par/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf abstract-par-$(haskell-abstract-par_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-blaze-builder-conduit
haskell-blaze-builder-conduit:$(HASKELLBLAZEBUILDERCONDUIT)_$(ARCH).deb
$(HASKELLBLAZEBUILDERCONDUIT): $(SPREZZ)/haskell-blaze-builder-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-builder-conduit-$(haskell-blaze-builder-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-simple-reflect
haskell-simple-reflect:$(HASKELLSIMPLEREFLECT)_$(ARCH).deb
$(HASKELLSIMPLEREFLECT): $(SPREZZ)/haskell-simple-reflect/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf simple-reflect-$(haskell-simple-reflect_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-gnuidn
haskell-gnuidn:$(HASKELLGNUIDN)_$(ARCH).deb
$(HASKELLGNUIDN): $(SPREZZ)/haskell-gnuidn/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnuidn-$(haskell-gnuidn_UPVER).tar.gz $(TARARGS) $@

.PHONY: xmonad
xmonad:$(XMONAD)_$(ARCH).deb
$(XMONAD): $(SPREZZ)/xmonad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmonad-$(xmonad_UPVER).tar.gz $(TARARGS) $@

.PHONY: tokyocabinet-haskell
tokyocabinet-haskell:$(TOKYOCABINETHASKELL)_$(ARCH).deb
$(TOKYOCABINETHASKELL): $(SPREZZ)/tokyocabinet-haskell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tokyocabinet-haskell-$(tokyocabinet-haskell_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-hs3
haskell-hs3:$(HASKELLHS3)_$(ARCH).deb
$(HASKELLHS3): $(SPREZZ)/haskell-hs3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hS3-$(haskell-hs3_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-network-info
haskell-network-info:$(HASKELLNETWORKINFO)_$(ARCH).deb
$(HASKELLNETWORKINFO): $(SPREZZ)/haskell-network-info/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-info-$(haskell-network-info_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-fsnotify
haskell-fsnotify:$(HASKELLFSNOTIFY)_$(ARCH).deb
$(HASKELLFSNOTIFY): $(SPREZZ)/haskell-fsnotify/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fsnotify-$(haskell-fsnotify_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-http-reverse-proxy
haskell-http-reverse-proxy:$(HASKELLHTTPREVERSEPROXY)_$(ARCH).deb
$(HASKELLHTTPREVERSEPROXY): $(SPREZZ)/haskell-http-reverse-proxy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf http-reverse-proxy-$(haskell-http-reverse-proxy_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-classy-prelude-conduit
haskell-classy-prelude-conduit:$(HASKELLCLASSYPRELUDECONDUIT)_$(ARCH).deb
$(HASKELLCLASSYPRELUDECONDUIT): $(SPREZZ)/haskell-classy-prelude-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf classy-prelude-conduit-$(haskell-classy-prelude-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-classy-prelude
haskell-classy-prelude:$(HASKELLCLASSYPRELUDE)_$(ARCH).deb
$(HASKELLCLASSYPRELUDE): $(SPREZZ)/haskell-classy-prelude/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf classy-prelude-$(haskell-classy-prelude_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-basic-prelude
haskell-basic-prelude:$(HASKELLBASICPRELUDE)_$(ARCH).deb
$(HASKELLBASICPRELUDE): $(SPREZZ)/haskell-basic-prelude/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf basic-prelude-$(haskell-basic-prelude_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-readargs
haskell-readargs:$(HASKELLREADARGS)_$(ARCH).deb
$(HASKELLREADARGS): $(SPREZZ)/haskell-readargs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ReadArgs-$(haskell-readargs_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-word8
haskell-word8:$(HASKELLWORD8)_$(ARCH).deb
$(HASKELLWORD8): $(SPREZZ)/haskell-word8/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf word8-$(haskell-word8_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-publicsuffixlist
haskell-publicsuffixlist:$(HASKELLPUBLICSUFFIXLIST)_$(ARCH).deb
$(HASKELLPUBLICSUFFIXLIST): $(SPREZZ)/haskell-publicsuffixlist/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf publicsuffixlist-$(haskell-publicsuffixlist_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-optparse-applicative
haskell-optparse-applicative:$(HASKELLOPTPARSEAPPLICATIVE)_$(ARCH).deb
$(HASKELLOPTPARSEAPPLICATIVE): $(SPREZZ)/haskell-optparse-applicative/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf optparse-applicative-$(haskell-optparse-applicative_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-project-template
haskell-project-template:$(HASKELLPROJECTTEMPLATE)_$(ARCH).deb
$(HASKELLPROJECTTEMPLATE): $(SPREZZ)/haskell-project-template/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf project-template-$(haskell-project-template_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-base64-conduit
haskell-base64-conduit:$(HASKELLBASE64CONDUIT)_$(ARCH).deb
$(HASKELLBASE64CONDUIT): $(SPREZZ)/haskell-base64-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base64-conduit-$(haskell-base64-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ifelse
haskell-ifelse:$(HASKELLIFELSE)_$(ARCH).deb
$(HASKELLIFELSE): $(SPREZZ)/haskell-ifelse/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IfElse-$(haskell-ifelse_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-network-multicast
haskell-network-multicast:$(HASKELLNETWORKMULTICAST)_$(ARCH).deb
$(HASKELLNETWORKMULTICAST): $(SPREZZ)/haskell-network-multicast/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-multicast-$(haskell-network-multicast_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-dav
haskell-dav:$(HASKELLDAV)_$(ARCH).deb
$(HASKELLDAV): $(SPREZZ)/haskell-dav/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DAV-$(haskell-dav_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-safesemaphore
haskell-safesemaphore:$(HASKELLSAFESEMAPHORE)_$(ARCH).deb
$(HASKELLSAFESEMAPHORE): $(SPREZZ)/haskell-safesemaphore/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SafeSemaphore-$(haskell-safesemaphore_UPVER).tar.gz $(TARARGS) $@

.PHONY: drift
drift:$(DRIFT)_$(ARCH).deb
$(DRIFT): $(SPREZZ)/drift/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DrIFT-$(drift_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-rosezipper
haskell-rosezipper:$(HASKELLROSEZIPPER)_$(ARCH).deb
$(HASKELLROSEZIPPER): $(SPREZZ)/haskell-rosezipper/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rosezipper-$(haskell-rosezipper_UPVER).tar.gz $(TARARGS) $@

.PHONY: ghc-testsuite
ghc-testsuite:$(GHCTESTSUITE)_$(ARCH).deb
$(GHCTESTSUITE): $(SPREZZ)/ghc-testsuite/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ghc-$(ghc-testsuite_UPVER)-testsuite.tar.bz2 $(TARARGS) $@

.PHONY: hlint
hlint:$(HLINT)_$(ARCH).deb
$(HLINT): $(SPREZZ)/hlint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hlint-$(hlint_UPVER).tar.gz $(TARARGS) $@

.PHONY: hpodder
hpodder:$(HPODDER)_$(ARCH).deb
$(HPODDER): $(SPREZZ)/hpodder/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hpodder_$(hpodder_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: hdbc-sqlite3
hdbc-sqlite3:$(HDBCSQLITE3)_$(ARCH).deb
$(HDBCSQLITE3): $(SPREZZ)/hdbc-sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc-sqlite3_$(hdbc-sqlite3_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: hdbc
hdbc:$(HDBC)_$(ARCH).deb
$(HDBC): $(SPREZZ)/hdbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdbc_$(hdbc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: haskell-lrucache
haskell-lrucache:$(HASKELLLRUCACHE)_$(ARCH).deb
$(HASKELLLRUCACHE): $(SPREZZ)/haskell-lrucache/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-lrucache_$(haskell-lrucache_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: haskell-reflection
haskell-reflection:$(HASKELLREFLECTION)_$(ARCH).deb
$(HASKELLREFLECTION): $(SPREZZ)/haskell-reflection/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf haskell-reflection_$(haskell-reflection_UPVER).orig.tar.gz $(TARARGS) $@

