.PHONY: pidgin-facebookchat
pidgin-facebookchat:$(PIDGINFACEBOOKCHAT)_$(ARCH).deb
$(PIDGINFACEBOOKCHAT): $(SPREZZ)/pidgin-facebookchat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pidgin-facebookchat-source-$(pidgin-facebookchat_UPVER).tar.bz2 $(TARARGS) $@

