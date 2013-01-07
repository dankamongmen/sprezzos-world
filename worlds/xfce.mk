.PHONY: thunar
thunar:$(THUNAR)_$(ARCH).deb
$(THUNAR): $(SPREZZ)/thunar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Thunar-$(thunar_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfconf
xfconf:$(XFCONF)_$(ARCH).deb
$(XFCONF): $(SPREZZ)/xfconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfconf-$(xfconf_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libxfce4ui
libxfce4ui:$(LIBXFCE4UI)_$(ARCH).deb
$(LIBXFCE4UI): $(SPREZZ)/libxfce4ui/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libxfce4ui-$(libxfce4ui_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libxfce4util
libxfce4util:$(LIBXFCE4UTIL)_$(ARCH).deb
$(LIBXFCE4UTIL): $(SPREZZ)/libxfce4util/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libxfce4util-$(libxfce4util_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-session
xfce4-session:$(XFCE4SESSION)_$(ARCH).deb
$(XFCE4SESSION): $(SPREZZ)/xfce4-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfce4-session-$(xfce4-session_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-terminal
xfce4-terminal:$(XFCE4TERMINAL)_$(ARCH).deb
$(XFCE4TERMINAL): $(SPREZZ)/xfce4-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Terminal-$(xfce4-terminal_UPVER).tar.bz2 $(TARARGS) $@

