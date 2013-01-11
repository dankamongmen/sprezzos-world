.PHONY: garcon
garcon:$(GARCON)_$(ARCH).deb
$(GARCON): $(SPREZZ)/garcon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf garcon-$(garcon_UPVER).tar.bz2 $(TARARGS) $@

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

.PHONY: thunar
thunar:$(THUNAR)_$(ARCH).deb
$(THUNAR): $(SPREZZ)/thunar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Thunar-$(thunar_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: tumbler
tumbler:$(TUMBLER)_$(ARCH).deb
$(TUMBLER): $(SPREZZ)/tumbler/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf tumbler-$(tumbler_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-session
xfce4-session:$(XFCE4SESSION)_$(ARCH).deb
$(XFCE4SESSION): $(SPREZZ)/xfce4-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfce4-session-$(xfce4-session_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-settings
xfce4-settings:$(XFCE4SETTINGS)_$(ARCH).deb
$(XFCE4SETTINGS): $(SPREZZ)/xfce4-settings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfce4-settings-$(xfce4-settings_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-terminal
xfce4-terminal:$(XFCE4TERMINAL)_$(ARCH).deb
$(XFCE4TERMINAL): $(SPREZZ)/xfce4-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Terminal-$(xfce4-terminal_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfce4-utils
xfce4-utils:$(XFCE4UTILS)_$(ARCH).deb
$(XFCE4UTILS): $(SPREZZ)/xfce4-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfce-utils-$(xfce4-utils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfconf
xfconf:$(XFCONF)_$(ARCH).deb
$(XFCONF): $(SPREZZ)/xfconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfconf-$(xfconf_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfdesktop4
xfdesktop4:$(XFDESKTOP4)_$(ARCH).deb
$(XFDESKTOP4): $(SPREZZ)/xfdesktop4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfdesktop-$(xfdesktop4_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xfwm4
xfwm4:$(XFWM4)_$(ARCH).deb
$(XFWM4): $(SPREZZ)/xfwm4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xfwm4-$(xfwm4_UPVER).tar.bz2 $(TARARGS) $@
