GIMPTOOL = /usr/bin/gimptool-2.0

%: %.c
	CFLAGS="$(CFLAGS) $(EXTRA_CFLAGS)" LDFLAGS="$(LFGLAGS) $(EXTRA_LDFLAGS)" $(GIMPTOOL) --build $<

build: $(PLUGIN)

install: build
	install -m 755 $(PLUGIN) $(DESTDIR)$(PLUGINBINDIR)

clean:
	rm -f $(PLUGIN)

.PHONY: install clean build
