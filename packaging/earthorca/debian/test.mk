LOCALE := fr_FR.UTF-8
HAS_LOCALE := $(shell locale -a | grep $(LOCALE:UTF-8=utf8))

debian/locales/%:
	mkdir -p debian/locales
	localedef -f $(word 2,$(subst ., ,$(notdir $@))) -i $(word 1,$(subst ., ,$(notdir $@))) $@

APP_TESTS := reftest crashtest jstestbrowser
TESTS := check xpcshell-tests $(APP_TESTS)

override_dh_auto_test: $(TESTS)

ifndef HAS_LOCALE
xpcshell-tests: export LOCPATH = $(CURDIR)/debian/locales
endif
xpcshell-tests: export LC_ALL=$(LOCALE)
xpcshell-tests: export EXTRA_TEST_ARGS += --app-path=$(CURDIR)/build-browser/dist/bin
$(APP_TESTS): export EXTRA_TEST_ARGS += --appname=$(CURDIR)/build-browser/dist/bin/firefox
$(APP_TESTS): export GRE_HOME = $(CURDIR)/build-xulrunner/dist/bin
$(APP_TESTS) xpcshell-tests: XVFB_RUN = xvfb-run -s "-screen 0 1024x768x24"
$(TESTS): export MOZ_PLUGIN_PATH = $(CURDIR)/build-xulrunner/dist/bin/plugins

ifeq ($(DEB_BUILD_ARCH),armel)
# Force armel JIT to compile ARMv4T instructions at runtime even when the buildd
# is > ARMv4T
$(TESTS): export ARM_FORCE_PLATFORM=4
endif

$(CURDIR)/build-browser/dist/bin/xulrunner:
	ln -s ../../../build-xulrunner/dist/bin $@

$(TESTS): $(CURDIR)/build-browser/dist/bin/xulrunner
	GNOME22_USER_DIR="$(CURDIR)/build-xulrunner/dist/.gnome2" \
	HOME="$(CURDIR)/build-xulrunner/dist" \
	$(XVFB_RUN) $(MAKE) -C build-xulrunner $@ 2>&1 | sed -u 's/^/$@> /'

xpcshell-tests: $(if $(HAS_LOCALE),,debian/locales/$(LOCALE))

xpcshell-tests-skip:
# This one fails because it supposes some kind of preexisting gnome/mailcap configuration
	rm -f build-xulrunner/_tests/xpcshell/uriloader/exthandler/tests/unit/test_handlerService.js

check-skip:
# This one fails because it only works in an american time zone. bz#515254
	rm -f js/src/jit-test/tests/sunspider/check-date-format-tofte.js

override_dh_auto_clean::
	rm -rf debian/locales

$(TESTS): %: %-skip

.PHONY: test $(TESTS) $(TESTS:%=%-skip)
