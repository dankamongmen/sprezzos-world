BUILD_DIR := debian/build
BUILD_DIR_TARGETS := build build-arch build-indep install

$(BUILD_DIR_TARGETS): builddir
	$(MAKE) -f debian/rules -C $(BUILD_DIR) $@ USE_BUILD_DIR=TRUE

cp_excludes := .pc debian
cp_targets =  $(filter-out $(cp_excludes),$(wildcard *))

builddir: tarcopy
tarcopy: debian/stamp-tarcopy
debian/stamp-tarcopy:
	mkdir -p $(BUILD_DIR)
	tar cf - $(cp_targets) | tar xvf - -C $(BUILD_DIR)
	ln -sv $(CURDIR)/debian $(BUILD_DIR)
	touch $@

clean: clean_extras
clean_extras:
	rm -rf $(BUILD_DIR) $(wildcard debian/stamp-*)

.PHONY: builddir tarcopy clean_extras
