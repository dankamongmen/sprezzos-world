SUBDIRS = script static_app binary_module

.PHONY: $(SUBDIRS)

%:: $(SUBDIRS)
	@:

$(SUBDIRS)::
	$(MAKE) -C $(SUBDIR) $@ $(MAKECMDGOALS)
