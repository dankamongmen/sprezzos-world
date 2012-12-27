# makefile snippet for removing dependency_libs lines from .la files

$(patsubst %,binary-install/%,$(DEB_PACKAGES)) :: binary-install/%:
	for file in $(wildcard debian/$(cdbs_curpkg)/usr/lib/*.la); do \
		sed -i "/dependency_libs/ s/'.*'/''/" $$file ; \
	done
ifneq (,$(DEB_HOST_MULTIARCH))
	for file in $(wildcard debian/$(cdbs_curpkg)/usr/lib/$(DEB_HOST_MULTIARCH)/*.la); do \
		sed -i "/dependency_libs/ s/'.*'/''/" $$file ; \
	done
endif
