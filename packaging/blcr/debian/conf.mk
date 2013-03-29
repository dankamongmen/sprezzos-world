#!/usr/bin/make -f
# Configuration for BLCR in Debian
define exported
ifneq (,$(shell grep '^.*[[:space:]]$(1)[[:space:]]vmlinux[[:space:]]EXPORT_SYMBOL' $(srctree)/Module.symvers))
export $(2)=y
export EXTRA_CFLAGS += -D$(2)
endif
endef

