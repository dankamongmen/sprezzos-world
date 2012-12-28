include ../../globalmakeflags

SUBDIRS=db frontend
INSTALLSUBDIRS=$(addprefix install-,$(SUBDIRS))
CLEANSUBDIRS=$(addprefix clean-,$(SUBDIRS))

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

install: $(INSTALLSUBDIRS)

$(INSTALLSUBDIRS):
	$(MAKE) -C $(patsubst install-%,%,$@) install

clean: $(CLEANSUBDIRS)

$(CLEANSUBDIRS):
	$(MAKE) -C $(patsubst clean-%,%,$@) clean

.PHONY: all install clean $(SUBDIRS) $(INSTALLSUBDIRS) $(CLEANSUBDIRS)
