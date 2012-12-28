include ../../../globalmakeflags

MODULES=$(DB_MODULES)
INSTALLMODULES=$(addprefix install-,$(MODULES))
CLEANMODULES=$(addprefix clean-,$(MODULES))
SOBJS=$(addsuffix .so,$(MODULES))

all: $(MODULES)

$(MODULES):
	$(MAKE) -C $@
	cp $@/$@.so .

install: $(INSTALLMODULES)

$(INSTALLMODULES):
	$(MAKE) -C $(patsubst install-%,%,$@) install

clean: $(CLEANMODULES)

$(CLEANMODULES):
	$(MAKE) -C $(patsubst clean-%,%,$@) clean
	-@rm -f $(SOBJS)

.PHONY: all install clean $(MODULES) $(INSTALLMODULES) $(CLEANMODULES)
