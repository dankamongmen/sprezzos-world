# use autoopts-config to determine
# 1) the locatation autogen      (autoopts-config --autogen)
# 2) the data dir for autogen    (autoopts-config --pkgdatadir)
# 3) the include dir for autogen (autoopts-config --cflags)
# 4) the lib dir for autogen     (autoopts-config --libs)

AUTOGEN_PROGRAM=$(shell autoopts-config --autogen)
AUTOGEN_DATADIR=$(shell autoopts-config --pkgdatadir)
AUTOGEN_FLAGS=-L$(AUTOGEN_DATADIR)

AUTOGEN_CFLAGS=$(shell autoopts-config --cflags)
AUTOGEN_LDFLAGS=$(shell autoopts-config --libs)

CFLAGS+=$(AUTOGEN_CFLAGS) -DTEST_CHECK_OPTS
LDFLAGS+=$(AUTOGEN_LDFLAGS)

.PHONY: build
build: main

.PHONY: clean
clean:
	rm -f *.o main checkopt.c checkopt.h

main: checkopt.c main.c
checkopt.c checkopt.h: checkopt.def

# additional rules
%.c: %.def
	$(AUTOGEN_PROGRAM) $(AUTOGEN_FLAGS) $^
