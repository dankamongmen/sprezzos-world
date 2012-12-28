# This is wrong 8:P  (Hey, big nose? Or girlie hair?)
MODULE=corba
SOBJ = corba.so
OBJS = corba.opic
include ../modules.mak

PROJ = dcf
PROJ_SERV = $(PROJ)-text
PROJ_SERVSRC = $($(PROJ)-text:%=%.c)
ORBIT_IDL = /usr/bin/orbit-idl
ORBIT_IDLFLAGS = --skeleton-impl
ORBIT_IDLTARGETS = $(PROJ)-stubs.c $(PROJ)-skels.c $(PROJ)-common.c $(PROJ).h
GNOME_CFLAGS = $(shell gnome-config --cflags gnome gnomeui gnorba gtk)
GNOME_LIBS = $(shell gnome-config --libs gnome gnomeui gnorba gtk)
CFLAGS += $(MODCFLAGS) $(GNOME_CFLAGS)
LDFLAGS = $(MODLDFLAGS) $(GNOME_LIBS)

$(PROJ)-all: $(ORBIT_IDLTARGETS) $(PROJ_SERV)

$(OBJS): $(ORBIT_IDLTARGETS)

$(PROJ)-text: $(PROJ)-text.o $(PROJ)-skels.o $(PROJ)-common.o
	$(CC) -o $(PROJ)-text $^ $(LDFLAGS)

$(ORBIT_IDLTARGETS): $(PROJ).idl
	$(ORBIT_IDL) $(ORBIT_IDLFLAGS) $<

$(PROJ_SERVSRC): $(PROJ).h

$(PROJ)-clean:
	-rm *.[oa] $(PROJ)-text

$(PROJ)-distclean: $(PROJ)-clean
	-rm -f $(PROJ)-stubs.[oc] $(PROJ)-skels.[oc] $(PROJ)-skelimpl.[oc] \
		$(PROJ)-common.[oc] $(PROJ).h

.PHONY: $(PROJ)-all $(PROJ)-clean $(PROJ)-distclean
