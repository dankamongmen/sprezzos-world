#!/usr/bin/make -f
SHELL= /bin/sh

# Define standard compile/install flags
CC = gcc
CFLAGS = -O2 -Wall
INSTALL = install
INCLS = -I. 
DEFS = 
CFLAGS += $(DEFS) $(INCLS)
LDFLAGS = 
LIBS =	/usr/lib/libmilter/libmilter.a \
	/usr/lib/libmilter/libsm.a \
	-lldap -llber \
	-lbind \
	-pthread

all: sample

sample: sample.c 
	@rm -f $@
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $? $(LIBS)

clean:
	rm -rf sample *~
