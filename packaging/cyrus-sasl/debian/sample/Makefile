#!/usr/bin/make -f

INCDIR1=$(T)/../include
INCDIR2=$(T)/include
LIBDIR=$(T)/lib/.libs

all: sample-server sample-client

sample-server: sample-server.c
	gcc -g -o sample-server sample-server.c -I. -I$(T) -I$(INCDIR1) -I$(INCDIR2) -L$(LIBDIR) -lsasl2

sample-client: sample-client.c
	gcc -g -o sample-client sample-client.c -I. -I$(T) -I$(INCDIR1) -I$(INCDIR2) -L$(LIBDIR) -lsasl2
