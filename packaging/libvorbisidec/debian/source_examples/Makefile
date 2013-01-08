#!/usr/bin/make

# makefile documenting how to build example code from the
# libvorbisidec ("tremor") library distribution.

# Author: Daniel Kahn Gillmor <dkg@fifthhorseman.net>

TARGETS = ivorbisfile_example iseeking_example

all: $(TARGETS)

%: %.c
	gcc -g -o $@ $< -lvorbisidec

clean:
	rm -f $(TARGETS)

# it is OK to unzip gzipped source (dh_installexamples auto-compresses
# larger files in the examples directory)
%.c: %.c.gz
	gzip -d $<
# but save any .c files that get unzipped!
.PRECIOUS: %.c

.PHONY: all clean
