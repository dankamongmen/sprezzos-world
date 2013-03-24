CC=gcc
CFLAGS=-O2
LDFLAGS=-lucl

all: simple uclpack

%.c: %.c.gz
	gzip -d < "$<" > "$@"

simple: simple.o

uclpack: uclpack.o

clean:
	rm -f simple uclpack *.o

.PHONY: all clean
