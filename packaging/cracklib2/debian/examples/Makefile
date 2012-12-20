# Makefile for cracklib2 example.

CC=gcc
CFLAGS=-g -O2 -Wall
LDFLAGS=

cracklib_example: cracklib_example.o
	$(CC) $(LDFLAGS) -o $@ -lcrack $^

clean:
	rm -f *.o *~ cracklib_example

