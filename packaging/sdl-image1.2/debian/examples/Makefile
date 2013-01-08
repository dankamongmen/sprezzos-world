# Makefile for showimage

CC = gcc
CFLAGS = -Wall -O
LIBS = -I/usr/include/SDL -lSDL_image
EXE = showimage

all: $(EXE)

showimage: showimage.c Makefile
	$(CC) -o $@ $@.c $(CFLAGS) $(LIBS)

clean:
	-rm *.o $(EXE)
