CFLAGS=-Wall -W -ggdb -D_GNU_SOURCE
LDFLAGS=
OBJS=$(subst .c,.o,$(wildcard *.c))
BIN=anna
LIBS=-ldebconfclient -ldebian-installer
STRIP=strip

ifdef DEBUG
CFLAGS:=$(CFLAGS) -g3 -DDODEBUG
LDFLAGS:=-g
endif

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) -o $(BIN) $(OBJS) $(LIBS)

# Size optimized and stripped binary target.
small: CFLAGS:=-Os -fomit-frame-pointer $(CFLAGS) -DSMALL
small: clean $(BIN)
	$(STRIP) --remove-section=.comment --remove-section=.note $(BIN)
	ls -l $(BIN)

clean:
	-rm -f $(BIN) $(OBJS) *~

anna.o util.o retriever.o: anna.h
anna.o retriever.o: util.h
anna.o retriever.o: retriever.h
