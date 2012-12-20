CC := gcc
CFLAGS := -g
LIBS := -levent

PROGS := event-test signal-test time-test

all : $(PROGS)

% : %.c
	$(CC) $(CFLAGS) -o $@ $< $(LIBS)

clean :
	rm $(PROGS)

.PHONY : all clean
