include ../../globalmakeflags
CFLAGS += -I..
BIN=configtest dbtest
OBJS=$(subst .c,.o,$(wildcard *.c))
LDFLAGS += -Wl,-rpath,$(shell cd ..; pwd) -ldl

all: $(BIN)

configtest: ../strutl.o ../configuration.o configtest.o ../debug.o
	$(CC) $(LDFLAGS) -o $@ $^

dbtest: ../strutl.o ../configuration.o ../priority.o ../template.o ../question.o ../database.o dbtest.o ../debug.o
	$(CC) -o $@ $^ $(LDFLAGS)

fetest: ../strutl.o ../configuration.o ../priority.o ../template.o ../question.o ../database.o ../frontend.o fetest.o
	$(CC) -o $@ $^ $(LDFLAGS)

#strtest: ../strutl.o strtest.o
#	$(CC) -o $@ $^ $(LDFLAGS)

clean:
	-rm -f $(BIN) $(OBJS) *.dat *~
