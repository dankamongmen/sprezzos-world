CFLAGS += -Wall

LUA_CFLAGS := $(shell pkg-config lua5.1 --cflags)
LUA_LINK_FLAGS := $(shell pkg-config lua5.1 --libs)

all: test

app: CFLAGS += $(LUA_CFLAGS) $(LUA_LINK_FLAGS)

test: app
	./app

clean:
	$(RM) *.o app
