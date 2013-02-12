LIBTOOL = libtool --silent --tag=CC

LUA_CFLAGS := $(shell pkg-config lua5.1 --cflags)
LUA_LDFLAGS := $(shell pkg-config lua5.1 --libs)
LUA_LDSFLAGS := $(shell pkg-config lua5.1 --static --libs)
# this is the path where you'll eventually install the module
RPATH=$(shell pkg-config lua5.1 --define-variable=prefix=/usr/local \
	--variable=INSTALL_CMOD)

# Following is a build and use example for a binary Lua module "foo".  The
# implementation is in "lua-foo.c".  To support dynamic loading conventions
# it will need to have an open function named "luaopen_foo" which registers
# the global module "foo".

test: test-dynamic test-static test-static-all

# compile source to make objects for static and dynamic link
lua-foo.lo: lua-foo.c
	$(LIBTOOL) --mode=compile $(CC) -c -Wall -O2 $(LUA_CFLAGS) lua-foo.c

# link objects to make static and dynamic libraries.  The .so will be
# left in "./.libs/".  Note that the Lua library and its dependencies are
# not called out on the link line since they are assumed to be part of
# whatever our library is linked to.  We want to avoid duplicate library
# copies, which is a waste of space and can cause run-time problems.
liblua-foo.la foo.so: lua-foo.lo
	$(LIBTOOL) --mode=link $(CC) \
		-rpath $(RPATH) $(LUA_LDSFLAGS) -o liblua-foo.la lua-foo.lo
	ln -sf ./.libs/liblua-foo.so foo.so

# If all went well, we can dynamically load the module into Lua.  The
# following will load the library into the interpreter and call a function.
test-dynamic: foo.so
	lua5.1 -l foo -e 'print(foo.get_greeting())'

app: app.c liblua-foo.la
	$(LIBTOOL) --mode=link $(CC) -Wall -O2 $(LUA_CFLAGS) \
		-static $(LUA_LDFLAGS) -o app app.c liblua-foo.la

app-s: app.c liblua-foo.la
	$(LIBTOOL) --mode=link $(CC) -Wall -O2 $(LUA_CFLAGS) \
		-static -Wc,-static $(LUA_LDSFLAGS) -o app-s app.c liblua-foo.la

test-static: app
	./app
	ldd app

test-static-all: app-s
	./app-s
	ldd app-s || true

# install static and dynamic libraries for module to global location
install: liblua-foo.la
	mkdir -p $(RPATH)
	$(LIBTOOL) --mode=install install liblua-foo.la $(RPATH)/liblua-foo.la
	rm $(RPATH)/liblua-foo.la

clean:
	$(RM) *.o *.lo *.la *.so app
	$(RM) -r ./.libs/
