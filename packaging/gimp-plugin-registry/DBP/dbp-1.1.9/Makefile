#
# all our Makefiles are lovingly handcrafted by our semi-skilled technicians
#
# GIMPARGS = $(shell gimptool-2.0 --cflags --libs)
GIMPARGS = $(shell pkg-config --cflags --libs gimpui-2.0)
# NODEPS = -DGIMP_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED
# gimp progress api changed in 2.3.something - compiles OK with this though
NODEPS = -DGTK_DISABLE_DEPRECATED

all: dbp

dbp: *.cc *.h
	g++ -o dbp -Wall -O2 -I. *.cc $(GIMPARGS) $(NODEPS) $(CPPFLAGS) $(LDFLAGS)

install: dbp
	gimptool-2.0 --install-bin dbp

install-admin: dbp
	gimptool-2.0 --install-admin-bin dbp

clean:
	rm dbp

