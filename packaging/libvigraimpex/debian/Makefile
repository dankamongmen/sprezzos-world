CXXFLAGS = -g -O2 -Wall
LDFLAGS = -lvigraimpex

SOURCES = $(wildcard *.cxx)
TARGETS = $(SOURCES:.cxx=)

all: $(TARGETS)

%.o: %.cxx
	$(COMPILE.cc) -o $(@) $(<)

$(TARGETS): %: %.o
	$(LINK.cc) -o $(@) $(<)

clean:
	rm -f *.o $(TARGETS)

# vim:ts=4 sw=4 noet
