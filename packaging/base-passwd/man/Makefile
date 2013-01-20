LANGS := de es fr ja ru
MANFILES := $(foreach ll,$(LANGS),update-passwd.$(ll).8)

all: $(MANFILES)

update: po4a/po/base-passwd.pot

clean:
	rm -f $(MANFILES)

po4a/po/base-passwd.pot $(MANFILES): update-passwd.8
	cd po4a && po4a po4a.cfg
