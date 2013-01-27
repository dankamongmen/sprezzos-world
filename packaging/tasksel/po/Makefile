XGETTEXT = xgettext --keyword=_ --keyword=N_ --add-comments=TRANS:
POTFILE = tasksel.pot
MOS:=$(addsuffix .mo, $(LANGS))

all: $(POTFILE) $(MOS)
	touch build_stamp

$(POTFILE): ../tasksel.pl
	$(XGETTEXT) -o $(POTFILE)-update ../tasksel.pl
	if test -f $(POTFILE); then \
	  if diff -I '^"POT-Creation-Date:' -I '^Report-Msgid-Bugs-To:' $(POTFILE) $(POTFILE)-update >/dev/null 2>&1; then \
	      rm -f $(POTFILE)-update; \
	  else \
	      rm -f $(POTFILE); \
	      mv $(POTFILE)-update $(POTFILE); \
	  fi; \
	else \
	  mv $(POTFILE)-update $(POTFILE); \
	fi

%.mo: %.po
	msgfmt -o $@ $<

clean:
	-rm -f build_stamp
	-rm -f *.mo

update: $(addprefix update-, $(LANGS))

update-%: %.po $(POTFILE)
	mv $< $<.bak
	msgmerge $<.bak $(POTFILE) > $<

