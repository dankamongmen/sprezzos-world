DOMAIN = debian-tasks
POTFILE = $(DOMAIN).pot
MOS:=$(addsuffix .mo, $(LANGS))

all: $(POTFILE) $(MOS)
	touch build_stamp

$(POTFILE): ../../$(DOMAIN).desc
	sed -e 's/^Description:/_&/' ../../$(DOMAIN).desc | \
		perl -ne 'BEGIN {$$/="\n\n"}; print unless /Section: l10n/' > $(DOMAIN).desc
	PODEBCONF_HEADER=/dev/null debconf-updatepo --podir=.
	mv templates.pot $(POTFILE)-update
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
	-rm -f $(DOMAIN).desc
	-rm -f *.mo

update: $(addprefix update-, $(LANGS))

update-%: %.po $(POTFILE)
	mv $< $<.bak
	msgmerge $<.bak $(POTFILE) > $<
