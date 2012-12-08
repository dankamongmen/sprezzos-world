packages = $(addprefix pinentry-, curses gtk2 qt4)
binaries = $(addprefix pinentry-, curses gtk-2 qt4)
suffixes = install manpages postinst prerm README.Debian
targets = $(foreach suffix,$(suffixes),$(foreach package,$(packages),$(package).$(suffix))) \
	$(foreach binary,$(binaries),$(binary).1)


all: $(targets)

pinentry-curses.%: pinentry.%.in
	m4 -P -Dflavor=curses $< >$@

pinentry-gtk2.% pinentry-gtk-2.%: pinentry.%.in
	m4 -P -Dflavor=gtk-2 $< >$@

pinentry-qt4.%: pinentry.%.in
	m4 -P -Dflavor=qt4 $< >$@


clean:
	rm -f $(targets)


.PHONY: all clean
