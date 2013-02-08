VERSION ?= 0.7
CFLAGS ?= -Wall -W -g -O2 -D'IFUPDOWN_VERSION="$(VERSION)"'
ARCH ?= linux

BASEDIR ?= $(DESTDIR)

CFILES := addrfam.c execute.c config.c main.c arch$(ARCH).c
HFILES := header.h arch$(ARCH).h
PERLFILES := defn2c.pl defn2man.pl
DEFNFILES := inet.defn ipx.defn inet6.defn can.defn

OBJ := main.o addrfam.o execute.o config.o \
	$(patsubst %.defn,%.o,$(DEFNFILES)) arch$(ARCH).o meta.o link.o

MAN := $(patsubst %.defn,%.man,$(DEFNFILES))
DEFNFILES += meta.defn link.defn

default : executables
all : executables docs

executables : ifup ifdown ifquery ifup.8 ifdown.8 ifquery.8 interfaces.5
docs : ifupdown.ps.gz ifup.8.ps.gz interfaces.5.ps.gz ifupdown.pdf

.PHONY : executables 
.PHONY : clean clobber

install :
	install -m 0755 -d     ${BASEDIR}/sbin
	install -m 0755 ifup   ${BASEDIR}/sbin
	ln ${BASEDIR}/sbin/ifup ${BASEDIR}/sbin/ifdown	
	ln ${BASEDIR}/sbin/ifup ${BASEDIR}/sbin/ifquery

clean :
	rm -f *.aux *.toc *.log *.bbl *.blg *.ps *.eps *.pdf
	rm -f *.o *.d $(patsubst %.defn,%.c,$(DEFNFILES)) *~
	rm -f $(patsubst %.defn,%.man,$(DEFNFILES))
	rm -f ifup ifdown ifquery interfaces.5 ifdown.8 ifquery.8
	rm -f ifupdown.dvi *.ps{,.gz}

clobber : clean
	rm -f ifupdown.tex $(PERLFILES) $(CFILES) $(HFILES) $(DEFNFILES) arch*

distclean : clobber
	rm -f makecdep.sh makenwdep.sh Makefile
ifup: $(OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) $(OUTPUT_OPTION)

ifdown: ifup
	ln -sf ifup ifdown

ifquery: ifup
	ln -sf ifup ifquery
interfaces.5: interfaces.5.pre $(MAN)
	sed $(foreach man,$(MAN),-e '/^##ADDRESSFAM##$$/r $(man)') \
	     -e '/^##ADDRESSFAM##$$/d' < $< > $@	

ifdown.8 ifquery.8: ifup.8
	ln -sf $< $@

%.5.ps: %.5
	groff -mandoc -Tps $< > $@
%.8.ps: %.8
	groff -mandoc -Tps $< > $@
ifupdown.dvi: modules.eps execution.eps
ifupdown.ps: modules.eps execution.eps
ifupdown.pdf: modules.pdf execution.pdf
%.tex : %.nw
	noweave -delay -index -latex $< >$@

%.bbl : %.tex biblio.bib
	latex $<
	bibtex $(basename $<)

%.dvi : %.tex %.bbl
	latex $<
	latex $<

%.pdf : %.tex %.bbl
	pdflatex $<
	pdflatex $<

%.ps : %.dvi
	dvips -o $@ $<

%.gz : %
	gzip --best --stdout $< >$@
%.eps : %.dia
	dia --nosplash -e $@ $<

%.pdf : %.eps
	gs -q -sDEVICE=pdfwrite -dNOPAUSE -sOutputFile=$@ - < $<
%.d: %.nw makenwdep.sh
	./makenwdep.sh $< > $@
%.d: %.c makecdep.sh
	./makecdep.sh $< > $@
%.c : %.defn defn2c.pl
	./defn2c.pl $< > $@
%.man: %.defn defn2man.pl
	./defn2man.pl $< > $@

include-deps := YES
ifneq "" "$(filter %clean,$(MAKECMDGOALS))"
include-deps := NO
endif
ifeq "clobber" "$(MAKECMDGOALS)"
include-deps := NO
endif
ifeq "$(strip $(include-deps))" "YES"
include ifupdown.d
endif
