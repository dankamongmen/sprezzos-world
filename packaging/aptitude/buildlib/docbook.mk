# Makefile rules for targets from docbook source
#

dbiftargets = $(if $(findstring $(1),$(DOCBOOK_TARGETS)),$(2))

DOCBOOK_IMAGES ?= $(patsubst %.svg,%.png,$(IMAGES))

DOCBOOK_XML ?= aptitude.xml manpage.xml
DOCBOOK_HTML_XSL ?= aptitude-html.xsl aptitude-common.xsl

DOCBOOK_HTML ?= $(wildcard output-html/*.html) $(wildcard output-html/*.css)
DOCBOOK_HTML_IMAGES ?= $(wildcard output-html/images/*.png) $(wildcard output-html/images/*.gif)
DOCBOOK_MANS ?= aptitude.8 aptitude-create-state-bundle.1 aptitude-run-state-bundle.1

docbook-stamp: $(DOCBOOK_TARGETS)
	touch docbook-stamp

docbook-man: docbook-man-stamp
docbook-html: docbook-html-stamp docbook-css-stamp docbook-html-images-stamp
docbook-readme: docbook-readme-stamp
docbook-pdf: docbook-pdf-stamp

.PHONY: docbook-man docbook-html docbook-readme docbook-pdf
.PHONY: clean-local clean-docbookclean-docbook-readme
.PHONY: clean-docbook-man clean-docbook-fo clean-docbook-pdf
.PHONY: clean-docbook-html clean-docbook-css clean-docbook-html-images
.PHONY: install-data-docbook uninstall-docbook
.PHONY: install-docbook-html uninstall-docbook-html

vpath %.xsl $(top_srcdir)/doc
vpath %.css $(top_srcdir)/doc

DOCBOOK_INSTALL_CANDIDATES = docbook-html
DOCBOOK_INSTALL_TARGETS = $(filter $(DOCBOOK_INSTALL_CANDIDATES),$(DOCBOOK_TARGETS))

install-data-hook: install-data-docbook
install-data-docbook: $(addprefix install-, $(DOCBOOK_INSTALL_TARGETS))

uninstall-hook: uninstall-docbook
uninstall-docbook: $(addprefix uninstall-, $(DOCBOOK_INSTALL_TARGETS))

dbinstall = @list='$(1)'; test -n "$(2)" || list=; \
	if test -n "$$list"; then \
	  echo " $(MKDIR_P) '$(DESTDIR)$(2)'"; \
	  $(MKDIR_P) "$(DESTDIR)$(2)" || exit 1; \
	fi; \
	for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  echo "$$d$$p"; \
	done | $(am__base_list) | \
	while read files; do \
	  echo " $(INSTALL_DATA) $$files '$(DESTDIR)$(2)'"; \
	  $(INSTALL_DATA) $$files "$(DESTDIR)$(2)" || exit $$?; \
	done

dbuninstall = @list='$(1)'; test -n "$(2)" || list=; \
	files=`for p in $$list; do echo $$p; done | sed -e 's|^.*/||'`; \
	dir='$(DESTDIR)$(2)'; $(am__uninstall_files_from_dir)

#man_MANS += $(filter aptitude.8,$(DOCBOOK_MANS))
#notrans_man_MANS += $(filter-out aptitude.8,$(DOCBOOK_MANS))

install-docbook-html: docbook-html
	@$(NORMAL_INSTALL)
	$(call dbinstall,$(DOCBOOK_HTML),$(htmldir))
	$(call dbinstall,$(DOCBOOK_HTML_IMAGES),$(imagesdir))
uninstall-docbook-html:
	@$(NORMAL_UNINSTALL)
	$(call dbuninstall,$(DOCBOOK_HTML),$(htmldir))
	$(call dbuninstall,$(DOCBOOK_HTML_IMAGES),$(imagesdir))

clean-local: clean-docbook
clean-docbook: $(addprefix clean-,$(DOCBOOK_TARGETS))
	-rm -f docbook-stamp
clean-docbook-man:
	-rm -fr output-man/ docbook-man-stamp $(DOCBOOK_MANS)
clean-docbook-html: clean-docbook-css clean-docbook-html-images
	-rm -fr output-html/ docbook-html-stamp
clean-docbook-css:
	-rm -f output-html/aptitude.css docbook-css-stamp
clean-docbook-html-images:
	-rm -fr output-html/images/ docbook-html-images-stamp
clean-docbook-readme:
	-rm -fr output-readme/ docbook-readme-stamp $(README)
clean-docbook-fo:
	-rm -fr output-fo/ docbook-fo-stamp
clean-docbook-pdf: clean-docbook-fo
	-rm -fr docbook-pdf-stamp $(aptitude.pdf)

docbook-css-stamp: aptitude.css docbook-html-stamp
	-rm -f output-html/aptitude.css
	cp $< output-html/
	touch docbook-css-stamp

docbook-html-images-stamp: $(DOCBOOK_IMAGES) docbook-html-stamp
	mkdir output-html/images/
	cp -f $(filter-out docbook-html-stamp,$^) output-html/images/
	for x in caution important note tip warning; do \
	  ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png \
	      output-html/images/; \
	done
	for x in home next prev up; do \
	  ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.gif \
	      output-html/images/; \
	done
	touch docbook-html-images-stamp

$(DOCBOOK_MANS): $(findstring docbook-man,$(DOCBOOK_TARGETS))
$(README): $(findstring docbook-readme,$(DOCBOOK_TARGETS))
$(aptitude.pdf): $(findstring docbook-pdf,$(DOCBOOK_TARGETS)

if HAVE_RSVG_CONVERT

images/%.png: images/%.svg
	$(RSVG_CONVERT) -x 1.5 -y 1.5 -f png -o $@ $<

endif

if HAVE_XSLTPROC

docbook-man-stamp: $(DOCBOOK_XML) aptitude-man.xsl aptitude-common.xsl
	-rm -fr output-man/
	$(XSLTPROC) -o output-man/aptitude.8 $(firstword $(filter %.xsl,$^)) $<
	ln -f $(addprefix output-man/,$(DOCBOOK_MANS)) .
	@if [ -x "$(srcdir)/fixman" ]; then \
	  for i in $(DOCBOOK_MANS); do \
	    echo "$(srcdir)/fixman $$i"; \
	    . $(srcdir)/fixman $$i; \
          done; \
        fi
	touch docbook-man-stamp

docbook-html-stamp: $(DOCBOOK_XML) $(DOCBOOK_HTML_XSL)
	-rm -fr output-html/
	$(XSLTPROC) -o output-html/ $(firstword $(filter %.xsl,$^)) $<
	touch docbook-html-stamp

docbook-readme-stamp: $(DOCBOOK_XML) aptitude-txt.xsl aptitude-common.xsl
	-rm -fr output-readme/ $(README)
	$(XSLTPROC) -o output-readme/index.html $(firstword $(filter %.xsl,$^)) $<
	@echo "$(HTML2TEXT) output-readme/index.html $(README_encoding) > $(README)"; \
	$(HTML2TEXT) output-readme/index.html $(README_encoding) > $(README) \
	    || (rm -f $(README); exit 1)
	touch docbook-readme-stamp

docbook-fo-stamp: $(DOCBOOK_XML) aptitude-fo.xsl aptitude-common.xsl $(IMAGES)
	-rm -fr output-fo/
	$(XSLTPROC) -o output-fo/aptitude.fo $(firstword $(filter %.xsl,$^)) $<

	mkdir output-fo/images/
	ln -f $(srcdir)/images/*.png output-fo/images/
	for x in caution important note tip warning; do \
	  ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png \
	      output-html/images/; \
	done

	touch docbook-fo-stamp

endif

if HAVE_FOP

docbook-pdf-stamp: docbook-fo-stamp
	$(FOP) output-fo/aptitude.fo -pdf $(aptitude.pdf)
	touch docbook-pdf-stamp

endif
