info_TEXINFOS = enblend.texi \
                enfuse.texi

enblend_TEXINFOS = auxmac.texi auxmac.tex \
                   varsenblend.texi fdl.texi \
                   mask-template-characters.texi \
                   helpful-programs.texi \
                   seam-generators.texi \
                   color-profiles.texi \
                   tuning-memory-usage.texi \
                   understanding-masks.texi \
                   workflow.texi filespec.texi \
                   external-masks.texi \
                   bug-reports.texi authors.texi \
                   photographic-workflow.fig \
                   external-mask-workflow.fig \
                   seam-line-visualization.tif \
                   default.css

enfuse_TEXINFOS = auxmac.texi auxmac.tex \
                  varsenfuse.texi fdl.texi \
                  mask-template-characters.texi \
                  helpful-programs.texi \
                  color-profiles.texi \
                  tuning-memory-usage.texi \
                  understanding-masks.texi \
                  workflow.texi filespec.texi \
                  external-masks.texi \
                  bug-reports.texi authors.texi \
                  config-edge.gp config.gp \
                  entropy-cutoff.gp.in \
                  exposure-cutoff.gp.in \
                  entropy.gp.in gaussian.gp.in \
                  laplacian-of-gaussian.gp.in \
                  sharp-edge.gp.in sharp-edge.data \
                  smooth-edge.gp.in smooth-edge.data \
                  local-analysis-window.fig \
                  photographic-workflow.fig \
                  focus-stack-decision-tree.fig \
                  external-mask-workflow.fig \
                  default.css

AM_MAKEINFOFLAGS = @AM_MAKEINFOFLAGS@ \
                   -I $(top_builddir) -I $(srcdir)
AM_MAKEINFOHTMLFLAGS = @AM_MAKEINFOHTMLFLAGS@ \
                   -I $(top_builddir) -I $(srcdir) \
                   --css-include=@srcdir@/default.css \
                   $(MAKEINFOHTMLFLAGS)
export TEXINPUTS=$(top_builddir):$(srcdir)

TEXI2DVI = texi2dvi $(TEXI2DVIFLAGS) $(EXTRATEXI2DVIFLAGS)

EXTRA_DIST = \
        docstrings seam-line-visualization.tif makeinfo-4.13-docbook.sed \
        CMakeLists.txt macros.cmake \
        CreateVersTexi.pl define2set.pl fig2txt.pl ReplaceValues.pl

RASTER_DIR = @RASTER_DIR@

# created by make(1), user probably wants to rebuild (often?)
MOSTLYCLEANFILES = varsenblend.texi varsenfuse.texi

# created by make(1)
CLEANFILES = entropy.{txt,eps,svg,pdf} \
             entropy-cutoff.{txt,eps,svg,pdf} \
             exposure-cutoff.{txt,eps,svg,pdf} \
             external-mask-workflow.{txt,eps,svg,pdf} \
             gaussian.{txt,eps,svg,pdf} \
             laplacian-of-gaussian.{txt,eps,svg,pdf} \
             local-analysis-window.{txt,eps,svg,pdf} \
             photographic-workflow.{txt,eps,svg,pdf} \
             focus-stack-decision-tree.{txt,eps,svg,pdf} \
             sharp-edge.{txt,eps,svg,pdf} \
             smooth-edge.{txt,eps,svg,pdf} \
             seam-line-visualization.{eps,png} \
             *.xml \
             *.fig.bak \
             *.msg

# created by configure(1) or LaTeX
DISTCLEANFILES = texinfo.tex \
                 enblend.sc enblend.scs \
                 enfuse.sc enfuse.scs

MAINTAINERCLEANFILES = enblend.info enfuse.info


# Phony Targets

.PHONY: install-html
install-html:
	@echo '*** Target "install-html" has been deprecated.'
	@echo '*** Use target "install-xhtml" instead of "install-html".'
	false

.PHONY: html
html:
	@echo '*** Target "html" has been deprecated.'
	@echo '*** Use target "xhtml" instead of "html".'
	false

.PHONY: clean-local
clean-local:
	-rm -rf enblend.xhtml enfuse.xhtml raster


XMLLINT_FLAGS = --noout --valid

# The following exported variables are respected by html2xhml(1).
export TIDY SED XMLLINT

.PHONY: xhtml
xhtml: $(HTMLS:.html=.xhtml)

.PHONY: validate-xhtml
validate-xhtml: $(HTMLS:.html=.xhtml)
	for x in $(HTMLS); do \
          xhtml=`basename $$x .html`.xhtml; \
	  if test -d $$xhtml; then \
	    for y in $$xhtml/*.xhtml; do \
	      $(XMLLINT) $(XMLLINT_FLAGS) $$y; \
	    done; \
	  else \
	    $(XMLLINT) $(XMLLINT_FLAGS) $$xhtml; \
          fi; \
	done

.PHONY: install-xhtml
install-xhtml: xhtml
	@$(NORMAL_INSTALL)
	test -z "$(htmldir)" || $(MKDIR_P) "$(DESTDIR)$(htmldir)"
	for x in $(HTMLS); do \
          xhtml=`basename $$x .html`.xhtml; \
	  if test -d $$x; then \
	    $(MKDIR_P) "$(DESTDIR)$(htmldir)/$$xhtml"; \
	    $(INSTALL_DATA) $$xhtml/* "$(DESTDIR)$(htmldir)/$$xhtml"; \
	  else \
	    $(INSTALL_DATA) *.xhtml *.svg $(RASTER_DIR) "$(DESTDIR)$(htmldir)"; \
	  fi; \
	done

.PHONY: uninstall-xhtml
uninstall-xhtml:
	@$(NORMAL_UNINSTALL)
	for x in $(HTMLS); do \
          xhtml=`basename $$x .html`.xhtml; \
	  if test -d $$x; then \
	    rm -rf "$(DESTDIR)$(htmldir)/$$xhtml"; \
	  else \
	    rm -rf "$(DESTDIR)$(htmldir)/*.xhtml" \
                   "$(DESTDIR)$(htmldir)/*.svg" \
                   "$(DESTDIR)$(htmldir)/$(RASTER_DIR)"; \
	  fi; \
	done


.PHONY: xml
xml: $(info_TEXINFOS:.texi=.xml)

.PHONY: validate-xml
validate-xml: $(info_TEXINFOS:.texi=.xml)
	for x in $(info_TEXINFOS); do \
	  $(XMLLINT) $(XMLLINT_FLAGS) `basename $$x .texi`.xml; \
	done


# Implicit Rules

%.txt: %.gp
	$(MKDIR_P) $(RASTER_DIR)
	$(GNUPLOT) $<

%.pdf: %.gp
	$(MKDIR_P) $(RASTER_DIR)
	$(GNUPLOT) $<

%.eps: %.gp
	$(MKDIR_P) $(RASTER_DIR)
	$(GNUPLOT) $<

%.svg: %.gp
	$(MKDIR_P) $(RASTER_DIR)
	$(GNUPLOT) $<

$(RASTER_DIR)/%.png: %.gp
	$(MKDIR_P) $(RASTER_DIR)
	$(GNUPLOT) $<


%.txt: %.fig
	$(SED) -e '1,/---BEGIN-TEXT---/d' \
               -e '/---END-TEXT---/,$$d' \
               -e 's/^# \?//' < $< > $@

%.eps: %.fig
	$(FIG2DEV) -L eps $< $@

%.pdf: %.fig
	$(FIG2DEV) -L pdf $< $@

%.png: %.fig
	$(FIG2DEV) -L png $< $@

%.svg: %.fig
	$(FIG2DEV) -L svg $< | \
	    $(SED) -e '/<desc>/d' -e 's/style="stroke-width:.025in/style="stroke-width:0.001px/' > $@

$(RASTER_DIR)/%.png: %.fig
	$(MKDIR_P) $(RASTER_DIR)
	$(FIG2DEV) -L png $< $@


%.eps: %.tif
	$(CONVERT) $< eps2:$@

%.png: %.tif
	$(CONVERT) $< $@


%.xhtml: %.html
	test -d "$<" && test "$<" = enblend.html && { \
            $(MKDIR_P) "$@/$(RASTER_DIR)"; \
            for i in $(enblend_TEXINFOS); do \
              base="$${i%%.*}"; \
              test -f "$$base.svg" && { cp "$$base.svg" "$@"; \
                                        cp "$(RASTER_DIR)/$$base.png" "$@/$(RASTER_DIR)"; } ; \
              test -f "$$base.png" && cp "$$base.png" "$@"; \
            done; } ; true
	test -d "$<" && test "$<" = enfuse.html && { \
            $(MKDIR_P) "$@/$(RASTER_DIR)"; \
            for i in $(enfuse_TEXINFOS); do \
              base="$${i%%.*}"; \
              test -f "$$base.svg" && { cp "$$base.svg" "$@"; \
                                        cp "$(RASTER_DIR)/$$base.png" "$@/$(RASTER_DIR)"; } ; \
              test -f "$$base.png" && cp "$$base.png" "$@"; \
            done; } ; true
	$(srcdir)/html2xhtml \
	    --meta="enblend-version,$(VERSION)" \
	    --tidy-flags="$(EXTRATIDYFLAGS)" \
	    --xmllint-flags="$(EXTRAXMLLINTFLAGS)" \
	    $<


%.xml: %.texi
	- $(MAKEINFO) $(AM_MAKEINFOFLAGS) --docbook --output=- $< > $(basename $@)-orig.xml
	$(SED) \
	    -e '1,100s#<book id="\([^"]*\)"#<book id="$@"#' \
	    -f $(srcdir)/../doc/makeinfo-4.13-docbook.sed \
	    < $(basename $@)-orig.xml > $(basename $@)-clean.xml
	$(XMLLINT) \
	    --valid --noblanks --noent --nsclean --format --encode iso-8859-1 $(basename $@)-clean.xml \
	    > $@ || { rm $@; false; }


# Explicit Rules

varsenblend.texi: $(srcdir)/../src/enblend.cc \
                  $(srcdir)/../src/bounds.h \
                  $(srcdir)/../src/common.h \
                  $(srcdir)/../src/global.h
	$(PERL) $(srcdir)/docstrings $^ > $@

enblend.info: $(enblend_TEXINFOS) \
              $(srcdir)/versenblend.texi $(top_builddir)/config-h.texi \
              photographic-workflow.txt \
              external-mask-workflow.txt

enblend.dvi: $(enblend_TEXINFOS) \
             $(srcdir)/versenblend.texi $(top_builddir)/config-h.texi \
             photographic-workflow.eps \
             external-mask-workflow.eps \
             seam-line-visualization.eps

enblend.html: $(enblend_TEXINFOS) \
              $(srcdir)/versenblend.texi $(top_builddir)/config-h.texi \
              photographic-workflow.svg $(RASTER_DIR)/photographic-workflow.png \
              external-mask-workflow.svg $(RASTER_DIR)/external-mask-workflow.png \
              seam-line-visualization.png

enblend.pdf: $(enblend_TEXINFOS) \
             photographic-workflow.pdf \
             external-mask-workflow.pdf \
             seam-line-visualization.png

enblend.xhtml: enblend.html

enblend.xml: $(enblend_TEXINFOS) \
             $(srcdir)/versenblend.texi $(top_builddir)/config-h.texi \
             photographic-workflow.svg \
             external-mask-workflow.svg \
             seam-line-visualization.png


varsenfuse.texi: $(srcdir)/../src/enfuse.cc \
                 $(srcdir)/../src/bounds.h \
                 $(srcdir)/../src/common.h \
                 $(srcdir)/../src/global.h
	$(PERL) $(srcdir)/docstrings $^ > $@

enfuse.info: $(enfuse_TEXINFOS) \
             $(srcdir)/versenblend.texi $(top_builddir)/config-h.texi \
             photographic-workflow.txt \
             external-mask-workflow.txt \
             focus-stack-decision-tree.txt \
             entropy.txt \
             entropy-cutoff.txt \
             exposure-cutoff.txt \
             gaussian.txt \
             laplacian-of-gaussian.txt \
             local-analysis-window.txt \
             sharp-edge.txt \
             smooth-edge.txt

enfuse.dvi: $(enfuse_TEXINFOS) \
            $(srcdir)/versenfuse.texi $(top_builddir)/config-h.texi \
            photographic-workflow.eps \
            external-mask-workflow.eps \
            focus-stack-decision-tree.eps \
            entropy.eps \
            entropy-cutoff.eps \
            exposure-cutoff.eps \
            gaussian.eps \
            laplacian-of-gaussian.eps \
            local-analysis-window.eps \
            sharp-edge.eps \
            smooth-edge.eps

enfuse.html: $(enfuse_TEXINFOS) \
             $(srcdir)/versenfuse.texi $(top_builddir)/config-h.texi \
             photographic-workflow.svg $(RASTER_DIR)/photographic-workflow.png \
             external-mask-workflow.svg $(RASTER_DIR)/external-mask-workflow.png \
             focus-stack-decision-tree.svg $(RASTER_DIR)/ \
             entropy.svg $(RASTER_DIR)/entropy.png \
             entropy-cutoff.svg $(RASTER_DIR)/entropy-cutoff.png \
             exposure-cutoff.svg $(RASTER_DIR)/exposure-cutoff.png \
             gaussian.svg $(RASTER_DIR)/gaussian.png \
             laplacian-of-gaussian.svg $(RASTER_DIR)/laplacian-of-gaussian.png \
             local-analysis-window.svg $(RASTER_DIR)/local-analysis-window.png \
             sharp-edge.svg $(RASTER_DIR)/sharp-edge.png \
             smooth-edge.svg $(RASTER_DIR)/smooth-edge.png

enfuse.pdf: $(enfuse_TEXINFOS) \
            $(srcdir)/versenfuse.texi $(top_builddir)/config-h.texi \
            photographic-workflow.pdf \
            external-mask-workflow.pdf \
            focus-stack-decision-tree.pdf \
            entropy.pdf \
            entropy-cutoff.pdf \
            exposure-cutoff.pdf \
            gaussian.pdf \
            laplacian-of-gaussian.pdf \
            local-analysis-window.pdf \
            sharp-edge.pdf \
            smooth-edge.pdf

enfuse.xhtml: enfuse.html

enfuse.xml: $(enfuse_TEXINFOS) \
            $(srcdir)/versenfuse.texi $(top_builddir)/config-h.texi \
            photographic-workflow.svg \
            external-mask-workflow.svg \
            focus-stack-decision-tree.svg \
            entropy.svg \
            entropy-cutoff.svg \
            exposure-cutoff.svg \
            gaussian.svg \
            laplacian-of-gaussian.svg \
            local-analysis-window.svg \
            sharp-edge.svg \
            smooth-edge.svg
