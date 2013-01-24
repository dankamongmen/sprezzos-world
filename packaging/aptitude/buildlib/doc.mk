# Standard makefile rules for documentation
#

HTML2TEXT = $(top_srcdir)/doc/html-to-text

mandir = $(if $(findstring en,$(LC)),@mandir@,@mandir@/${LC})
htmldir = @htmldir@/${LC}
imagesdir = ${htmldir}/images

#man_MANS =
#notrans_man_MANS =
html_DATA =
images_DATA =
pkgdata_DATA = $(README)
pdf_DATA = $(aptitude.pdf)
