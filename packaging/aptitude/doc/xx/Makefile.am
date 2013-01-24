#SUBDIRS = images
#EXTRA_DIST = 

# Language code.  Example: ja
LC = xx

# Always include this file which sets up install targets.
include $(top_srcdir)/buildlib/doc.mk
# Also include this file if you are using the po4a system.
include $(top_srcdir)/buildlib/po4a.mk

# Name of the translated README file (user's manual in text format)
#
# This file will be accessible from the programs help menu if you
# also translate "README" to "README.xx" in the program translation.
#
# Comment this out if you have not translated the user's manual.
README = README.xx
README_encoding = utf-8

# List of images to include in the html output.
#
# Most of these are screenshots which you will have to generate
# yourself.  Some are diagrams translated using po4a and
# contained in the list PO4A_IMAGES (set by po4a.mk).
IMAGES := $(wildcard $(srcdir)/images/*.png) $(PO4A_IMAGES)

# Uncomment this to provide your own stylesheet for producing the html
# output.  Such a stylesheet could, for example, change the output
# encoding of the html document.
#
# Set this to your stylesheet, then any that it imports.  Only the
# first value is passed to xsltproc.
#DOCBOOK_HTML_XSL = aptitude-html-xx.xsl aptitude-html.xsl aptitude-common.xsl

# By default the docbook-man target will output all available
# man pages.  Uncomment this line if you only have the main one
# translated.
#DOCBOOK_MANS = aptitude.8

# Specify the desired output formats.
DOCBOOK_TARGETS = docbook-html docbook-man docbook-readme

# Include this file to setup targets which generate the documents from
# the translated docbook source.
include $(top_srcdir)/buildlib/docbook.mk

# Finally, this target ensures that all desired documents get built.
all-local: docbook-stamp

# Install targets for man pages must be specified manually for now.
man_MANS = aptitude.8
notrans_man_MANS = aptitude-create-state-bundle.1 aptitude-run-state-bundle.1
