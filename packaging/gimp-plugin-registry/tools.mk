#!/usr/bin/make -f

PLUGINS = $(shell find . -mindepth 2 -maxdepth 2 -name Makefile | \
                  sed 's,^\./,,g;s,/.*,,g;' | \
                  sort)

GIMPVER=$(shell dpkg -s libgimp2.0-dev | grep Version | awk '{print $$2}')

VERSION_FILES = $(shell find . -mindepth 2 -maxdepth 2 -name version)
DESC_FILES = $(shell find . -mindepth 2 -maxdepth 2 -name description)
COPYRIGHT_FILES = $(shell find . -path ./debian -prune -o -name copyright -print)

