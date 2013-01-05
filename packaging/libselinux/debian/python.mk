#! /usr/bin/make --no-print-directory -f

## Default target
PYTHON_VERSIONS := $(shell pyversions -r)
all: $(PYTHON_VERSIONS)

## Targets share the same output files, so must be run serially
.NOTPARALLEL:
.PHONY: all $(PYTHON_VERSIONS)

## SELinux does not have a very nice build process
extra_python_args  = PYLIBVER=$@
extra_python_args += PYTHONLIBDIR=-L$(DESTDIR)/usr/lib/$@
extra_python_args += PYINC=-I/usr/include/$@

## How to build and install each individually-versioned copy
$(PYTHON_VERSIONS): python%:
	+$(MAKE) $(extra_python_args) clean-pywrap
	+$(MAKE) $(extra_python_args) install-pywrap
	@# Fix the python library directory path
	mv	$(DESTDIR)/usr/lib/$@/site-packages \
		$(DESTDIR)/usr/lib/$@/dist-packages
