# Build python modules for all known python versions

include /usr/share/python/python.mk
ifeq (,$(py_sitename))
  py_libdir_sh = $$(_py_=$(strip $(1)); python$${_py_\#python*} -c 'from distutils import sysconfig; print(sysconfig.get_python_lib())')
endif

PY_DIR := $(DEB_BUILDDIR)/python
PYDEFAULTVER := $(shell pyversions --version --default)
PYVERSIONS := $(shell pyversions --version --installed)

otherpy = \
	set -e; for v in $(PYVERSIONS); do \
		$(RM) $(PY_DIR); \
		ln -sf $(PY_DIR)$$v $(PY_DIR); \
		$(MAKE) \
			-C $(PY_DIR) \
			pyexecdir=$(call py_libdir_sh, $$v) \
			PYTHON=python$$v \
			PYTHON_VERSION=$$v \
			PYTHON_INCLUDES=-I/usr/include/python$$v \
			$1; \
		$(RM) $(PY_DIR); \
	done; \
	ln -sf $(PY_DIR)$(PYDEFAULTVER) $(PY_DIR)

configure/python-libvirt::
	set -e; for v in $(PYVERSIONS); do \
		cp -la $(PY_DIR) $(PY_DIR)$$v; \
	done
	$(RM) -r $(PY_DIR)
	ln -sf $(PY_DIR)$(PYDEFAULTVER) $(PY_DIR)

build/python-libvirt::
	$(call otherpy, all)

install/python-libvirt::
	$(call otherpy, install DESTDIR=$(CURDIR)/debian/tmp)
