ifeq (1,$(strip $(shell if dpkg --compare-versions `dpkg -s libgimp2.0-dev | awk '/^Version/ {print $$2}'` lt 2.7~; then echo 1; fi)))
SCRIPT = layerfx.py
else
SCRIPT = layerfx.2.8.py
endif

include ../common-py.mk
