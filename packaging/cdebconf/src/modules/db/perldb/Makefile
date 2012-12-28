MODULE=perldb
SOBJ=perldb.so
OBJS=perldb.opic

MODCFLAGS:= $(shell perl -MExtUtils::Embed -e ccopts)
MODLDFLAGS:= $(shell perl -MExtUtils::Embed -e ldopts)

include ../modules.mak

