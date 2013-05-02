# small sample Makefile do demonstrate usage of gmsl.
# Essentially, you should
#
# 1. include the library
# 2. invoke each function with $(call func,arg1,arg2,...)


include gmsl

# If any of the targets are "fail", throw an assertion. So 'make fail' will
# assert
$(call assert,$(call not,$(filter fail,$(MAKECMDGOALS))))



# If we're trying to compile with different -O flags at the same time, throw an
# error. So the following command to compile tst.c into tst.o will throw an
# error:
#
#   CFLAGS="-O1 -O2" make -n -f example.mk tst.o
define validate
$(if $(call chop,$(sort $(filter -O%,$1))), \
  $(error Have conflict with -O options in "$1"),$1)
endef

%.o: %.c
	$(CC) $(call validate,$(CPPFLAGS) $(CFLAGS)) -o $@ $<

%::
	true
