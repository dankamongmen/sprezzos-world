############################ -*- Mode: Makefile -*- ###########################
## local-vars.mk --- 
## Author           : Manoj Srivastava ( srivasta@glaurung.green-gryphon.com ) 
## Created On       : Sat Nov 15 10:43:00 2003
## Created On Node  : glaurung.green-gryphon.com
## Last Modified By : Manoj Srivastava
## Last Modified On : Wed Dec  3 23:16:54 2003
## Last Machine Used: glaurung.green-gryphon.com
## Update Count     : 19
## Status           : Unknown, Use with caution!
## HISTORY          : 
## Description      : 
## 
## arch-tag: 1a76a87e-7af5-424a-a30d-61660c8f243e
## 
###############################################################################

AUTOCONF_FILES = INSTALL Makefile.in aclocal.m4 conf.in config.guess     \
                 config.sub configure depcomp doc/Makefile.in            \
                 doc/mdate-sh doc/texinfo.tex examples/Makefile.in       \
                 examples/fastwc/Makefile.in examples/manual/Makefile.in \
                 install-sh m4/Makefile.in missing mkinstalldirs         \
                 tests/Makefile.in tests/TEMPLATE/Makefile.in            \
                 tests/test-alloc-extra/Makefile.in                      \
                 tests/test-array-nr/Makefile.in                         \
                 tests/test-array-r/Makefile.in                          \
                 tests/test-basic-nr/Makefile.in                         \
                 tests/test-basic-r/Makefile.in                          \
		 tests/test-bison-nr/Makefile.in                         \
		 tests/test-bison-yylloc/Makefile.in                     \
		 tests/test-bison-yylval/Makefile.in                     \
		 tests/test-c++-basic/Makefile.in                        \
		 tests/test-c++-multiple-scanners/Makefile.in            \
		 tests/test-c++-yywrap/Makefile.in                       \
		 tests/test-c-cpp-nr/Makefile.in                         \
		 tests/test-c-cpp-r/Makefile.in                          \
		 tests/test-ccl/Makefile.in                              \
		 tests/test-concatenated-options/Makefile.in             \
		 tests/test-debug-nr/Makefile.in                         \
		 tests/test-debug-r/Makefile.in                          \
		 tests/test-extended/Makefile.in                         \
		 tests/test-header-nr/Makefile.in                        \
		 tests/test-header-r/Makefile.in                         \
		 tests/test-include-by-buffer/Makefile.in                \
		 tests/test-include-by-push/Makefile.in                  \
		 tests/test-include-by-reentrant/Makefile.in             \
		 tests/test-linedir-r/Makefile.in                        \
		 tests/test-lineno-nr/Makefile.in                        \
		 tests/test-lineno-r/Makefile.in                         \
		 tests/test-mem-nr/Makefile.in                           \
		 tests/test-mem-r/Makefile.in                            \
		 tests/test-multiple-scanners-nr/Makefile.in             \
		 tests/test-multiple-scanners-r/Makefile.in              \
		 tests/test-noansi-nr/Makefile.in                        \
		 tests/test-noansi-r/Makefile.in                         \
		 tests/test-posix/Makefile.in                            \
		 tests/test-posixly-correct/Makefile.in                  \
		 tests/test-prefix-nr/Makefile.in                        \
		 tests/test-prefix-r/Makefile.in                         \
		 tests/test-pthread/Makefile.in                          \
		 tests/test-quotes/Makefile.in                           \
		 tests/test-reject/Makefile.in                           \
		 tests/test-rescan-nr/Makefile.in                        \
		 tests/test-rescan-r/Makefile.in                         \
		 tests/test-string-nr/Makefile.in                        \
		 tests/test-string-r/Makefile.in                         \
		 tests/test-table-opts/Makefile.in                       \
		 tests/test-top/Makefile.in                              \
		 tests/test-yyextra/Makefile.in tools/Makefile.in        \
                 po/Makevars.template ylwrap
FILES_TO_CLEAN  = TAGS tags debian/files debian/substvars ChangeLog      \
                  flex.spec $(AUTOCONF_FILES)
STAMPS_TO_CLEAN = 
DIRS_TO_CLEAN   = doc/flex.html debian/stamp autom4te.cache

# Location of the source dir
SRCTOP    := $(shell if [ "$$PWD" != "" ]; then echo $$PWD; else pwd; fi)
TMPTOP     = $(SRCTOP)/debian/$(package)
LINTIANDIR = $(TMPTOP)/usr/share/lintian/overrides
DOCBASEDIR = $(TMPTOP)/usr/share/doc-base
MENUDIR    = $(TMPTOP)/usr/share/menu


BINDIR  = $(TMPTOP)$(PREFIX)/bin
LIBDIR  = $(TMPTOP)$(PREFIX)/lib
MANDIR  = $(TMPTOP)$(PREFIX)/share/man
DOCDIR  = $(TMPTOP)$(PREFIX)/share/doc/$(package)
INFODIR = $(TMPTOP)$(PREFIX)/share/info
DOCTOP  = $(TMPTOP)/usr/share/doc
MAN1DIR = $(MANDIR)/man1
MAN3DIR = $(MANDIR)/man3
MAN5DIR = $(MANDIR)/man5
MAN7DIR = $(MANDIR)/man7
MAN8DIR = $(MANDIR)/man8

define checkdir
	@test -f debian/rules -a -f flexdef.h || \
          (echo Not in correct source directory; exit 1)
endef

define checkroot
	@test $$(id -u) = 0 || (echo need root priviledges; exit 1)
endef
