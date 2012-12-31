/*
 * \file configCompiler.h
 * This file is part of enblend.
 * Licence details can be found in the file COPYING.
 *
 * This is the compilation configuration file for enblend.
 * You might want to change some of the defaults if something goes wrong
 * during the compilation.
 */

#ifndef _CONFIG_COMPILER_H
#define _CONFIG_COMPILER_H

/* Default verbosity level of Enblend and Enfuse. A value of zero reduces the
   message output to only warnings and errors. Values of one and more make
  Enblend and Enfuse report progress in detail. */
#define DEFAULT_VERBOSITY 1

/* Define if you want to compile Enblend and Enfuse with image cache */
#cmakedefine CACHE_IMAGES 1

/* Define to 1 if the `closedir' function returns void instead of `int'. */
#cmakedefine CLOSEDIR_VOID 1

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_DIRENT_H 1

/* Define if you have the <ext/slist> header file */
#cmakedefine HAVE_EXT_SLIST 1

/* Define to 1 if you have the <fenv.h> header file. */
#cmakedefine HAVE_FENV_H 1

/* Define to 1 if you have the `fesetround' function. */
#cmakedefine HAVE_FESETROUND 1

/* Define to 1 if you have the `floor' function. */
#cmakedefine HAVE_FLOOR 1

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#cmakedefine HAVE_FSEEKO 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the <limits.h> header file. */
#cmakedefine HAVE_LIMITS_H 1

/* Define if you have C99's lrint function. */
#cmakedefine HAVE_LRINT 1

/* Define if you have C99's lrintf function. */
#cmakedefine HAVE_LRINTF 1

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#cmakedefine HAVE_MALLOC 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#cmakedefine HAVE_MEMSET 1

/* Define to 1 if you have the `mkstemp' function. */
#cmakedefine HAVE_MKSTEMP 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
#cmakedefine HAVE_NDIR_H 1

/* Define to 1 if you have the `pow' function. */
#cmakedefine HAVE_POW 1

/* Define if you have POSIX threads libraries and header files. */
#cmakedefine HAVE_PTHREAD 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#cmakedefine HAVE_PTRDIFF_T 1

/* Define to 1 if you have the `sqrt' function. */
#cmakedefine HAVE_SQRT 1

/* Define to 1 if stdbool.h conforms to C99. */
#cmakedefine HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#cmakedefine HAVE_STRCHR 1

/* Define to 1 if you have the `strcspn' function. */
#cmakedefine HAVE_STRCSPN 1

/* Define to 1 if you have the `strdup' function. */
#cmakedefine HAVE_STRDUP 1

/* Define to 1 if you have the `strerror_r' function. */
#cmakedefine HAVE_STRERROR_R 1

/* Define to 1 if you have the `strerror' function. */
#cmakedefine HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the `strrchr' function. */
#cmakedefine HAVE_STRRCHR 1

/* Define to 1 if you have the `strtol' function. */
#cmakedefine HAVE_STRTOL 1

/* Define to 1 if you have the `strtok_r' function. */
#cmakedefine HAVE_STRTOK_R 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_SYS_DIR_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_SYS_NDIR_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to 1 if you have the <windows.h> header file. */
#cmakedefine HAVE_WINDOWS_H 1

/* Define to 1 if the system has the type `_Bool'. */
/* #undef HAVE__BOOL */

/* Name of package */
#define PACKAGE "enblend-enfuse"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "https://bugs.launchpad.net/enblend"

/* Define to the full name of this package. */
#define PACKAGE_NAME "enblend-enfuse"

/* Define to the full name and version of this package. */
#cmakedefine PACKAGE_STRING "${PACKAGE_STRING}"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "enblend-enfuse"

/* Define to the version of this package. */
#cmakedefine PACKAGE_VERSION "${PACKAGE_VERSION}"

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define as the return type of signal handlers (`int' or `void'). */
#cmakedefine RETSIGTYPE ${RETSIGTYPE}

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS 1

/* Define to 1 if strerror_r returns char *. */
#define STRERROR_R_CHAR_P 1

/* Version number of package */
#define VERSION "${ENBLEND_VERSION_ONLY}"

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
#cmakedefine WORDS_BIGENDIAN 1

/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to `long int' if <sys/types.h> does not define. */
#cmakedefine HAVE_OFF_T 1
#if ! defined(HAVE_OFF_T)
  #define off_t long int
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
#cmakedefine HAVE_SIZE_T 1
#if ! defined(HAVE_SIZE_T)
  #define size_t unsigned int
#endif
#endif
