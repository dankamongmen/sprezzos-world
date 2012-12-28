/** 
 * @brief strutl.c
 * @brief misc. routines for string handling
 */

#ifndef _STRUTL_H_
#define _STRUTL_H_

#include <stdarg.h>
#include <string.h>
#include "question.h"

#include "debian-installer/macros.h"

/* Does this compiler support sentinel checking? */
#if DI_GNUC_PREREQ(4,0)
#  define ATTRIBUTE_SENTINEL __attribute__ ((__sentinel__))
#else
#  define ATTRIBUTE_SENTINEL
#endif

#define STRESCAPE_822      0
#define STRESCAPE_QUOTE    1
#define STRESCAPE_CAPB     2

/**
 * @brief strcmp with explicit start/stop 
 * @param s1, e1 start and stop of first string
 * @param s2, e2 start and stop of second string
 * @return -1, 0, 1 as for strcmp
 */
int strcountcmp(const char *s1, const char *e1, const char *s2, const char *e2);

/**
 * @brief Strips leading and trailing spaces from a string; also 
 *        strips trialing newline characters from the string
 * @param buf string to strip
 */
char *strstrip(char *buf);

/**
 * @brief Converts a string to lowercase
 * @param buf string to convert
 */
char *strlower(char *buf);

/**
 * @brief concatenates arbitrary number of strings to a buffer, with 
 *        bounds checking
 * @param buf output buffer
 * @param len Length of buffer
 * @param ... one or or strings to concatenate (end with NULL)
 */
void strvacat(char *buf, size_t len, ...)
#if defined(__GNUC__) && __GNUC__ >= 4
	__attribute__((sentinel))
#endif
	;
int strparsecword(char **inbuf, char *outbuf, size_t maxlen);
int strparsequoteword(char **inbuf, char *outbuf, size_t maxlen);
int strgetargc(const char *inbuf);
int strchoicesplit(const char *inbuf, char **argv, size_t maxnarg);
int strchoicesplitsort(const char *origbuf, const char *transbuf, const char *listorder, char **oargv, char **targv, int *tindex, size_t maxnarg);
int strcmdsplit(char *inbuf, char **argv, size_t maxnarg);
void strunescape(const char *inbuf, char *outbuf, const size_t maxlen, const int quote);
void strescape(const char *inbuf, char *outbuf, const size_t maxlen, const int quote);
char *unescapestr(const char *in);
char *escapestr(const char *in);
int strwrap(const char *str, const int width, char *lines[], int maxlines);
int strlongest(char **strs, int count);

size_t strwidth(const char *width);
int strpad(char *what, size_t maxsize);
int strtruncate (char *what, size_t maxsize);

typedef const char * (* lookup_function)(const char * name, void * user_data);
#define LOOKUP_FUNCTION(F) ((lookup_function) F)
char * strexpand(const char * src, lookup_function func, void * user_data);

#define STRALIGN_TAB          "\t"
#define STRALIGN_ALIGN_CENTER "\x0e"
#define STRALIGN_ALIGN_RIGHT  "\x0f"
int stralign(char **strs, int count);

char *strjoinv(const char *sep, va_list ap);
char *strjoin(const char *sep, ...) ATTRIBUTE_SENTINEL;

char *strreplace(const char *src, const char *from, const char *to);

#endif
