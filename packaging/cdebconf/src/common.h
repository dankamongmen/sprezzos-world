/**
 *
 * @file common.h
 * @brief Common utility routines and definitions
 *
 */
#ifndef _CDEBCONF_COMMON_H_
#define _CDEBCONF_COMMON_H_

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "debug.h"
#include "constants.h"

#define DIE(fmt, args...) 					\
 	do {							\
		fprintf(stderr, "%s:%d (%s): ", __FILE__, __LINE__, __FUNCTION__); \
		fprintf(stderr, fmt, ##args);			\
		fprintf(stderr, "\n");				\
		exit(EXIT_FAILURE);					\
	} while(0)

#ifndef NODEBUG
#define INFO(level, fmt, args...)					\
	debug_printf(level, fmt, ##args)
#define ASSERT(cond) do { if (!(cond)) DIE("%s:%d (%s): Assertion failed: %s", __FILE__, __LINE__, __FUNCTION__, #cond); } while (0)
#else
#define INFO(level, fmt, args...)	/* nothing */
#define ASSERT(cond)
#endif


/* Be careful with these macros; they evaluate the string arguments multiple
   times!
 */
#define STRDUP(s) ((s) == NULL ? NULL : strdup(s))
#define STRDUP_NOTNULL(s) ((s) == NULL ? strdup("") : strdup(s))
#define STRLEN(s) ((s) == NULL ? 0 : strlen(s))
#define STRCPY(d,s) strcpy(d,((s) == NULL ? "" : (s)))
#define DIM(ar) (sizeof(ar)/sizeof(ar[0]))

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#endif
