/**
 *
 * @file common.h
 *
 * @brief Common constants.  Useful for plugin developers, therefor
 * split into its own file.  Also some useful macros
 *
 *
 */
#ifndef _CDEBCONF_CONSTANTS_H_
#define _CDEBCONF_CONSTANTS_H_

/**
 * @def DEBCONF_MAX_CONFIGPATH_LEN
 * @brief maximum length of a configuration path to a configuration
 */
#define DEBCONF_MAX_CONFIGPATH_LEN  128

#define DC_NOTOK	0
#define DC_OK		1
#define DC_NOTIMPL	2
#define DC_AUTHNEEDED	3
#define DC_REJECT	4

#define DC_GOBACK	30

#define DC_NO		0
#define DC_YES		1

#define INFO_ERROR	0
#define INFO_WARN	1
#define INFO_DEBUG	5
#define INFO_VERBOSE	20

#define NEW(type) (type *)malloc(sizeof(type))
#define DELETE(x) do { free(x); x = 0; } while (0)
#define CHOMP(s) do { if (s[strlen(s)-1] == '\n') s[strlen(s)-1] = '\0'; } while (0)

#endif /* CDEBCONF_CONSTANTS_H */
