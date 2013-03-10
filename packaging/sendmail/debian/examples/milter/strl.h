/*-------------------------------------------------------------------*/
/* strl.h: include file for functions in strl.c                      */
/*         buried in the bottom of include/sendmail/sendmail.h       */
/*         extracted and placed herein.                              */
/*                                                                   */
/* referenced by:  ./sample.c                                        */
/* defined by:     /usr/lib/libmilter/libsm.a                        */
/*                                                                   */
/* Richard Nelson <cowboy@debian.org>                                */
/*-------------------------------------------------------------------*/

#define strlcpy sm_strlcpy
#define strlcat sm_strlcat

extern size_t   sm_strlcpy __P((char *, const char *, size_t));
extern size_t   sm_strlcat __P((char *, const char *, size_t));

