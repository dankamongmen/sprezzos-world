/* vim:ts=4:et:sts=4:sw=4
 * utils.h
 * "$Id: utils.h 238 2011-01-09 17:05:31Z robert $"
 */

#ifndef _UTILS_H_
#define _UTILS_H_

#define DOCTYPE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n" \
                "  http://www.w3.org/TR/html40/loose.dtd>\n"

#define CONTENTTYPE "Content-type: text/html\n\n"
#define QUERY_MAGIC_STR "query="


extern void error_page(int status, char *s, char *t, ...)
__attribute__ ((noreturn))
__attribute__ ((format (printf, 3, 4)));

extern char * xstrdup(const char *s);
extern void * xmalloc(size_t size);
extern void * xrealloc(void *ptr, size_t size);

extern char * urldecode(char *s);
extern char * urlencode(char *s);
extern void print_html_enc(char *s, FILE *f);
extern int querystring2argv(int *argc, char ***argv);
extern int is_lynx();

#endif

