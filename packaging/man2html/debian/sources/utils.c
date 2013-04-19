/* vim:ts=4:et:sts=4:sw=4
 *
 * utils.c - man2html utility functions
 *
 * Author: Robert Luberda <robert@debian.org>, Jan 2003
 * Copyright: GPL
 *
 * "$Id: utils.c 238 2011-01-09 17:05:31Z robert $"
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "utils.h"



void error_page(int status, char *s, char *t, ...)
{
    va_list p;

    switch(status)
    {
        case 400:
            printf("Status: 400 Bad Request\n");
            break;
        case 403:
            printf("Status: 403 Forbidden\n");
            break;
        case 404:
            printf("Status: 404 Not Found\n");
            break;
        case 500:
            printf("Status: 500 Internal Server Error\n");
            break;
        case 0:
        default:
            break;
    }

    printf(CONTENTTYPE DOCTYPE);
    printf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n"
           "<BODY>\n<H1>%s</H1>\n", s, s);
    va_start(p, t);
    vfprintf(stdout, t, p);
    va_end(p);
    printf("</BODY></HTML>\n");
    if (status == 0)
        exit(EXIT_SUCCESS);
    else
        exit(EXIT_FAILURE);
}


char * xstrdup(const char *s)
{
    char *p = strdup(s);
    if (p == NULL)
        error_page(500, "Out of memory",
                   "Sorry, out of memory, aborting...\n");
    return p;
}

void * xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error_page(500, "Out of memory",
                   "Sorry, out of memory, aborting...\n");
    return p;
}

void * xrealloc(void *ptr, size_t size)
{
    void *p = realloc(ptr,size);
    if (p == NULL)
        error_page(500, "Out of memory",
                   "Sorry, out of memory, aborting...\n");
    return p;
}


/* decode CGI urls */
char * urldecode(char *s)
{
    char *p, *q, *out;
    char buf[3];


    out = strdup(s);
    p = s;
    q = out;
    while (*p)
    {
        if (*p == '+')
            *q++ = ' ';
        else if ((*p == '%') && isxdigit(*(p+1)) && isxdigit(*(p+2)))
        {
            buf[0] = *(p+1);
            buf[1] = *(p+2);
            buf[2] = '\0';
            *q++   = (char)(strtol(buf, NULL, 16));
            p      += 2;
        }
        else
            *q++ = *p;
        p++;
    };
    *q = '\0';
    return out;
}

/* encode urls */
char * urlencode(char *s)
{
    char *p, *q, *out;
    char buf[3];
    size_t i, len;

    len = strlen(s) + 1;
    out = (char*) malloc(len * sizeof(char));
    p = s;
    q = out;
    while (*p)
    {
        if (*p == ' ')
            *q++ = '+';
        else if (isalnum(*p) || *p == '_' || *p == ':' || *p == '-'
                 || *p == '.' || *p == '/')
            *q++ = *p;
        else
        {
            sprintf(buf, "%02x", (unsigned char)*p);
            len += 2;
            i = q - out;
            out = (char*) realloc(out, len * sizeof(char));
            q = out + i;
            *q++ = '%';
            *q++ = buf[0];
            *q++ = buf[1];
        }
        p++;
    }
    *q = '\0';
    return out;
}

void print_html_enc(char *s, FILE *f)
{
    while (*s)
    {
        if (*s == '<')
            fputs("&lt;", f);
        else if (*s == '>')
            fputs("&gt;", f);
        else if (*s == '&')
            fputs("&amp;", f);
        else if (*s == '"')
            fputs("&quot;", f);
        else
            fputc(*s, f);
        s++;
    }
}

int querystring2argv(int *argc, char ***argv)
{
    char *t, *s, *q;
    char **new_argv;
    int new_argc;

    if (!(t = getenv("QUERY_STRING")))
        return 0;

    if (!(s = strstr(t, QUERY_MAGIC_STR)))
        return 0;

    s += sizeof(QUERY_MAGIC_STR) - 1;

    /* kill at first '&' */
    if ((q = strstr(s, "&")))
        *q = '\0';

    s = urldecode(s);

    new_argv = malloc(2 * sizeof(char**));
    new_argv[0] = *argv[0];
    new_argc = 1;

    q = strtok(s, " \t\n\r");
    while (q)
    {
        new_argv[new_argc++] = strdup(q);
        q = strtok(NULL, " \t\n\r");
        new_argv = realloc(new_argv, (new_argc + 1) * sizeof(char**));
    }
    *(new_argv + new_argc) = NULL;
    *argc = new_argc;
    *argv = new_argv;

    free(s);
    return 1;
}

/*
# Do we need lynxcgi URLs? For the moment our criterion is
# 1) HTTP_USER_AGENT=Lynx*  and 2) HTTP_HOST is unset.
*/
int is_lynx()
{
    char *t;

    t = getenv("HTTP_HOST");
    if (t && *t)
        return 0;

    t = getenv("HTTP_USER_AGENT");
    if (!t || !*t)
        return 0;

    return !strncasecmp(t, "lynx", 4);
}


#if UTILS_TEST
int main(int argc, char ** argv)
{
    char * s;
    querystring2argv(&argc, &argv);

    while (argc)
        puts(argv[--argc]);
    return 0;
}

#endif

