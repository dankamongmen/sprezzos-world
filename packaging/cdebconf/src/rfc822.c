#include <stdio.h>
#include <ctype.h>

#include "common.h"
#include "rfc822.h"
#include "strutl.h"


/*
 * Function: rfc822_parse_stanza
 * Input: a FILE pointer to an open readable file containing a stanza in rfc822 
 *    format.
 * Output: a pointer to a dynamically allocated rfc822_header structure
 * Description: parse a stanza from file into the returned header struct
 */

struct rfc822_header* rfc822_parse_stanza(FILE *file)
{
    struct rfc822_header *head, **tail, *cur;
    static size_t buflen = 8192;
    static char *buf = NULL;

    if (!buf) {
        buf = malloc(buflen * sizeof *buf);
        if (!buf)
            DIE("Out of memory");
    }

    head = NULL;
    tail = &head;
    cur = NULL;

    /*    fprintf(stderr,"rfc822_parse_stanza(file)\n");*/
    while (fgets(buf, buflen, file))
    {
        char *tmp;
        size_t tmplen = strlen(buf);

        if (*buf == '\n')
            break;

        while (buf[tmplen - 1] != '\n') {
            buflen += 8192;
            buf = realloc(buf, buflen * sizeof *buf);
            if (!buf)
                DIE("Out of memory");
            if (!fgets(buf + tmplen, buflen - tmplen, file))
                break;
            tmplen += strlen(buf + tmplen);
        }

        CHOMP(buf);
        tmp = buf;

        if (isspace(*tmp))
        {
            /* continuation line, just append it */
            int len;

            if (cur == NULL)
                break; /* should report an error here */

            len = strlen(cur->value) + strlen(tmp) + 2;

            cur->value = realloc(cur->value, len);
            strvacat(cur->value, len, "\n", tmp, NULL);
        } 
        else 
        {
            while (*tmp != 0 && *tmp != ':')
                tmp++;
            *tmp++ = '\0';

            cur = NEW(struct rfc822_header);
            if (cur == NULL)
                return NULL;
            memset(cur, '\0',sizeof(struct rfc822_header));    

            cur->header = strdup(buf);

            while (isspace(*tmp))
                tmp++;

            cur->value = strdup(unescapestr(tmp));

            *tail = cur;
            tail = &cur->next;
        }
    }

    return head;
}


char *rfc822_header_lookup(struct rfc822_header *list, const char* key)
{
/*    fprintf(stderr,"rfc822_header_lookup(list,key=%s)\n",key);*/
    while (list && (strcasecmp(key, list->header) != 0))
        list = list->next;
    if (!list)
        return NULL;
/*    fprintf(stderr,"rfc822_header_lookup returning: '%s'\n", list->value);*/
    return list->value;
}


void rfc822_header_destroy(struct rfc822_header *list)
{
    struct rfc822_header *cur = list, *next;

    while (cur) {
        free(cur->header);
        free(cur->value);
        next = cur->next;
        DELETE(cur);
        cur = next;
    }
}
