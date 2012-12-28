#ifndef _RFC822_H_
#define _RFC822_H_

struct rfc822_header {
    char *header;
    char *value;
    struct rfc822_header *next;
};

struct rfc822_header *rfc822_parse_stanza(FILE *file);
char *rfc822_header_lookup(struct rfc822_header *list, const char* key);
void rfc822_header_destroy(struct rfc822_header *list);
#endif
