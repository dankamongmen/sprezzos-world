/**
 * @file template.c
 * @brief interface to debconf templates
 */
#ifndef _TEMPLATE_H_
#define _TEMPLATE_H_

#include <stdbool.h>

struct template_l10n_fields
{
	char *language;
	char *defaultval;
	char *choices;
	char *indices;
	char *description;
	char *extended_description;
	struct template_l10n_fields *next;
};

struct template
{
	char *tag;
	unsigned int ref;
	char *type;
	char *help;
	struct template_l10n_fields *fields;
	struct template *next;
};

extern const char *template_fields_list[];

bool load_all_translations(void);

struct template *template_new(const char *tag);
void template_delete(struct template *t);
void template_ref(struct template *t);
void template_deref(struct template *t);
struct template *template_dup(const struct template *t);
struct template *template_l10nmerge(struct template *ret, const struct template *t);
void template_l10nclear(struct template *t);
struct template *template_load(const char *filename);
const char *template_lget(const struct template *t, const char *lang, const char *field);
void template_lset(struct template *t, const char *lang, const char *field, const char *value);
const char *template_next_lang(const struct template *t, const char *l);

#endif

/*
Local variables:
c-file-style: "linux"
End:
*/
