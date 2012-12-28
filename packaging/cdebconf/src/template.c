#include "common.h"
#include "template.h"
#include "strutl.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

static const char *template_get_internal(const struct template *t,
                const char *lang, const char *field);
static struct template_l10n_fields *template_cur_l10n_fields(const struct template *t,
                const char *lang);
static char *getlanguage(void);
static void remove_newlines(char *);

const char *template_fields_list[] = {
        "tag",
        "type",
        "default",
        "choices",
        "indices",
        "description",
        "extended_description",
        "help",
        NULL
};

struct cache_list_lang
{
	char *lang;
	struct cache_list_lang *next;
};
struct cache_list_lang *cache_list_lang_ptr = NULL;
/*   cache_cur_lang contains the colon separated list of languages  */
static char *cache_cur_lang = NULL;

static char *getlanguage(void)
{
	const char *envlang = getenv("LANGUAGE");
	struct cache_list_lang *p, *q;
	char *cpb, *cpe;

	if ((cache_cur_lang == NULL && envlang != NULL) ||
	    (cache_cur_lang != NULL && envlang == NULL) ||
	    (cache_cur_lang != NULL && envlang != NULL && strcmp(cache_cur_lang, envlang) != 0))
	{
		/*   LANGUAGE has changed, reset cache_cur_lang...  */
		DELETE(cache_cur_lang);
		/*   ... and language linked list  */
		for (p = cache_list_lang_ptr; p != NULL; p = q)
		{
			DELETE(p->lang);
			q = p->next;
			free(p);
		}
		cache_list_lang_ptr = NULL;
		if (envlang == NULL)
			return NULL;

		cache_list_lang_ptr = (struct cache_list_lang *)
			malloc(sizeof(struct cache_list_lang));
		memset(cache_list_lang_ptr, 0, sizeof(struct cache_list_lang));

		p = cache_list_lang_ptr;
		cache_cur_lang = strdup(envlang);
		cpb = cache_cur_lang;
		while((cpe = strchr(cpb, ':')) != NULL)
		{
			p->lang = strndup(cpb, (int) (cpe - cpb));
			p->next = (struct cache_list_lang *)
					malloc(sizeof(struct cache_list_lang));
			cpb = cpe + 1;
			p = p->next;
		}
		p->lang = strdup(cpb);
		p->next = NULL;
	}

	/*  Return the first language  */
	if (cache_list_lang_ptr == NULL)
		return NULL;

	return cache_list_lang_ptr->lang;
}

static bool allow_i18n(void)
{
	const char *i18nenv = getenv("DEBCONF_NO_I18N");
	if (i18nenv && !strcmp(i18nenv, "1"))
		return false;
	else
		return true;
}

bool load_all_translations(void)
{
	static int translations = -1;
	if (translations == -1) {
		const char *translations_env = getenv("DEBCONF_DROP_TRANSLATIONS");
		if (translations_env && !strcmp(translations_env, "1"))
			translations = 0;
		else
			translations = 1;
	}
	return (translations == 1);
}

/*
 * Function: template_new
 * Input: a tag, describing which template this is.  Can be null.
 * Output: a blank template struct.  Tag is strdup-ed, so the original
           string may change without harm.
           The fields structure is also allocated to store English fields
 * Description: allocate a new, empty struct template.
 * Assumptions: NEW succeeds
 * Todo: 
 */

struct template *template_new(const char *tag)
{
	struct template_l10n_fields *f = NEW(struct template_l10n_fields);
	struct template *t = NEW(struct template);
	memset(f, 0, sizeof(struct template_l10n_fields));
	f->language = strdup("");
	memset(t, 0, sizeof(struct template));
	t->ref = 1;
	t->tag = STRDUP(tag);
	t->fields = f;
	return t;
}

void template_delete(struct template *t)
{
	struct template_l10n_fields *p, *q;

	DELETE(t->tag);
	DELETE(t->type);
	DELETE(t->help);
	p = t->fields;
	DELETE(t);
	while (p != NULL)
	{
		q = p->next;
		DELETE(p->language);
		DELETE(p->defaultval);
		DELETE(p->choices);
		DELETE(p->indices);
		DELETE(p->description);
		DELETE(p->extended_description);
		DELETE(p);
		p = q;
	}
}

void template_ref(struct template *t)
{
	t->ref++;
}

void template_deref(struct template *t)
{
	if (--t->ref == 0)
		template_delete(t);
}

/*
 * Function: template_dup
 * Input: a template, being the template we want to duplicate
 * Output: a copy of the template passed as input
 * Description: duplicate a template
 * Assumptions: template_new succeeds, STRDUP succeeds.
 */

struct template *template_dup(const struct template *t)
{
        struct template *ret = template_new(t->tag);
        struct template_l10n_fields *from, *to;

        ret->type = STRDUP(t->type);
        ret->help = STRDUP(t->help);
        if (t->fields == NULL)
                return ret;

        ret->fields = NEW(struct template_l10n_fields);
        memset(ret->fields, 0, sizeof(struct template_l10n_fields));

        from = t->fields;
        to = ret->fields;
        /*  Iterate over available languages  */
        while (1)
        {
                to->language = STRDUP(from->language);
                to->defaultval = STRDUP(from->defaultval);
                to->choices = STRDUP(from->choices);
                to->indices = STRDUP(from->indices);
                to->description = STRDUP(from->description);
                to->extended_description = STRDUP(from->extended_description);

                if (from->next == NULL)
                {
                        to->next = NULL;
                        break;
                }
                to->next = NEW(struct template_l10n_fields);
                memset(to->next, 0, sizeof(struct template_l10n_fields));
                from = from->next;
                to = to->next;
        }
        return ret;
}

#define SAME_STRINGS(a,b) ((a == NULL && b == NULL) || (a != NULL && b != NULL && strcmp(a,b) == 0))

struct template *template_l10nmerge(struct template *ret, const struct template *t)
{
        struct template_l10n_fields *from, *to;
        int same_choices, same_description;

        if (ret->type == NULL)
                ret->type = STRDUP(t->type);
        else if (t->type == NULL)
                return NULL;
        else if (strcmp(ret->type, t->type) != 0)
                return NULL;
        if (t->fields == NULL)
                return ret;

        if (ret->fields == NULL)
        {
                ret->fields = NEW(struct template_l10n_fields);
                memset(ret->fields, 0, sizeof(struct template_l10n_fields));
        }

        from = t->fields;
        to = ret->fields;
        same_choices = SAME_STRINGS(from->choices, to->choices);
        same_description = SAME_STRINGS(from->description, to->description) &&
            SAME_STRINGS(from->extended_description, to->extended_description);
        /*  Delete outdated translations */
        if (!same_choices || !same_description)
        {
                while (to->next != NULL)
                {
                        to = to->next;
                        if (!same_choices)
                        {
                                DELETE(to->choices);
                                DELETE(to->indices);
                        }
                        if (!same_description)
                        {
                                DELETE(to->description);
                                DELETE(to->extended_description);
                        }
                }
        }

        /*  Iterate over available languages  */
        for (from = t->fields; from != NULL; from = from->next)
        {
                to = template_cur_l10n_fields(ret, from->language);
                if (to == NULL)
                {
                        to = ret->fields;
                        while (to->next != NULL)
                                to = to->next;
                        to->next = NEW(struct template_l10n_fields);
                        memset(to->next, 0, sizeof(struct template_l10n_fields));
                        to = to->next;
                        to->language = STRDUP(from->language);
                }
                if (from->defaultval != NULL && *(from->defaultval) != '\0')
                        to->defaultval = STRDUP(from->defaultval);
                if (from->choices != NULL && *(from->choices) != '\0')
                        to->choices = STRDUP(from->choices);
                if (from->indices != NULL && *(from->indices) != '\0')
                        to->indices = STRDUP(from->indices);
                if (from->description != NULL && *(from->description) != '\0')
                        to->description = STRDUP(from->description);
                if (from->extended_description != NULL && *(from->extended_description) != '\0')
                        to->extended_description = STRDUP(from->extended_description);
        }
        return ret;
}

void template_l10nclear(struct template *t)
{
    struct template_l10n_fields *p = t->fields, *q;

    while (p != NULL)
    {
        q = p->next;
        DELETE(p->language);
        DELETE(p->defaultval);
        DELETE(p->choices);
        DELETE(p->indices);
        DELETE(p->description);
        DELETE(p->extended_description);
        DELETE(p);
        p = q;
    }

    t->fields = NEW(struct template_l10n_fields);
    memset(t->fields, 0, sizeof(struct template_l10n_fields));
    t->fields->language = strdup("");
}

static const char *template_field_get(const struct template_l10n_fields *p,
                const char *field)
{
    if (strcasecmp(field, "default") == 0)
        return p->defaultval;
    else if (strcasecmp(field, "choices") == 0)
        return p->choices;
    else if (strcasecmp(field, "indices") == 0)
        return p->indices;
    else if (strcasecmp(field, "description") == 0)
        return p->description;
    else if (strcasecmp(field, "extended_description") == 0)
        return p->extended_description;
    return NULL;
}

static void template_field_set(struct template_l10n_fields *p,
                const char *field, const char *value)
{
    if (strcasecmp(field, "default") == 0)
    {
        DELETE(p->defaultval);
        p->defaultval = STRDUP(value);
    }
    else if (strcasecmp(field, "choices") == 0)
    {
        DELETE(p->choices);
        p->choices = STRDUP(value);
    }
    else if (strcasecmp(field, "indices") == 0)
    {
        DELETE(p->indices);
        p->indices = STRDUP(value);
    }
    else if (strcasecmp(field, "description") == 0)
    {
        DELETE(p->description);
        p->description = STRDUP(value);
    }
    else if (strcasecmp(field, "extended_description") == 0)
    {
        DELETE(p->extended_description);
        p->extended_description = STRDUP(value);
    }
}

/*
 * Function: template_lget
 * Input: a template
 * Input: a language name
 * Input: a field name
 * Output: the value of the given field in the given language, field
 *         name may be any of type, default, choices, indices,
 *         description, extended_description and help
 * Description: get field value
 * Assumptions: 
 */

const char *template_lget(const struct template *t,
                const char *lang, const char *field)
{
    const char *ret = NULL;
    char *orig_field;
    char *altlang;
    char *cp;
    struct cache_list_lang *cl;

    if (strcasecmp(field, "tag") == 0)
        return t->tag;
    else if (strcasecmp(field, "type") == 0)
        return t->type;
    else if (strcasecmp(field, "help") == 0)
        return t->help;

    /*   If field is Foo-xx.UTF-8 then call template_lget(t, "xx", "Foo")  */
    if (strchr(field, '-') != NULL)
    {
        orig_field = strdup(field);
        altlang = strchr(orig_field, '-');
        *altlang = 0;
        altlang++;
        if (strcasecmp(altlang, "C") == 0)
            ret = template_lget(t, "C", orig_field);
        else {
            if (!allow_i18n()) {
                free(orig_field);
                return NULL;
            }
            cp = strcasestr(altlang, ".UTF-8");
            if (cp + 6 == altlang + strlen(altlang) && cp != altlang + 1)
            {
                *cp = 0;
                ret = template_lget(t, altlang, orig_field);
            }
#ifndef NODEBUG
            else
                fprintf(stderr, "Unknown localized field: %s\n", field);
#endif
        }
        free(orig_field);
        return ret;
    }

    if (lang == NULL)
    	return template_field_get(t->fields, field);

    if (*lang == 0)
    {
        getlanguage();
        for (cl = cache_list_lang_ptr; cl != NULL; cl = cl->next)
        {
            ret = template_get_internal(t, cl->lang, field);
            if (ret != NULL)
                return ret;
        }
    }
    else
    {
        ret = template_get_internal(t, lang, field);
        if (ret != NULL)
            return ret;
    }

    /*  Default value  */
    return template_field_get(t->fields, field);
}

/*
 * Function: template_get_internal
 * Input: a template
 * Input: a language name
 * Input: a field name
 * Output: the value of the given field in the given language, field
 *         name may be any of type, default, choices, indices,
 *         description, extended_description and help
 * Description: get field value
 * Assumptions: Arguments have been previously checked, lang and field
 *              are not NULL
 */

static const char *template_get_internal(const struct template *t,
                const char *lang, const char *field)
{
    const struct template_l10n_fields *p;
    const char *altret = NULL;

    p = t->fields->next;
    while (p != NULL)
    {
        /*  Exact match  */
        if (strcmp(p->language, lang) == 0)
            return template_field_get(p, field);

        /*  Language is xx_XX and a xx field is found  */
        if (strlen(p->language) == 2 && strncmp(lang, p->language, 2) == 0)
            altret = template_field_get(p, field);

        p = p->next;
    }
    if (altret != NULL)
        return altret;

    return NULL;
}

void template_lset(struct template *t, const char *lang,
                const char *field, const char *value)
{
    struct template_l10n_fields *p, *last;
    char *orig_field;
    char *altlang;
    char *cp;
    const char *curlang = NULL;

    if (strcasecmp(field, "tag") == 0)
    {
        t->tag = STRDUP(value);
        return;
    }
    else if (strcasecmp(field, "type") == 0)
    {
        t->type = STRDUP(value);
        return;
    }
    else if (strcasecmp(field, "help") == 0)
    {
        t->help = STRDUP(value);
        return;
    }

    /*   If field is Foo-xx.UTF-8 then call template_lset(t, "xx", "Foo")  */
    if (strchr(field, '-') != NULL)
    {
        orig_field = strdup(field);
        altlang = strchr(orig_field, '-');
        *altlang = 0;
        altlang++;
        if (strcasecmp(altlang, "C") == 0)
            template_lset(t, "C", orig_field, value);
        else {
            if (!allow_i18n()) {
                free(orig_field);
                return;
            }
            cp = strcasestr(altlang, ".UTF-8");

            /* Plain debconf supports undefined character sets, on the
               form "Description-nb_NO: ", which is valid if the text is
               ASCII (but debconf still uses that syntax regardless of
               validity if the application does not specify a character
               set). To avoid losing these fields, we simply read them
               in as if they were UTF-8 fields, as valid ASCII is always
               valid UTF-8 as well.
              
               -- sesse, 2006-06-19
            */
            if ((cp + 6 == altlang + strlen(altlang) && cp != altlang + 1)
	        || strchr(altlang, '.') == NULL)
            {
                if (cp != NULL)
                    *cp = 0;
                template_lset(t, altlang, orig_field, value);
            }
#ifndef NODEBUG
            else
                fprintf(stderr, "Unknown localized field: %s\n", field);
#endif
        }
        free(orig_field);
        return;
    }

    if (lang == NULL)
    	return template_field_set(t->fields, field, value);

    if (*lang == 0)
        curlang = getlanguage();
    else if (load_all_translations() ||
             strcmp(lang, "C") == 0 || strncmp(lang, "en", 2) == 0)
        curlang = lang;
    else {
        char *p;
        struct cache_list_lang *cl;

        getlanguage();
        for (cl = cache_list_lang_ptr; cl != NULL; cl = cl->next) {
            char *wantlang;

            wantlang = strdup(cl->lang);
            p = strpbrk(wantlang, "_.@");
            if (p)
                *p = '\0';
            if (strncmp(lang, wantlang, strlen(wantlang)) == 0) {
                curlang = lang;
                free(wantlang);
                break;
            }
            free(wantlang);
        }
        if (curlang == NULL) {
            INFO(INFO_VERBOSE, "Dropping %s/%s for %s", t->tag, field, lang);
            return;
        }
    }

    p = t->fields;
    last = p;
    while (p != NULL)
    {
        if (curlang == NULL || strcmp(p->language, curlang) == 0)
        {
            template_field_set(p, field, value);
            return;
        }
        last = p;
        p = p->next;
    }
    p = NEW(struct template_l10n_fields);
    memset(p, 0, sizeof(struct template_l10n_fields));
    p->language = STRDUP(curlang);
    last->next = p;
    template_field_set(p, field, value);
}

static struct template_l10n_fields *template_cur_l10n_fields(const struct template *t,
                const char *lang)
{
    struct template_l10n_fields *p;

    p = t->fields;
    while (p != NULL)
    {
        if (lang == NULL || strcmp(p->language, lang) == 0)
            return p;
        p = p->next;
    }
    return NULL;
}

const char *template_next_lang(const struct template *t, const char *lang)
{
    const struct template_l10n_fields *p = template_cur_l10n_fields(t, lang);
    if (p != NULL && p->next != NULL)
        return p->next->language;
    return NULL;
}

/* remove extraneous linebreaks */
/* remove internal linebreaks unless the next
 * line starts with a space; also change
 * lines that contain a . only to an empty line
 */
static void remove_newlines(char *text)
{
	char *in, *out;
	int asis = 0;

	in = out = text;
	for (; *in != 0; in++)
	{
		*out = *in;
		if (*in == '\n')
		{
			if (*(in+1) == '.' && *(in+2) == '\n')
			{
				out++;
				*out = *in;
				in+=2;
				asis=0;
			}
			else if (*(in+1) == 0)
				*out = 0;
			else if (*(in+1) == ' ')
				asis=1;
			else if (asis)
				asis=0;
			else
				*out = ' ';
		}
		out++;
	}
}

struct template *template_load(const char *filename)
{
	char buf[4096];
	char *line;
	char extdesc[8192];
	char *lang;
	char *p;
	char *cp;
	FILE *fp;
	struct template *tlist = NULL, *t = 0;
	unsigned int i;
	int linesize;
	bool i18n = allow_i18n();

	if ((fp = fopen(filename, "r")) == NULL)
		return NULL;
	while (fgets(buf, sizeof(buf), fp))
	{
		line = strdup(buf);
		linesize = sizeof(buf);
		while (strlen(buf) == sizeof(buf)-1)
		{
			fgets(buf, sizeof(buf), fp);
			line = (char*) realloc(line, linesize + sizeof(buf));
			linesize = linesize + sizeof(buf);
			strcat(line, buf);
		}
		
		lang = NULL;
		p = line;
		CHOMP(p);
		if (*p == 0)
		{
			if (t != 0)
			{
				t->next = tlist;
				tlist = t;
				t = 0;
			}
		}
		if (strstr(p, "Template: ") == p)
			t = template_new(p+10);
		else if (strstr(p, "Type: ") == p && t != 0)
			template_lset(t, NULL, "type", p+6);
		else if (strstr(p, "Help: ") == p && t != 0)
			template_lset(t, NULL, "help", p+6);
		else if (strstr(p, "Default: ") == p && t != 0)
			template_lset(t, NULL, "default", p+9);
		else if (i18n && strstr(p, "Default-") == p && t != 0)
		{
			if (strcasestr(p, "Default-C: ") == p)
				template_lset(t, "C", "default", p+11);
			else
			{
				cp = strcasestr(p, ".UTF-8: ");
				if (cp != NULL && cp != p+8)
				{
					lang = strndup(p+8, (int) (cp - p - 8));
					template_lset(t, lang, "default", cp+8);
				}
				else
				{
#ifndef NODEBUG
					fprintf(stderr, "Unknown localized field: %s\n", p);
#endif
					continue;
				}
			}
		}
		else if (strstr(p, "Choices: ") == p && t != 0)
			template_lset(t, NULL, "choices", p+9);
		else if (i18n && strstr(p, "Choices-") == p && t != 0)
		{
			if (strcasestr(p, "Choices-C: ") == p)
				template_lset(t, "C", "choices", p+11);
			else
			{
				cp = strcasestr(p, ".UTF-8: ");
				if (cp != NULL && cp != p+8)
				{
					lang = strndup(p+8, (int) (cp - p - 8));
					template_lset(t, lang, "choices", cp+8);
				}
				else
				{
#ifndef NODEBUG
					fprintf(stderr, "Unknown localized field: %s\n", p);
#endif
					continue;
				}
			}
		}
		else if (strstr(p, "Indices: ") == p && t != 0)
			template_lset(t, NULL, "indices", p+9);
		else if (i18n && strstr(p, "Indices-") == p && t != 0)
		{
			if (strcasestr(p, "Indices-C: ") == p)
				template_lset(t, "C", "indices", p+11);
			else
			{
				cp = strcasestr(p, ".UTF-8: ");
				if (cp != NULL && cp != p+8)
				{
					lang = strndup(p+8, (int) (cp - p - 8));
					template_lset(t, lang, "indices", cp+8);
				}
				else
				{
#ifndef NODEBUG
					fprintf(stderr, "Unknown localized field: %s\n", p);
#endif
					continue;
				}
			}
		}
		else if (strstr(p, "Description: ") == p && t != 0)
		{
			template_lset(t, NULL, "description", p+13);
			extdesc[0] = 0;
			i = fgetc(fp);
			/* Don't use fgets unless you _need_ to, a
			   translated description may follow and it
			   will get lost if you just fgets without
			   thinking about what you are doing. :)

			   -- tfheen, 2001-11-21
			*/
                        while (i == ' ')
			{
                                if (i != ungetc(i, fp)) { /* ERROR */ }
                                fgets(buf, sizeof(buf), fp);
                                strvacat(extdesc, sizeof(extdesc), buf+1,
					 (char *) 0);
                                i = fgetc(fp);
			}
                        ungetc(i, fp); /* toss the last one back */
			if (*extdesc != 0)
			{
				remove_newlines(extdesc);
				template_lset(t, NULL, "extended_description", extdesc);
			}
		}
		else if (i18n && strstr(p, "Description-") == p && t != 0)
		{
			if (strcasestr(p, "Description-C: ") == p)
			{
				lang = strdup("C");
				template_lset(t, lang, "description", p+15);
			}
			else
			{
				cp = strcasestr(p, ".UTF-8: ");
				if (cp != NULL && cp != p+12)
				{
					lang = strndup(p+12, (int) (cp - p - 12));
					template_lset(t, lang, "description", cp+8);
				}
				else
				{
#ifndef NODEBUG
					fprintf(stderr, "Unknown localized field: %s\n", p);
#endif
					/*  Skip extended description  */
					lang = NULL;
				}
			}
			extdesc[0] = 0;
			i = fgetc(fp);
			/* Don't use fgets unless you _need_ to, a
			   translated description may follow and it
			   will get lost if you just fgets without
			   thinking about what you are doing. :)

			   -- tfheen, 2001-11-21
			*/
			while (i == ' ') 
			{
				if (i != ungetc(i, fp)) { /* ERROR */ }
				fgets(buf, sizeof(buf), fp);
				strvacat(extdesc, sizeof(extdesc), buf+1,
					 (char *) 0);
                                i = fgetc(fp);
			}
                        ungetc(i, fp); /* toss the last one back */
			if (*extdesc != 0 && lang)
			{
				remove_newlines(extdesc);
				template_lset(t, lang, "extended_description", extdesc);
			}
		}
		free(lang);
		free(line);
	}

	if (t != 0)
	{
		t->next = tlist;
		tlist = t;
		t = 0;
	}

	fclose(fp);
	t = tlist;
	return tlist;
}
/*
Local variables:
c-file-style: "linux"
End:
*/
