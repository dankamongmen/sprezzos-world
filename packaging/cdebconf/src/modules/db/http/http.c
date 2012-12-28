#include "common.h"
#include "configuration.h"
#include "database.h"
#include "http.h"
#include "rfc822.h"
#include "template.h"
#include "question.h"
#include "strutl.h"

#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

static int parse_flags(char *string)
{
    int ret = 0;
    if (string == NULL)
        return ret;
    if (strstr(string, "seen") != NULL)
    {
        ret |= DC_QFLAG_SEEN;
    }
    return ret;
}

static void parse_variables(struct question *q, char *string)
{
    char *wc, *owc;
    if (!string)
        return;
    owc = wc = strdup(string);

    while (wc != NULL && *wc != '\0')
    {
        char *delim = wc;
        char *striptmp_var, *striptmp_val;
        int finished = 0;
        while (*delim != '=' && *delim != '\0')
            delim++;
        if (*delim == '\0')
            finished = 1;
        *delim = '\0';

        striptmp_var = strdup(strstrip(wc));

        delim++;
        wc = delim;
        while (*delim != '\n' && *delim != '\0')
            delim++;
        if (*delim == '\0')
            finished = 1;
        *delim = '\0';

        striptmp_val = strdup(strstrip(wc));
        question_variable_add(q,striptmp_var, striptmp_val);

        free(striptmp_val);
        free(striptmp_var);

        wc = delim;
        wc++;
        if (finished != 0)
            break;

        while (*wc == ' ' || *wc == '\t')
        {
            wc++;
        }
        
    }
    free(owc);
}

static void parse_owners(struct question *q, char *string)
{
    char *wc, *owc;
   
    if (!string)
	    return;

    owc = wc = strdup(string);

    while (wc != NULL)
    {
        char *delim = wc;
        int finished = 0;
        while (*delim != ',' && *delim != '\0')
            delim++;
        if (*delim == '\0')
            finished = 1;
        *delim = '\0';
        question_owner_add(q,wc);
        if (finished != 0)
            break;
        wc = delim;
        while (*wc == ' ' || *wc == '\t' || *wc == '\0')
        {
            wc++;
        }
        
    }

    free(owc);
}

static char *translate_tag_name(const char *buf)
{
	char *ret = STRDUP(buf);
	/* remove / from the tag name so that we can use it as a filename */
	char *t = ret;
	for (; *t != 0; t++)
		if (*t == '/') *t = ':';  /* : is illegal in templates etc */
	return ret;
}

static char *template_url(struct template_db *db, const char *tag)
{
	static char filename[1024];
	static char tmp[1024];
	char tagname[1024];
	filename[0] = 0;

	strncpy(tagname, tag, sizeof(tagname));

    snprintf(tmp, sizeof(tmp), "%s::baseurl", db->configpath);

	snprintf(filename, sizeof(filename), "%s/%s",
		db->config->get(db->config, tmp, ""), tagname);

	return filename;
}

static char *question_url(struct question_db *db, const char *tag)
{
	static char filename[1024];
    static char tmp[1024];
	char tagname[1024];

	filename[0] = 0;

	strncpy(tagname, tag, sizeof(tagname));

    snprintf(tmp, sizeof(tmp), "%s::baseurl", db->configpath);

	snprintf(filename, sizeof(filename), "%s/%s",
		db->config->get(db->config, tmp, "."), tagname);

	return filename;
}

static struct template *http_lookup_cached_template(
	const struct template_db *db, const char *tag)
{
	struct template_db_cache *dbdata = db->data;
	struct template *result;
	for (result = dbdata->templates; result; result = result->next)
	{
		if (strcmp(result->tag, tag) == 0) break;
	}
	return result;
}

static void http_remove_cached_template(struct template_db *db,
	const char *tag)
{
	struct template_db_cache *dbdata = db->data;
	struct template **result;
	for (result = &dbdata->templates; *result; result = &(*result)->next)
	{
		if (strcmp((*result)->tag, tag) == 0)
		{
			template_deref(*result);
			*result = (*result)->next;
			break;
		}
	}
}


/*static struct question *http_lookup_cached_question(
	const struct database *db, const char *tag)
{
	struct question_db_cache *dbdata = db->data;
	struct question *result;
	for (result = dbdata->questions; result; result = result->next) 
	{
		if (strcmp(result->tag, tag) == 0) break;
	}
	return result;
}*/


static int http_template_initialize(struct template_db *db, struct configuration *cfg)
{
	struct template_db_cache *dbdata;
	dbdata = malloc(sizeof(struct template_db_cache));

	if (dbdata == NULL)
		return DC_NOTOK;

	dbdata->templates = NULL;
	db->data = dbdata;

	return DC_OK;
}

static int http_template_shutdown(struct template_db *db)
{
	struct template_db_cache *dbdata = db->data;
	struct template *t;

	while (dbdata->templates != NULL)
	{
		t = dbdata->templates;
		dbdata->templates = dbdata->templates->next;
		t->next = NULL;
		template_deref(t);
	}

	return DC_OK;
}

static int http_template_set(struct template_db *db, struct template *t)
{
	return DC_NOTIMPL;
}

static struct template *http_template_get_real(struct template_db *db, 
	const char *ltag)
{
	char buf[1024]; 
	struct template *t;
	char *translated_tag = translate_tag_name(ltag);
	char *url;

	if (ltag == NULL) return DC_NOTOK;
	url = template_url(db, ltag);
	snprintf(buf, sizeof(buf), "wget  '%s' -O '/tmp/cdebconf_t_%s'", 
		 url, translated_tag);
	
	system(buf);
	snprintf(buf, sizeof(buf), "/tmp/cdebconf_t_%s", translated_tag);
	t = template_load(buf);
	free(translated_tag);
	
	return t;
}

static struct template *http_template_get(struct template_db *db, 
	const char *ltag)
{
	struct template *result;

	result = http_lookup_cached_template(db, ltag);
	if (!result && (result = http_template_get_real(db, ltag)))
	{
		struct template_db_cache *dbdata = db->data;
		result->next = dbdata->templates;
		dbdata->templates = result;
	}
	if (result)
		template_ref(result);

	return result;
}

static int http_template_remove(struct template_db *db, const char *tag)
{
	if (tag == NULL) return DC_NOTOK;

	http_remove_cached_template(db, tag);
	return DC_NOTIMPL;
}

static struct template *http_template_iterate(struct template_db *db,
	void **iter)
{
	return NULL;
}

static int http_question_initialize(struct question_db *db, struct configuration *cfg)
{
	struct question_db_cache *dbdata;
	dbdata = malloc(sizeof(struct question_db_cache));

	if (dbdata == NULL)
		return DC_NOTOK;

	dbdata->questions = NULL;
	db->data = dbdata;

	return DC_OK;
}

static int http_question_shutdown(struct question_db *db)
{
	struct question_db_cache *dbdata = db->data;
	struct question *q;

	while (dbdata->questions != NULL)
	{
		q = dbdata->questions;
		dbdata->questions = dbdata->questions->next;
		q->next = q->prev = NULL;
		question_deref(q);
	}

	return DC_OK;
}

static int http_question_set(struct question_db *db, struct question *q)
{
	return DC_NOTIMPL;
}

static struct question *http_question_get(struct question_db *db, 
	const char *ltag)
{
	struct question *q;
	char *path;
	char *translated_tag = translate_tag_name(ltag);
	char *url;
	char *buf;
	FILE *inf;
	struct rfc822_header *header = NULL;

	if (ltag == NULL) return DC_NOTOK;
	url = question_url(db, ltag);

	asprintf(&buf, "wget '%s' -O '/tmp/cdebconf_q_%s'", 
		 url, translated_tag);
	
	system(buf);
	INFO(INFO_DEBUG, "%s: url = [%s]", __FILE__, url);
	asprintf(&path, "/tmp/cdebconf_q_%s", translated_tag);

	if (path == NULL ||
	    (inf = fopen(path, "r")) == NULL)
		{
			if (errno == ENOENT)
				return DC_NOTOK;

			INFO(INFO_ERROR, "Cannot get question %s: %s",
			     ltag, strerror(errno));
			return DC_NOTOK;
		}

    while ((header = rfc822_parse_stanza(inf)) != NULL)
    {
        struct question *tmp;
        const char *name;

        name = rfc822_header_lookup(header, "name");

        if (name == NULL || *name == 0)
        {
            INFO(INFO_ERROR, "Read a stanza without a name");
            rfc822_header_destroy(header);
            continue;
        }

        tmp = question_new(name);

        question_setvalue(tmp, rfc822_header_lookup(header, "value"));
        tmp->flags = parse_flags(rfc822_header_lookup(header,"flags"));
        parse_owners(tmp, rfc822_header_lookup(header, "owners"));
        parse_variables(tmp, rfc822_header_lookup(header, "variables"));
        tmp->template = db->tdb->methods.get(db->tdb, rfc822_header_lookup(header, "template"));
        if (tmp->template == NULL) {
                tmp->template = template_new(name);
                db->tdb->methods.set(db->tdb, tmp->template);
        }
        rfc822_header_destroy(header);
    }

    fclose(inf);
    return q;
}

static int http_question_disown(struct question_db *db, const char *tag, 
	const char *owner)
{
	return DC_NOTIMPL;
}

static struct question *http_question_iterate(struct question_db *db,
	void **iter)
{
	return NULL;
}

struct template_db_module debconf_template_db_module = {
    initialize: http_template_initialize,
    shutdown: http_template_shutdown,
    set: http_template_set,
    get: http_template_get,
    remove: http_template_remove,
    iterate: http_template_iterate,
};

struct question_db_module debconf_question_db_module = {
    initialize: http_question_initialize,
    shutdown: http_question_shutdown,
    set: http_question_set,
    get: http_question_get,
    disown: http_question_disown,
    iterate: http_question_iterate,
    };

/*
  Local Variables:
  c-basic-offset: 8
  tab-width: 8
  indent-tabs-mode: t
  End:
*/
