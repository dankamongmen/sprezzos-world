#include "common.h"
#include "configuration.h"
#include "database.h"
#include "textdb.h"
#include "template.h"
#include "question.h"
#include "strutl.h"

#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

static void translate_tag_name(char *buf)
{
	/* remove / from the tag name so that we can use it as a filename */
	char *t = buf;
	for (; *t != 0; t++)
		if (*t == '/') *t = ':';  /* : is illegal in templates etc */
}

static char *template_filename(struct template_db *db, const char *tag)
{
	static char filename[1024];
	static char tmp[1024];
	char tagname[1024];
	filename[0] = 0;

	strncpy(tagname, tag, sizeof(tagname));
	translate_tag_name(tagname);

    snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);

	snprintf(filename, sizeof(filename), "%s/%s",
		db->config->get(db->config, tmp, ""), tagname);

	return filename;
}

static char *question_filename(struct question_db *db, const char *tag)
{
	static char filename[1024];
    static char tmp[1024];
	char tagname[1024];

	filename[0] = 0;

	strncpy(tagname, tag, sizeof(tagname));
	translate_tag_name(tagname);

    snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);

	snprintf(filename, sizeof(filename), "%s/%s",
		db->config->get(db->config, tmp, "."), tagname);

	return filename;
}

static struct template *textdb_lookup_cached_template(
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

static void textdb_remove_cached_template(struct template_db *db,
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

/*
static struct question *textdb_lookup_cached_question(
	const struct database *db, const char *tag)
{
	struct question_db_cache *dbdata = db->data;
	struct question *result;
	for (result = dbdata->questions; result; result = result->next) 
	{
		if (strcmp(result->tag, tag) == 0) break;
	}
	return result;
}
*/

static int textdb_template_initialize(struct template_db *db, struct configuration *cfg)
{
	struct template_db_cache *dbdata;
	dbdata = malloc(sizeof(struct template_db_cache));

	if (dbdata == NULL)
		return DC_NOTOK;

	dbdata->templates = NULL;
	db->data = dbdata;

	return DC_OK;
}

static int textdb_template_shutdown(struct template_db *db)
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

static int textdb_template_set(struct template_db *db, struct template *t)
{
	FILE *outf;
	char *filename;
	const char *p, *lang;
	const char **field;

	if (template_lget(t, NULL, "tag") == NULL) return DC_NOTOK;
	filename = template_filename(db, template_lget(t, NULL, "tag"));
	
	if ((outf = fopen(filename, "w")) == NULL)
		return DC_NOTOK;

	fprintf(outf, "template {\n");

	for (field = template_fields_list; *field != NULL; field++)
	{
		p = template_lget(t, NULL, *field);
		if (p != NULL)
			fprintf(outf, "\t%s \"%s\";\n", *field, escapestr(p));
	}

	lang = template_next_lang(t, NULL);
	while (lang) 
	{
		for (field = template_fields_list; *field != NULL; field++)
		{
			p = template_lget(t, lang, *field);
			if (p != NULL && p != template_lget(t, NULL, *field))
			{
				if (strcmp(lang, "C") == 0)
					fprintf(outf, "\t%s-C \"%s\";\n",
							*field, escapestr(p));
				else
					fprintf(outf, "\t%s-%s.UTF-8 \"%s\";\n",
							*field, lang, escapestr(p));
			}
		}
		lang = template_next_lang(t, lang);
	}

	fprintf(outf, "};\n");
	fclose(outf);
	
	return DC_OK;
}

static struct template *textdb_template_get_real(struct template_db *db, 
	const char *ltag)
{
	struct configuration *rec;
	struct template *t;
	char *filename;
	struct configitem *node;

	if (ltag == NULL) return DC_NOTOK;
	filename = template_filename(db, ltag);

	rec = config_new();
	if (rec->read(rec, filename) != DC_OK)
	{
		config_delete(rec);
		return NULL;
	}
	
	node = rec->tree(rec, "template");
	if (node == 0 || node->child == 0 || node->child->value == 0)
	{
		config_delete(rec);
		return NULL;
	}

	t = template_new(0);
	node = node->child;
	while (node)
	{
		template_lset(t, NULL, node->tag,
			      STRDUP(unescapestr(node->value)));
		node = node->next;
	}

	config_delete(rec);

	return t;
}

static struct template *textdb_template_get(struct template_db *db, 
	const char *ltag)
{
	struct template *result;

	result = textdb_lookup_cached_template(db, ltag);
	if (!result && (result = textdb_template_get_real(db, ltag)))
	{
		struct template_db_cache *dbdata = db->data;
		result->next = dbdata->templates;
		dbdata->templates = result;
	}
	if (result)
		template_ref(result);
	return result;
}

static int textdb_template_remove(struct template_db *db, const char *tag)
{
	char *filename;

	if (tag == NULL) return DC_NOTOK;

	textdb_remove_cached_template(db, tag);

	filename = template_filename(db, tag);
	if (unlink(filename) == 0)
		return DC_OK;
	else
		return DC_NOTOK;
}

static struct template *textdb_template_iterate(struct template_db *db,
	void **iter)
{
	DIR *dir;
	struct dirent *ent;
    char tmp[1024];

	if (*iter == NULL)
	{
        snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);
		dir = opendir(db->config->get(db->config, tmp, ""));
		if (dir == NULL)
			return NULL;
		*iter = dir;
	}
	else
	{
		dir = (DIR *)*iter;
	}

	if ((ent = readdir(dir)) == NULL)
	{
		closedir(dir);
		return NULL;
	}

	return textdb_template_get(db, ent->d_name);
}

static int textdb_question_initialize(struct question_db *db, struct configuration *cfg)
{
	struct question_db_cache *dbdata;
	dbdata = malloc(sizeof(struct question_db_cache));

	if (dbdata == NULL)
		return DC_NOTOK;

	dbdata->questions = NULL;
	db->data = dbdata;

	return DC_OK;
}

static int textdb_question_shutdown(struct question_db *db)
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

static int textdb_question_set(struct question_db *db, struct question *q)
{
	FILE *outf;
	char *filename;
	struct questionvariable *var;
	struct questionowner *owner;

	if (q->tag == NULL) return DC_NOTOK;
	filename = question_filename(db, q->tag);
	
	if ((outf = fopen(filename, "w")) == NULL)
		return DC_NOTOK;

	fprintf(outf, "question {\n");
	fprintf(outf, "\ttag \"%s\";\n", escapestr(q->tag));
	fprintf(outf, "\tvalue \"%s\";\n", (q->value ? escapestr(q->value) : ""));
	fprintf(outf, "\tflags 0x%08X;\n", q->flags);
	fprintf(outf, "\ttemplate \"%s\";\n", escapestr(q->template->tag));
	if ((var = q->variables))
	{
		fprintf(outf, "\tvariables {\n");
		do {
			/* escapestr uses a static buf, so we do this in
			 * two steps */
			fprintf(outf, "\t\t%s ", escapestr(var->variable));
			fprintf(outf, "\"%s\";\n", escapestr(var->value));
		} while ((var = var->next));
		fprintf(outf, "\t};\n");
	}
	if ((owner = q->owners))
	{
		fprintf(outf, "\towners:: {\n");
		do {
			fprintf(outf, "\t\t\"%s\";\n", escapestr(owner->owner));
		} while ((owner = owner->next));
		fprintf(outf, "\t};\n");
	}

	fprintf(outf, "};\n");
	fclose(outf);
	
	return DC_OK;
}

static struct question *textdb_question_get(struct question_db *db, 
	const char *ltag)
{
	struct configuration *rec;
	struct question *q;
	char *filename;
	struct configitem *node;

	if (ltag == NULL) return DC_NOTOK;
	filename = question_filename(db, ltag);

    INFO(INFO_DEBUG, "%s: filename = [%s]", __FILE__, filename);

	rec = config_new();
	if (rec->read(rec, filename) != DC_OK)
	{
		config_delete(rec);
		return NULL;
	}

	q = question_new(0);

	q->tag = STRDUP(unescapestr(rec->get(rec, "question::tag", 0)));
	q->value = STRDUP(unescapestr(rec->get(rec, "question::value", 0)));
	q->flags = rec->geti(rec, "question::flags", 0);
	q->template = db->tdb->methods.get(db->tdb,
		unescapestr(rec->get(rec, "question::template", "")));
	
	/* TODO: variables and owners */
	if ((node = rec->tree(rec, "question::variables")) != 0)
	{
		node = node->child;
		for (; node != 0; node = node->next)
			question_variable_add(q, node->tag, node->value);
	}

	if ((node = rec->tree(rec, "question::owners")) != 0)
	{
		node = node->child;
		for (; node != 0; node = node->next)
			if (node->tag && node->tag[0] != 0 && node->tag[0] != ':')
				question_owner_add(q, node->tag);
	}

    INFO(INFO_DEBUG, "Read q = %s", q->tag);

	if (q->tag == 0 || q->value == 0 || q->template == 0)
	{
		question_deref(q);
		q = 0;
	}

	config_delete(rec);

	return q;
}

static int textdb_question_disown(struct question_db *db, const char *tag, 
	const char *owner)
{
	struct question *q = textdb_question_get(db, tag);
	if (q == NULL) return DC_NOTOK;
	question_owner_delete(q, owner);
	textdb_question_set(db, q);
	question_deref(q);
	return DC_OK;
}

static int textdb_question_remove(struct question_db *db, const char *tag)
{
	char *filename;

	if (tag == NULL) return DC_NOTOK;

	/* questions are not cached at present */

	filename = question_filename(db, tag);
	if (unlink(filename) == 0)
		return DC_OK;
	else
		return DC_NOTOK;
}

static struct question *textdb_question_iterate(struct question_db *db,
	void **iter)
{
	DIR *dir;
	struct dirent *ent;
    static const char *path = "";
    char tmp[1024];
    int ret;
    struct stat st;

	if (*iter == NULL)
	{
        snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);
        path = db->config->get(db->config, tmp, ".");
        INFO(INFO_VERBOSE, "Checking %s -> %s", tmp, path);

		dir = opendir(path);
		if (dir == NULL)
			return NULL;
		*iter = dir;
	}
	else
	{
		dir = (DIR *)*iter;
	}

    do {
        if ((ent = readdir(dir)) == NULL)
        {
            INFO(INFO_DEBUG, "readdir returned NULL");
            closedir(dir);
            return NULL;
        }

        snprintf(tmp, sizeof(tmp), "%s/%s", path, ent->d_name);
        ret = stat(tmp, &st);
    } while (ret < 0 || S_ISDIR(st.st_mode));

    INFO(INFO_DEBUG, "Getting %s", ent->d_name);
	return textdb_question_get(db, ent->d_name);
}

struct template_db_module debconf_template_db_module = {
    .initialize = textdb_template_initialize,
    .shutdown = textdb_template_shutdown,
    .set = textdb_template_set,
    .get = textdb_template_get,
    .remove = textdb_template_remove,
    .iterate = textdb_template_iterate,
};

struct question_db_module debconf_question_db_module = {
    .initialize = textdb_question_initialize,
    .shutdown = textdb_question_shutdown,
    .set = textdb_question_set,
    .get = textdb_question_get,
    .disown = textdb_question_disown,
    .remove = textdb_question_remove,
    .iterate = textdb_question_iterate,
};

/*
  Local Variables:
  c-basic-offset: 8
  tab-width: 8
  indent-tabs-mode: t
  End:
*/
