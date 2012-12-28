#include <EXTERN.h>
#include <perl.h>
#undef DIE /* use our own DIE */

#include "common.h"
#include "configuration.h"
#include "database.h"
#include "perldb.h"
#include "template.h"
#include "question.h"
#include "strutl.h"

#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#define DBDATA(db) ((struct perldb_data *)db->data)

static PerlInterpreter *perlinterp = 0;

static int perldb_initialize(struct database *db, struct configuration *cfg)
{
	char *embedded[] = { "", "-e", "0" };
	struct perldb_data *data = NEW(struct perldb_data);

	memset(data, 0, sizeof(struct perldb_data));
	db->data = data;

	perlinterp = perl_alloc();
	perl_construct(perlinterp);
	perl_parse(perlinterp, NULL, 3, embedded, NULL);
	perl_run(perlinterp);

	perl_eval_pv("use Data::Dumper", TRUE);
	
	return DC_OK;
}

static int perldb_shutdown(struct database *db)
{
	perl_destruct(perlinterp);
	perl_free(perlinterp);
	return DC_OK;
}

static int perldb_load(struct database *db)
{
	int ret = DC_OK;
	const char *questiondb, *templatedb;
	char perlstr[1024];
	SV **tmp, *junk;
	dSP;

	questiondb = db->config->get(db->config, 
		"database::driver::perldb::questiondb", 0);
	templatedb = db->config->get(db->config, 
		"database::driver::perldb::templatedb", 0);

	ENTER;
	SAVETMPS;
	PUSHMARK(SP);
	snprintf(perlstr, sizeof(perlstr), "require \"%s\"", questiondb);
	perl_call_pv(perlstr, G_EVAL|G_SCALAR);
	snprintf(perlstr, sizeof(perlstr), "require \"%s\"", templatedb);
	perl_call_pv(perlstr, G_EVAL|G_SCALAR);
	SPAGAIN;
	if (SvTRUE(ERRSV))
	{
		STRLEN n_a;
		fprintf(stderr, "%s\n", SvPV(ERRSV, n_a));
		junk = POPs;
		ret = DC_NOTOK;
	}
	PUTBACK;
	FREETMPS;
	LEAVE;

	tmp = hv_fetch(PL_defstash, "questions", 9, 0);
	if (tmp != 0)
		DBDATA(db)->questions = *tmp;
	tmp = hv_fetch(PL_defstash, "templates", 9, 0);
	if (tmp != 0)
		DBDATA(db)->templates = *tmp;

	return ret;
}

static int perldb_save(struct database *db)
{
	const char *questiondb, *templatedb;
	int count;
	SV *dumper;
	dSP;

	questiondb = db->config->get(db->config, 
		"database::driver::perldb::questiondb", 0);
	templatedb = db->config->get(db->config, 
		"database::driver::perldb::templatedb", 0);

	ENTER;
	SAVETMPS;
	PUSHMARK(SP);

	count = perl_call_pv("Data::Dumper->new([\%questions], ['*questions']", G_SCALAR);
	SPAGAIN;
	if (count == 1)
	{
		dumper = POPs;

		XPUSHs(dumper);
		PUTBACK;
		count = perl_call_method("Dump", G_SCALAR);
	}
	PUTBACK;

	FREETMPS;
	LEAVE;


	return DC_OK;
}

static int perldb_template_set(struct database *db, struct template *t)
{
	return DC_OK;
}

static struct template *perldb_template_get(struct database *db, 
	const char *ltag)
{
	return NULL;
}

static int perldb_template_remove(struct database *db, const char *tag)
{
	return DC_OK;
}

static struct template *perldb_template_iterate(struct database *db,
	void **iter)
{
	return NULL;
}

static int perldb_question_set(struct database *db, struct question *q)
{
	return DC_OK;
}

static struct question *perldb_question_get(struct database *db, 
	const char *ltag)
{
	return NULL;
}

static int perldb_question_disown(struct database *db, const char *tag, 
	const char *owner)
{
	return DC_OK;
}

static struct question *perldb_question_iterate(struct database *db,
	void **iter)
{
	return NULL;
}

struct database_module debconf_database_module =
{
	initialize: perldb_initialize,
	shutdown: perldb_shutdown,
	load: perldb_load,
	save: perldb_save,

	template_set: perldb_template_set,
	template_get: perldb_template_get,
	template_remove: perldb_template_remove,
	template_iterate: perldb_template_iterate,

	question_get: perldb_question_get,
	question_set: perldb_question_set,
	question_disown: perldb_question_disown,
	question_iterate: perldb_question_iterate
};
