/***********************************************************************
 *
 * cdebconf - An implementation of the Debian Configuration Management
 *            System
 *
 * File: src/modules/mysql/mysql.c
 *
 * Description: mysql DB module for cdebconf
 *
 * cdebconf is (c) 2000-2001 Randolph Chung and others under the following
 * license.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 ***********************************************************************/
#include "common.h"
#include "configuration.h"
#include "database.h"
#include "mysql.h"
#include "template.h"
#include "question.h"
#include "strutl.h"

#include <dirent.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>

#define CFGPREFIX	"database::driver::mysql::"
#define DBDATA(db)	((struct dbdata *)db)
#define DBH(db)		&(DBDATA(db)->dbh)

#ifdef __GNUC__
static MYSQL_RES *mysql_runquery(struct database *db, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));
#endif

static MYSQL_RES *mysql_runquery(struct database *db, const char *fmt, ...)
{
	va_list ap;
	char buf[1024];

	va_start(ap, fmt);
	vsnprintf(buf, sizeof(buf), fmt, ap);
	va_end(ap);

	INFO(INFO_DEBUG, "SQL: [%s]", buf);
	if (mysql_query(DBH(db), buf) != 0)
		return NULL;

	/* Fetch all results */
	return mysql_store_result(DBH(db));
}

 
static int mysql_initialize(struct database *db, struct configuration *cfg)
{
	struct dbdata *data = NEW(struct dbdata);
	memset(data, 0, sizeof(struct dbdata));
	db->data = data;

	mysql_init(DBH(db));
	mysql_real_connect(DBH(db), 
		cfg->get(cfg, CFGPREFIX "host", 0),
		cfg->get(cfg, CFGPREFIX "user", 0),
		cfg->get(cfg, CFGPREFIX "passwd", 0),
		cfg->get(cfg, CFGPREFIX "database", "DebianConfig"),
		cfg->geti(cfg, CFGPREFIX "port", 0),
		cfg->get(cfg, CFGPREFIX "socket", 0),
		0
	);

	if (mysql_errno(DBH(db)) != 0)
		return DC_NOTOK;

	return DC_OK;
}

static int mysql_shutdownmod(struct database *db)
{
	mysql_close(DBH(db));
	return DC_OK;
}

static int mysql_load(struct database *db)
{
	return DC_OK;
}

static int mysql_save(struct database *db)
{
	return DC_OK;
}

static int mysql_template_set(struct database *db, struct template *t)
{
	return DC_OK;
}

static struct template *mysql_template_get(struct database *db, 
	const char *ltag)
{
	struct template *result = NULL;
	return result;
}

static int mysql_template_remove(struct database *db, const char *tag)
{
	return DC_OK;
}

static struct template *mysql_template_iterate(struct database *db,
	void **iter)
{
	return 0;
}

static int mysql_question_set(struct database *db, struct question *q)
{
	return DC_OK;
}

static struct question *mysql_question_get(struct database *db, 
	const char *ltag)
{
	struct question *q = NULL;
	return q;
}

static int mysql_question_disown(struct database *db, const char *tag, 
	const char *owner)
{
	return DC_OK;
}

static struct question *mysql_question_iterate(struct database *db,
	void **iter)
{
	return 0;
}

struct database_module debconf_database_module =
{
	initialize: mysql_initialize,
	shutdown: mysql_shutdownmod,
	load: mysql_load,
	save: mysql_save,

	template_set: mysql_template_set,
	template_get: mysql_template_get,
	template_remove: mysql_template_remove,
	template_iterate: mysql_template_iterate,

	question_get: mysql_question_get,
	question_set: mysql_question_set,
	question_disown: mysql_question_disown,
	question_iterate: mysql_question_iterate
};
