#include "common.h"
#include "database.h"
#include "configuration.h"
#include "question.h"
#include "template.h"
#include "priority.h"

#include <dlfcn.h>
#include <unistd.h>
#include <string.h>

static int common_accept(const char *type,
    struct configitem *accept_types, struct configitem *reject_types)
{
    struct configitem *child;
    int found;

    if (accept_types != NULL)
    {
        found = 0;
        for (child = accept_types->child; child != NULL; child = child->next)
        {
            if (strcmp(child->value, type) == 0)
                found = 1;
        }
        if (!found)
            return DC_REJECT;
    }

    if (reject_types != NULL)
    {
        found = 0;
        for (child = reject_types->child; child != NULL; child = child->next)
        {
            if (strcmp(child->value, type) == 0)
                found = 1;
        }
        if (found)
            return DC_REJECT;
    }

    return DC_OK;
}

/**
 *
 * Template database 
 *
 */
static int template_db_initialize(struct template_db *db, struct configuration *cfg)
{
    return DC_OK;
}

static int template_db_shutdown(struct template_db *db)
{
    return DC_OK;
}

static int template_db_load(struct template_db *db)
{
    return DC_OK;
}

static int template_db_reload(struct template_db *db)
{
    return DC_OK;
}

static int template_db_save(struct template_db *db)
{
    return DC_OK;
}

static int template_db_set(struct template_db *db, struct template *t)
{
    return DC_NOTIMPL;
}

static struct template *template_db_get(struct template_db *db, 
    const char *name)
{
    return 0;
}

static int template_db_remove(struct template_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static int template_db_lock(struct template_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static int template_db_unlock(struct template_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static struct template *template_db_iterate(struct template_db *db, 
    void **iter)
{
    return 0;
}

static int template_db_accept(struct template_db *db,
    const char *name, const char *type)
{
    char tmp[1024];
    struct configitem *accept_types, *reject_types;

    if (type == NULL || *type == '\0')
    {
        struct template *template = db->methods.get(db, name);
        if (template != NULL && template->type != NULL)
            type = template->type;
        else
            type = "";
    }

    snprintf(tmp, sizeof(tmp), "%s::accept_types", db->configpath);
    accept_types = db->config->tree(db->config, tmp);
    snprintf(tmp, sizeof(tmp), "%s::reject_types", db->configpath);
    reject_types = db->config->tree(db->config, tmp);

    return common_accept(type, accept_types, reject_types);
}

struct template_db *template_db_new(struct configuration *cfg,
    const char *instance)
{
    struct template_db *db;
    void *dlh;
    struct template_db_module *mod;
    char tmp[256];
    const char *modpath, *modname, *driver;

    if (instance != NULL) {
        modname = instance;
    } else {
        modname = cfg->get(cfg, "global::default::template", getenv("DEBCONF_TEMPLATE"));
    }

    if (modname == NULL)
        DIE("No template database instance defined");

    modpath = cfg->get(cfg, "global::module_path::database", 0);
    if (modpath == NULL)
        DIE("Database module path not defined (global::module_path::database)");

    snprintf(tmp, sizeof(tmp), "template::instance::%s::driver", modname);
    driver = cfg->get(cfg, tmp, 0);

    if (driver == NULL)
        DIE("Template instance driver not defined (%s)", tmp);

    snprintf(tmp, sizeof(tmp), "%s/%s.so", modpath, driver);
    if ((dlh = dlopen(tmp, RTLD_NOW)) == NULL)
        DIE("Cannot load template database module %s: %s", tmp, dlerror());

    if ((mod = (struct template_db_module *)dlsym(dlh, "debconf_template_db_module")) == NULL)
        DIE("Malformed template database module %s", modname);

    db = NEW(struct template_db);
    memset(db, 0, sizeof(struct template_db));
    db->handle = dlh;
    db->modname = STRDUP(modname);
    db->data = NULL;
    db->config = cfg;
    snprintf(db->configpath, sizeof(db->configpath), 
             "template::instance::%s", modname);

    memcpy(&db->methods, mod, sizeof(struct template_db_module));

#define SETMETHOD(method) if (db->methods.method == NULL) db->methods.method = template_db_##method

    SETMETHOD(initialize);
    SETMETHOD(shutdown);
    SETMETHOD(load);
    SETMETHOD(reload);
    SETMETHOD(save);
    SETMETHOD(set);
    SETMETHOD(get);
    SETMETHOD(remove);
    SETMETHOD(lock);
    SETMETHOD(unlock);
    SETMETHOD(iterate);
    SETMETHOD(accept);

#undef SETMETHOD

    if (db->methods.initialize(db, cfg) == 0)
    {
        template_db_delete(db);
        return NULL;
    }

    return db;
}

void template_db_delete(struct template_db *db)
{
    db->methods.shutdown(db);
    free(db->modname);
    dlclose(db->handle);

    DELETE(db);
}

/************************************************************************
 * Function: loadtemplate
 * Inputs: tdb - templates database
 *         qdb - questions database
 *         filename - which file to load templates from
 *         owner - owner for the templates
 * Outputs: none
 * Description: loads all the templates from a file
 * Assumptions: none
 ************************************************************************/
void template_db_loadfile(struct template_db *tdb, struct question_db *qdb, const char *filename, const char *owner, int flags)
{
    struct template *t, *oldt;
    struct question *q;

    if (!tdb)
    {
        INFO(INFO_ERROR, "Templates database not initialised");
        return;
    }

    t = template_load(filename);
    while (t)
    {
        oldt = tdb->methods.get(tdb, t->tag);
        if (oldt == NULL || (flags & DC_LOADTEMPLATE_MERGE) == 0 || NULL == template_l10nmerge(oldt, t))
        {
            if (tdb->methods.set(tdb, t) != DC_OK)
                INFO(INFO_ERROR, "Cannot add template %s", t->tag);
        }
        if (oldt)
            template_deref(oldt);

        if (qdb)
        {
            q = qdb->methods.get(qdb, t->tag);
            if (q == NULL)
            {
                q = question_new(t->tag);
                q->template = t;
                template_ref(t);
            }
            else if (q->template != t)
            {
                template_deref(q->template);
                q->template = t;
                template_ref(t);
            }
            question_owner_add(q, owner);
            if (qdb->methods.set(qdb, q) != DC_OK)
                INFO(INFO_ERROR, "Cannot add question %s", t->tag);
            question_deref(q);
        }
        oldt = t;
        t = t->next;
        template_deref(oldt);
    }
}

/**
 *
 * Config database 
 *
 */
static int question_db_initialize(struct question_db *db, struct configuration *cfg)
{
    return DC_OK;
}

static int question_db_shutdown(struct question_db *db)
{
    return DC_OK;
}

static int question_db_load(struct question_db *db)
{
    return DC_OK;
}

static int question_db_save(struct question_db *db)
{
    return DC_OK;
}

static int question_db_set(struct question_db *db, struct question *t)
{
    return DC_NOTIMPL;
}

static struct question *question_db_get(struct question_db *db, 
    const char *name)
{
    return 0;
}

static int question_db_disown(struct question_db *db, const char *name, 
    const char *owner)
{
    return DC_NOTIMPL;
}

static int question_db_disownall(struct question_db *db, const char *owner)
{
    struct question *q;
    void *iter = 0;

    while ((q = db->methods.iterate(db, &iter)))
    {
        db->methods.disown(db, q->tag, owner);
        question_deref(q);
    }
    return 0;
}

static int question_db_remove(struct question_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static int question_db_lock(struct question_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static int question_db_unlock(struct question_db *db, const char *name)
{
    return DC_NOTIMPL;
}

static int question_db_is_visible(struct question_db *db, const char *name,
    const char *priority)
{
    struct question *q = 0, *qp = 0, *qs = 0;
    struct configuration *config = db->config;
    const char *wantprio = NULL;
    const char *showold = NULL;
    int ret = DC_YES;

    /* Always display error messages */
    q = db->methods.get(db, name);
    if (q != NULL && q->template != NULL && q->template->type != NULL && strcmp(q->template->type, "error") == 0)
    {
        question_deref(q);
        return DC_YES;
    }
    /* priority can either come from the command line, environment
     * or from debconf configuration
     */
    wantprio = config->get(config, "_cmdline::priority", NULL);

    if (wantprio == NULL)
        wantprio = getenv("DEBIAN_PRIORITY");

    if (wantprio == NULL)
        if ((qp = db->methods.get(db, "debconf/priority")) != NULL)
        {
            wantprio = question_getvalue(qp, NULL);
            question_deref(qp);
        }

    /* error; no priority specified -- last resort fallback to medium */
    if (wantprio == NULL || strlen(wantprio) == 0)
        wantprio = "medium";

    if (priority_compare(priority, wantprio) < 0)
    {
        question_deref(q);
        return DC_NO;
    }
    
    if (q != NULL && (q->flags & DC_QFLAG_SEEN) != 0)
    {
        ret = DC_NO;
        showold = config->get(config, "_cmdline::showold", NULL);
        if (showold == NULL)
            if ((qs = db->methods.get(db, "debconf/showold")) != NULL)
            {
                showold = question_getvalue(qs, NULL);
                question_deref(qs);
            }
        if (showold != NULL)
        {
            if (strcmp(showold, "true") == 0)
                ret = DC_YES;
            else
                ret = DC_NO;
        }
    }

    question_deref(q);
    return ret;
}

static struct question *question_db_iterate(struct question_db *db,
    void **iter)
{
    return 0;
}

static int question_db_accept(struct question_db *db,
    const char *name, const char *type)
{
    char tmp[1024];
    struct configitem *accept_types, *reject_types;

    if (type == NULL || *type == '\0')
    {
        struct question *question = db->methods.get(db, name);
        if (question != NULL && question->template != NULL &&
                question->template->type != NULL)
            type = question->template->type;
        else
            type = "";
    }

    snprintf(tmp, sizeof(tmp), "%s::accept_types", db->configpath);
    accept_types = db->config->tree(db->config, tmp);
    snprintf(tmp, sizeof(tmp), "%s::reject_types", db->configpath);
    reject_types = db->config->tree(db->config, tmp);

    return common_accept(type, accept_types, reject_types);
}

struct question_db *question_db_new(struct configuration *cfg, 
                                    struct template_db *tdb, 
                                    const char *instance)
{
    struct question_db *db;
    void *dlh;
    struct question_db_module *mod;
    char tmp[256];
    const char *modpath, *driver, *modname = NULL;

    if (instance != NULL)
        modname = instance;

    if (modname == NULL)
        modname = getenv("DEBCONF_CONFIG");

    if (modname == NULL)
        modname = cfg->get(cfg, "global::default::config", 0);

    if (modname == NULL)
        DIE("No question database instance defined");

    modpath = cfg->get(cfg, "global::module_path::database", 0);
    if (modpath == NULL)
        DIE("Database module path not defined (global::module_path::database)");

    snprintf(tmp, sizeof(tmp), "config::instance::%s::driver", modname);
    driver = cfg->get(cfg, tmp, 0);

    if (driver == NULL)
        DIE("Config instance driver not defined (%s)", tmp);

    snprintf(tmp, sizeof(tmp), "%s/%s.so", modpath, driver);
    if ((dlh = dlopen(tmp, RTLD_NOW)) == NULL)
        DIE("Cannot load config database module %s: %s", tmp, dlerror());

    if ((mod = (struct question_db_module *)dlsym(dlh, "debconf_question_db_module")) == NULL)
        DIE("Malformed config database module %s", modname);

    db = NEW(struct question_db);
    memset(db, 0, sizeof(struct question_db));
    db->handle = dlh;
    db->modname = STRDUP(modname);
    db->data = NULL;
    db->config = cfg;
    db->tdb = tdb;
    snprintf(db->configpath, sizeof(db->configpath),
             "config::instance::%s", modname);

    memcpy(&db->methods, mod, sizeof(struct question_db_module));

#define SETMETHOD(method) if (db->methods.method == NULL) db->methods.method = question_db_##method

    SETMETHOD(initialize);
    SETMETHOD(shutdown);
    SETMETHOD(load);
    SETMETHOD(save);
    SETMETHOD(set);
    SETMETHOD(get);
    SETMETHOD(disown);
    SETMETHOD(disownall);
    SETMETHOD(remove);
    SETMETHOD(lock);
    SETMETHOD(unlock);
    SETMETHOD(is_visible);
    SETMETHOD(iterate);
    SETMETHOD(accept);

#undef SETMETHOD

    if (db->methods.initialize(db, cfg) == 0)
    {
        question_db_delete(db);
        return NULL;
    }

    return db;
}

void question_db_delete(struct question_db *db)
{
    db->methods.shutdown(db);
    free(db->modname);
    dlclose(db->handle);

    DELETE(db);
}

