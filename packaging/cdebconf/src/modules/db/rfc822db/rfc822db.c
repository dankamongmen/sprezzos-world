#include "common.h"
#include "configuration.h"
#include "database.h"
#include "rfc822.h"
#include "rfc822db.h"
#include "template.h"
#include "question.h"
#include "strutl.h"

#include <dirent.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <search.h>

FILE *outf = NULL;

static const struct {
    const char *name;
    unsigned int value;
} debconf_qflags[] = {
    { "seen", DC_QFLAG_SEEN },
    { 0, 0 }
};

static struct template *rfc822db_template_get(struct template_db *db,
    const char *ltag);

int nodetemplatecomp(const void *pa, const void *pb) {
  return strcmp(((struct template *)pa)->tag, 
                ((struct template *)pb)->tag);
}

int nodequestioncomp(const void *pa, const void *pb) {
  return strcmp(((struct question *)pa)->tag, 
                ((struct question *)pb)->tag);
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
        wc = delim + 1;
        while (*wc == ' ' || *wc == '\t')
        {
            wc++;
        }
        
    }

    free(owc);
}

static unsigned int parse_flags(char *string)
{
    unsigned int ret = 0;
    char *wc, *owc;
   
    if (!string)
	    return 0;

    owc = wc = strdup(string);

    while (wc != NULL)
    {
        char *delim = wc;
        int i, finished = 0;
        while (*delim != ' ' && *delim != '\t' && *delim != '\0')
            delim++;
        if (*delim == '\0')
            finished = 1;
        *delim = '\0';
        for (i = 0; debconf_qflags[i].name; i++)
        {
            if (0 == strcmp(wc, debconf_qflags[i].name))
            {
                ret |= debconf_qflags[i].value;
            }
        }
        if (finished != 0)
            break;
        wc = delim + 1;
        while (*wc == ' ' || *wc == '\t')
        {
            wc++;
        }        
    }

    free(owc);
    return ret;
}

static FILE *rfc822db_file_open(struct configuration *config, const char *configpath, int *retval)
{
    const char *path;
    FILE *inf = NULL;
    char tmp[1024];

    *retval = DC_OK;
    
    snprintf(tmp, sizeof(tmp), "%s::path", configpath);
    path = config->get(config, tmp, 0);
    if (path == NULL)
    {
        INFO(INFO_VERBOSE, "Cannot open database <empty>");
        *retval = DC_NOTOK;
    }
    else if ((inf = fopen(path, "r")) == NULL)
    {
        if (errno == ENOENT)
        {
            const char *modestr;
            int mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;

            INFO(INFO_DEBUG, "Database file doesn't exist. Trying to create it");

            snprintf(tmp, sizeof(tmp), "%s::mode", configpath);
            modestr = config->get(config, tmp, 0);
            if (modestr)
                mode = strtol(modestr, NULL, 8);

            outf = fopen(path, "w");
            if ((outf == NULL) || (fclose(outf) != 0) || (chmod(path, mode) != 0))
            {
                INFO(INFO_VERBOSE, "Cannot create database file %s: %s",
                     path, strerror(errno));
                *retval = DC_NOTOK;
            }
            outf = NULL;
        }
        else
            *retval = DC_NOTOK;
    }

    return inf;
}

void rfc822db_template_destroyitem(void *data)
{
    template_deref((struct template *) data);
}

/* templates */
static int rfc822db_template_initialize(struct template_db *db, struct configuration *cfg)
{
    struct template_db_cache *dbdata;
    /*    fprintf(stderr,"rfc822db_initialize(db,cfg)\n");*/
    dbdata = malloc(sizeof(struct template_db_cache));
    if (dbdata == NULL)
        return DC_NOTOK;

    dbdata->root = NULL;
    dbdata->iterator = NULL;
    dbdata->dirty = false;
    db->data = dbdata;

    return DC_OK;
}

static int rfc822db_template_shutdown(struct template_db *db)
{
    struct template_db_cache *dbdata = db->data;
    if (dbdata == NULL)
        return DC_OK;
    if (dbdata->root)
        tdestroy(dbdata->root, rfc822db_template_destroyitem);
    if (dbdata->iterator)
        di_slist_destroy(dbdata->iterator, rfc822db_template_destroyitem);
    free(dbdata);
    db->data = NULL;
    return DC_OK;
}

/*
 * Function: rfc822db_template_load
 * Input: template database
 * Output: DC_OK/DC_NOTOK
 * Description: parse a template db file and put it into the cache
 * Assumptions: the file is in valid rfc822 format
 */
static int rfc822db_template_load(struct template_db *db)
{
    struct template_db_cache *dbdata = db->data;
    FILE *inf;
    struct rfc822_header *header = NULL;
    int openerror;

    INFO(INFO_VERBOSE, "rfc822db_template_load(db)");
    inf = rfc822db_file_open(db->config, db->configpath, &openerror);
    if (!inf)
    {
        return openerror;
    }

    while ((header = rfc822_parse_stanza(inf)) != NULL)
    {
        struct template *tmp;
        const char *name;
        struct rfc822_header *h;

        name = rfc822_header_lookup(header, "name");
        if (name == NULL)
        {
            INFO(INFO_ERROR, "Read a stanza without a name");
            rfc822_header_destroy(header);
            continue;
        }

        tmp = template_new(name);
        for (h = header; h != NULL; h = h->next)
            if (strcmp(h->header, "Name") != 0)
                template_lset(tmp, NULL, h->header, h->value);

        tmp->next = NULL;
        tsearch(tmp, &dbdata->root, nodetemplatecomp);
        rfc822_header_destroy(header);
    }

    fclose(inf);

    return DC_OK;
}

/*
 * Function: rfc822db_template_reload
 * Input: template database
 * Output: DC_OK/DC_NOTOK
 * Description: reparse a template db file and update the cache; used when
 *              the language is changed
 * Assumptions: the file is in valid rfc822 format
 */
static int rfc822db_template_reload(struct template_db *db)
{
    struct template_db_cache *dbdata = db->data;
    char tmp[1024];
    const char *path;
    FILE *inf;
    struct rfc822_header *header = NULL;

    INFO(INFO_VERBOSE, "rfc822db_template_reload(db)");
    snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);
    path = db->config->get(db->config, tmp, 0);
    if (path == NULL ||
        (inf = fopen(path, "r")) == NULL)
    {
        INFO(INFO_VERBOSE, "Cannot open template file %s",
            path ? path : "<empty>");
        return DC_NOTOK;
    }

    while ((header = rfc822_parse_stanza(inf)) != NULL)
    {
        struct template *tmp;
        bool is_new = false;
        const char *name;
        struct rfc822_header *h;

        name = rfc822_header_lookup(header, "name");
        if (name == NULL)
        {
            INFO(INFO_ERROR, "Read a stanza without a name");
            rfc822_header_destroy(header);
            continue;
        }

        INFO(INFO_VERBOSE, "Template %s:", name);
        tmp = rfc822db_template_get(db, name);
        if (tmp)
            template_l10nclear(tmp);
        else
        {
            tmp = template_new(name);
            is_new = true;
        }
        for (h = header; h != NULL; h = h->next)
            if (strcmp(h->header, "Name") != 0) {
                INFO(INFO_VERBOSE, "  %s=%s", h->header, h->value);
                template_lset(tmp, NULL, h->header, h->value);
            }

        tmp->next = NULL;
        if (is_new)
            tsearch(tmp, &dbdata->root, nodetemplatecomp);
        else
            template_deref(tmp);
        rfc822_header_destroy(header);
    }

    fclose(inf);

    return DC_OK;
}

void rfc822db_template_dump(const void *node, const VISIT which, const int depth)
{
    const char *p, *lang;
    const char **field;
    const struct template *t = (*(struct template **) node);

    switch (which) {
    case preorder:
        break;
    case endorder:
        break;
    case postorder: 
    case leaf:
        p = template_lget(t, NULL, "tag");
        INFO(INFO_VERBOSE, "dumping template %s", p);

        for (field = template_fields_list; *field != NULL; field++)
        {
            p = template_lget(t, NULL, *field);
            if (p != NULL)
            {
                if (strcmp(*field, "tag") == 0)
                    fprintf(outf, "Name: %s\n", escapestr(p));
                else
                    fprintf(outf, "%c%s: %s\n",
                        toupper((*field)[0]), (*field)+1, escapestr(p));
            }
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
                        fprintf(outf, "%c%s-C: %s\n",
                            toupper((*field)[0]), (*field)+1, escapestr(p));
                    else
                        fprintf(outf, "%c%s-%s.UTF-8: %s\n",
                            toupper((*field)[0]), (*field)+1, lang, escapestr(p));
                }
            }
            lang = template_next_lang(t, lang);
        }
        fprintf(outf, "\n");
    }
}

static int rfc822db_template_save(struct template_db *db)
{
    struct template_db_cache *dbdata = db->data;
    char tmp[1024];
    const char *path;
    struct stat st;

    if (outf != NULL)
    {
            INFO(INFO_ERROR, "Internal inconsistency error, outf is not NULL");
            return DC_NOTOK;
    }

    snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);
    path = db->config->get(db->config, tmp, 0);
    if (path == NULL)
    {
        INFO(INFO_ERROR, "Cannot open template file <empty>");
        return DC_NOTOK;
    }

    if (!dbdata->dirty && stat(path, &st) == 0)
    {
        INFO(INFO_DEBUG, "Template database %s clean; not saving", path);
        return DC_OK;
    }

    if ((outf = fopen(path, "w")) == NULL)
    {
        INFO(INFO_ERROR, "Cannot open template file %s: %s",
            path, strerror(errno));
        return DC_NOTOK;
    }

    twalk(dbdata->root, rfc822db_template_dump);

    if (fclose(outf) == EOF)
        perror("fclose");
    outf = NULL;
    return DC_OK;
}

static struct template *rfc822db_template_get(struct template_db *db, 
    const char *ltag)
{
        struct template_db_cache *dbdata = db->data;
        struct template *t, t2;
        t2.tag = (char*) ltag; /* Get rid of warning from gcc -- 
                                  this should be safe */
        t = tfind(&t2, &dbdata->root, nodetemplatecomp);
        if (t)
        {
                t = (*(struct template **) t);
                template_ref(t);
        }

        return t;
}

static int rfc822db_template_set(struct template_db *db, struct template *template)
{
        struct template_db_cache *dbdata = db->data;
    INFO(INFO_VERBOSE, "rfc822db_template_set(db,t=%s)", template->tag);

    tdelete(template, &dbdata->root, nodetemplatecomp);
    tsearch(template, &dbdata->root, nodetemplatecomp);
    dbdata->dirty = true;
   
    template_ref(template);

    return DC_OK;
}

static int rfc822db_template_remove(struct template_db *db, const char *tag)
{
    struct template_db_cache *dbdata = db->data;
    struct template *t, t2;

    INFO(INFO_VERBOSE, "rfc822db_template_remove(db,tag=%s)",tag);

    memset(&t2, 0, sizeof (struct template));
    t2.tag = (char*) tag;
    t = tfind(&t2, &dbdata->root, nodetemplatecomp);

    if (t)
    {
            t = *(struct template **) t;
            tdelete(t, &dbdata->root, nodetemplatecomp);
            dbdata->dirty = true;
            template_deref(t);
            return DC_OK;
    }
    
    return DC_NOTOK;
}

/* TODO: This is an ugly hack because there's no better way to do this
 * within the constraints of twalk() (since there's no user-data argument).
 * If we ever switch to some other tree API, this should go away
 * immediately. If we ever need iterate() to be thread-safe, this *needs* to
 * go away.
 */
di_slist *template_iterator;
void rfc822db_template_makeiterator(const void *nodep, const VISIT which,
    const int depth)
{
    if (which == postorder || which == leaf)
        di_slist_append(template_iterator,
                        template_dup(*(struct template **) nodep));
}

static struct template *rfc822db_template_iterate(struct template_db *db,
    void **iter)
{
    struct template_db_cache *dbdata = db->data;
    di_slist_node *node;
    struct template *t;

    INFO(INFO_VERBOSE, "rfc822db_template_iterate(db,*iter=%p)", *iter);

    node = *(di_slist_node **) iter;
    if (node == NULL) {
        if (dbdata->iterator)
            di_slist_destroy(dbdata->iterator, rfc822db_template_destroyitem);
        dbdata->iterator = di_slist_alloc();
        template_iterator = dbdata->iterator; /* non-thread-safe */
        twalk(dbdata->root, rfc822db_template_makeiterator);
        template_iterator = NULL;
        *iter = node = dbdata->iterator->head;
    } else
        *iter = node = node->next;

    if (node == NULL) {
        di_slist_destroy(dbdata->iterator, rfc822db_template_destroyitem);
        dbdata->iterator = NULL;
        return NULL;
    }

    t = (struct template *) node->data;
    template_ref(t);
    return t;
}

void rfc822db_question_destroyitem(void *data)
{
    question_deref((struct question *) data);
}

/* config database */
static int rfc822db_question_initialize(struct question_db *db, struct configuration *cfg)
{
    struct question_db_cache *dbdata;
    /*    fprintf(stderr,"rfc822db_initialize(db,cfg)\n");*/
    dbdata = malloc(sizeof(struct question_db_cache));

    if (dbdata == NULL)
        return DC_NOTOK;

    dbdata->root = NULL;
    dbdata->iterator = NULL;
    dbdata->dirty = false;
    db->data = dbdata;

    return DC_OK;
}

static int rfc822db_question_shutdown(struct question_db *db)
{
    struct question_db_cache *dbdata = db->data;
    if (dbdata == NULL)
        return DC_OK;
    if (dbdata->root)
        tdestroy(dbdata->root, rfc822db_question_destroyitem);
    if (dbdata->iterator)
        di_slist_destroy(dbdata->iterator, rfc822db_question_destroyitem);
    free(dbdata);
    db->data = NULL;
    return DC_OK;
}

/*
 * Function: rfc822db_question_load
 * Input: question database
 * Output: DC_OK/DC_NOTOK
 * Description: parse a question db file and put it into the cache
 * Assumptions: the file is in valid rfc822 format
 */
static int rfc822db_question_load(struct question_db *db)
{
    struct question_db_cache *dbdata = db->data;
    FILE *inf;
    struct rfc822_header *header = NULL;
    int openerror;

    INFO(INFO_VERBOSE, "rfc822db_question_load(db)");
    inf = rfc822db_file_open(db->config, db->configpath, &openerror);
    if (!inf)
    {
        return openerror;
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
        tsearch(tmp, &dbdata->root, nodequestioncomp);
        rfc822_header_destroy(header);
    }

    fclose(inf);

    return DC_OK;
}

void rfc822db_question_dump(const void *node, const VISIT which, const int depth)
{
  struct questionowner *owner;
  struct questionvariable *var;

  const struct question *q = (*(struct question **) node);
  switch (which) {
  case preorder:
    break;
  case endorder:
    break;
  case postorder: 
  case leaf:

        INFO(INFO_VERBOSE, "dumping question %s", (q)->tag); 
        fprintf(outf, "Name: %s\n", escapestr((q)->tag));
        fprintf(outf, "Template: %s\n", escapestr((q)->template->tag));
        if ((q)->value)
            fprintf(outf, "Value: %s\n", escapestr((q)->value));

        if ((owner = (q)->owners))
        {
            fprintf(outf, "Owners: ");
            for (; owner != NULL; owner = owner->next)
            {
                fprintf(outf, "%s", escapestr(owner->owner));
                if (owner->next != NULL)
                    fprintf(outf, ", ");
            }
            fprintf(outf, "\n");
        }

        if ((q)->flags)
        {
            int i;
            fprintf(outf, "Flags:");

            for (i = 0; debconf_qflags[i].name; i++)
            {
                if ((q)->flags & debconf_qflags[i].value)
                {
                    fprintf(outf, " %s", debconf_qflags[i].name);
                }
            }

            fprintf(outf, "\n");
        }
        
        if ((var = (q)->variables))
        {
            fprintf(outf, "Variables:\n");
            for (; var != NULL; var = var->next)
            {
                /* escapestr uses a static buf, so we do this in
                 * two steps */
                fprintf(outf, " %s = ", (var->variable ? escapestr(var->variable) : ""));
                fprintf(outf, "%s\n", (var->value ? escapestr(var->value) : ""));
            }
        }

        fprintf(outf, "\n");
  }
}


static int rfc822db_question_save(struct question_db *db)
{
    struct question_db_cache *dbdata = db->data;
    const char *path;
    char tmp[1024];
    struct stat st;
    
    snprintf(tmp, sizeof(tmp), "%s::path", db->configpath);
    path = db->config->get(db->config, tmp, 0);
    if (path == NULL)
    {
        INFO(INFO_ERROR, "Cannot open question file <empty>");
        return DC_NOTOK;
    }

    if (!dbdata->dirty && stat(path, &st) == 0)
    {
        INFO(INFO_DEBUG, "Question database %s clean; not saving", path);
        return DC_OK;
    }

    if ((outf = fopen(path, "w")) == NULL)
    {
        INFO(INFO_ERROR, "Cannot open question file %s: %s",
            path, strerror(errno));
        return DC_NOTOK;
    }

    twalk(dbdata->root, rfc822db_question_dump);

    if (fclose(outf) == EOF)
        perror("fclose");
    outf = NULL;

    return DC_OK;
}

static struct question *rfc822db_question_get(struct question_db *db, 
    const char *ltag)
{
    struct question_db_cache *dbdata = db->data;
    struct question *q, q2;

    memset(&q2, 0, sizeof (struct question));
    q2.tag = strdup(ltag);
    q = tfind(&q2, &dbdata->root, nodequestioncomp);
    free(q2.tag);
    if (q != NULL)
    {
            q = *(struct question **)q;
            question_ref(q);
    }

    return q;
}

static int rfc822db_question_set(struct question_db *db, struct question *question)
{
    struct question_db_cache *dbdata = db->data;
    struct question *q;

    INFO(INFO_VERBOSE, "rfc822db_question_set(db,q=%s,q=%p)", question->tag, question);

    /* merge old and new owner lists */
    q = tfind(question, &dbdata->root, nodequestioncomp);
    if (q != NULL) {
        q = *(struct question **)q;
        const struct questionowner *owner = q->owners;
        while (owner) {
            question_owner_add(question, owner->owner);
            owner = owner->next;
        }
    }

    tdelete(question, &dbdata->root, nodequestioncomp);
    tsearch(question, &dbdata->root, nodequestioncomp);
    dbdata->dirty = true;
    question_ref(question);

    return DC_OK;
}

static int rfc822db_question_remove(struct question_db *db, const char *tag)
{
    struct question_db_cache *dbdata = db->data;
    struct question *q, q2;

    INFO(INFO_VERBOSE, "rfc822db_question_remove(db,tag=%s)", tag);

    memset(&q2, 0, sizeof (struct question));
    q2.tag = (char*) tag;
    q = tfind(&q2, &dbdata->root, nodequestioncomp);
    if (q != NULL) {
        q = *(struct question **) q;
        tdelete(q, &dbdata->root, nodequestioncomp);
        dbdata->dirty = true;
        question_deref(q);
        return DC_OK;
    }
    return DC_NOTOK;
}

static int rfc822db_question_disown(struct question_db *db, const char *tag, 
    const char *owner)
{
    struct question *q = rfc822db_question_get(db, tag);
    /*    fprintf(stderr,"rfc822db_question_disown(db, tag=%s, owner=%s)\n",
          tag,owner);*/
    if (q == NULL) 
        return DC_NOTOK;
    question_owner_delete(q, owner);
    if (q->owners == NULL)
        rfc822db_question_remove(db, q->tag);
    else
        rfc822db_question_set(db, q);
    question_deref(q);

    return DC_OK;
}

/* TODO: This is an ugly hack because there's no better way to do this
 * within the constraints of twalk() (since there's no user-data argument).
 * If we ever switch to some other tree API, this should go away
 * immediately. If we ever need iterate() to be thread-safe, this *needs* to
 * go away.
 */
di_slist *question_iterator;
void rfc822db_question_makeiterator(const void *nodep, const VISIT which,
    const int depth)
{
    if (which == postorder || which == leaf)
        di_slist_append(question_iterator,
                        question_dup(*(struct question **) nodep));
}

static struct question *rfc822db_question_iterate(struct question_db *db,
    void **iter)
{
    struct question_db_cache *dbdata = db->data;
    di_slist_node *node;
    struct question *q;

    INFO(INFO_VERBOSE, "rfc822db_question_iterate(db,*iter=%p)", *iter);

    node = *(di_slist_node **) iter;
    if (node == NULL) {
        if (dbdata->iterator)
            di_slist_destroy(dbdata->iterator, rfc822db_question_destroyitem);
        dbdata->iterator = di_slist_alloc();
        question_iterator = dbdata->iterator; /* non-thread-safe */
        twalk(dbdata->root, rfc822db_question_makeiterator);
        question_iterator = NULL;
        *iter = node = dbdata->iterator->head;
    } else
        *iter = node = node->next;

    if (node == NULL) {
        di_slist_destroy(dbdata->iterator, rfc822db_question_destroyitem);
        dbdata->iterator = NULL;
        return NULL;
    }

    q = (struct question *) node->data;
    question_ref(q);
    return q;
}

struct template_db_module debconf_template_db_module = {
    .initialize = rfc822db_template_initialize,
    .shutdown = rfc822db_template_shutdown,
    .load = rfc822db_template_load,
    .reload = rfc822db_template_reload,
    .save = rfc822db_template_save,
    .set = rfc822db_template_set,
    .get = rfc822db_template_get,
    .remove = rfc822db_template_remove,
    .iterate = rfc822db_template_iterate,
};

struct question_db_module debconf_question_db_module = {
    .initialize = rfc822db_question_initialize,
    .shutdown = rfc822db_question_shutdown,
    .load = rfc822db_question_load,
    .save = rfc822db_question_save,
    .set = rfc822db_question_set,
    .get = rfc822db_question_get,
    .disown = rfc822db_question_disown,
    .remove = rfc822db_question_remove,
    .iterate = rfc822db_question_iterate,
};

void dump_question(struct question *q) {
    fprintf(stderr,"\nDUMPING QUESTION\n");
    fprintf(stderr,"Question: %s\n", q->tag);
    fprintf(stderr,"Value: %s\n", q->value);
    fprintf(stderr,"Ref count: %d\n", q->ref);
    fprintf(stderr,"Flags: %d\n", q->flags);
    fprintf(stderr,"Template: %p", q->template);
    fprintf(stderr,"(%s)\n", q->template->tag);
}

#if 0
int main(int argc, char** argv) 
{
  FILE *in;
  struct question *t = NULL;
  /*  struct rfc822_header *head; */
  in = fopen("testdata", "r");
  t = rfc822db_parse_questions(t,in);

  while (t) {
      struct questionowner *q = t->owners;
      struct questionvariable *v = t->variables;
      printf("\nname: %s\nvalue: %s\n", t->tag, t->value);
      while(q) {
          printf("owner: %s\n",q->owner);
          q = q->next;
      }

      while(v) {
          printf("vars: %s = %s\n",v->variable, v->value);
          v = v->next;
      }

    t = t->next;
  }

  /*  head = rfc822db_parse_stanza(in);
  while (head != NULL) {
    printf("key: %s\nvalue:%s\n", head->header, head->value);
    head = head->next;
    }*/
  return 0;
}
#endif
