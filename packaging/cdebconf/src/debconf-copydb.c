/**
 * @file debconf-copydb.c
 * @brief Allows the user to load template/question from
 *        one database to another
 */
#include "confmodule.h"
#include "configuration.h"
#include "frontend.h"
#include "database.h"
#include "question.h"
#include "template.h"

#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <unistd.h>
#include <locale.h>
#include <sys/types.h>
#include <regex.h>
#include <stdbool.h>

static struct option g_dpc_args[] = {
    { "help", 0, NULL, 'h' },
    { "pattern", 1, NULL, 'p' },
    { 0, 0, 0, 0 }
};

static void usage(const char *exename)
{
    printf("%s [-h|--help] [-p|--pattern pattern] source-db dest-db\n", exename);
    printf("\tsource-db, dest-db - config databases to copy from/to\n");
    printf("\t-h, --help - this help message\n");
    printf("\t-p, --pattern pattern - copy only names matching this pattern\n");
    exit(0);
}

int main(int argc, char **argv)
{
    struct configuration *config;
    struct template_db *tdb1, *tdb2;
    struct question_db *db1, *db2;
    struct question *q;
    struct template *t;
    char *configpath1, *configpath2;
    const char *tdb1name, *tdb2name;
    char *db1name = 0, *db2name = 0;
    char *pattern = 0;
    regex_t pattern_regex;
    void *iter;
    int c;
    bool tdb2_changed = false;

    setlocale(LC_ALL, "");
    
    while ((c = getopt_long(argc, argv, "hp:", g_dpc_args, NULL)) > 0)
    {
        switch (c)
        {
        case 'h': usage(argv[0]); break;
        case 'p': pattern = optarg; break;
        }
    }

    if (optind + 2 > argc)
        usage(argv[0]);

    /* always load all translations */
    unsetenv("DEBCONF_DROP_TRANSLATIONS");

    db1name = argv[optind];
    db2name = argv[optind+1];

    if (db1name == NULL || db2name == NULL)
        usage(argv[0]);

    /* parse the configuration info */
    config = config_new();
    if (config->read(config, DEBCONFCONFIG) == 0)
        DIE("Error reading configuration information");

    /* find out which template databases to load; fall back to global
     * default if not configured otherwise
     */
    if (asprintf(&configpath1, "config::instance::%s::template",
                 db1name) == -1)
        DIE("Out of memory");
    tdb1name = config->get(config, configpath1, NULL);
    if (asprintf(&configpath2, "config::instance::%s::template",
                 db2name) == -1)
        DIE("Out of memory");
    tdb2name = config->get(config, configpath2, NULL);

    /* initialize database modules */
    if ((tdb1 = template_db_new(config, tdb1name)) == 0)
        DIE("Cannot initialize first DebConf template database");
    if ((tdb2 = template_db_new(config, tdb2name)) == 0)
        DIE("Cannot initialize second DebConf template database");
    if ((db1 = question_db_new(config, tdb1, db1name)) == 0)
        DIE("Cannot initialize first DebConf database");
    if ((db2 = question_db_new(config, tdb2, db2name)) == 0)
        DIE("Cannot initialize second DebConf database");

    /* load database */
    tdb1->methods.load(tdb1);
    tdb2->methods.load(tdb2);
    db1->methods.load(db1);
    db2->methods.load(db2);
    
    /* maybe compile pattern regex */
    if (pattern) {
        int err = regcomp(&pattern_regex, pattern, REG_EXTENDED | REG_NOSUB);
        if (err != 0) {
            int errmsgsize = regerror(err, &pattern_regex, NULL, 0);
            char *errmsg = malloc(errmsgsize);
            if (errmsg == NULL)
                DIE("Out of memory");
            regerror(err, &pattern_regex, errmsg, errmsgsize);
            DIE("regcomp: %s", errmsg);
        }
    }

    /* 
     * Iterate through all the questions and put them into db2
     */

    /* TODO: error checking */
    iter = 0;
    while ((q = db1->methods.iterate(db1, &iter)) != NULL)
    {
        if (pattern) {
            if (regexec(&pattern_regex, q->tag, 0, 0, 0) != 0)
                goto nextq;
        }

        db2->methods.set(db2, q);
        if ((t = tdb2->methods.get(tdb2, q->template->tag)) == NULL) {
            /* Must copy the template as well */
            t = tdb1->methods.get(tdb1, q->template->tag);
            tdb2->methods.set(tdb2, t);
            tdb2_changed = true;
        }
        template_deref(t);
nextq:
        question_deref(q);
    }

    if (pattern)
        regfree(&pattern_regex);

    db2->methods.save(db2);
    if (tdb2_changed)
        tdb2->methods.save(tdb2);
    question_db_delete(db1);
    question_db_delete(db2);
    template_db_delete(tdb1);
    template_db_delete(tdb2);

    return 0;
}

