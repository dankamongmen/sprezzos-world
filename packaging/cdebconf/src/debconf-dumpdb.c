/**
 * @file debconf-dumpdb.c
 * @brief Dump a database
 *
 * This code was found on debian-boot@lists.debian.org 2003-07-30,
 * posted by Tollef Fog Heen.
 *
 */
#include "confmodule.h"
#include "configuration.h"
#include "frontend.h"
#include "database.h"
#include "template.h"
#include "question.h"

#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <unistd.h>
#include <locale.h>
#include <sys/types.h>
#include <regex.h>

static struct option g_dpc_args[] = {
    { "help", 0, NULL, 'h' },
    { "pattern", 1, NULL, 'p' },
    { 0, 0, 0, 0 }
};

static void usage(const char *exename)
{
    printf("%s [-h|--help] [-p|--pattern pattern] [source-db]\n", exename);
    printf("\tsource-db -  config database to dump\n");
    printf("\t-h, --help - this help message\n");
    printf("\t-p, --pattern pattern - dump only names matching this pattern\n");
    exit(0);
}

int main(int argc, char **argv)
{
    struct configuration *config;
    struct template_db *tdb;
    struct question_db *qdb;
    struct question *q;
    char *configpath;
    const char *tdbname;
    char *dbname = 0;
    char *pattern = 0;
    regex_t pattern_regex;
    void *iter;
    int c;

    setlocale(LC_ALL, "");
    
    while ((c = getopt_long(argc, argv, "hp:", g_dpc_args, NULL)) > 0)
    {
        switch (c)
        {
        case 'h': usage(argv[0]); break;
        case 'p': pattern = optarg; break;
        }
    }

    if (optind + 1 > argc)
        usage(argv[0]);

    dbname = argv[optind];

    if (dbname == NULL)
        usage(argv[0]);

    /* suppress warnings about unknown localized fields */
    setenv("DEBCONF_NO_I18N", "1", 1);

    /* parse the configuration info */
    config = config_new();
    if (config->read(config, DEBCONFCONFIG) == 0)
        DIE("Error reading configuration information");

    /* find out which template databases to load; fall back to global
     * default if not configured otherwise
     */
    if (asprintf(&configpath, "config::instance::%s::template", dbname) == -1)
        DIE("Out of memory");
    tdbname = config->get(config, configpath, NULL);

    /* initialize database modules */
    if ((tdb = template_db_new(config, tdbname)) == 0)
        DIE("Cannot initialize debconf template database");
    if ((qdb = question_db_new(config, tdb, dbname)) == 0)
        DIE("Cannot initialize debconf database");

    /* load database */
    tdb->methods.load(tdb);
    qdb->methods.load(qdb);
    
    /* 
     * Iterate through all the questions and print them out
     */

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

    /* TODO: error checking */
    iter = 0;
    while ((q = qdb->methods.iterate(qdb, &iter)) != NULL)
    {
        if (pattern) {
            if (regexec(&pattern_regex, q->tag, 0, 0, 0) != 0)
                goto nextq;
        }

        printf("%s %s %s\n", q->tag, q->template->type, q->value);
nextq:
        question_deref(q);
    }

    if (pattern)
        regfree(&pattern_regex);

    question_db_delete(qdb);
    template_db_delete(tdb);

    return 0;
}
