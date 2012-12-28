/**
 * @file debconf-get-selections.c
 * @brief Output contents of debconf database
 *
 */

#include "common.h"
#include "configuration.h"
#include "database.h"
#include "question.h"
#include "template.h"

#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <locale.h>
#include <string.h>

static struct option options[] = {
    { "installer", 0, NULL, 'i' },
    { "old", 0, NULL, 'o' },
    { 0, 0, 0, 0 }
};

static const char *defaultowner = "unknown";
static const char *templatedb = NULL;
static const char *configdb = NULL;

static void parsecmd(int argc, char **argv)
{
    int c;

    while ((c = getopt_long(argc, argv, "", options, NULL)) > 0)
    {
        switch (c)
        {
            case 'i':
                defaultowner = "d-i";
                templatedb = "di_templatedb";
                configdb = "di_configdb";
                break;
            case 'o':
                templatedb = "old_templatedb";
                configdb = "old_configdb";
                break;
            default:
                break;
        }
    }
}

int main(int argc, char **argv)
{
    struct configuration *config;
    struct template_db *tdb;
    struct question_db *qdb;
    struct question *q;
    void *iter;

    parsecmd(argc, argv);
    
    setlocale(LC_ALL, "");

    config = config_new();

    /* parse the configuration info */
    if (config->read(config, DEBCONFCONFIG) == 0)
        DIE("Error reading configuration information");

    /* initialize database modules */
    if ((tdb = template_db_new(config, templatedb)) == 0)
        DIE("Cannot initialize DebConf template database");
    if ((qdb = question_db_new(config, tdb, configdb)) == 0)
        DIE("Cannot initialize DebConf config database");

    /* load database */
    tdb->methods.load(tdb);
    qdb->methods.load(qdb);

    iter = 0;
    while ((q = qdb->methods.iterate(qdb, &iter)) != NULL)
    {
        struct questionowner *owner = q->owners;
        const char *type = template_lget(q->template, NULL, "type");

        if ((type == NULL) ||
            (strcmp(type, "title") == 0) ||
            (strcmp(type, "text") == 0))
            continue;

	    printf("# %s\n", q_get_description(0, q));
        if ((strcmp(type, "select") == 0) ||
            (strcmp(type, "multiselect") == 0))
            printf("# Choices: %s\n", q_get_choices(0, q));

        if (!owner)
        {
            printf("%s\t%s\t%s\t%s\n", defaultowner, q->tag,
                type, q->value?q->value:"");
        }
        else
        {
            while (owner)
            {
                printf("%s\t%s\t%s\t%s\n", owner->owner, q->tag,
                    type, q->value?q->value:"");
                owner = owner->next;
            }
        }
        question_deref(q);
    }

    template_db_delete(tdb);
    question_db_delete(qdb);

    config_delete(config);

    return 0;
}

