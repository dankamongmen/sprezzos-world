
#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <locale.h>
#include <string.h>

#include "common.h"
#include "configuration.h"
#include "database.h"
#include "question.h"
#include "template.h"

static int debug = 0;
static int checkonly = 0;
static int unseen = 0;

static struct option params[] = {
    { "help", 0, NULL, 'h' },
    { "verbose", 0, &debug, 'v' },
    { "checkonly", 0, &checkonly, 'c' },
    { "unseen", 0, &unseen, 'u' },
    { 0, 0, 0, 0 }
};

static const struct {
    const char *name;
    unsigned int value;
} flags[] = {
    { "seen", DC_QFLAG_SEEN },
    { 0, 0 }
};

static const char * known_types[] = {
    "select",
    "boolean",
    "string",
    "multiselect",
    "note",
    "password",
    "text",
    "title",
    NULL
};

static void usage(const char *exename)
{
    printf("%s [-vcu] [file]\n", exename);
    puts("\t-v, --verbose     verbose output\n"
         "\t-c, --checkonly   only check the input file format\n"
         "\t-u, --unseen      do not set the 'seen' flag when preseeding values\n");
    exit(0);
}

static void load_answer(const char *owner, const char *label, const char * type,
    const char *content, struct question_db *qdb, struct template_db *tdb)
{
    if (debug)
        fprintf(stderr, "info: Loading answer for '%s'\n", label); 

    struct template *tp = tdb->methods.get(tdb, label);
    if (!tp)
    {
        tp = template_new(label);
        template_lset(tp, NULL, "type", type);
        template_lset(tp, NULL, "description", "Dummy template");
        template_lset(tp, NULL, "extended_description",
            "This is a fake template used to pre-seed the debconf database. "
            "If you are seeing this, something is probably wrong.");
    }
    else
    {
        template_lset(tp, NULL, "default", content);
        template_lset(tp, NULL, "type", type);
    }

    tdb->methods.set(tdb, tp);
    template_deref(tp);

    struct question *q = qdb->methods.get(qdb, label);

    if (!q)
    {
        fprintf(stderr, "Cannot find a question for %s\n", label);
        return;
    }

    question_owner_add(q, owner);
    question_setvalue(q, content);
    if (!unseen)
        q->flags |= DC_QFLAG_SEEN;

    qdb->methods.set(qdb, q);

    question_deref(q);
}

static void set_flag(const char *label, const char *flag, unsigned int value,
    const char *content, struct question_db *qdb)
{
    if (debug)
        fprintf(stderr, "info: Setting %s flag\n", flag); 

    struct question *q = qdb->methods.get(qdb, label);

    if (!q)
    {
        fprintf(stderr, "Cannot find a question for %s\n", label);
        return;
    }

    if (0 == strcmp(content, "true"))
        q->flags |= value;
    else if (0 == strcmp(content, "false"))
        q->flags &= ~value;        

    qdb->methods.set(qdb, q);

    question_deref(q);
}

static int flag_value(const char *type)
{
    int i;

    if (!type)
        return 0;

    for (i = 0; flags[i].name != NULL; i++)
    {
        if (0 == strcmp(flags[i].name, type))
            return flags[i].value;
    }

    return 0;
}

static int is_known_type(const char *type)
{
    int i;

    if (!type)
        return 0;

    for (i = 0; known_types[i] != NULL; i++)
    {
        if (0 == strcmp(known_types[i], type))
            return 1;
    }

    return 0;
}

static size_t strlenmunge(char *buf, int *line)
{
    size_t tmplen = strlen(buf);
    if (buf[tmplen - 1] == '\n')
    {
        (*line)++;
        if (tmplen >= 2 && buf[tmplen - 2] == '\r')
        {
            buf[tmplen - 1] = '\0';
            buf[tmplen - 2] = '\n';
            tmplen -= 1;
        }

        if (tmplen >= 2 && buf[tmplen - 2] == '\\')
        {
            buf[tmplen - 2] = '\0';
            tmplen -= 2;
        }
    }

    return tmplen;
}

#define BUFFERSIZE 2048
#define IS_SPACE(c) ((c == ' ') || (c == '\t'))
#define NOT_SET(var) (!var || *var == ' ' || *var == '\t')

int main(int argc, char **argv)
{
    int bufsize = BUFFERSIZE;
    char *buf;
    FILE *stream = stdin;
    struct configuration *config;
    struct template_db *tdb;
    struct question_db *qdb;
    int c, line = 0;

    setlocale(LC_ALL, "");

    config = config_new();

    while ((c = getopt_long(argc, argv, "hvcu:", params, NULL)) >= 0)
    {
        switch (c)
        {
            case 'h': usage(argv[0]); break;
            case 'v': debug = 1; break;
            case 'c': checkonly = 1; break;
            case 'u': unseen = 1; break;
            default: break;
        }
    }

    if (optind < argc)
    {
        stream = fopen(argv[optind], "r");
        if (!stream)
        {
            fprintf(stderr, "Can't open %s\n", argv[optind]);
            return 1;
        }
    }

    /* parse the configuration info */
    if (config->read(config, DEBCONFCONFIG) == 0)
        DIE("Error reading configuration information");

    /* initialize database modules */
    if ((tdb = template_db_new(config, NULL)) == 0)
        DIE("Cannot initialize DebConf template database");
    if ((qdb = question_db_new(config, tdb, NULL)) == 0)
        DIE("Cannot initialize DebConf config database");

    /* load database */
    tdb->methods.load(tdb);
    qdb->methods.load(qdb);

    buf = (char *)malloc(bufsize * sizeof(*buf));

    while (fgets(buf, bufsize, stream))
    {
        size_t tmplen = strlenmunge(buf, &line);
        
        while (buf[tmplen-1] != '\n')
        {
            bufsize += BUFFERSIZE;
            buf = realloc(buf, bufsize * sizeof(*buf));
            if (!buf)
                DIE("Could not allocate memory !");
            if (!fgets(buf + tmplen, bufsize - tmplen, stream))
                break;
            tmplen = strlenmunge(buf, &line);
        }

        CHOMP(buf);
        
        {
            char *ch = buf;
            char *owner = NULL, *label = NULL, *type = NULL, *content = NULL;

            while (IS_SPACE(*ch))
                ch++;

            owner = ch;

            while (*ch && NOT_SET(content))
            {
                if (IS_SPACE(*ch))
                {
                    *ch = '\0';
                    if (NOT_SET(label))
                        label = ch+1;
                    else if (NOT_SET(type))
                        type = ch+1;
                    else if (NOT_SET(content))
                        content = ch+1;
                }
                ch++;
            }

            if (owner && owner[0] && owner[0] != '#' &&
                label && label[0] && type && type[0] && content)
            {
                int flagvalue = flag_value(type);
                
                if (flagvalue)
                {
                    if (debug)
                        fprintf(stderr, "info: Trying to set %s flag to %s\n", type, content);
                    set_flag(label, type, flagvalue, content, qdb);
                }
                else if (is_known_type(type))
                {
                    if (debug)
                        fprintf(stderr, "info: Trying to set '%s' [%s] to '%s'\n", label, type, content);
                    load_answer(owner, label, type, content, qdb, tdb);
                }
                else
                    fprintf(stderr, "warning: Unknown type %s, skipping line %i\n", type, line);
            }
        }
    }

    fclose(stream);

    if (!checkonly)
    {
        qdb->methods.save(qdb);
        tdb->methods.save(tdb);
    }

    free(buf);
    question_db_delete(qdb);
    template_db_delete(tdb);
    config_delete(config);
    
    return 0;
}
