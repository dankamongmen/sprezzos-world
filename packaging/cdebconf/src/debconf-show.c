
#include "common.h"
#include "configuration.h"
#include "database.h"
#include "question.h"
#include "template.h"

#include <getopt.h>
#include <locale.h>
#include <string.h>

int listowners = 0;
int listdbs = 0;
static struct option options[] = {
    { "db", 1, NULL, 'd' },
    { "listowners", 0, &listowners, 'o' },
    { "listdbs", 0, &listdbs, 'l' },
    { 0, 0, 0, 0 }
};

static void usage(const char *exename)
{
    printf("Usage:\n");
    printf("\t%s packagename [...] [--db=dbname]\n", exename);
    printf("\t%s --listowners [--db=dbname]\n", exename);
    printf("\t%s --listdbs\n", exename);
    exit(0);
}

int main(int argc, char **argv)
{
	struct configuration *config = config_new();
	struct template_db *tdb;
	struct question_db *qdb;
	struct question *q;
	void *iter = 0;
	const char *tdbname = NULL;

    int c;
	const char *dbname = NULL;
	struct questionowner *owners = NULL;

    setlocale(LC_ALL, "");

    while ((c = getopt_long(argc, argv, "", options, NULL)) >= 0)
    {
        switch (c)
        {
            case 'd':
				dbname = optarg;
                break;
            default:
                break;
        }
    }

	config = config_new();

	if (config->read(config, DEBCONFCONFIG) == 0)
		DIE("Error reading configuration information");

	if (listdbs)
	{
		struct configitem *instance, *item;

		instance = config->tree(config,"config::instance");
		if (instance)
		{
			for (item=instance->child; item != NULL; item = item->next)
			{
				printf("%s %s\n", item->tag, item->value);
			}
		}
		
		config_delete(config);
		
		return 0;
	}
	
	if (dbname)
	{
		/* find out which template databases to load; fall back to global
		 * default if not configured otherwise
		 */
	    char *configpath;
		if (asprintf(&configpath, "config::instance::%s::template", dbname) == -1)
		    DIE("Out of memory");
		tdbname = config->get(config, configpath, NULL);
		free(configpath);
	}

	/* initialize database modules */
	if ((tdb = template_db_new(config, tdbname)) == 0)
		DIE("Cannot initialize debconf template database");
	if ((qdb = question_db_new(config, tdb, dbname)) == 0)
		DIE("Cannot initialize debconf database");

	tdb->methods.load(tdb);
	qdb->methods.load(qdb);

	if (listowners)
	{
		while((q = qdb->methods.iterate(qdb, &iter)) != NULL)
		{
			struct questionowner *owner = q->owners;

			while (owner)
			{
				struct questionowner *o = owners;
				while (o != NULL)
				{
					if (0 == strcmp(o->owner, owner->owner))
						break;
					o = o->next;
				}
				if (o == NULL)
				{
					o = malloc(sizeof(struct questionowner));
					o->next = owners;
					o->owner = owner->owner;
					owners = o;
					fprintf(stdout, "%s\n", owner->owner);
				}
				owner = owner->next;
			}
		}
	}
	else
	{
		for (c = optind; c < argc; c++)
		{
			struct questionowner *o = malloc(sizeof(struct questionowner));
			o->next = owners;
			o->owner = argv[c];
			owners = o;
		}

		if (!owners)
		{
			usage(argv[0]);
		}

		while((q = qdb->methods.iterate(qdb, &iter)) != NULL)
		{
			const char seen = (q->flags & DC_QFLAG_SEEN)?'*':' ';
			const char *type = q->template->type?q->template->type:"";
			const char *value = strcmp("password", type)?q->value:"(password omitted)";

			const struct questionowner *owner = q->owners;
			while (owner)
			{
				const struct questionowner *o = owners;
				while (o != NULL)
				{
					if (0 == strcmp(o->owner, owner->owner))
					{
						fprintf(stdout, "%c %s:", seen, q->tag);
						if (value)
							fprintf(stdout, " %s", value);
						fputc('\n', stdout);
						break;
					}
					o = o->next;
				}
				owner = owner->next;
			}
		}
	}

	while (owners)
	{
		struct questionowner *o = owners;
		owners = o->next;
		free(o);
	}

	question_db_delete(qdb);
	template_db_delete(tdb);

	config_delete(config);

	return 0;
}
