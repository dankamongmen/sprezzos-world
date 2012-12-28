/**
 * @file debconf-communicate.c
 * @brief Configuration module interface
 */
#include "confmodule.h"
#include "configuration.h"
#include "frontend.h"
#include "database.h"

#include <signal.h>
#include <string.h>
#include <getopt.h>
#include <locale.h>
#include <errno.h>

static struct configuration *config = NULL;
static struct frontend *frontend = NULL;
static struct confmodule *confmodule = NULL;
static struct question_db *questions = NULL;
static struct template_db *templates = NULL;

static struct option options[] = {
    { "frontend", 1, 0, 'f' },
    { 0, 0, 0, 0 }
};

static int save()
{
        return confmodule->save(confmodule);
}

static void cleanup()
{
	if (frontend != NULL)
		frontend_delete(frontend);
	if (questions != NULL)
		question_db_delete(questions);
	if (templates != NULL)
		template_db_delete(templates);
	if (config != NULL)
		config_delete(config);
}

static void sighandler(int sig)
{
	save();
	if (sig != SIGUSR1) {
		cleanup();
		exit(1);
	}
}

static void help(const char *exename)
{
    fprintf(stderr, "%s [-ffrontend] [package]\n", exename);
    exit(-1);
}

static void parsecmdline(struct configuration *config, int argc, char **argv)
{
    int c;

    while ((c = getopt_long(argc, argv, "f:", options, NULL)) > 0)
    {
        switch (c)
        {
            case 'f':
                config->set(config, "_cmdline::frontend", optarg);
                break;
            default:
                break;
        }
    }

    if (optind > argc)
    {
        help(argv[0]);
    }
}

int main(int argc, char **argv)
{
    int code = 127;
    char buf[1024], *ret;
    char *in;
    size_t insize = 1024;

	signal(SIGINT, sighandler);
	signal(SIGTERM, sighandler);
	signal(SIGUSR1, sighandler);
	setlocale (LC_ALL, "");

	config = config_new();
	if (!config) {
	  DIE("Cannot read new config");
	}
	parsecmdline(config, argc, argv);

	/* parse the configuration info */
	if (config->read(config, DEBCONFCONFIG) == 0)
		DIE("Error reading configuration information");

	/* initialize database and frontend modules */
    if ((templates = template_db_new(config, NULL)) == 0)
        DIE("Cannot initialize DebConf template database");
	if ((questions = question_db_new(config, templates, NULL)) == 0)
		DIE("Cannot initialize DebConf configuration database");
	if ((frontend = frontend_new(config, templates, questions)) == 0)
		DIE("Cannot initialize DebConf frontend");

	/* load templates and config */
	templates->methods.load(templates);
    questions->methods.load(questions);

	/* startup the confmodule; run the config script and talk to it */
	confmodule = confmodule_new(config, templates, questions, frontend);

    if (optind < argc)
        confmodule->owner = argv[optind];
    else
        confmodule->owner = "unknown";

    in = malloc(insize);
    if (!in)
        DIE("Out of memory");
    memset(in, 0, insize);

    /* start feeding user commands to debconf ... */
    while (1) {
        int done = 0;
        buf[0] = 0;
        in[0] = 0;
        while (strchr(buf, '\n') == NULL) {
            int r = read(0, buf, sizeof(buf) - 1);
            if (r < 0) {
                if (errno == EINTR)
                    continue;
                code = 127;
                done = 1;
                break;
            }
            if (r == 0) {
                done = 1;
                break;
            }
            buf[r] = 0;
            if (strlen(in) + r + 1 > insize) {
                insize += sizeof(buf);
                in = realloc(in, insize);
                if (!in)
                    DIE("Out of memory");
            }
            strcat(in, buf);
        }
        if (done)
            break;

        ret = confmodule->process_command(confmodule, in);

        if (ret[0] == 0)
            break;

        /* extract the first number as the return code */
        code = atoi(ret);

        write(1, ret, strlen(ret));
        write(1, "\n", 1);
        free(ret);
    }
    free(in);

	/* shutting down .... sync the database and shutdown the modules */
	save();
	cleanup();

	return code;
}

