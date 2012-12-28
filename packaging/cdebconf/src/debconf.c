/**
 * @file debconf.c
 * @brief Configuration module interface
 */
#include "confmodule.h"
#include "configuration.h"
#include "question.h"
#include "frontend.h"
#include "database.h"

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <debian-installer.h>

static struct configuration *config = NULL;
static struct frontend *frontend = NULL;
static struct confmodule *confmodule = NULL;
static struct question_db *questions = NULL;
static struct template_db *templates = NULL;
static const char *owner;
static const char *priority;

static struct option options[] = {
    { "owner", 1, 0, 'o' },
    { "frontend", 1, 0, 'f' },
    { "priority", 1, 0, 'p' },
    { 0, 0, 0, 0 },
};

static int save(void)
{
	return confmodule->save(confmodule);
}

static void cleanup()
{
	if (confmodule->frontend != NULL)
		frontend_delete(confmodule->frontend);
	if (confmodule->questions != NULL)
		question_db_delete(confmodule->questions);
	if (confmodule->templates != NULL)
		template_db_delete(confmodule->templates);
	if (confmodule->config != NULL)
		config_delete(confmodule->config);
}

static void sighandler(int sig)
{
	int status = 1;
	if (sig == SIGCHLD)
	{
		while (waitpid(0, &status, WNOHANG) > 0)
			sigchld_status = status;
		signal_received = sig;
		return;
	}
	save();
	/*
	 * SIGUSR1 used to reconfigure the language. Now it
	 * only saves the database.
	 */
	if (sig == SIGUSR1)
		return;
	cleanup();
	exit(status);
}

static void help(const char *exename)
{
    fprintf(stderr, "%s [-ffrontend] [-ppriority] [-oowner] <config script>\n", exename);
    fprintf(stderr, "%s [--frontend=frontend] [--priority=priority] [--owner=owner] <config script>\n", exename);
    exit(-1);
}

static void parsecmdline(struct configuration *config, int argc, char **argv)
{
    int c;

    while ((c = getopt_long(argc, argv, "+o:p:f:", options, NULL)) > 0)
    {
        switch (c)
        {
            case 'o':
                owner = optarg;
                break;
            case 'p':
                priority = optarg;
                break;
            case 'f':
                config->set(config, "_cmdline::frontend", optarg);
                break;
            default:
                break;
        }
    }

    if (optind >= argc)
    {
        help(argv[0]);
    }
}

static void guess_owner(char **argv)
{
    const char *runningscript = argv[optind];
    const char *filename;
    const char *extension;

    /* Already set with --owner */
    if (owner)
        return;

    /* Override */
    if ((owner = getenv("DEBCONF_PACKAGE")) != NULL)
        return;

    /* Set by dpkg since 1.15.4, so it should stop here in normal conditions */
    if ((owner = getenv("DPKG_MAINTSCRIPT_PACKAGE")) != NULL)
        return;

    filename = strrchr(runningscript, '/');
    if (filename == NULL)
        filename = runningscript;
    else
        filename++;
    extension = strrchr(filename ,'.');

    if (extension &&
        (!strcmp(extension+1, "preinst") || !strcmp(extension+1, "postinst") ||
         !strcmp(extension+1, "prerm") || !strcmp(extension+1, "postrm") ||
         !strcmp(extension+1, "config")))
        owner = strndup(filename, extension - filename);
}

static void runconfigscript(int argc, char **argv)
{
    char *base, *dot;
    const char *ext = strrchr(argv[optind], '.');
    if (!ext)
        return;

    if (strcmp(ext+1, "postinst") && strcmp(ext+1, "preinst"))
        return;

    base = malloc(strlen(argv[optind]) + strlen(".templates") + 1);
    strcpy(base, argv[optind]);
    dot = strrchr(base, '.');
    if (dot)
    {
        strcpy(dot, ".templates");
        template_db_loadfile(templates, questions, base, owner, DC_LOADTEMPLATE_MERGE);

        strcpy(dot, ".config");
		if (!strcmp(ext+1, "postinst") && (argc > optind + 1) &&
		    !strcmp(argv[optind + 1], "configure") && (0 == access(base, F_OK)))
        {
            char configurestring[] = "configure";
            int exitcode;
            char *version = getenv("DPKG_MAINTSCRIPT_VERSION");
            if (!version && (argc > optind + 2))
            {
                version = argv[optind + 2];
            }

            /* startup the confmodule; run the config script and talk to it */
            confmodule = confmodule_new(config, templates, questions, frontend);
            confmodule->owner = owner;
            char * args[] = 
            {
                NULL,
                base,
                configurestring,
                version
            };
            confmodule->run(confmodule, version ? 4 : 3, args );
            confmodule->communicate(confmodule);
            confmodule->shutdown(confmodule);

        	exitcode = confmodule->exitcode;

            confmodule->save(confmodule);

            confmodule_delete(confmodule);

            if (exitcode)
                exit(exitcode);

            /* The frontend might have been changed in the config script */
            frontend = confmodule->frontend;
        }
    }

    free(base);
}

int main(int argc, char **argv)
{
	struct sigaction sa;

	sa.sa_handler = &sighandler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_NOCLDSTOP | SA_RESTART;
	sigaction(SIGCHLD, &sa, NULL);

	signal(SIGINT, sighandler);
	signal(SIGTERM, sighandler);
	signal(SIGUSR1, sighandler);
	setlocale (LC_ALL, "");

	config = config_new();
	if (!config) {
	  DIE("Cannot read new config");
	}
	parsecmdline(config, argc, argv);

    if (!priority)
        priority = getenv("DEBIAN_PRIORITY");
    if (priority)
        config->set(config, "_cmdline::priority", priority);

	/* parse the configuration info */
	if (config->read(config, DEBCONFCONFIG) == 0)
		DIE("Error reading configuration information");

	/* initialize database and frontend modules */
	if ((templates = template_db_new(config, NULL)) == 0)
        	DIE("Cannot initialize debconf template database");
    	if (templates->methods.load(templates) != DC_OK)
        	DIE("Cannot initialize debconf template database");
	if ((questions = question_db_new(config, templates, NULL)) == 0)
		DIE("Cannot initialize debconf configuration database");
	if (questions->methods.load(questions) != DC_OK)
		DIE("Cannot initialize debconf configuration database");
	if ((frontend = frontend_new(config, templates, questions)) == 0)
		DIE("Cannot initialize debconf frontend");

    guess_owner(argv);
    
    /* set title */
    if (owner)
    {
        char *title;
        if (asprintf(&title, "Configuring %s", owner) >= 0)
        {
            frontend->methods.set_title(frontend, title);
            free(title);
        }
    }
    else
        owner = "unknown";

    runconfigscript(argc, argv);

    /* reset signal_received, otherwise ->communicate() will exit immediately */
    signal_received = 0;
    
	/* startup the confmodule; run the config script and talk to it */
	confmodule = confmodule_new(config, templates, questions, frontend);
        confmodule->owner = owner;
	confmodule->run(confmodule, argc - optind + 1, argv + optind - 1);
	confmodule->communicate(confmodule);
        confmodule->shutdown(confmodule);

	/* shutting down .... sync the database and shutdown the modules */
	save();
	cleanup();

	return confmodule->exitcode;
}
