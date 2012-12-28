/**
 * @file dpkg-reconfigure.c
 * @brief dpkg-reconfigure utility that allows users to 
 *        reconfigure a package after it's been installed
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <getopt.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <locale.h>
#include <stdbool.h>


#include "confmodule.h"
#include "configuration.h"
#include "frontend.h"
#include "database.h"
#include "question.h"
#include "template.h"
#include "strutl.h"

#define PACKAGEFIELD	"Package: "
#define STATUSFIELD	"Status: "
#define VERSIONFIELD	"Version: "
#define ARCHITECTUREFIELD	"Architecture: "

struct option g_dpc_args[] = {
	{ "help", 0, NULL, 'h' },
	{ "frontend", 1, NULL, 'f' },
	{ "priority", 1, NULL, 'p' },
	{ "default-priority", 0, NULL, 'd' },
	{ "all", 0, NULL, 'a' },
	{ "unseen-only", 0, NULL, 'u' },
	{ "force", 0, NULL, 'F' },
	{ "no-reload", 0, NULL, 'n' },
	{ 0, 0, 0, 0 }
};

struct package_name {
    char *name;
    struct package_name *next;
};

static struct configuration *g_config;
static struct frontend *g_frontend;
static struct template_db *g_templates;
static struct question_db *g_questions;
static int opt_all = 0, opt_force = 0, no_reload = 0;

/************************************************************************
 * Function: cleanup
 * Inputs: none
 * Outputs: none
 * Description: cleans up if before we exit
 * Assumptions: none
 ************************************************************************/
static void cleanup(void)
{
	if (g_frontend != NULL)
		frontend_delete(g_frontend);
	if (g_templates != NULL)
	{
		g_templates->methods.save(g_templates);
		template_db_delete(g_templates);
	}
	if (g_questions != NULL)
	{
		g_questions->methods.save(g_questions);
		question_db_delete(g_questions);
	}
	if (g_config != NULL)
		config_delete(g_config);
}

/************************************************************************
 * Function: sighandler
 * Inputs: sig - signal caught
 * Outputs: none
 * Description: signal handler
 * Assumptions: only catches SIGINT right now
 ************************************************************************/
static void sighandler(int sig)
{
	cleanup();
}

/************************************************************************
 * Function: usage
 * Inputs: none
 * Outputs: none
 * Description: prints a usage message and exits
 * Assumptions: none
 ************************************************************************/
static void usage(void)
{
	printf("Usage: dpkg-reconfigure [options] packages\n"
        "  -f, --frontend <frontend>    Select the frontend to use.\n"
        "  -p, --priority <priority>    Specify the minimum priority.\n"
        "      --default-priority    Use default priority.\n"
        "  -a, --all            Reconfigure all packages.\n"
        "  -u, --unseen-only        Show only not yet seen questions.\n"
        "      --force            Force reconfiguration of broken packages.\n"
        "      --no-reload        Do not reload temlates.\n"
        "  -h, --help            Display this help.\n");
	exit(0);
}

/************************************************************************
 * Function: file_exists
 * Inputs: filename - name of file to check
 *         mode - minimum mode requirement (mask)
 * Outputs: true if filename exists and is a regular file, false otherwise
 * Description: checks to see if a file exists
 * Assumptions: none
 ************************************************************************/
static bool file_exists(const char *filename, mode_t mode)
{
	struct stat buf;
	if (!filename || !*filename) return false;
	stat(filename, &buf);
	if (!S_ISREG(buf.st_mode)) return false;
	if ((buf.st_mode & mode) == 0) return false;
	return true;
}

/************************************************************************
 * Function: getfield
 * Inputs: package - which package to get the status of
 *         field - field to get value of
 * Outputs: char * - status of package
 * Description: gets the value of a field for a package from the status file
 * Assumptions: returns pointer to a static buffer
 ************************************************************************/
static char *getfield(const char *package, const char *fieldname)
{
	static char field[100] = {0};
	int fieldlen;
	int pipefd[2];
	int pid;

	fieldlen = strlen(fieldname);
	if (pipe(pipefd) < 0)
	{
		perror("pipe");
		return NULL;
	}

	pid = fork();
	if (pid < 0)
	{
		perror("fork");
		close(pipefd[0]);
		close(pipefd[1]);
		return NULL;
	}
	else if (pid == 0) /* child */
	{
		close(pipefd[0]);
		dup2(pipefd[1], 1);
		close(pipefd[1]);
		execlp("dpkg-query", "dpkg-query", "--status", package, NULL);
		/* if we reach here, then execlp failed */
		perror("execlp");
		_exit (127);
	}
	else /* parent */
	{
		FILE *fp;
		char *buf = NULL;
		size_t size;

		close(pipefd[1]);
		fp = fdopen(pipefd[0], "r");

		while (getline(&buf, &size, fp) > 0)
		{
			CHOMP(buf);
				if (strncmp(buf, fieldname, fieldlen) == 0)
				{
					strncpy(field, buf+fieldlen, sizeof(field));
					break;
				}
		}
		free(buf);
		fclose(fp);
	}
	return field;
}

/************************************************************************
 * Function: getallpackages
 * Inputs: none
 * Outputs: char * - space separated list of packages
 * Description: gets the list of installed packages
 ************************************************************************/
static char *getallpackages()
{
	char *buf = NULL;
	int pipefd[2];
	int pid;

	if (pipe(pipefd) < 0)
	{
		perror("pipe");
		return NULL;
	}

	pid = fork();
	if (pid < 0)
	{
		perror("fork");
		close(pipefd[0]);
		close(pipefd[1]);
		return NULL;
	}
	else if (pid == 0) /* child */
	{
		close(pipefd[0]);
		dup2(pipefd[1], 1);
		close(pipefd[1]);
		execlp("dpkg-query", "dpkg-query", "--show", "-f=${Package} ", NULL);
		/* if we reach here, then execlp failed */
		perror("execlp");
		_exit (127);
	}
	else /* parent */
	{
		FILE *fp;
		size_t size = 0, offset = 0;

		close(pipefd[1]);
		fp = fdopen(pipefd[0], "r");

		while (!feof(fp))
		{
			size += 2048;
			buf = realloc(buf, size);
			if (!buf)
				exit(1);
			fread(buf+offset,sizeof(*buf), 2048, fp);
			offset = size;
		}
		fclose(fp);
	}
	return buf;
}

/************************************************************************
 * Function: getalltriggers
 * Inputs: none
 * Outputs: struct package_name * - a linked list of package names
 * Description: gets the list of packages with pending triggers
 ************************************************************************/
static struct package_name *getalltriggers()
{
    struct package_name *retval = NULL;
    int pipefd[2];
    int pid;

    if (pipe(pipefd) < 0)
    {
        perror("pipe");
        return NULL;
    }

    pid = fork();
    if (pid < 0)
    {
        perror("fork");
        close(pipefd[0]);
        close(pipefd[1]);
        return NULL;
    }
    else if (pid == 0) /* child */
    {
        close(pipefd[0]);
        dup2(pipefd[1], 1);
        close(pipefd[1]);
        execlp("dpkg-query", "dpkg-query", "--show",
         "-f=${Package} ${binary:Package}\t${Triggers-Pending}\n", NULL);
        /* if we reach here, then execlp failed */
        perror("execlp");
        _exit (127);
    }
    else /* parent */
    {
        FILE *fp;
        char *buf = NULL;
        size_t size;

        close(pipefd[1]);
        fp = fdopen(pipefd[0], "r");

        while (getline(&buf, &size, fp) > 0)
        {
            CHOMP(buf);
            char *tab = strchr(buf, '\t');
            if (tab && tab[1]) /* there are triggers pending */
            {
                *tab = '\0';
                char *space = strchr(buf, ' ');
                if (space)
                {
                    struct package_name *newpkg = (struct package_name *)malloc(sizeof(struct package_name));
                    *space = '\0';
                    space++;
                    newpkg->name = strdup( (*space) ? space : buf);
                    newpkg->next = retval;
                    retval = newpkg;
                }
            }
        }
        
        free(buf);
    }
    
    return retval;
}

/************************************************************************
 * Function: is_confmodule
 * Inputs: filename - filename to check
 * Outputs: 1 if filename is a confmodule, 0 otherwise
 * Description: "checks" to see if filename is a confmodule
 * Assumptions: filename is a confmodule if it contains the string
 *              "confmodule". This is ugly, gross, broken, etc but we
 *              don't have much choice, and we want this to be compatible
 *              with perl-debconf
 ************************************************************************/
static int is_confmodule(const char *filename)
{
	FILE *fp;
	char buf[1024];
	int i;
	int found = 0;

	if ((fp = fopen(filename, "r")) != 0)
	{
		while (fgets(buf, sizeof(buf), fp))
		{
			for (i = 0; buf[i] != 0; i++)
				buf[i] = tolower(buf[i]);
			if (strstr(buf, "confmodule"))
			{
				found = 1;
				break;
			}
		}
		fclose(fp);
	}
	return found;
}

/************************************************************************
 * Function: runconfmodule
 * Inputs: argc, argv - arguments for the confmodule
 * Outputs: int - status of running the confmodule
 * Description: runs the given confmodule
 * Assumptions: none
 ************************************************************************/
static int runconfmodule(const char *pkg, int argc, char **argv)
{
	struct confmodule *confmodule = NULL;
	int ret;

	confmodule = confmodule_new(g_config, g_templates, g_questions, g_frontend);
	confmodule->owner = pkg;
	confmodule->run(confmodule, argc, argv);
	confmodule->communicate(confmodule);
	confmodule->shutdown(confmodule);
	ret = confmodule->exitcode;
	g_frontend = confmodule->frontend;
	confmodule_delete(confmodule);

	return ret;
}

static char *control_path(const char *pkg, const char *script) {
	int pipefd[2];
	int pid;

	if (pipe(pipefd) < 0)
	{
		perror("pipe");
		return NULL;
	}

	pid = fork();
	if (pid < 0)
	{
		perror("fork");
		close(pipefd[0]);
		close(pipefd[1]);
		return NULL;
	}
	else if (pid == 0) /* child */
	{
		close(pipefd[0]);
		dup2(pipefd[1], 1);
		close(pipefd[1]);
		execlp("dpkg-query", "dpkg-query", "--control-path",
		       pkg, script, NULL);
		/* if we reach here, then execlp failed */
		perror("execlp");
		_exit (127);
	}
	else /* parent */
	{
		FILE *query;
		char *path = NULL;
		size_t size = 0, read;

		close(pipefd[1]);
		query = fdopen(pipefd[0], "r");
		read = getline(&path, &size, query);
		fclose(query);

		if (read == -1)
			return NULL;

		if (path)
			CHOMP(path);

		return path;
	}
}

static int runscript (const char *pkg, const char *script, const char *param, const char *version) {
	int ret;
	char *filename;
	char *argv[5] = {0};

	setenv("DPKG_MAINTSCRIPT_NAME", script, 1);

	filename = control_path(pkg, script);
	if (! file_exists(filename,S_IXUSR|S_IXGRP|S_IXOTH))
	{
		ret = 0; /* it's ok if the script doesn't exist */
	}
	else if (strcmp(script, "config") == 0 || is_confmodule(filename))
	{
		argv[1] = filename;
		/* not actually modified, but this ultimately ends up in
		 * execv(), which wants argv elements to be char *, so
		 * silence the warning
		 */
		argv[2] = (char *) param;
		argv[3] = (char *)version;
		ret = runconfmodule(pkg, 4, argv);
	}
	else
	{
		int filenamesize = strlen(filename) + strlen(param) + strlen(version) + 3;
		filename = realloc(filename, filenamesize);
		/* according to debconf:
		 * Since a non confmodule might run other programs that
		 * use debconf, checkpoint the db state and
		 * reinitialize when the script finishes 
		 */
		g_templates->methods.save(g_templates);
		g_questions->methods.save(g_questions);
	
		unsetenv("DEBIAN_HAS_FRONTEND");

		strvacat(filename, filenamesize, " ", param, " ", version, NULL);
		ret = system(filename);
	
		setenv("DEBIAN_HAS_FRONTEND", "1", 1);
	
		g_templates->methods.load(g_templates);
		g_questions->methods.load(g_questions);
	}

	free(filename);
	return ret;
}

static int reconfigure(char *pkg)
{
	int ret;
	char *filename;
	char *version = NULL;

	g_frontend->methods.set_title(g_frontend, pkg);
	if (!opt_force && strstr(getfield(pkg, STATUSFIELD), " ok installed") == 0)
		DIE("%s is not fully installed", pkg);

	if (!no_reload)
	{
		filename = control_path(pkg, "templates");
		if (file_exists(filename, S_IRUSR|S_IRGRP|S_IROTH))
			template_db_loadfile(g_templates, g_questions, filename, pkg, DC_LOADTEMPLATE_NONE);
		free(filename);
	}

	setenv("DPKG_MAINTSCRIPT_PACKAGE", getfield(pkg, PACKAGEFIELD), 1);
	setenv("DPKG_MAINTSCRIPT_ARCH", getfield(pkg, ARCHITECTUREFIELD), 1);

	version = getfield(pkg, VERSIONFIELD);

	setenv("DPKG_MAINTSCRIPT_VERSION", version, 1);

	/* Simulation of reinstalling a package, without bothering with
	   removing the files and putting them back. Just like in a
	   regular reinstall, run config, and postinst scripts in
	   sequence, with args. Do not run postrm, because the postrm
	   can depend on the package's files actually being gone
	   already. */
	ret = runscript(pkg, "prerm", "upgrade", version);
	if (ret == 0)
		ret = runscript(pkg, "config", "reconfigure", version);
	if (ret == 0)
		ret = runscript(pkg, "postinst", "configure", version);

	return ret;
}

static void checkandruntriggers(struct package_name *pre)
{
    int count = 0;
    struct package_name *head = NULL;
    struct package_name *p = getalltriggers();
    while (p != NULL)
    {
        struct package_name *q, *next = p->next;
        for (q = pre; q != NULL; q = q->next)
        {
            if (!strcmp(p->name, q->name))
                break;
        }
        if (q == NULL)
        {
            p->next = head;
            head = p;
        }
        p = next;
        count++;
    }
    if (count)
    {
        int i = 2, pid;
        char **args = (char **)malloc((count + 3) * sizeof(char *));
        args[0] = "dpkg";
        args[1] = "--configure";
        p = head;
        for (p = head; p != NULL; p = p->next)
        {
            args[i++] = p->name;
        }
        args[i] = NULL;
        
        pid = fork();
        if (pid < 0)
        {
        }
        else if (pid == 0) /* child */
        {
            execvp("dpkg", args);
    		perror("execvp");
    		_exit (127);
        }
        else /* parent */
        {
            wait(NULL);
        }
    }
}

/************************************************************************
 * Function: main
 * Inputs: argc, argv - arguments passed in
 * Outputs: int - 0 if no error, >0 otherwise
 * Description: main entry point to dpkg-reconfigure
 * Assumptions: none
 ************************************************************************/
int main(int argc, char **argv)
{
	int opt, ret = 0;
	int unseen_only=0;
	int default_priority=0;
	char *priority_override=NULL;
	struct package_name *triggers_pre;

	signal(SIGINT, sighandler);
	setlocale (LC_ALL, "");

	if (getuid() != 0)
		DIE("%s must be run as root", argv[0]);

	g_config = config_new();

	while ((opt = getopt_long(argc, argv, "hf:p:duaFn", g_dpc_args, NULL)) > 0)
	{
		switch (opt)
		{
		case 'h': usage(); break;
		case 'f': g_config->set(g_config, "_cmdline::frontend", optarg); break;
		case 'p': priority_override=strdup(optarg); break;
		case 'd': default_priority=1; break;
		case 'u': unseen_only=1; break;
		case 'a': opt_all = 1; break;
		case 'F': opt_force = 1; break;
		case 'n': no_reload = 1; break;
		}
	}
	if (!opt_all && optind == argc) {
		fprintf(stderr, "please specify a package to reconfigure\n");
		exit(1);
	}
	
	/* Default is to force showing of old questions by default
	 * for reconfiguring, and show low priority questions. */
	if (! unseen_only)
		g_config->set(g_config, "_cmdline::showold", "true");
	if (priority_override)
		g_config->set(g_config, "_cmdline::priority", priority_override);
	else if (! default_priority && (getenv("DEBIAN_PRIORITY") == NULL))
		g_config->set(g_config, "_cmdline::priority", "low");

	/* parse the configuration info */
	if (g_config->read(g_config, DEBCONFCONFIG) == 0)
		DIE("Error reading configuration information");

	/* initialize database and frontend modules */
	if ((g_templates = template_db_new(g_config, NULL)) == 0)
		DIE("Cannot initialize DebConf templates database");
	if (g_templates->methods.load(g_templates) == 0)
		DIE("Cannot initialize DebConf templates database");
	if ((g_questions = question_db_new(g_config, g_templates, NULL)) == 0)
		DIE("Cannot initialize DebConf config database");
	if (g_questions->methods.load(g_questions) == 0)
		DIE("Cannot initialize DebConf config database");

	if ((g_frontend = frontend_new(g_config, g_templates, g_questions)) == 0)
		DIE("Cannot initialize DebConf frontend");

	triggers_pre = getalltriggers();
	
	setenv("DEBIAN_HAS_FRONTEND", "1", 1);
	setenv("DEBCONF_RECONFIGURE", "1", 1);
	if (opt_all)
	{
		char *packages = getallpackages();
		char *pkg = strtok(packages, " ");
		while (pkg != NULL && ret == 0)
		{
			ret = reconfigure(pkg);
			pkg = strtok(NULL, " ");
		}
	}
	else
	{
		int i;
		for (i = optind; i < argc && ret == 0; i++)
			ret = reconfigure(argv[i]);
	}

	checkandruntriggers(triggers_pre);
 
	/* shutting down .... sync the database and shutdown the modules */
	cleanup();
	return ret;
}
