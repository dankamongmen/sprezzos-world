#include "confmodule.h"
#include "commands.h"
#include "frontend.h"
#include "database.h"
#include "question.h"
#include "template.h"
#include "strutl.h"
#include "debconfclient.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>

#include <debian-installer.h>

static commands_t commands[] = {
#include "commands-list.h"
    { 0, 0 }
};

volatile sig_atomic_t signal_received = 0;
volatile sig_atomic_t sigchld_status = 0;

/* private functions */
/*
 * @brief helper function to process incoming commands
 * @param struct confmodule *mod - confmodule object
 * @param char *in - input command
 * @param char *out - reply buffer
 * @param size_t outsize - reply buffer length
 * @return int - DC_OK, DC_NOTOK, DC_NOTIMPL
 */
static char *_confmodule_process(struct confmodule *mod, char *in)
{
    int i;
    char *retval = NULL;
    char *argv[2] = { "", "" };

    char *inp = strstrip(in);
	
    INFO(INFO_DEBUG, "--> %s", inp);

    if (*inp == '#') return NULL;

    if (mod->frontend->capability & DCF_CAPB_ESCAPE)
        strunescape(inp, inp, strlen(inp)+1, STRESCAPE_CAPB);

    strcmdsplit(inp, argv, DIM(argv));

    for (i = 0; commands[i].command != 0; i++)
    {
        if (strcasecmp(argv[0], commands[i].command) == 0) {
            retval = (*commands[i].handler)(mod, argv[1]);
            break;
        }
    }

    if (retval == NULL)
        fprintf(stderr, "E: Unimplemented function\n");
		
    return retval;
}

/*
 * @brief mark a file descriptor close-on-exec
 * @param int fd - file descriptor
 * @return int - 0 on success, -1 with errno set on error
 */
static int _confmodule_cloexec(int fd)
{
    int flags = fcntl (fd, F_GETFD);
    if (flags < 0)
        return flags;
    if (fcntl (fd, F_SETFD, flags | FD_CLOEXEC) < 0)
        return -1;
    return 0;
}

/* public functions */
static int confmodule_communicate(struct confmodule *mod)
{
    char buf[1023];
    char *in;
    size_t insize = 1024;
    char *out;
    //size_t outsize = 4096;
    int ret = 0;

    in = malloc(insize);
    if (!in)
        return DC_NOTOK;
    memset(in, 0, insize);

    //out = malloc(outsize);
    //if (!out)
    //return DC_NOTOK;
    //memset(out, 0, outsize);

    while (1) {
        buf[0] = 0;
        in[0] = 0;
        while (strchr(buf, '\n') == NULL) {
            if (signal_received) {
                free(in);
                return DC_OK;
            }

            ret = read(mod->infd, buf, sizeof(buf) - 1);
            if (ret < 0) {
                if (errno == EINTR)
                    continue;
                free(in);
                return DC_NOTOK;
            }
            if (ret == 0) {
                free(in);
                return DC_OK;
            }
            buf[ret] = 0;
            if (strlen(in) + ret + 1 > insize) {
                insize += sizeof(buf);
                in = realloc(in, insize);
            }
            strcat(in, buf);
        }

        if (signal_received) {
            free(in);
            return DC_OK;
        }

        out = _confmodule_process(mod, in);
        if (out == NULL) {
            continue;
        }
        if (out[0] == 0) /* STOP called */
        {
            ret = DC_OK;
            break;
        }
        INFO(INFO_DEBUG, "<-- %s", out);
        write(mod->outfd, out, strlen(out));
        write(mod->outfd, "\n", 1);
        free(out);
    }
    free(in);
    return ret;
}

static char *confmodule_process_command(struct confmodule *mod, char *cmd)
{
    char *out;

    out = _confmodule_process(mod, cmd);
    if (out == NULL) {
        asprintf(&out, "%u Not implemented", DC_NOTOK);
    }
    INFO(INFO_DEBUG, "<-- %s", out);

    return out;
}

static int confmodule_shutdown(struct confmodule *mod)
{
    int status;

    while (waitpid(mod->pid, &status, WNOHANG) > 0)
        sigchld_status = status;

    mod->exitcode = di_exec_mangle_status(sigchld_status);

    return mod->exitcode;
}

static inline void check_fd(int fd, int newfd, bool old[])
{
    if (fd <= 2)
        old[fd] = false;
    dup2(fd, newfd);
    close(fd);
}

static pid_t confmodule_run(struct confmodule *mod, int argc, char **argv)
{
    pid_t pid;
    int i;
    char **args;
    bool old[3] = { true, true, true };
    int config[5]; /* 0=read/to, 1=write/to, 2=read/from, 3=write/from, 4=null */
    pipe(&config[0]);
    pipe(&config[2]);
    switch ((pid = fork()))
    {
        case -1:
            mod->frontend->methods.shutdown(mod->frontend);
            DIE("Cannot execute client config script");
            break;
        case 0:
            /* 50=read/to, 51=write/to, 52=read/from, 53=write/from, 54=null */
            config[4] = open("/dev/null", O_RDWR);
            for (i = 0; i < 5; i++)
                check_fd(config[i], 50 + i, old);
            for (i = 0; i <= 2; i++)
                dup2(old[i] ? i : 54, DEBCONF_OLD_FD_BASE + i);
            dup2(50, 0); dup2(53, 1); dup2(53, 3);
            for (i = 0; i < 5; i++)
                close(50 + i);

            args = (char **)malloc(sizeof(char *) * argc);
            for (i = 1; i < argc; i++)
                args[i-1] = argv[i];
            args[argc-1] = NULL;
            if (execv(argv[1], args) != 0)
                perror("execv");
            /* should never reach here, otherwise execv failed :( */
            exit (127);
        default:
            close(config[0]); close(config[3]);
            mod->infd = config[2];
            mod->outfd = config[1];
            _confmodule_cloexec(mod->infd);
            _confmodule_cloexec(mod->outfd);
    }
    mod->pid = pid;

    return pid;
}

static int confmodule_update_seen_questions(struct confmodule *mod, enum seen_action action)
{
    struct question *q;
    struct question *qlast = NULL;
    int i, narg;

    switch (action)
    {
        case STACK_SEEN_ADD:
            if (mod->seen_questions == NULL)
                narg = 0;
            else
                narg = mod->number_seen_questions;

            i = narg;
            for (q = mod->frontend->questions; q != NULL; q = q->next)
                narg++;
            if (narg == 0)
                return DC_OK;

            mod->seen_questions = (char **) realloc(mod->seen_questions, sizeof(char *) * narg);
            for (q = mod->frontend->questions; q != NULL; q = q->next)
            {
                *(mod->seen_questions+i) = strdup(q->tag);
                i++;
            }
            mod->number_seen_questions = i;
            break;
        case STACK_SEEN_REMOVE:
            if (mod->seen_questions == NULL)
                return DC_OK;

            for (q = mod->frontend->questions; q != NULL; q = q->next)
                qlast = q;

            for (q = qlast; q != NULL; q = q->prev)
            {
                if (strcmp(*(mod->seen_questions + mod->number_seen_questions - 1), q->tag) != 0)
                    return DC_OK;
                DELETE(*(mod->seen_questions + mod->number_seen_questions - 1));
                (mod->number_seen_questions) --;
                if (mod->number_seen_questions == 0)
                {
                    DELETE(mod->seen_questions);
                    break;
                }
            }
            break;
        case STACK_SEEN_SAVE:
            if (mod->seen_questions == NULL)
                return DC_OK;

            narg = mod->number_seen_questions;
            for (i = 0; i < narg; i++)
            {
                q = mod->questions->methods.get(mod->questions, *(mod->seen_questions+i));
                if (q == NULL)
                    return DC_NOTOK;
#ifndef DI_UDEB
                q->flags |= DC_QFLAG_SEEN;
#endif
                DELETE(*(mod->seen_questions+i));
            }
            DELETE(mod->seen_questions);
            mod->number_seen_questions = 0;
            break;
        default:
            /* should never happen */
            DIE("Mismatch argument in confmodule_update_seen_questions");
    }

    return DC_OK;
}

static int confmodule_save(struct confmodule *mod)
{
    int ret = DC_OK;

    if (!load_all_translations())
    {
        /* This isn't entirely accurate; really it should be done in
         * rfc822db's implementation of mod->templates->methods.save().
         * However, that doesn't have convenient access to the questions
         * database, so we do it here instead for the time being.
         */
        struct question *q = mod->questions->methods.get(
            mod->questions, "debconf/translations-dropped");
        if (q != NULL)
        {
            if (strcmp(question_getvalue(q, ""), "true") != 0)
            {
                question_setvalue(q, "true");
                mod->questions->methods.set(mod->questions, q);
            }
            question_deref(q);
        }
    }

    ret |= mod->update_seen_questions(mod, STACK_SEEN_SAVE);
    if (mod->questions)
        ret |= mod->questions->methods.save(mod->questions);
    if (mod->templates)
        ret |= mod->templates->methods.save(mod->templates);
    return ret != DC_OK ? DC_NOTOK : DC_OK;
}

struct confmodule *confmodule_new(struct configuration *config,
        struct template_db *templates, struct question_db *questions, 
        struct frontend *frontend)
{
    struct confmodule *mod = NEW(struct confmodule);
    memset(mod, 0, sizeof(struct confmodule));

    mod->exitcode = 126;
    mod->config = config;
    mod->templates = templates;
    mod->questions = questions;
    mod->frontend = frontend;
    mod->seen_questions = NULL;
    mod->backed_up = 0;
    mod->run = confmodule_run;
    mod->communicate = confmodule_communicate;
    mod->process_command = confmodule_process_command;
    mod->shutdown = confmodule_shutdown;
    mod->update_seen_questions = confmodule_update_seen_questions;
    mod->save = confmodule_save;

    /* TODO: I wish we don't need gross hacks like this.... */
    setenv("DEBIAN_HAS_FRONTEND", "1", 1);

    return mod;
}

void confmodule_delete(struct confmodule *mod)
{
    DELETE(mod);
}

/* vim: expandtab sw=4
*/
