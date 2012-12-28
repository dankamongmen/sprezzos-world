#include "common.h"
#include "commands.h"
#include "frontend.h"
#include "database.h"
#include "question.h"
#include "template.h"
#include "plugin.h"
#include "strutl.h"

#include <dlfcn.h>
#include <sys/wait.h>

#define CHECKARGC(pred) \
({\
    char *out; \
    if (!(argc pred)) {\
        if (asprintf(&out, "%u Incorrect number of arguments", \
                CMDSTATUS_SYNTAXERROR) == -1) \
            return strdup("1"); \
        else \
            return out; \
    } \
})

char *
command_input(struct confmodule *mod, char *arg)
{
    int visible = 1;
    struct question *q = 0;
    char *qtag;
    char *priority;
    char *argv[3];
    char *out;
    int argc;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 2);

    priority = argv[0];
    qtag = argv[1];

    q = mod->questions->methods.get(mod->questions, qtag);
    if (q == NULL) 
    {
        if (asprintf(&out, "%u \"%s\" doesn't exist",
                CMDSTATUS_BADQUESTION, qtag) == -1)
            return strdup("1");
        else
            return out;
    }

    /* check priority */
    visible = (mod->frontend->interactive && 
            mod->questions->methods.is_visible(mod->questions, qtag, priority));

    if (visible)
        visible = mod->frontend->methods.add(mod->frontend, q);
    else
        mod->frontend->methods.add_noninteractive(mod->frontend, q);

    free(q->priority);
    q->priority = strdup(priority);

    if (visible) 
    {
        mod->backed_up = 0;
        asprintf(&out, "%u question will be asked", CMDSTATUS_SUCCESS);
    }
    else
        asprintf(&out, "%u question skipped", CMDSTATUS_INPUTINVISIBLE);
    question_deref(q);
    return out;
}

char *
command_clear(struct confmodule *mod, char *arg)
{
    char *argv[3];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 0);

    mod->frontend->methods.clear(mod->frontend);
    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_version(struct confmodule *mod, char *arg)
{
    int ver;
    char *argv[3];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 1);

    ver = atoi(argv[0]);

    if (ver < DEBCONF_VERSION)
        asprintf(&out, "%u Version too low (%d)", CMDSTATUS_BADVERSION, ver);
    else if (ver > DEBCONF_VERSION)
        asprintf(&out, "%u Version too high (%d)", CMDSTATUS_BADVERSION, ver);
    else
        asprintf(&out, "%u %.1f", CMDSTATUS_SUCCESS, DEBCONF_VERSION);
    return out;
}

char *
command_capb(struct confmodule *mod, char *arg)
{
    int i;
    char *argv[32];
    int argc;
    char *out, *outend;
    size_t outalloc;
    struct plugin *plugin;
    void *plugin_state;

    argc = strcmdsplit(arg, argv, DIM(argv));
    /* FIXME: frontend.h should provide a method to prevent direct
     *        access to capability member */
    mod->frontend->capability = 0;
    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "backup") == 0)
            mod->frontend->capability |= DCF_CAPB_BACKUP;
        else if (strcmp(argv[i], "progresscancel") == 0)
            mod->frontend->capability |= DCF_CAPB_PROGRESSCANCEL;
        else if (strcmp(argv[i], "align") == 0)
            mod->frontend->capability |= DCF_CAPB_ALIGN;
        else if (strcmp(argv[i], "escape") == 0)
            mod->frontend->capability |= DCF_CAPB_ESCAPE;
    }

    if (asprintf(&out, "%u multiselect backup progresscancel align escape",
                 CMDSTATUS_SUCCESS) == -1)
        DIE("Out of memory");

    plugin_state = NULL;
    outend = strchr(out, '\0');
    outalloc = outend - out + 1;
    while ((plugin = plugin_iterate(mod->frontend, &plugin_state)) != NULL) {
        size_t namelen;
        char *newout;

        namelen = strlen(plugin->name);
        outalloc += 8 + namelen;
        newout = realloc(out, outalloc);
        if (!newout)
            DIE("Out of memory");
        outend = newout + (outend - out);
        out = newout;
        outend = mempcpy(outend, " plugin-", 8);
        outend = mempcpy(outend, plugin->name, namelen);
        *outend = '\0';
    }

    return out;
}

char *
command_title(struct confmodule *mod, char *arg)
{
    char *out;

    mod->frontend->methods.set_title(mod->frontend, arg);
    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_beginblock(struct confmodule *mod, char *arg)
{
    char *out;

    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_endblock(struct confmodule *mod, char *arg)
{
    char *out;

    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_go(struct confmodule *mod, char *arg)
{
    char *argv[3];
    int argc;
    char *out;
    const char *running_frontend = NULL;
    const char *requested_frontend = NULL;
    int ret;

    argc = strcmdsplit(arg, argv, DIM(argv) - 1);
    CHECKARGC(== 0);

    requested_frontend = getenv("DEBIAN_FRONTEND");

    if (requested_frontend)
    {
        if (mod != NULL && mod->frontend != NULL)
            running_frontend = mod->frontend->name;

        if (requested_frontend && running_frontend &&
            strcmp(running_frontend, requested_frontend) != 0)
        {
            struct frontend *new_frontend;

            /* Shut down the current frontend, but do not delete unless opening
             the new one actually succeeds */
            mod->frontend->methods.shutdown(mod->frontend);
            
            new_frontend = frontend_new(mod->config, mod->templates, mod->questions);
            if (new_frontend)
            {
                struct frontend *old_frontend = mod->frontend;
                mod->frontend = new_frontend;
                new_frontend->questions = old_frontend->questions;
                new_frontend->questions_noninteractive = old_frontend->questions_noninteractive;
                frontend_delete(old_frontend);
            }
            else
            {
                /* If the new frontend failed, reinitialise the old one */
                mod->frontend->methods.initialize(mod->frontend, mod->config);
            }
        }
    }

    mod->frontend->methods.go_noninteractive(mod->frontend);
    ret = mod->frontend->methods.go(mod->frontend);
    if (ret == CMDSTATUS_GOBACK || mod->backed_up != 0)
    {
        mod->backed_up = 1;
        asprintf(&out, "%u backup", CMDSTATUS_GOBACK);
        mod->update_seen_questions(mod, STACK_SEEN_REMOVE);
    }
    else if (ret == DC_NOTOK) /* TODO return value namespace */
    {
        mod->backed_up = 0;
        asprintf(&out, "%u internal error", CMDSTATUS_INTERNALERROR);
        mod->update_seen_questions(mod, STACK_SEEN_REMOVE);
    }
    else
    {
        mod->backed_up = 0;
        asprintf(&out, "%u ok", CMDSTATUS_SUCCESS);
        mod->update_seen_questions(mod, STACK_SEEN_ADD);
    }
    mod->frontend->methods.clear(mod->frontend);

    return out;
}

static char *escapestr_frontend(const char *in)
{
	static size_t buflen = 0;
	static char *buf = NULL;
	size_t inlen;
	const char *p;

	if (in == 0) return 0;

	inlen = strlen(in) + 1;

	/* Each newline will consume an additional byte due to escaping. */
	for (p = in; *p; ++p)
		if (*p == '\\' || *p == '\n')
			++inlen;

	if (buflen < inlen) {
		buflen = inlen;
		buf = realloc(buf, buflen * sizeof *buf);
		if (!buf)
			DIE("Out of memory");
	}

	strescape(in, buf, buflen, STRESCAPE_CAPB);
	return buf;
}

char *
command_get(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *argv[3];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 1);

    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
        asprintf(&out, "%u %s doesn't exist",
                CMDSTATUS_BADQUESTION, argv[0]);
    else 
    {
        const char *value = question_getvalue(q, "C");
        if (mod->frontend->capability & DCF_CAPB_ESCAPE)
        {
            char *esc = escapestr_frontend(value);
            asprintf(&out, "%u %s", CMDSTATUS_ESCAPEDDATA, esc ? esc : "");
        }
        else
            asprintf(&out, "%u %s",
                    CMDSTATUS_SUCCESS, value ? value : "");
    }
    question_deref(q);

    return out;
}

char *
command_set(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *argv[2] = { "", "" };
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(>= 1);

    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
        asprintf(&out, "%u %s doesn't exist",
                CMDSTATUS_BADQUESTION, argv[0]);
    else
    {
        char *prev = STRDUP(question_getvalue(q, ""));
        question_setvalue(q, argv[1]);

        if (frontend_qdb_set(mod->questions, q, prev))
            asprintf(&out, "%u value set", CMDSTATUS_SUCCESS);
        else
            asprintf(&out, "%u cannot set value", CMDSTATUS_INTERNALERROR);

        free(prev);
    }
    question_deref(q);

    return out;
}

char *
command_reset(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *argv[2];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 1);

    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
    else
    {
        DELETE(q->value);
        q->flags &= ~DC_QFLAG_SEEN;

        if (mod->questions->methods.set(mod->questions, q) != 0)
            asprintf(&out, "%u", CMDSTATUS_SUCCESS);
        else
            asprintf(&out, "%u cannot reset value", CMDSTATUS_INTERNALERROR);
    }
    question_deref(q);

    return out;
}

char *
command_subst(struct confmodule *mod, char *arg)
{
    struct question *q;
    struct questionvariable;
    char *variable;
    char *argv[3] = { "", "", "" };
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(>= 2);

    variable = argv[1];
    q = mod->questions->methods.get(mod->questions, argv[0]);

    if (q == NULL)
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
    else
    {
        question_variable_add(q, variable, argv[2]);

        if (mod->questions->methods.set(mod->questions, q) != 0)
            asprintf(&out, "%u", CMDSTATUS_SUCCESS);
        else
            asprintf(&out, "%u substitution failed", CMDSTATUS_INTERNALERROR);
    }
    question_deref(q);

    return out;
}

char *
command_register(struct confmodule *mod, char *arg)
{
    struct question *q;
    struct template *t;
    char *argv[4];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 2);

    t = mod->templates->methods.get(mod->templates, argv[0]);
    if (t == NULL) {
        asprintf(&out, "%u No such template, \"%s\"", CMDSTATUS_BADQUESTION, argv[0]);
        return out;
    }
    q = mod->questions->methods.get(mod->questions, argv[1]);
    if (q == NULL)
        q = question_new(argv[1]);
    if (q == NULL) {
        asprintf(&out, "%u Internal error making question", CMDSTATUS_INTERNALERROR);
        return out;
    }
    question_owner_add(q, mod->owner);
    q->template = t;
        /* steal reference from mod->templates->methods.get above */
    mod->questions->methods.set(mod->questions, q);
    question_deref(q);
    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_unregister(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *argv[3];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 1);

    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL) {
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
        return out;
    }
    question_owner_delete(q, mod->owner);
    question_deref(q);
    asprintf(&out, "%u", CMDSTATUS_SUCCESS);

    return out;
}

char *
command_purge(struct confmodule *mod, char *arg)
{
    char *out;

    mod->questions->methods.disownall(mod->questions, mod->owner);
    asprintf(&out, "%u", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_metaget(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *value;
    char *argv[4];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 2);
    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
    {
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
        return out;
    }

    value = question_get_raw_field(q, "", argv[1]);
    if (value == NULL)
        asprintf(&out, "%u %s does not exist", CMDSTATUS_BADQUESTION, argv[1]);
    else
    {
        if (mod->frontend->capability & DCF_CAPB_ESCAPE)
        {
            char *esc = escapestr_frontend(value);
            asprintf(&out, "%u %s", CMDSTATUS_ESCAPEDDATA, esc);
        }
        else
            asprintf(&out, "%u %s", CMDSTATUS_SUCCESS, value);
    }
    free(value);

    question_deref(q);

    return out;
}

char *
command_fget(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *field;
    char *argv[4];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 2);
    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
    {
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
        return out;
    }

    field = argv[1];
    /* isdefault is for backward compability only */
    if (strcmp(field, "seen") == 0)
        asprintf(&out, "%u %s", CMDSTATUS_SUCCESS,
                ((q->flags & DC_QFLAG_SEEN) ? "true" : "false"));
    else if (strcmp(field, "isdefault") == 0)
        asprintf(&out, "%u %s", CMDSTATUS_SUCCESS,
                ((q->flags & DC_QFLAG_SEEN) ? "false" : "true"));
    else
        asprintf(&out, "%u %s", CMDSTATUS_SUCCESS, "false");
    question_deref(q);

    return out;
}

char *
command_fset(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *field;
    char *argv[5];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 3);
    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q == NULL)
    {
        asprintf(&out, "%u %s doesn't exist", CMDSTATUS_BADQUESTION, argv[0]);
        return out;
    }

    field = argv[1];
    if (strcmp(field, "seen") == 0)
    {
        q->flags &= ~DC_QFLAG_SEEN;
        if (strcmp(argv[2], "true") == 0)
            q->flags |= DC_QFLAG_SEEN;
    }
    else if (strcmp(field, "isdefault") == 0)
    {
        q->flags &= ~DC_QFLAG_SEEN;
        if (strcmp(argv[2], "false") == 0)
            q->flags |= DC_QFLAG_SEEN;
    }
    asprintf(&out, "%u %s", CMDSTATUS_SUCCESS, argv[2]);

    question_deref(q);
    return out;
}

char *
command_exist(struct confmodule *mod, char *arg)
{
    struct question *q;
    char *argv[3];
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 1);

    q = mod->questions->methods.get(mod->questions, argv[0]);
    if (q)
    {
        question_deref(q);
        asprintf(&out, "%u true", CMDSTATUS_SUCCESS);
    }
    else
    {
        asprintf(&out, "%u false", CMDSTATUS_SUCCESS);
    }
    return out;
}

char *
command_stop(struct confmodule *mod, char *arg)
{
    char *argv[3];
    int argc;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 0);
	
    if (mod->pid != 0)
    {
        waitpid(mod->pid, NULL, 0);
    }

    return strdup("");
}

char *
command_x_loadtemplatefile(struct confmodule *mod, char *arg)
{
    struct template *t = NULL;
    struct question *q = NULL;
    char *argv[3] = { "", "", "" };
    int argc;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(>= 1);
    CHECKARGC(<= 2);
    t = template_load(argv[0]);
    while (t)
    {
        mod->templates->methods.set(mod->templates, t);
        q = mod->questions->methods.get(mod->questions, t->tag);
        if (q == NULL) {
            q = question_new(t->tag);
            q->template = t;
            template_ref(t);
        }
        else if (q->template != t) {
            template_deref(q->template);
            q->template = t;
            template_ref(t);
        }
        if (*argv[1])
            question_owner_add(q, argv[1]);
        mod->questions->methods.set(mod->questions, q);
        question_deref(q);
        t = t->next;
    }
    asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    return out;
}

char *
command_progress(struct confmodule *mod, char *arg)
{
    int min, max;
    struct question *q = NULL;
    char *argv[6];
    int argc;
    char *out;
    int ret;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(>= 1);

    if (strcasecmp(argv[0], "start") == 0)
    {
        CHECKARGC(== 4);

        min = atoi(argv[1]);
        max = atoi(argv[2]);

        if (min > max)
        {
            asprintf(&out, "%u min (%d) > max (%d)", 
                    CMDSTATUS_SYNTAXERROR, min, max);
            return out;
        }

        q = mod->questions->methods.get(mod->questions, argv[3]);
        if (q == NULL)
        {
            asprintf(&out, "%u %s does not exist",
                    CMDSTATUS_BADQUESTION, argv[3]);
            return out;
        }
        mod->frontend->methods.progress_start(mod->frontend,
                min, max, q);
        question_deref(q);
    }
    else if (strcasecmp(argv[0], "set") == 0)
    {
        CHECKARGC(== 2);
        ret = mod->frontend->methods.progress_set(mod->frontend, atoi(argv[1]));
        if (ret == DC_GOBACK)
        {
            asprintf(&out, "%u progress bar cancelled",
                    CMDSTATUS_PROGRESSCANCELLED);
            return out;
        }
    }
    else if (strcasecmp(argv[0], "step") == 0)
    {
        CHECKARGC(== 2);
        ret = mod->frontend->methods.progress_step(mod->frontend, atoi(argv[1]));
        if (ret == DC_GOBACK)
        {
            asprintf(&out, "%u progress bar cancelled",
                    CMDSTATUS_PROGRESSCANCELLED);
            return out;
        }
    }
    else if (strcasecmp(argv[0], "info") == 0)
    {
        CHECKARGC(== 2);
        q = mod->questions->methods.get(mod->questions, argv[1]);
        if (q == NULL)
        {
            asprintf(&out, "%u %s does not exist",
                    CMDSTATUS_BADQUESTION, argv[1]);
            return out;
        }
        ret = mod->frontend->methods.progress_info(mod->frontend, q);
        question_deref(q);
        if (ret == DC_GOBACK)
        {
            asprintf(&out, "%u progress bar cancelled",
                    CMDSTATUS_PROGRESSCANCELLED);
            return out;
        }
    }
    else if (strcasecmp(argv[0], "stop") == 0)
    {
        mod->frontend->methods.progress_stop(mod->frontend);
    }
    else
    {
        asprintf(&out, "%u unknown subcommand %s",
                CMDSTATUS_SYNTAXERROR, argv[0]);
        return out;
    }

    asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    return out;
}

/*
 * takes a template name and sets the debconf title to the description
 * of the template. This will allow us to localise titles.
 */
char *
command_settitle(struct confmodule *mod, char *arg)
{
    struct question *q = NULL;
    char *value;
    char *out;

    q = mod->questions->methods.get(mod->questions, arg);
    if (q == NULL)
    {
        asprintf(&out, "%u %s does not exist", CMDSTATUS_BADQUESTION, arg);
	return out;
    }
    value = question_get_raw_field(q, "", "description");
    question_deref(q);
    if (value == NULL)
    {
	asprintf(&out, "%u %s description field does not exist",
		 CMDSTATUS_BADQUESTION, arg);
	return out;
    }

    mod->frontend->methods.set_title(mod->frontend, value);
    free(value);

    asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    return out;
}

char *command_x_save(struct confmodule *mod, char *arg)
{
    char *argv[2];
    int argc, ret = DC_OK;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 0);
    if (mod)
        ret = mod->save(mod);
    if (ret == DC_OK)
        asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    else
        asprintf(&out, "%u not OK", CMDSTATUS_INTERNALERROR);
    return out;
}

/*
 * Displays the given template as an out-of-band informative message. Unlike
 * inputting a note, this doesn't require an acknowledgement from the user,
 * and depending on the frontend it may not even be displayed at all. Like
 * PROGRESS INFO, frontends should display the info persistently until some
 * other info comes along.
 */
char *
command_info(struct confmodule *mod, char *arg)
{
    struct question *q = NULL;
    char *out;

    q = mod->questions->methods.get(mod->questions, arg);
    if (q == NULL)
    {
        asprintf(&out, "%u %s does not exist", CMDSTATUS_BADQUESTION, arg);
        return out;
    }
    mod->frontend->methods.info(mod->frontend, q);
    question_deref(q);

    asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    return out;
}

/*
 * Accept template data from the client
 * (for use with the passthrough frontend)
 */
char *
command_data(struct confmodule *mod, char *arg)
{
    char *argv[3];
    const char *tag = NULL, *item = NULL, *value = NULL;
    int argc;
    struct template *t = NULL;
    struct question *q = NULL;
    char *out;

    argc = strcmdsplit(arg, argv, DIM(argv));
    CHECKARGC(== 3);

    tag = argv[0];
    item = argv[1];

    value = unescapestr(argv[2]);

    t = mod->templates->methods.get(mod->templates, tag);
    if (t == NULL)
    {
        t = template_new(tag);
        mod->templates->methods.set(mod->templates, t);
        q = mod->questions->methods.get(mod->questions, t->tag);
        if (q == NULL) {
            q = question_new(t->tag);
            q->template = t;
            template_ref(t);
        }
        else if (q->template != t) {
            template_deref(q->template);
            q->template = t;
            template_ref(t);
        }
        template_lset(t, NULL, item, value);
        mod->questions->methods.set(mod->questions, q);
        question_deref(q);
    }
    else
        template_lset(t, NULL, item, value);
    template_deref(t);

    asprintf(&out, "%u OK", CMDSTATUS_SUCCESS);
    return out;
}

/* vim: expandtab sw=4
*/
