/***********************************************************************
 *
 * cdebconf - An implementation of the Debian Configuration Management
 *            System
 *
 * File: passthrough.c
 *
 * Description: Passthrough UI for cdebconf
 *
 * cdebconf is (c) 2000-2009 Randolph Chung and others under the following
 * license.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 ***********************************************************************/
#include "common.h"
#include "commands.h"
#include "frontend.h"
#include "question.h"
#include "strutl.h"
#include "template.h"

#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "debian-installer.h"

struct passthrough_data {
    FILE *readfh, *writefh;
};

static int talk(struct frontend *obj, char **value, ...) ATTRIBUTE_SENTINEL;

static int
talk(struct frontend *obj, char **value, ...)
{
    struct passthrough_data *data = obj->data;
    va_list ap;
    char *command;
    char buf[1023];
    char *in;
    size_t inlen = 0, insize = 1024;
    char *inp, *space;
    int ret;

    if (value)
        *value = NULL;

    va_start(ap, value);
    command = strjoinv(" ", ap);
    va_end(ap);

    INFO(INFO_DEBUG, "----> %s", command);
    fprintf(data->writefh, "%s\n", command);
    fflush(data->writefh);
    free(command);

    /* This is disgusting; it's more or less cloned from
     * confmodule_communicate.
     */
    in = di_malloc0(insize);
    buf[0] = 0;
    in[0] = 0;
    while (strchr(buf, '\n') == NULL) {
        if (fgets(buf, sizeof(buf), data->readfh) == NULL) {
            if (feof(data->readfh))
                di_warning("unexpected EOF on data->readfh");
            else
                di_warning("read from data->readfh failed: %s", strerror(errno));
            free(in);
            return CMDSTATUS_INTERNALERROR;
        }
        if (inlen + strlen(buf) + 1 > insize) {
            insize += sizeof(buf);
            in = di_realloc(in, insize);
        }
        strcat(in, buf);
    }
    inp = strstrip(in);
    INFO(INFO_DEBUG, "<---- %s", inp);
    space = strchr(inp, ' ');
    if (space)
        *space = '\0';
    ret = atoi(inp);
    if (value) {
        if (space)
            *value = STRDUP(space + 1);
        else
            *value = STRDUP("");
    }
    free(in);
    return ret;
}

static int
passthrough_initialize(struct frontend *obj, struct configuration *cfg)
{
    struct passthrough_data *data;
    const char *env_pipe = getenv("DEBCONF_PIPE");
    const char *env_readfd = getenv("DEBCONF_READFD");
    const char *env_writefd = getenv("DEBCONF_WRITEFD");

    if (!env_pipe && (!env_readfd || !env_writefd)) {
        di_warning("neither DEBCONF_PIPE nor DEBCONF_READFD and DEBCONF_WRITEFD were set");
        return DC_NOTOK;
    }

    obj->interactive = 1;
    obj->data = data = calloc(1, sizeof(struct passthrough_data));

    if (env_pipe) {
        int sock = socket(AF_UNIX, SOCK_STREAM, 0);
        struct sockaddr_un addr;
        if (sock < 0) {
            di_warning("failed to create Unix-domain socket: %s", strerror(errno));
            return DC_NOTOK;
        }
        memset(&addr, 0, sizeof(addr));
        addr.sun_family = AF_UNIX;
        strncpy(addr.sun_path, env_pipe, sizeof(addr.sun_path) - 1);
        if (connect(sock, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
            di_warning("failed to connect Unix-domain socket to %s: %s", env_pipe, strerror(errno));
            return DC_NOTOK;
        }
        data->readfh = data->writefh = fdopen(sock, "r+");
        if (!data->readfh) {
            di_warning("failed to open stream for Unix-domain socket connected to %s: %s", env_pipe, strerror(errno));
            return DC_NOTOK;
        }
    } else {
        data->readfh = fdopen(atoi(env_readfd), "r");
        if (!data->readfh) {
            di_warning("failed to open fd %s: %s", env_readfd, strerror(errno));
            return DC_NOTOK;
        }
        data->writefh = fdopen(atoi(env_writefd), "w");
        if (!data->writefh) {
            di_warning("failed to open fd %s: %s", env_writefd, strerror(errno));
            return DC_NOTOK;
        }
    }

    return DC_OK;
}

static int
passthrough_shutdown(struct frontend *obj)
{
    struct passthrough_data *data = obj->data;
    if (data && data->readfh)
        fclose(data->readfh);
    if (data && data->writefh && data->writefh != data->readfh)
        fclose(data->writefh);
    free(data);
    return DC_OK;
}

static unsigned long
passthrough_query_capability(struct frontend *obj)
{
    char *capb_all, *capb_walk, *capb;
    unsigned long ret = 0;

    if (talk(obj, &capb_all, "CAPB", NULL) != 0)
        return 0;
    capb_walk = capb_all;
    for (capb = strsep(&capb_walk, " "); capb;
         capb = strsep(&capb_walk, " ")) {
        if (strcmp(capb, "backup") == 0)
            ret |= DCF_CAPB_BACKUP;
        else if (strcmp(capb, "progresscancel") == 0)
            ret |= DCF_CAPB_PROGRESSCANCEL;
        else if (strcmp(capb, "align") == 0)
            ret |= DCF_CAPB_ALIGN;
    }
    free(capb_all);
    return ret;
}

static void
passthrough_set_title(struct frontend *obj, const char *title)
{
    DELETE(obj->title);
    obj->title = STRDUP(title);
    /* TODO: We really ought to use SETTITLE instead, but for apparently
     * historical reasons the frontend set_title method only gets the
     * description.
     */
    talk(obj, NULL, "TITLE", title, NULL);
}

static int
passthrough_go(struct frontend *obj)
{
    struct question *q;

    /* TODO:
     *   - would be nice to cache the previous value to avoid unnecessarily
     *     resetting this
     *   - support more capabilities than just backup
     *   - this should be done in a proper frontend "capb" method
     */
    if (obj->capability & DCF_CAPB_BACKUP)
        talk(obj, NULL, "CAPB", "backup", NULL);
    else
        talk(obj, NULL, "CAPB", NULL);

    for (q = obj->questions; q; q = q->next) {
        char *desc, *extdesc;
        const char *value;
        struct questionvariable *qv;

        talk(obj, NULL, "DATA", q->tag, "type", q->template->type, NULL);

        desc = q_get_description(obj, q);
        if (desc) {
            char *desc_esc = strreplace(desc, "\\n", "\\\\n");
            talk(obj, NULL, "DATA", q->tag, "description", desc_esc, NULL);
            free(desc_esc);
            free(desc);
        }

        extdesc = q_get_extended_description(obj, q);
        if (extdesc) {
            char *extdesc_esc = strreplace(extdesc, "\\n", "\\\\n");
            talk(obj, NULL, "DATA", q->tag, "extended_description",
                 extdesc_esc, NULL);
            free(extdesc_esc);
            free(extdesc);
        }

        if (strcmp(q->template->type, "select") == 0 ||
            strcmp(q->template->type, "multiselect") == 0) {
            char *choices_c, *choices_c_esc, *choices, *choices_esc;

            choices_c = question_get_field(obj, q, "C", "choices");
            choices_c_esc = strreplace(choices_c, "\\n", "\\\\n");
            talk(obj, NULL, "DATA", q->tag, "choices-c", choices_c_esc, NULL);
            free(choices_c_esc);
            free(choices_c);

            choices = q_get_choices(obj, q);
            choices_esc = strreplace(choices, "\\n", "\\\\n");
            talk(obj, NULL, "DATA", q->tag, "choices", choices_esc, NULL);
            free(choices_esc);
            free(choices);
        }

        value = question_getvalue(q, "");
        if (value && *value)
            talk(obj, NULL, "SET", q->tag, value, NULL);

        for (qv = q->variables; qv; qv = qv->next)
            talk(obj, NULL, "SUBST", q->tag, qv->variable, qv->value, NULL);

        talk(obj, NULL, "INPUT", q->priority, q->tag, NULL);
    }

    /* Tell the agent to display the question(s), and check for a back
     * button.
     */
    if (obj->questions && talk(obj, NULL, "GO", NULL) == 30 &&
        (obj->capability & DCF_CAPB_BACKUP))
        return DC_GOBACK;

    /* Retrieve the answers. */
    for (q = obj->questions; q; q = q->next) {
        char *value;
        if (talk(obj, &value, "GET", q->tag, NULL) == 0) {
            /* debconf does translation to C here, but we avoid the need for
             * that by sending Choices-C.
             */
            question_setvalue(q, value);
            INFO(INFO_DEBUG, "Got \"%s\" for %s", value, q->tag);
        }
        free(value);
    }

    return DC_OK;
}

static void
progress_data(struct frontend *obj, struct question *q)
{
    char *desc, *extdesc;

    talk(obj, NULL, "DATA", q->tag, "type", q->template->type, NULL);

    desc = q_get_description(obj, q);
    if (desc) {
        talk(obj, NULL, "DATA", q->tag, "description", desc, NULL);
        free(desc);
    }

    extdesc = q_get_extended_description(obj, q);
    if (extdesc) {
        talk(obj, NULL, "DATA", q->tag, "extended_description", extdesc, NULL);
        free(extdesc);
    }
}

static void
passthrough_progress_start(struct frontend *obj, int min, int max,
                           struct question *title)
{
    char *min_str, *max_str;

    progress_data(obj, title);
    asprintf(&min_str, "%d", min);
    asprintf(&max_str, "%d", max);
    talk(obj, NULL, "PROGRESS", "START", min_str, max_str, title->tag, NULL);
    free(max_str);
    free(min_str);
}

static int
passthrough_progress_set(struct frontend *obj, int val)
{
    char *val_str;
    int ret;

    asprintf(&val_str, "%d", val);
    ret = talk(obj, NULL, "PROGRESS", "SET", val_str, NULL);
    free(val_str);
    return ret;
}

static int
passthrough_progress_step(struct frontend *obj, int step)
{
    char *step_str;
    int ret;

    asprintf(&step_str, "%d", step);
    ret = talk(obj, NULL, "PROGRESS", "STEP", step_str, NULL);
    free(step_str);
    return ret;
}

static int
passthrough_progress_info(struct frontend *obj, struct question *info)
{
    progress_data(obj, info);
    return talk(obj, NULL, "PROGRESS", "INFO", info->tag, NULL);
}

static void
passthrough_progress_stop(struct frontend *obj)
{
    talk(obj, NULL, "PROGRESS", "STOP", NULL);
}

struct frontend_module debconf_frontend_module =
{
    .initialize = passthrough_initialize,
    .shutdown = passthrough_shutdown,
    .query_capability = passthrough_query_capability,
    .set_title = passthrough_set_title,
    .go = passthrough_go,
    .progress_start = passthrough_progress_start,
    .progress_set = passthrough_progress_set,
    .progress_step = passthrough_progress_step,
    .progress_info = passthrough_progress_info,
    .progress_stop = passthrough_progress_stop,
};
