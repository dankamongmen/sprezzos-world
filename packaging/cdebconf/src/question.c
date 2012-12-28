#include "common.h"
#include "question.h"
#include "template.h"
#include "strutl.h"
#include "configuration.h"
#include "database.h"
#include "frontend.h"
#include <assert.h>

struct question *question_new(const char *tag)
{
	struct question *q;

	q = NEW(struct question);
	memset(q, 0, sizeof(struct question));
	q->ref = 1;
	q->tag = STRDUP(tag);
        q->priority = NULL;

	return q;
}

void question_delete(struct question *question)
{
    struct questionowner **ownerp;

    DELETE(question->tag);
    if (question->template)
        template_deref(question->template);
    for (ownerp = &question->owners; *ownerp != NULL;)
    {
        struct questionowner *currentp = *ownerp;
        *ownerp = currentp->next;
        DELETE(currentp->owner);
        DELETE(currentp);
    }
    free(question->priority);
    DELETE(question);
}

void question_ref(struct question *q)
{
	++q->ref;
}

void question_deref(struct question *q)
{
	if (q == NULL) return;
	if (--q->ref == 0)
		question_delete(q);
}


struct question *question_dup(const struct question *q)
{
        struct question *ret = question_new(q->tag);
        struct questionvariable *qv = q->variables;
        struct questionowner *qo = q->owners;
        ret->value = STRDUP(q->value);
        ret->flags = q->flags;
        ret->template = q->template;
        template_ref(ret->template);
/*        ret->template = template_dup(q->template); */
        while (qv)
        {
                question_variable_add(ret,qv->variable,qv->value);
                qv = qv->next;
        }
        while (qo)
        {
                question_owner_add(ret,qo->owner);
                qo = qo->next;
        }
        return ret;
}

void question_setvalue(struct question *q, const char *value)
{
	/* Be careful about the self-assignment case... */
	if (q->value != value)
	{
		DELETE(q->value);
		q->value = STRDUP(value);
	}
}

/* Note that Default-* fields contain *untranslated* choices, so it's usual
 * to call this with lang="" and then compare the answer with untranslated
 * choices.
 */
const char *question_getvalue(const struct question *q, const char *lang)
{
	if (q->value)
		return q->value;
	return template_lget(q->template, lang, "default");
}

const char *question_get_variable(const struct question *q, const char *var)
{
	struct questionvariable *qvi = q->variables;

	for (; qvi != 0; qvi = qvi->next)
		if (strcmp(qvi->variable, var) == 0)
			return qvi->value;

	return NULL;
}

void question_variable_add(struct question *q, const char *var, 	
	const char *value)
{
	struct questionvariable *qvi = q->variables;
	struct questionvariable **qlast = &q->variables;

	INFO(INFO_DEBUG, "Adding [%s] -> [%s]", var, value);
	for (; qvi != 0; qlast = &qvi->next, qvi = qvi->next)
		if (strcmp(qvi->variable, var) == 0 && qvi->value != value)
		{
			DELETE(qvi->value);
			qvi->value = STRDUP(value);
			return;
		}
	
	qvi = NEW(struct questionvariable);
	memset(qvi, 0, sizeof(struct questionvariable));
	qvi->variable = STRDUP(var);
	qvi->value = STRDUP(value);
	*qlast = qvi;
}

void question_owner_add(struct question *q, const char *owner)
{
	struct questionowner **ownerp = &q->owners;
	
	while (*ownerp != 0)
	{
		if (strcmp((*ownerp)->owner, owner) == 0)
			return;
		ownerp = &(*ownerp)->next;
	}

	*ownerp = NEW(struct questionowner);
	memset(*ownerp, 0, sizeof(struct questionowner));
	(*ownerp)->owner = STRDUP(owner);
	(*ownerp)->next = 0;
}

void question_owner_delete(struct question *q, const char *owner)
{
	struct questionowner **ownerp;

	for (ownerp = &q->owners; *ownerp != 0;)
	{
		if (strcmp((*ownerp)->owner, owner) == 0)
		{
			struct questionowner *currentp = *ownerp;

			*ownerp = currentp->next;
			DELETE(currentp->owner);
			DELETE(currentp);
		}
		else
		{
			ownerp = &(*ownerp)->next;
		}
	}
}

/* used by question_expand_vars */
static const char * lookup_vars(const char * name,
                                struct questionvariable * variables)
{
    struct questionvariable * current;

    /* Skip directives */
    if ('!' == name[0]) {
        return NULL;
    }
    for (current = variables; NULL != current; current = current->next) {
        if (0 == strcmp(current->variable, name)) {
            return current->value;
        }
    }
    return "";
}

static char *question_expand_vars(const struct question *q, const char *field)
{
    return strexpand(field, LOOKUP_FUNCTION(lookup_vars), q->variables);
}

char *question_get_raw_field(const struct question *q, const char *lang,
	const char *field)
{
    char *ret = NULL; 
	
    assert(q);
    assert(field);

    if (strcmp(field, "value") == 0)
        ret = question_expand_vars(q, question_getvalue(q, lang));
	else if (strcasecmp(field, "owners") == 0)
	{
		struct questionowner *owner = q->owners;
		while (owner)
		{
			if (ret)
			{
				char *oldret = ret;
				ret = realloc(ret, strlen(ret) + 2 + strlen(owner->owner) + 1);
				if (ret)
				{
					strcat(ret, ", ");
					strcat(ret, owner->owner);
				}
				else
					ret = oldret;
			}
			else
				ret = strdup(owner->owner);
			
			owner = owner->next;
		}
	}
    else
        ret = question_expand_vars(q, template_lget(q->template, lang, field));

    /* question_get_field must always return a valid string. */
    if (ret == NULL)
        return strdup("");
    else
        return ret;
}

static const char * lookup_directive(const char * directive, struct frontend * fe)
{
    assert('!' == directive[0]);
    return fe->methods.lookup_directive(fe, directive + 1 /* drop '!' */);
}

char * question_get_field(struct frontend * fe, const struct question * q,
                          const char * lang, const char * field)
{
    char * raw;
    char * ret;

    raw = question_get_raw_field(q, lang, field);
    ret = strexpand(raw, LOOKUP_FUNCTION(lookup_directive), fe);
    free(raw);
    return ret;
}

/*
 * Function: question_get_text
 * Input: struct frontend *obj - frontend object
 *        const char *template - template name
 *        const char *fallback - string to use if not available
 * Output: const char * - ptr to string, translated if possible
 * Description: get the translated version of a string
 * Assumptions: None.
 */
const char *
question_get_text(struct frontend *obj, const char *template, 
                  const char *fallback)
{
	struct question *q = obj->qdb->methods.get(obj->qdb, template);
	const char *text;
	text = (q ? q_get_description(obj, q) : fallback);
	question_deref(q);
	return text;
}

