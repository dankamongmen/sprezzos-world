/**
 * @file question.c
 * @brief interfaces for handling debconf questions
 */
#ifndef _QUESTION_H_
#define _QUESTION_H_

#define DC_QFLAG_SEEN		(1 << 0)
#define DC_QFLAG_DONTPARSE	(1 << 1)

#define q_get_extended_description(fe, q)  question_get_field((fe), (q), "", "extended_description")
#define q_get_description(fe, q)           question_get_field((fe), (q), "", "description")
#define q_get_choices(fe, q)               question_get_field((fe), (q), "", "choices")
#define q_get_choices_vals(fe, q)          question_get_raw_field((q), "C", "choices")
#define q_get_indices(fe, q)               question_get_field((fe), (q), "", "indices")
#define q_get_help(fe, q)                  question_get_raw_field((q), "", "help")
#define q_get_raw_extended_description(q)  question_get_raw_field((q), "", "extended_description")
#define q_get_raw_description(q)           question_get_raw_field((q), "", "description")
#define q_get_raw_choices(q)               question_get_raw_field((q), "", "choices")
#define q_get_raw_choices_vals(q)          question_get_raw_field((q), "C", "choices")
#define q_get_raw_indices(q)               question_get_raw_field((q), "", "indices")

struct template;
struct frontend;

struct questionvariable {
	char *variable;
	char *value;
	struct questionvariable *next;
};

struct questionowner {
	char *owner;
	struct questionowner *next;
};

struct question {
	char *tag;
	unsigned int ref;
	char *value;
	unsigned int flags;

	struct template *template;
	struct questionvariable *variables;
	struct questionowner *owners;
	struct question *prev, *next;

        char *priority;
};

struct question *question_new(const char *tag);
void question_delete(struct question *question);

/**
 * @brief duplicate a question
 * @param q - the question to be duplicated
 * @return a deep copy of the question struct passed as input.  the 
 *         template pointer is not changed
 */
struct question *question_dup(const struct question *q);

void question_ref(struct question *);
void question_deref(struct question *);

void question_setvalue(struct question *q, const char *value);
const char *question_getvalue(const struct question *q, const char *lang);

void question_variable_add(struct question *q, const char *var, 	
	const char *value);
void question_variable_delete(struct question *q, const char *var, 	
	const char *value);
const char *question_get_variable(const struct question *q, const char *var);

void question_owner_add(struct question *q, const char *owner);
void question_owner_delete(struct question *q, const char *owner);
char *question_get_raw_field(const struct question *q, const char *lang,
	const char *field);
char *question_get_field(struct frontend *obj, const struct question *q,
        const char *lang, const char *field);

const char *question_get_text(struct frontend *obj, const char *template,
              const char *fallback);

#endif
