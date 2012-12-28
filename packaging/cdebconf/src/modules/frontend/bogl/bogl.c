#include "common.h"
#include "template.h"
#include "question.h"
#include "frontend.h"
#include "database.h"
#include "strutl.h"

#include <bogl/bogl.h>
#include <bogl/bowl.h>

/* Any private variables we may need. */
struct uidata {
};

typedef int (*handler_t)(struct frontend *ui, struct question *q);

struct question_handlers {
	const char *type;
	handler_t handler;
};

int bogl_handler_boolean(struct frontend *ui, struct question *q);
int bogl_handler_multiselect(struct frontend *ui, struct question *q);
int bogl_handler_select(struct frontend *ui, struct question *q);
int bogl_handler_note(struct frontend *ui, struct question *q);
int bogl_handler_string(struct frontend *ui, struct question *q);

static struct question_handlers question_handlers[] = {
	{ "boolean", bogl_handler_boolean },
	{ "multiselect", bogl_handler_multiselect },
	{ "select", bogl_handler_select },
	{ "note", bogl_handler_note },
	{ "string", bogl_handler_string },
        { "error", bogl_handler_note },
};

static const char *
get_text(struct frontend *obj, const char *template, const char *fallback )
{
	        struct question *q = obj->qdb->methods.get(obj->qdb, template);
		return q ? q_get_description(obj, q) : fallback;
}

static handler_t handler(const char *type)
{
	int i;
	for(i = 0; i < DIM(question_handlers); i++)
		if(! strcasecmp(type, question_handlers[i].type))
			return question_handlers[i].handler;
	return NULL;
}

/* FIXME: Needs disabled button widget. */
static void drawnavbuttons(struct frontend *ui, struct question *q)
{
	if(ui->methods.can_go_back(ui, q))
		bowl_new_button(get_text(ui, "debconf/button-goback", "Go Back"), DC_GOBACK);
	if(ui->methods.can_go_forward(ui, q))
		bowl_new_button(get_text(ui, "debconf/button-continue", "Next"), DC_OK);
}

static void drawdesctop(struct frontend *ui, struct question *q)
{
	bowl_title(ui->title);
	bowl_new_text(q_get_description(ui, q));
}

static void drawdescbot(struct frontend *ui, struct question *q)
{
	bowl_new_text(q_get_extended_description(ui, q));
}


/* boolean requires a new widget, the radio button :( */
/* Pretend with buttons - loses default info. */
int bogl_handler_boolean(struct frontend *ui, struct question *q)
{
	/* Should just make bowl_new_checkbox be properly const-qualified. */
	char *desc = strdup(q_get_description(ui, q));
	int ret;
	
#if 0
	char ans = ' ';

	if(q->value && *(q->value))
		ans = (strcmp(q->value, "true") == 0) ? '*' : ' ';
#endif
	
	bowl_flush();
	drawdesctop(ui, q);

	/* Should be:  bowl_new_radio(); drawnavbuttons(ui, q); */
	if(ui->methods.can_go_back(ui, q))
		bowl_new_button(get_text(ui, "debconf/button-goback", "Go Back"), 0);
	bowl_new_button(get_text(ui, "debconf/button-yes", "Yes"), 1);
	bowl_new_button(get_text(ui, "debconf/button-no", "No"), 2);

	drawdescbot(ui, q);
	bowl_layout();
	ret = bowl_run ();

	if(ret != 0)
		question_setvalue(q, (ret == 1) ? "true" : "false");

	free(desc);
	return (ret == 0) ? DC_GOBACK : DC_OK;
}

int bogl_handler_note(struct frontend *ui, struct question *q)
{
	bowl_flush();
	drawdesctop(ui, q);
	drawnavbuttons(ui, q);
	drawdescbot(ui, q);
	bowl_layout();
	return bowl_run();
}

int bogl_handler_select(struct frontend *ui, struct question *q)
{
	return DC_OK;
}

int bogl_handler_multiselect(struct frontend *ui, struct question *q)
{
	char **choices, **choices_translated, **defaults, *selected;
	int i, j, count, dcount, ret;
	const char *p;

	count = 0;
	p = q_get_choices_vals(ui, q);
	if (*p)
	{
		count++;
		for(; *p; p++)
			if(*p == ',')
				count++;
	}

	if (count <= 0) return DC_NOTOK;

	choices = malloc(sizeof(char *) * count);
	strchoicesplit(q_get_choices_vals(ui, q), choices, count);
	choices_translated = malloc(sizeof(char *) * count);
	strchoicesplit(q_get_choices(ui, q), choices_translated, count);
	selected = malloc(sizeof(char) * count);
	memset(selected, ' ', count);

	dcount = 1;
	for(p = question_get_field(ui, q, "C", "value"); *p; p++)
		if(*p == ',')
		  	dcount++;
	defaults = malloc(sizeof(char *) * dcount);
	dcount = strchoicesplit(question_get_field(ui, q, "C", "value"), defaults, dcount);
	for(j = 0; j < dcount; j++)
	{
		for(i = 0; i < count; i++)
			if(strcmp(choices[i], defaults[j]) == 0)
			{
				selected[i] = '*';
				break;
			}
		free(defaults[j]);
	}
	free(defaults);

	bowl_flush();
	drawdesctop(ui, q);
	bowl_new_checkbox(choices, selected, count, (count > 15) ? 15 : count);
	drawnavbuttons(ui, q);
	drawdescbot(ui, q);
	
	bowl_layout();
	ret = bowl_run();

	if(ret == DC_OK)
	{
		/* Be safe - allow for commas and spaces. */
		char *answer = malloc(strlen(q_get_choices(ui, q)) + 1 + count);
		answer[0] = 0;
		for(i = 0; i < count; i++)
			if (selected[i] == '*')
			{
				if(answer[0] != 0)
					strcat(answer, ", ");
				strcat(answer, choices_translated[i]);
			}
		question_setvalue(q, answer);
	}

	for(i = 0; i < count; i++)
		free(choices[i]);
	free(choices);

	return ret;
}

int bogl_handler_string(struct frontend *ui, struct question *q)
{
	char *s;
	int ret;

	bowl_flush();
	drawdesctop(ui, q);
	bowl_new_input(&s, question_get_field(ui, q, "C", "value"));
	drawnavbuttons(ui, q);
	drawdescbot(ui, q);
	bowl_layout();
	ret = bowl_run();
	
	if(ret == DC_OK)
		question_setvalue(q, s);
	free(s);
	
	return ret;
}

static int bogl_initialize(struct frontend *ui, struct configuration *cfg)
{
	ui->interactive = 1;
	ui->data = NULL;

	bowl_init();
	
	bowl_done ();
	
	return DC_OK;
}

static int bogl_shutdown(struct frontend *ui)
{
	bowl_done();
	
	return DC_OK;
}

static int bogl_go(struct frontend *ui)
{
	struct question *q = ui->questions;
	int ret;
	
	bowl_init ();

	while(q)
	{
		handler_t hdl;
		ret = DC_OK;
		hdl = handler(q->template->type);
		if (hdl == NULL)
			return DC_NOTIMPL;
		(*hdl)(ui, q);
		if(ret == DC_OK)
			frontend_qdb_set(ui->qdb, q, 0);
		else
			return ret;
		
		q = q->next;
	}
	
	bowl_done ();

	return DC_OK;
}

struct frontend_module debconf_frontend_module = {
	initialize: bogl_initialize,
	shutdown: bogl_shutdown,
	go: bogl_go,
};
