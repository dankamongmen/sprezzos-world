/***********************************************************************
 *
 * cdebconf - An implementation of the Debian Configuration Management
 *            System
 *
 * File: text.c
 *
 * Description: text UI for cdebconf
 * Some notes on the implementation - this is meant to be an accessibility-
 * friendly implementation. I've taken care to make the prompts work well
 * with screen readers and the like.
 *
 * cdebconf is (c) 2000-2001 Randolph Chung and others under the following
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
#include "template.h"
#include "question.h"
#include "frontend.h"
#include "database.h"
#include "plugin.h"
#include "strutl.h"

#include "cdebconf_text.h"

#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <limits.h>
#include <wchar.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>

struct frontend_data {
	char *previous_title;
};

typedef int (text_handler)(struct frontend *obj, struct question *q);

#define MAKE_UPPER(C) do { if (islower((int) C)) { C = (char) toupper((int) C); } } while(0)

#if defined(__s390__) || defined (__s390x__)
#define ISEMPTY(buf) (buf[0] == 0 || (buf[0] == '.' && buf[1] == 0))
#else
#define ISEMPTY(buf) (buf[0] == 0)
#endif

/*
 * Function: getwidth
 * Input: none
 * Output: int - width of screen
 * Description: get the width of the current terminal
 * Assumptions: doesn't handle resizing; caches value on first call
 */
static int getwidth(void)
{
	static int res = 80;
	static int inited = 0;
	int fd;
	struct winsize ws;

	if (inited == 0)
	{
		inited = 1;
		if ((fd = open("/dev/tty", O_RDONLY)) > 0)
		{
			if (ioctl(fd, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0)
				res = ws.ws_col;
			close(fd);
		}
	}
	return res;
}

/*
 * Function: wrap_print
 * Input: const char *str - string to display
 * Output: none
 * Description: prints a string to the screen with word wrapping 
 * Assumptions: string fits in <500 lines
 */
static void wrap_print(const char *str)
{
	/* Simple greedy line-wrapper */
	int i, lc;
	char *lines[500];

	lc = strwrap(str, getwidth() - 1, lines, DIM(lines));

	for (i = 0; i < lc; i++)
	{
		printf("%s\n", lines[i]);
		DELETE(lines[i]);
	}
}

/*
 * Function: text_handler_displaydesc
 * Input: struct frontend *obj - UI object
 *        struct question *q - question for which to display the description
 * Output: none
 * Description: displays the description for a given question 
 * Assumptions: none
 */
static void text_handler_displaydesc(struct frontend *obj, struct question *q) 
{
	char *descr = q_get_description(obj, q);
	char *ext_descr = q_get_extended_description(obj, q);
	if (strcmp(q->template->type, "note") == 0 ||
	    strcmp(q->template->type, "error") == 0)
	{
		if (strcmp(q->template->type, "error") == 0)
			printf(question_get_text(obj, "debconf/text-error", "!! ERROR: %s"), descr);
		else
			printf("%s", descr);
		printf("\n\n");
		if (*ext_descr)
			wrap_print(ext_descr);
	}
	else
	{
		if (*ext_descr)
			wrap_print(ext_descr);
		wrap_print(descr);
	}
	free(descr);
	free(ext_descr);
}

static void
get_answer(char *answer, int size)
{
	fgets(answer, size, stdin);
	CHOMP(answer);
}

static void
show_help (struct frontend *obj, struct question *q)
{
	char *descr = q_get_description(obj, q);
	char *help = q_get_help(obj, q);
	if (*help) {
		struct question *help_q = obj->qdb->methods.get(obj->qdb, help);
		if (help_q) {
			char *help_descr = q_get_description(obj, help_q);
			char *help_ext_descr = q_get_extended_description(obj, help_q);
			wrap_print(help_descr);
			printf("\n");
			if (*help_ext_descr) {
				wrap_print(help_ext_descr);
				printf("\n");
			}
			free(help_ext_descr);
			free(help_descr);
			question_deref(help_q);
		}
		free(help);
	}
	printf("%s\n", question_get_text(obj, "debconf/text-help-keystrokes", "KEYSTROKES:"));
	printf(" ");
	printf(question_get_text(obj, "debconf/text-help-keystroke", "'%c'"), CHAR_HELP);
	printf(" %s\n", question_get_text(obj, "debconf/text-help-help", "Display this help message"));
	if (obj->methods.can_go_back (obj, q)) {
		printf(" ");
		printf(question_get_text(obj, "debconf/text-help-keystroke", "'%c'"), CHAR_GOBACK);
		printf(" %s\n", question_get_text(obj, "debconf/text-help-goback", "Go back to previous question"));
	}
	if (strcmp(q->template->type, "string") == 0 ||
	    strcmp(q->template->type, "passwd") == 0 ||
	    strcmp(q->template->type, "multiselect") == 0) {
		printf(" ");
		printf(question_get_text(obj, "debconf/text-help-keystroke", "'%c'"), CHAR_CLEAR);
		printf(" %s\n", question_get_text(obj, "debconf/text-help-clear", "Select an empty entry"));
	}
	wrap_print(descr);
	free(descr);
}

struct choices {
	int count;
	char **choices;
	char **choices_translated;
	char *selected;
	int *tindex;
};

static void
printlist (struct frontend *obj, struct question *q, const struct choices *choices)
{
	int choice_min = -1;
	int num_cols, num_lines;
	int used_cols = 1;
	int last_col_height = 0;
	int i, k, l;
	int logcount = 0;
	int *col_width;
	int total_width = 0;
	char **output;
	int line = 0, max_len = 0, col = 0;
	int width = getwidth();
	char **fchoices = malloc(sizeof(char *) * choices->count);
	int horiz = 0;

	if (getenv("DEBCONF_TEXT_HORIZ"))
		horiz = 1;

	i = choices->count;
	do {
		i /= 10;
		logcount++;
	} while (i > 0);

	if (obj->methods.can_align(obj, q)) {
		stralign(choices->choices_translated, choices->count);
		/* Display only one column to get meaningful alignment. */
		width = 1;
	}

	/*  Set string arrays  */
	for (i=0; i < choices->count; i++)
	{
		/*  Trailing spaces are a placeholder to add [*] for
		    selected values
		    Comma is needed for proper speech synthesis pause */
		asprintf(&(fchoices[i]), "%3d: %s,    ", i+1, choices->choices_translated[i]);
		if (choices->selected[choices->tindex[i]])
			strcpy(fchoices[i]+strlen(fchoices[i])-5, " [*],");
		if (strwidth(fchoices[i]) < choice_min || choice_min == -1)
			choice_min = strwidth(fchoices[i]);
		if (strwidth(fchoices[i]) > width)
			width = strwidth(fchoices[i]);
	}
	num_cols = width / choice_min;
	if (num_cols > choices->count)
		num_cols = choices->count;
	col_width = malloc (sizeof(int) * num_cols);
	num_cols++;
	while (1)
	{
  COLUMN:
		num_cols--;
		if (num_cols == 0)
			break;
		num_lines = (choices->count - 1) / num_cols + 1;
		used_cols = (choices->count - 1) / num_lines + 1;
		last_col_height = choices->count % num_lines;
		for (i = 0; i < num_cols; i++)
			col_width[i] = 0;
		for (i = 0; i < choices->count; i++)
		{
			int current_col;
			if (horiz)
			{
				if (!last_col_height || i < last_col_height * num_cols)
					current_col = i % used_cols;
				else
					current_col = ((i - last_col_height * num_cols) % (used_cols-1));
			}
			else
				current_col = i / num_lines;
			if (strwidth(fchoices[i]) > col_width[current_col])
			{
				col_width[current_col] = strwidth(fchoices[i]);
				total_width = 0;
				for (k = 0; k < num_cols; k++)
					total_width += col_width[k];
				if (total_width > width)
					goto COLUMN;
			}
		}
		break;
	}
	if (num_cols == 0)
	{
		num_lines = choices->count;
		num_cols = 1;
	}
	output = malloc(sizeof(char *) * num_lines);
	for (i = 0; i < num_lines; i++)
	{
		output[i] = malloc(MB_LEN_MAX * width + 1);
		*(output[i]) = '\0';
	}
	for (i = 0; i < choices->count; i++)
	{
		if (horiz)
		{
			int j = line * used_cols + col;
			if (last_col_height && line > last_col_height)
				j -= line - last_col_height;
			strcat(output[line], fchoices[j]);
		}
		else
			strcat(output[line], fchoices[i]);
		if (strwidth(output[line]) > max_len)
			max_len = strwidth(output[line]);
		line++;
		if (line >= num_lines)
		{
			col++;
			if (col != num_cols)
			{
				for (l = 0; l < num_lines; l++)
					strpad(output[l], max_len);
			}

			line = 0;
			max_len = 0;
		}
	}
	for (l = 0; l < num_lines; l++)
	{
		printf("%s\n", output[l]);
		free(output[l]);
	}
	free(output);
	free(col_width);
	for (i = 0; i < choices->count; i++)
		free(fchoices[i]);
	free(fchoices);
}

/*
 * Function: text_handler_boolean
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK, DC_GOBACK
 * Description: handler for the boolean question type
 * Assumptions: none
 */
static int text_handler_boolean(struct frontend *obj, struct question *q)
{
	char buf[30];
	int ans = 0;
	int def = 0;
	const char *defval;

	defval = question_getvalue(q, "");
	if (defval)
	{
		if (strcmp(defval, "true") == 0)
			def = 1;
		else 
			def = 2;
	}

	do {
		printf("  %d: %s%s", 1, question_get_text(obj, "debconf/yes", "Yes"), (1 == def ? " [*]" : "    "));
		printf("  %d: %s%s", 2, question_get_text(obj, "debconf/no", "No"), (2 == def ? " [*]" : ""));
		printf("\n");
		if (def)
			printf(question_get_text(obj, "debconf/text-prompt-default",
					"Prompt: '%c' for help, default=%d> "), CHAR_HELP, def);
		else
			printf(question_get_text(obj, "debconf/text-prompt",
					"Prompt: '%c' for help> "), CHAR_HELP);
		get_answer(buf, sizeof(buf));
		if (buf[0] == CHAR_HELP && buf[1] == 0)
			show_help(obj, q);
		else if (obj->methods.can_go_back (obj, q) &&
		         buf[0] == CHAR_GOBACK && buf[1] == 0)
			return DC_GOBACK;
		else if (buf[0] == '1')
			ans = 1;
		else if (buf[0] == '2')
			ans = 2;
		else if (defval != 0 && ISEMPTY(buf))
			ans = def;
	} while (ans == 0);

	question_setvalue(q, (ans == 1 ? "true" : "false"));
	return DC_OK;
}

/*
 * Function: choices_delete
 * Input: struct choices *c - choices values, translations, indices, and select
 * Description: frees the structure and its content
 * Assumptions: none
 */
static void choices_delete(struct choices *c)
{
	int i;
	if (c)
	{
		if (c->choices)
		{
			for (i = 0; i < c->count; i++)
				free(c->choices[i]);
			free(c->choices);
		}
		if (c->choices_translated)
		{
			for (i = 0; i < c->count; i++)
				free(c->choices_translated[i]);
			free(c->choices_translated);
		}
		free(c->selected);
		free(c->tindex);
		free(c);
	}
}

/*
 * Function: choices_get
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: struct choices * - choices values, translations, indices, and select
 * Description: retrieve question choices, translations, indices
 * Assumptions: none
 */
static struct choices *choices_get(struct frontend *obj, struct question *q)
{
	struct choices *retval = NULL;
	int i, count;
	char *choices_vals = q_get_choices_vals(obj, q);

	count = strgetargc(choices_vals);
	if (count > 0)
	{
		int dcount;
		retval = (struct choices *)malloc(sizeof(struct choices));
		retval->count = count;
		retval->choices = malloc(sizeof(char *) * count);
		retval->choices_translated = malloc(sizeof(char *) * count);
		for (i = 0; i < count; ++i)
		{
			retval->choices[i] = retval->choices_translated[i] = NULL;
		}
		retval->tindex = malloc(sizeof(int) * count);
		retval->selected = calloc(1, sizeof(char) * retval->count);
		
		char *indices = q_get_indices(obj, q);
		char *choices = q_get_choices(obj, q);
		dcount = strchoicesplitsort(choices_vals, choices, indices, retval->choices, retval->choices_translated, retval->tindex, count);
		free(choices);
		free(indices);

		if (dcount != count)
		{
			choices_delete(retval);
			retval = NULL;
		}
	}

	free(choices_vals);

	return retval;
}

/*
 * Function: text_handler_multiselect
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK
 * Description: handler for the multiselect question type
 * Assumptions: none
 */
static int text_handler_multiselect(struct frontend *obj, struct question *q)
{
	struct choices *choices = NULL;
	char **defaults;
	char *defval, *cp;
	char answer[4096] = {0};
	int i, j, dcount, choice;
	int ret = DC_OK;

	choices = choices_get(obj, q);
	if (choices == NULL)
		return DC_NOTOK;

	defaults = malloc(sizeof(char *) * choices->count);
	dcount = strchoicesplit(question_getvalue(q, ""), defaults, choices->count);

	for (j = 0; j < dcount; j++) {
		for (i = 0; i < choices->count; i++) {
			if (strcmp(choices->choices[choices->tindex[i]], defaults[j]) == 0)
				choices->selected[choices->tindex[i]] = 1;
		}
		free(defaults[j]);
	}
	free(defaults);
	
	i = 0;

	defval = malloc(10*choices->count);
	*defval = '\0';
	for (i = 0; i < choices->count; i++)
		if (choices->selected[choices->tindex[i]])
		{
			char buf[10];
			if (*defval != '\0')
				strcat(defval, " ");
			sprintf(buf, "%d", i+1);
			strcat(defval, buf);
		}

  DISPLAY:
	printlist (obj, q, choices);
	printf(question_get_text(obj, "debconf/text-prompt-default-string", 
		"Prompt: '%c' for help, default=%s> "), CHAR_HELP, defval);
	get_answer(answer, sizeof(answer));
	if (answer[0] == CHAR_HELP && answer[1] == 0)
	{
		show_help(obj, q);
		goto DISPLAY;
	}
	else if (answer[0] == CHAR_CLEAR && answer[1] == 0)
	{
		for (i = 0; i < choices->count; i++)
			choices->selected[i] = 0;
	}
	else if (obj->methods.can_go_back (obj, q) &&
	         answer[0] == CHAR_GOBACK && answer[1] == 0)
	{
		ret = DC_GOBACK;
		goto CleanUp_DEFVAL;
	}

	if (!(ISEMPTY(answer)))
	{
		for (i = 0; i < choices->count; i++)
			choices->selected[i] = 0;
		for (cp = answer; cp != NULL && *cp != '\0'; cp = strchr(cp+1, ' '))
		{
			choice = atoi(cp) - 1;
			if (choice >= 0 && choice < choices->count)
				choices->selected[choices->tindex[choice]] = 1;
		}
	}

	answer[0] = 0;
	for (i = 0; i < choices->count; i++)
	{
		if (choices->selected[i])
		{
			if (answer[0] != 0)
				strvacat(answer, sizeof(answer), ", ", NULL);
			strvacat(answer, sizeof(answer), choices->choices[i], NULL);
		}
	}
	question_setvalue(q, answer);

  CleanUp_DEFVAL:
	free(defval);
	choices_delete(choices);
	
	return ret;
}

/*
 * Function: text_handler_select
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK
 * Description: handler for the select question type
 * Assumptions: none
 *
 * TODO: factor common code with multiselect
 */
static int text_handler_select(struct frontend *obj, struct question *q)
{
	struct choices *choices = NULL;
	char answer[10];
	int i, choice, def = -1;
	const char *defval;
	int ret = DC_OK;

	choices = choices_get(obj, q);
	if (choices == NULL)
		return DC_NOTOK;

	if (choices->count == 1)
		defval = choices->choices[0];
	else
		defval = question_getvalue(q, "");

	if (defval != NULL)
	{
		for (i = 0; i < choices->count; i++)
			if (strcmp(choices->choices[choices->tindex[i]], defval) == 0)
			{
				choices->selected[choices->tindex[i]] = 1;
				def = i;
			}
	}

	i = 0;
	choice = -1;
	do {
		printlist (obj, q, choices);
		if (def >= 0 && choices->choices_translated[def]) {
			printf(question_get_text(obj, "debconf/text-prompt-default", 
				"Prompt: '%c' for help, default=%d> "),
					CHAR_HELP, def+1);
		} else {
			printf(question_get_text(obj, "debconf/text-prompt",
				"Prompt: '%c' for help> "), CHAR_HELP);
		}
		get_answer(answer, sizeof(answer));
		if (answer[0] == CHAR_HELP)
		{
			show_help(obj, q);
			continue;
		}
		if (obj->methods.can_go_back (obj, q) &&
		    answer[0] == CHAR_GOBACK && answer[1] == 0)
		{
			ret = DC_GOBACK;
			goto CleanUp_SELECTED;
		}
		if (ISEMPTY(answer))
			choice = def;
		else
			choice = atoi(answer) - 1;
	} while (choice < 0 || choice >= choices->count);
	question_setvalue(q, choices->choices[choices->tindex[choice]]);

  CleanUp_SELECTED:
	choices_delete(choices);
	
	return ret;
}

/*
 * Function: text_handler_note
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK, DC_GOBACK
 * Description: handler for the note question type
 * Assumptions: none
 */
static int text_handler_note(struct frontend *obj, struct question *q)
{
	char buf[100] = {0};
	printf("%s ", question_get_text(obj, "debconf/cont-prompt",
			       "[Press enter to continue]"));
	fflush(stdout);
	while (1)
	{
		get_answer(buf, sizeof(buf));
		if (buf[0] == CHAR_HELP && buf[1] == 0)
			show_help(obj, q);
		else if (obj->methods.can_go_back (obj, q) &&
		         buf[0] == CHAR_GOBACK && buf[1] == 0)
			return DC_GOBACK;
		else
			break;
	}
	return DC_OK;
}

/*
 * Function: text_handler_password
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK
 * Description: handler for the password question type
 * Assumptions: none
 *
 * TODO: this can be *MUCH* improved. no editing is possible right now
 */
static int text_handler_password(struct frontend *obj, struct question *q)
{
	struct termios oldt, newt;
	char passwd[256] = {0};
	int i, c;

	tcgetattr(0, &oldt);
	memcpy(&newt, &oldt, sizeof(struct termios));
	cfmakeraw(&newt);
	while (1)
	{
		tcsetattr(0, TCSANOW, &newt);
		i = 0;
		while ((c = fgetc(stdin)) != EOF)
		{
			if (c == '\r' || c == '\n')
				break;
			else if (c == '\b')
			{
				if (i > 0)
					i--;
				continue;
			}
			passwd[i] = (char)c;
			i++;
		}
		passwd[i] = 0;
		tcsetattr(0, TCSANOW, &oldt);
		if (passwd[0] == CHAR_HELP && passwd[1] == 0)
			show_help(obj, q);
		else
			break;
	}
	if (obj->methods.can_go_back (obj, q) &&
	    passwd[0] == CHAR_GOBACK && passwd[1] == 0)
		return DC_GOBACK;
	if (passwd[0] == CHAR_CLEAR && passwd[1] == 0)
		question_setvalue(q, "");
	else
		question_setvalue(q, passwd);
	return DC_OK;
}

/*
 * Function: text_handler_string
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK
 * Description: handler for the string question type
 * Assumptions: none
 */
static int text_handler_string(struct frontend *obj, struct question *q)
{
	char buf[1024] = {0};
	const char *defval = question_getvalue(q, "");
	while (1) {
		if (defval)
			printf(question_get_text(obj, "debconf/text-prompt-default-string", "Prompt: '%c' for help, default=%s> "), CHAR_HELP, defval);
		else
			printf(question_get_text(obj, "debconf/text-prompt", "Prompt: '%c' for help> "), CHAR_HELP);
		fflush(stdout);
		get_answer(buf, sizeof(buf));
		if (buf[0] == CHAR_HELP && buf[1] == 0)
			show_help(obj, q);
		else
			break;
	}
	if (obj->methods.can_go_back (obj, q) &&
	    buf[0] == CHAR_GOBACK && buf[1] == 0)
		return DC_GOBACK;
	if (ISEMPTY(buf) && defval == 0)
		question_setvalue(q, "");
	else if (ISEMPTY(buf) && defval != 0)
		question_setvalue(q, defval);
	else if (buf[0] == CHAR_CLEAR && buf[1] == 0)
		question_setvalue(q, "");
	else
		question_setvalue(q, buf);
	return DC_OK;
}

/*
 * Function: text_handler_text
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK, DC_GOBACK
 * Description: handler for the text question type
 * Assumptions: none
 */
static int text_handler_text(struct frontend *obj, struct question *q)
{
	return text_handler_note(obj, q);
}

/*
 * Function: text_handler_error
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question to ask
 * Output: int - DC_OK, DC_NOTOK, DC_GOBACK
 * Description: handler for the error question type. Currently equal to _note
 * Assumptions: none
 */
static int text_handler_error(struct frontend *obj, struct question *q)
{
	return text_handler_note(obj, q);
}

/* ----------------------------------------------------------------------- */
static struct question_handlers {
	const char *type;
	text_handler *handler;
} question_handlers[] = {
	{ "boolean",	text_handler_boolean },
	{ "multiselect", text_handler_multiselect },
	{ "note",	text_handler_note },
	{ "password",	text_handler_password },
	{ "select",	text_handler_select },
	{ "string",	text_handler_string },
	{ "text",	text_handler_text },
	{ "error",	text_handler_error },
	{ "",		NULL },
};

/*
 * Function: text_initialize
 * Input: struct frontend *obj - frontend UI object
 *        struct configuration *cfg - configuration parameters
 * Output: int - DC_OK, DC_NOTOK
 * Description: initializes the text UI
 * Assumptions: none
 *
 * TODO: SIGINT is ignored by the text UI, otherwise it interfers with
 * the way question/answers are handled. this is probably not optimal
 */
static int text_initialize(struct frontend *obj, struct configuration *conf)
{
	struct frontend_data *data = NEW(struct frontend_data);
	char *term = getenv("TERM");
	char *palette = getenv("FRONTEND_BACKGROUND");
	data->previous_title = NULL;
	obj->data = data;
	obj->interactive = 1;
	signal(SIGINT, SIG_IGN);
	if (palette && !strcmp(palette, "dark") &&
			term && (!strcmp(term, "linux") || !strcmp(term, "bterm"))) {
		/* Hard-code for these cases */
		printf("\e[37m\e[40m\e[1m\e[H\e[J");
		fflush(stdout);
	}
	return DC_OK;
}

static int text_shutdown(struct frontend *obj)
{
	struct frontend_data *data = (struct frontend_data *)obj->data;
	if (NULL != data)
	{
		free(data->previous_title);
		free(data);
		obj->data = NULL;
	}
	return DC_OK;
}

/*
 * Function: text_can_go_back
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question object
 * Output: int - DC_OK, DC_NOTOK
 * Description: tells whether confmodule supports backing up
 * Assumptions: none
 */
static bool
text_can_go_back(struct frontend *obj, struct question *q)
{
	return (obj->capability & DCF_CAPB_BACKUP);
}

/*
 * Function: text_can_align
 * Input: struct frontend *obj - frontend object
 *        struct question *q - question object
 * Output: int - DC_OK, DC_NOTOK
 * Description: tells whether confmodule supports aligning columns
 * Assumptions: none
 */
static bool
text_can_align(struct frontend *obj, struct question *q)
{
	return (obj->capability & DCF_CAPB_ALIGN);
}

/*
 * Function: text_lookup_directive
 * Input: struct frontend *obj - frontend object
 *        const char *directive - the directive
 * Output: const char * - directive value or NULL
 * Description: returns directive value in the given context
 * Assumptions: none
 */
static const char *
text_lookup_directive(struct frontend *obj, const char *directive)
{
	if (obj->methods.can_align(obj, obj->questions)) {
		if (strcmp("TAB", directive) == 0) {
			return STRALIGN_TAB;
		} else if (strcmp("ALIGN=CENTER", directive) == 0) {
			return STRALIGN_ALIGN_CENTER;
		} else if (strcmp("ALIGN=RIGHT", directive) == 0) {
			return STRALIGN_ALIGN_RIGHT;
		}
	}
	/* Remove unhandled directives */
	return "";
}

/*
 * Function: text_go
 * Input: struct frontend *obj - frontend object
 * Output: int - DC_OK, DC_NOTOK
 * Description: asks all pending questions
 * Assumptions: none
 */
static int text_go(struct frontend *obj)
{
	struct frontend_data *data = (struct frontend_data *) obj->data;
	struct question *q = obj->questions;
	int i;
	int ret = DC_OK;

	while (q != NULL) {
		for (i = 0; i < DIM(question_handlers); i++) {
			text_handler *handler;
			struct plugin *plugin = NULL;

			if (*question_handlers[i].type)
				handler = question_handlers[i].handler;
			else {
				plugin = plugin_find(obj, q->template->type);
				if (plugin) {
					INFO(INFO_DEBUG, "Found plugin for %s", q->template->type);
					handler = (text_handler *) plugin->handler;
				} else {
					INFO(INFO_DEBUG, "No plugin for %s", q->template->type);
					continue;
				}
			}

			if (plugin || strcmp(q->template->type, question_handlers[i].type) == 0)
			{
				if (!data->previous_title ||
				    strcmp(obj->title, data->previous_title) != 0)
				{
					size_t underline_len;
					char *underline;

					/* TODO: can't tell if we called go()
					 * twice during one progress bar, but
					 * I'm guessing that's relatively
					 * unimportant (c.f. #271707).
					 */
					if (obj->progress_title != NULL)
						putchar('\n');
					underline_len = strlen(obj->title);
					underline = malloc(underline_len + 1);
					memset(underline, '-', underline_len);
					underline[underline_len] = '\0';
					printf("%s\n%s\n\n", obj->title, underline);
					free(underline);
					free(data->previous_title);
					data->previous_title = strdup(obj->title);
				}
				text_handler_displaydesc(obj, q);
				ret = handler(obj, q);
				putchar('\n');
				if (ret == DC_OK)
					frontend_qdb_set(obj->qdb, q, 0);
				if (plugin)
					plugin_delete(plugin);
				break;
			}
		}
		if (ret == DC_NOTOK)
			break;
		if (i == DIM(question_handlers))
			return DC_NOTIMPL;
		if (ret == DC_OK)
			q = q->next;
		else if (ret == DC_GOBACK) {
			do {
			    q = q->prev;
			} while (q != NULL && 0 == strcmp("error", q->template->type));
		}
	}
	return ret;
}

static void text_progress_start(struct frontend *obj, int min, int max, struct question *title)
{
	char *title_desc;

	question_deref(obj->progress_title);
	obj->progress_title = title;
	question_ref(obj->progress_title);
	obj->progress_min = min;
	obj->progress_max = max;
	obj->progress_cur = min;

	title_desc = q_get_raw_description(title);
	printf("%s  ", title_desc);
	free(title_desc);
	fflush(stdout);
}

static int text_progress_set(struct frontend *obj, int val)
{
	static int last = 0;
	int new;

	obj->progress_cur = val;
	new = ((double)(obj->progress_cur - obj->progress_min) / 
		(double)(obj->progress_max - obj->progress_min) * 100.0);
	if (new < last)
		last = 0;
	/*  Prevent verbose output  */
	if (new / 10 == last / 10)
		return DC_OK;
	last = new;
	printf("... %d%%", new);
	fflush(stdout);

	return DC_OK;
}

static void text_progress_stop(struct frontend *obj)
{
	INFO(INFO_DEBUG, "%s", __FUNCTION__);
	printf("\n");
	fflush(stdout);
	question_deref(obj->progress_title);
	obj->progress_title = NULL;
}

struct frontend_module debconf_frontend_module =
{
	.initialize = text_initialize,
	.shutdown = text_shutdown,
	.lookup_directive = text_lookup_directive,
	.can_go_back = text_can_go_back,
	.can_align = text_can_align,
	.go = text_go,
	.progress_start = text_progress_start,
	.progress_set = text_progress_set,
	.progress_stop = text_progress_stop,
};
