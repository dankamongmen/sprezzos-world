#include "common.h"
#include "template.h"
#include "question.h"
#include "frontend.h"
#include "database.h"
#include "ncurses_fe.h"
#include "strutl.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#define WIN_QUERY	1
#define WIN_DESC	2
#define COLOR_FRAME	1
#define COLOR_QUERY	2
#define COLOR_DESC	3
#define UIDATA(obj) 	((struct uidata *)(obj)->data)

static struct question_handlers {
	const char *type;
	int (*handler)(struct frontend *, struct question *q);
} question_handlers[] = {
	{ "boolean",	nchandler_boolean },
	{ "multiselect", nchandler_multiselect },
	{ "note",	nchandler_note },
	{ "password",	nchandler_password },
	{ "select",	nchandler_select },
	{ "string",	nchandler_string },
	{ "text",	nchandler_text },
	{ "error",	nchandler_note },
};

/* Private variables */
struct uidata {
	int qrylines, desclines;
	WINDOW *qrywin, *descwin;
};

/* Slang compatibility functions */
#ifdef USESLANG
static int mvhline(int r, int c, int ch, int w)
{
	int i;
	if (ch == 0)
		ch = ACS_HLINE;
	for (i = 0; i < w; i++)
		mvaddch(r, c + i, ch);
	return 0;
}

static int mvvline(int r, int c, int ch, int h)
{
	int i;
	if (ch == 0)
		ch = ACS_VLINE;
	for (i = 0; i < h; i++)
		mvaddch(r + i, c, ch);
	return 0;
}

#endif

static void wfill(WINDOW *w)
{
	int x, y, xmax, ymax;

	getmaxyx(w, ymax, xmax);
	for (x = 0; x < xmax; x++)
		for (y = 0; y < ymax; y++)
			mvwaddch(w, y, x, ' ');
}

static int can_go_back(struct frontend *ui)
{
	return 1;
}

static int can_go_forward(struct frontend *ui)
{
	return 1;
}

static int longestlen(char **strs, int count)
{
	int i, max = 0;

	for (i = 0; i < count; i++)
	{
		if (strlen(strs[i]) > max)
			max = strlen(strs[i]);
	}
	return max;
}

/* not needed (yet?) 
static void attrprintf(WINDOW *win, int y, int x, chtype attr, char *fmt, ...)
#ifdef __GNUC__
	__attribute__((format(printf, 5, 6)))
#endif
{
	char *buf = 0;
	va_list ap;

	va_start(ap, fmt);
	vasprintf(&buf, fmt, ap);
	va_end(ap);

	while (*buf)
		mvwaddch(win, y, x++, ((unsigned char)(*buf++))|attr);
}
*/

static int wrapprint(WINDOW *win, const char *str, int ystart, const int width)
{
	/* Simple greedy line-wrapper */
	int len = STRLEN(str);
	char line[width+1];
	const char *s, *e, *end, *lb;

	if (str == 0) return ystart;

	s = e = str;
	end = str + len;
	
	while (len > 0)
	{
		/* try to fit the most characters */
		e = s + width;
		
		if (e >= end) 
		{
			e = end;
		}
		else
		{
			while (e > s && isalnum(*e)) e--;
			e++;
		}
		/* no word-break point found, so just break the line */
		if (e == s) e = s + width;

		/* if there's an explicit linebreak, honor it */
		lb = strchr(s, '\n');
		if (lb != NULL && lb < e) e = lb + 1;

		strncpy(line, s, e-s);
		line[e-s] = 0;
		mvwprintw(win, ystart++, 0, "%s%s", line, (lb == NULL || lb >= e ? "\n" : ""));

		len -= (e-s);
		s = e;
		while (*s == ' ') s++;
	}
	return ystart;
}

static void drawframe(struct frontend *ui, const int window, const char *title)
{
	struct uidata *data = UIDATA(ui);
	int ytop, ybottom;

	switch (window)
	{
	case WIN_QUERY:
		ytop = 0;
		ybottom = data->qrylines - 1;
		break;
	case WIN_DESC:
		ytop = data->qrylines;
		ybottom = data->qrylines + data->desclines - 1;
		break;
	}
	/* draw the borders */
	mvvline(ytop + 1, 0, 0, ybottom-ytop-1);
	mvvline(ytop + 1, COLS-1, 0, ybottom-ytop-1);
	mvhline(ytop, 1, 0, COLS-2);
	mvhline(ybottom, 1, 0, COLS-2);

	/* draw the corners */
	mvaddch(ytop, 0, ACS_ULCORNER);
	mvaddch(ytop, COLS-1, ACS_URCORNER);
	mvaddch(ybottom, 0, ACS_LLCORNER);
	mvaddch(ybottom, COLS-1, ACS_LRCORNER);

	/* draw the title */
	if (title != 0)
	{
		mvprintw(ytop, (COLS-strlen(title))/2, "%s", title);
	}
}

static void drawnavbuttons(struct frontend *ui, int selected)
{
	struct uidata *data = UIDATA(ui);
	WINDOW *win = data->qrywin;
	int ybut = data->qrylines - 2;

	mvhline(ybut - 1, 1, 0, COLS-2);
	mvaddch(ybut - 1, 0, ACS_LTEE);
	mvaddch(ybut - 1, COLS-1, ACS_RTEE);
	refresh();

	/* draw the actual buttons, note that these are drawn in the
	 * query window instead of in the parent (like the frame) */

	if (can_go_back(ui))
	{
		if (selected == 0) wstandout(win); else wstandend(win);
		mvwprintw(win, ybut-1, 2, "<Previous>");
	}

	if (can_go_forward(ui))
	{
		if (selected == 1) wstandout(win); else wstandend(win);
		mvwprintw(win, ybut-1, COLS-10, "<Next>");
	}
	wstandend(win);

	/* caller should call wrefresh() ! */
}

static void drawdesc(struct frontend *ui, struct question *q)
{
	WINDOW *qrywin = UIDATA(ui)->qrywin;
	WINDOW *descwin = UIDATA(ui)->descwin;
	char *descr = q_get_description(ui, q);
	char *ext_descr = q_get_extended_description(ui, q);

	drawframe(ui, WIN_QUERY, ui->title);
	wrapprint(qrywin, descr, 0, COLS-2);
	if (*ext_descr)
		wrapprint(descwin, ext_descr, 0, COLS-2);
	wclrtobot(qrywin);
	wclrtobot(descwin);
	wrefresh(stdscr);
	wrefresh(qrywin);
	wrefresh(descwin);
	free(descr);
	free(ext_descr);
}

static int nchandler_boolean(struct frontend *ui, struct question *q)
{
	const char *value = "true";
	int ret = 0, ans, pos = 2;
	int ybut = UIDATA(ui)->qrylines - 6;
	const char *dft = question_getvalue(q, "");
	WINDOW *win = UIDATA(ui)->qrywin;

	if (dft)
		value = dft;

	ans = (strcmp(value, "true") == 0);

	while (ret == 0) 
	{
		/* Draw the radio boxes */
		if (pos == 2) wstandout(win); else wstandend(win);
		mvwprintw(win, ybut, (COLS/2)-11, " (%c) Yes ",
			(ans ? '*' : ' '));
		if (pos == 3) wstandout(win); else wstandend(win);
		mvwprintw(win, ybut, (COLS/2)+4, " (%c) No ",
			(ans ? ' ' : '*'));
		wstandend(win);

		drawnavbuttons(ui, pos);

		wrefresh(win);

		switch (getch())
		{
		case KEY_LEFT:
		case KEY_UP:
			pos--;
			if (pos < 0) pos = 3;
			break;
		case KEY_RIGHT:
		case KEY_DOWN:
		case 9: /* tab */
			pos++;
			if (pos > 3) pos = 0;
			break;
		case ' ':
		case '\r':
		case '\n':
		case KEY_ENTER:
			switch (pos)
			{
				case 0:
					if (can_go_back(ui))
						ret = DC_GOBACK;
					/* go to previous if possible */
					break;
				case 1:
					ret = DC_OK;
				case 2: ans = 1; break;
				case 3: ans = 0; break;
			}
		}
	}

	if (ret == DC_OK)
		question_setvalue(q, (ans ? "true" : "false"));
	
	return ret;
}

static int nchandler_multiselect(struct frontend *ui, struct question *q)
{
	return 0;
}

static int nchandler_note(struct frontend *ui, struct question *q)
{
	int ret = 0, pos = 0;

	/* TODO scrolling support */

	while (ret == 0)
	{
		drawnavbuttons(ui, pos);
		wrefresh(UIDATA(ui)->qrywin);

		switch (getch())
		{
		case KEY_LEFT:
		case KEY_UP:
		case KEY_RIGHT:
		case KEY_DOWN:
		case 9: /* tab */
			if (pos == 0) pos = 1; else pos = 0;
			break;
		case ' ':
		case '\r':
		case '\n':
		case KEY_ENTER:
			switch (pos)
			{
				case 0:
					if (can_go_back(ui))
						ret = DC_GOBACK;
					/* go to previous if possible */
					break;
				case 1:
					ret = DC_OK;
					break;
			}
		}
	}
	return ret;
}

static int nchandler_password(struct frontend *ui, struct question *q)
{
	return 0;
}

static int nchandler_select(struct frontend *ui, struct question *q)
{
	char *choices[100] = {0};
	char *choices_translated[100] = {0};
	char *defaults[100] = {0};
	const char *defval = question_getvalue(q, "");

	int i, count, dcount, ret = 0, def = -1, pos = 2, xpos, ypos;
	int top, bottom, longest;
	WINDOW *win = UIDATA(ui)->qrywin;

	/* Parse out all the choices */
	count = strchoicesplit(q_get_choices_vals(ui, q), choices, DIM(choices));
	if (count <= 0) return DC_NOTOK;
	if (count == 1)
		defval = choices[0];

	strchoicesplit(q_get_choices(ui, q), choices_translated, DIM(choices_translated));
	dcount = strchoicesplit(question_get_field(ui, q, "C", "value"), defaults, DIM(defaults));

	/* See what the currently selected value should be -- either a
	 * previously selected value, or the default for the question
	 */
	if (defval != NULL)
	{
		for (i = 0; i < count; i++)
			if (strcmp(choices[i], defval) == 0)
				def = i + 1;
	}

	longest = longestlen(choices_translated, count);
	top = 0; bottom = MIN(count, UIDATA(ui)->qrylines-5);
	xpos = (COLS-longest)/2-1;

	while (ret == 0)
	{
		ypos = 2;
		for (i = top; i < bottom; i++)
		{
			if (pos == 2 && i == def) 
				wstandout(win); 
			else 
				wstandend(win);
			mvwprintw(win, ypos++, xpos, " %-*s ", longest, choices_translated[i]);
		}
		wstandend(win);

		drawnavbuttons(ui, pos);

		wrefresh(win);

		switch (getch())
		{
		case KEY_LEFT:
		case KEY_UP:
			def--;
			if (def < 0) def = count-1;

			/* check def against top/bottom */
			break;
		case KEY_RIGHT:
		case KEY_DOWN:
			def++;
			if (def >= count) def = 0;

			/* check def against top/bottom */
			break;
		case 9: /* TAB */
			pos++;
			if (pos > 2) pos = 0;
			break;
		case ' ':
		case '\r':
		case '\n':
		case KEY_ENTER:
			switch (pos)
			{
				case 0:
					if (can_go_back(ui))
						ret = DC_GOBACK;
					/* go to previous if possible */
					break;
				case 1:
					ret = DC_OK;
					break;
			}
		}
	}
	if (ret == DC_OK)
		question_setvalue(q, choices[def]);
	return ret;
}

static int nchandler_string(struct frontend *ui, struct question *q)
{
	return 0;
}

static int nchandler_text(struct frontend *ui, struct question *q)
{
	return 0;
}

/* ----------------------------------------------------------------------- */

static int ncurses_initialize(struct frontend *obj, struct configuration *cfg)
{
	struct uidata *uid = NEW(struct uidata);
	memset(uid, 0, sizeof(struct uidata));
	obj->interactive = 1;
	obj->data = uid;

	initscr();
	start_color();
	cbreak();
	noecho();
	nonl();
	keypad(stdscr, TRUE);

	uid->qrylines = 3 * LINES / 5;
	uid->desclines = LINES - uid->qrylines;

	uid->qrywin = newwin(uid->qrylines - 2, COLS - 2, 1, 1);
	uid->descwin = newwin(uid->desclines - 3, COLS - 2, uid->qrylines + 1, 1);

	if (uid->qrywin == NULL || uid->descwin == NULL)
		return DC_NOTOK;

	init_pair(COLOR_FRAME, COLOR_YELLOW, COLOR_BLUE);
	init_pair(COLOR_DESC, COLOR_WHITE, COLOR_BLUE);
	init_pair(COLOR_QUERY, COLOR_WHITE, COLOR_BLUE);
	init_pair(COLOR_DESC, COLOR_WHITE, COLOR_BLUE);

	attrset(COLOR_PAIR(COLOR_QUERY)|' ');
	wfill(stdscr);
	wattrset(uid->qrywin, COLOR_PAIR(COLOR_QUERY)|A_BOLD|' ');
	wfill(uid->qrywin);
	wattrset(uid->descwin, COLOR_PAIR(COLOR_DESC)|A_BOLD|' ');
	wfill(uid->descwin);

	drawframe(obj, WIN_QUERY, NULL);
	drawframe(obj, WIN_DESC, NULL);

	refresh();

	getch();

	return DC_OK;
}

static int ncurses_shutdown(struct frontend *obj)
{
	struct uidata *uid = UIDATA(obj);
	delwin(uid->qrywin);
	delwin(uid->descwin);
	endwin();
	DELETE(uid);
	return DC_OK;
}

static int ncurses_go(struct frontend *obj)
{
	struct question *q = obj->questions;
	struct question *qlast = 0;
	int i;
	int ret;

	while (q != 0)
	{
		ret = DC_OK;
		for (i = 0; i < DIM(question_handlers); i++)
			if (strcasecmp(q->template->type, question_handlers[i].type) == 0)
			{
				drawdesc(obj, q);
				ret = question_handlers[i].handler(obj, q);
				switch (ret)
				{
				case DC_OK:
					frontend_qdb_set(obj->qdb, q, 0);
					break;
				case DC_GOBACK:
					if (qlast != 0)
						q = qlast;
					else
						return ret;
					break;
				default:
					return ret;
				}
				break;
			}
		if (i == DIM(question_handlers))
			return DC_NOTIMPL;

		if (ret == DC_OK)
		{
			qlast = q;
			q = q->next;
		}
		
	}

	return DC_OK;
}

struct frontend_module debconf_frontend_module =
{
	initialize: ncurses_initialize,
	shutdown: ncurses_shutdown,
	go: ncurses_go,
};
