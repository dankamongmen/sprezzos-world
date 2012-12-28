#ifndef _FRONTEND_NCURSES_H_
#define _FRONTEND_NCURSES_H_

#ifdef USESLANG
#include <slcurses.h>
#else
#include <ncurses.h>
#endif

static int nchandler_boolean(struct frontend *ui, struct question *q);
static int nchandler_multiselect(struct frontend *ui, struct question *q);
static int nchandler_note(struct frontend *ui, struct question *q);
static int nchandler_password(struct frontend *ui, struct question *q);
static int nchandler_select(struct frontend *ui, struct question *q);
static int nchandler_string(struct frontend *ui, struct question *q);
static int nchandler_text(struct frontend *ui, struct question *q);

#endif
