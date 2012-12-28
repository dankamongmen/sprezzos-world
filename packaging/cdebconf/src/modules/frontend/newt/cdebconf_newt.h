
#ifndef _CDEBCONF_NEWT_H_
#define _CDEBCONF_NEWT_H_

/*  Horizontal offset between buttons and text box */
#define TEXT_PADDING 1
/*  Horizontal offset between text box and borders */
#define BUTTON_PADDING 4

/* Most plugins won't need to call this, but cdebconf-newt-terminal does. */
void cdebconf_newt_setup(void);

/* Hack for the benefit of cdebconf-newt-terminal; progress_info isn't in
 * the main frontend struct and won't fit in the obvious place without
 * breaking the plugin ABI. Always duplicates the info string.
 */
struct question *cdebconf_newt_get_progress_info(struct frontend *obj);

newtComponent cdebconf_newt_create_form(newtComponent scrollbar);

void cdebconf_newt_create_window(const int width, const int height, const char *title, const char *priority);

int
cdebconf_newt_get_text_height(const char *text, int win_width);

int
cdebconf_newt_get_text_width(const char *text);

#endif /* _CDEBCONF_NEWT_H_ */
