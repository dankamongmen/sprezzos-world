
#ifndef _CDEBCONF_TEXT_H_
#define _CDEBCONF_TEXT_H_

#define CHAR_GOBACK '<'
#define CHAR_HELP '?'
#define CHAR_CLEAR '!'

int cdebconf_text_get_width(const char *text);

void cdebconf_text_wrap_print(const char *str);

#endif /* _CDEBCONF_TEXT_H_ */
