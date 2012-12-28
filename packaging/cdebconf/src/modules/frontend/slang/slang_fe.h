#ifndef _SLANG_H_
#define _SLANG_H_

#include <slang.h>

struct slwindow {
	int x, y, h, w;
	int border;
	int drawcolor;
	int fillcolor;
	int selectedcolor;
	int donecolor;
	char *title;
};

#endif
