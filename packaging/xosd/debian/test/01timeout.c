#include "xosd.h"
int main(void) {
  xosd *osd = xosd_create(2);
  xosd_set_timeout(osd, 2);
  xosd_display(osd, 0, XOSD_string, "foo");
  sleep(1);
  xosd_set_timeout(osd, -1);
  xosd_display(osd, 1, XOSD_string, "bar");
}
