#!/usr/bin/env python

# bug557960: segfault on second call to SnackScreen.

from snack import *

s = SnackScreen()
g = Grid(1, 1)
ok = Button("Ok")
g.setField(ok, 0, 0)
s.gridWrappedWindow(g, "Ok")
F = Form()
F.add(ok)
res = F.run()
s.popWindow()
s.finish()

print "New screen"
s = SnackScreen()
# print "We do not get here if run in screen."
g = Grid(1, 1)
ok = Button("Ok")
g.setField(ok, 0, 0)
s.gridWrappedWindow(g, "segfaults in screen")
F = Form()
F.add(ok)
res = F.run()
s.popWindow()
s.finish()

