#!/usr/bin/env python
# encoding: utf-8
#


import os, numpy, math, sys, datetime, glob

list_of_files = glob.glob('./files/*')


files = [
'airfoil.dem.gz',
'blutux.rgb.gz',
'contours.dem.gz',
'delaunay-edges.dat.gz',
'demo.edf.gz',
'equipo2.tmp.gz',
'field2xy.tmp.gz',
'finance.dat.gz',
'finance.dem.gz',
'fit3.dat.gz',
'fit.dem.gz',
'fit.log.gz',
'glass.dat.gz',
'GM1_bonds.r3d.gz',
'GM1_sugar.pdb.gz',
'gpdemos.tcl.gz',
'hemisphr.dat.gz',
'image2.dem.gz',
'image.dem.gz',
'key.dem.gz',
'klein.dat.gz',
'lena.rgb.gz',
'moli3.dat.gz',
'nearmap.csv.gz',
'orbital_elements.dat.gz',
'pm3dcolors.dem.gz',
'pm3d.dem.gz',
'poldat.dem.gz',
'prob2.dem.gz',
'prob.dem.gz',
'random.dem.gz',
'random-points.gz',
'random.tmp.gz',
'stat.inc.gz',
'surface1.dem.gz',
'using.dat.gz',
'whale.dat.gz',
'world.dat.gz']

for i in list_of_files:
  if (i[-3:]<>'.gz'):
    for z in files:
      s = 'sed -i \'s/%s/<zcat %s/g\' %s' % (z[:-3], z, i)
      os.system(s)
