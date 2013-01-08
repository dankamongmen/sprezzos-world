#!/usr/bin/env python
#
# <alt_deps.py>
#
# Fold several dependency entries in a <substvars> file into a single
# alternative dependency.

import os
import sys

def mangle(substvars, keyfilter, initdeps):
	def __str2list(kv):
		kv[1] = map(lambda x: x.strip(), kv[1].split(','))
		return kv[0], list(kv[1])

	substvars=map(lambda x: x.split('=', 1), substvars)
	substvars=map(__str2list, substvars)

	initdeps=map(lambda x: x.split('=', 1), initdeps)
	initdeps=dict(map(__str2list, initdeps))
	
	deps=dict(filter(lambda x: x[0] in keyfilter, substvars))

	for k,v in initdeps.iteritems():
		deps[k] = deps.get(k, []) + v

	def __combine(a, b):
		if len(b) == 0:
			return [ [ x ] for x in a ]
		return [ y + [ x ] for x in a for y in b ]

	def __normalise(ll):
		ret = []
		for i in ll:
			ret = __combine(i, ret)
		return ret

	# This is deps.values(), essentially, but preserves ordering of
	# dependencies as given in keyfilter.
	deplist = [ deps.get(k) for k in keyfilter ]
	deplist = filter(lambda x: x is not None, deplist)

	return ", ".join(map(lambda x: " | ".join(x), __normalise(deplist)))

def usage():
	print "Usage: alt_deps.py <substvars> <newkey> <altdeps> " + \
	      "[ <adddeps> ...]"
	print " Fold several dependency entries in a <substvars> file into a"
	print " single alternative dependency.  The <substvars> is updated"
	print " with a properly normalised new dependency entry."
	print "  <substvars>  Name of substvars file to modify"
	print "  <newkey>     Name of new dependency key"
	print "  <altdeps>    Names of keys in <substvars> to fold into single alternative"
	print "  <adddeps>    Additional dependencies in substvars format"

def handle_sysexception(e):
	if e.filename is None:
		print e.strerror
	else:
		print ": ".join((e.filename, e.strerror))
	sys.exit(1)

if __name__ == "__main__":
	if len(sys.argv) < 4:
		usage()
		sys.exit(1)
	try:
		fd = open(sys.argv[1], 'r+')
	except EnvironmentError, e:
		handle_sysexception(e)

	newkey = sys.argv[2]
	keyfilter = sys.argv[3].split()
	try:
		initdeps = sys.argv[4:]
	except:
		initdeps = []

	substvars = fd.readlines()
	newdep = mangle(substvars, keyfilter, initdeps)
	substvars = filter(lambda x: not x.startswith(newkey + '='), substvars)
	substvars.append("=".join((newkey, newdep)) + '\n')

	try:
		fd.seek(0, os.SEEK_SET)
		fd.truncate()
		fd.writelines(substvars)
		fd.close()
	except EnvironmentError, e:
		handle_sysexception(e)

	sys.exit(0)
