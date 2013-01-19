def _setup():
    import os.path, sys
    version = None
    rules = os.path.join(__path__[0], "../../../rules.defs")
    f = file(rules)
    for l in f:
        l = l.strip().split()
        if l[0] == 'KERNELVERSION':
            version = l[-1]
    f.close()
    if version is None:
        raise RuntimeError("Can't find KERNELVERSION setting")
    global support
    support = '/usr/src/linux-support-%s' % version
    if not os.path.exists(support):
        raise RuntimeError("Can't find %s, please install the linux-support-%s package" % (support, version))
    sys.path.append('%s/lib/python' % support)

_setup()
