#!/usr/bin/env python

import os, os.path, re, shutil, sys

class Changelog(list):
    _rules = r"""
^
(?P<source>
    \w[-+0-9a-z.]+
)
\ 
\(
(?P<version>
    [^\(\)\ \t]+
)
\)
\s+
(?P<distribution>
    [-+0-9a-zA-Z.]+
)
\;
"""
    _re = re.compile(_rules, re.X)

    class Entry(object):
        __slot__ = 'distribution', 'source', 'version'

        def __init__(self, distribution, source, version):
            self.distribution, self.source, self.version = distribution, source, version

    def __init__(self, dir = '', version = None):
        if version is None:
            version = Version
        f = file(os.path.join(dir, "debian/changelog"))
        while True:
            line = f.readline()
            if not line:
                break
            match = self._re.match(line)
            if not match:
                continue
            try:
                v = version(match.group('version'))
            except Exception:
                if not len(self):
                    raise
                v = Version(match.group('version'))
            self.append(self.Entry(match.group('distribution'), match.group('source'), v))

class Version(object):
    _version_rules = ur"""
^
(?:
    (?P<epoch>
        \d+
    )
    :
)?
(?P<upstream>
    .+?
)   
(?:
    -
    (?P<revision>[^-]+)
)?
$
"""
    _version_re = re.compile(_version_rules, re.X)

    def __init__(self, version):
        match = self._version_re.match(version)
        if match is None:
            raise RuntimeError, "Invalid debian version"
        self.epoch = None
        if match.group("epoch") is not None:
            self.epoch = int(match.group("epoch"))
        self.upstream = match.group("upstream")
        self.revision = match.group("revision")

    def __str__(self):
        return self.complete

    @property
    def complete(self):
        if self.epoch is not None:
            return "%d:%s" % (self.epoch, self.complete_noepoch)
        return self.complete_noepoch

    @property
    def complete_noepoch(self):
        if self.revision is not None:
            return "%s-%s" % (self.upstream, self.revision)
        return self.upstream

class Main(object):
    def __init__(self, input_tar, override_version):
        self.log = sys.stdout.write

        self.input_tar = input_tar

        changelog = Changelog()[0]
        source = changelog.source
        version = changelog.version

        if override_version:
            version = Version('%s-undef' % override_version)

        self.log('Using source name %s, version %s\n' % (source, version.upstream))

        self.orig = '%s-%s' % (source, version.upstream)
        self.orig_tar = '%s_%s.orig.tar.gz' % (source, version.upstream)

    def __call__(self):
        import tempfile
        self.dir = tempfile.mkdtemp(prefix = 'genorig', dir = 'debian')
        try:
            self.upstream()
            self.remove()
            self.tar()
        finally:
            shutil.rmtree(self.dir)

    def upstream(self):
        self.log("Extracting tarball %s\n" % self.input_tar)
        match = re.match(r'(^|.*/).*\.(?P<extension>(tar\.(bz2|gz)|tbz2|tgz))?$', self.input_tar)
        if not match:
            raise RuntimeError("Can't identify name of tarball")
        cmdline = ['tar -xf', self.input_tar, '-C', self.dir]
        if match.group('extension') in ('tar.bz2', 'tbz2'):
            cmdline.append('-j')
        elif match.group('extension') in ('tar.gz', 'tgz'):
            cmdline.append('-z')
        if os.spawnv(os.P_WAIT, '/bin/sh', ['sh', '-c', ' '.join(cmdline)]):
            raise RuntimeError("Can't extract tarball")

    def remove(self):
        self.log("Remove files\n")
        root = os.path.join(self.dir, self.orig)
        os.unlink(os.path.join(root, "networking/ftp_ipv6_rfc2428.txt"))

    def tar(self):
        out = os.path.join("../orig", self.orig_tar)
        try:
            os.mkdir("../orig")
        except OSError: pass
        try:
            os.stat(out)
            raise RuntimeError("Destination already exists")
        except OSError: pass
        self.log("Generate tarball %s\n" % out)
        cmdline = ['tar -czf', out, '-C', self.dir, self.orig]
        try:
            if os.spawnv(os.P_WAIT, '/bin/sh', ['sh', '-c', ' '.join(cmdline)]):
                raise RuntimeError("Can't patch source")
            os.chmod(out, 0644)
        except:
            try:
                os.unlink(out)
            except OSError:
                pass
            raise

if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser(usage = "%prog [OPTION]... TAR [PATCH]")
    parser.add_option("-v", "--version", dest = "version", help = "Override version", metavar = "VERSION")
    options, args = parser.parse_args()
    Main(args[0], options.version)()
