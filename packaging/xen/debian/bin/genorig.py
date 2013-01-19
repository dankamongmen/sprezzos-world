#!/usr/bin/env python

import sys
sys.path.append(sys.path[0] + '/../lib/python')

import itertools
import os, os.path
import shutil
import subprocess

from debian_xen.debian import VersionXen
from debian_linux.debian import Changelog


class RepoHg(object):
    def __init__(self, repo, options):
        self.repo = repo
        self.tag = options.tag or 'tip'

    def do_archive(self, info):
        orig_dir = os.path.join(info.temp_dir, info.orig_dir)
        args = ('hg', 'archive', '-r', self.tag, os.path.realpath(orig_dir))
        subprocess.check_call(args, cwd=self.repo)


class RepoGit(object):
    def __init__(self, repo, options):
        self.repo = repo
        self.tag = options.tag or 'HEAD'

    def do_archive(self, info):
        temp_tar = os.path.join(info.temp_dir, 'orig.tar')
        args = ('git', 'archive', '--prefix', '%s/' % info.orig_dir, '-o', os.path.realpath(temp_tar), self.tag)
        subprocess.check_call(args, cwd=self.repo)
        subprocess.check_call(('tar', '-C', info.temp_dir, '-xf', temp_tar))


class Main(object):
    log = sys.stdout.write

    def __init__(self, options, repo):
        self.options = options

        self.changelog_entry = Changelog(version=VersionXen)[0]
        self.source = self.changelog_entry.source

        if options.version:
            self.version = options.version
        else:
            raise NotImplementedError

        if os.path.exists(os.path.join(repo, '.hg')):
            self.repo = RepoHg(repo, options)
        elif os.path.exists(os.path.join(repo, '.git')):
            self.repo = RepoGit(repo, options)
        else:
            raise NotImplementedError

        if options.component:
            self.orig_dir = options.component
            self.orig_tar = '%s_%s.orig-%s.tar.gz' % (self.source, self.version, options.component)
        else:
            self.orig_dir = '%s-%s' % (self.source, self.version)
            self.orig_tar = '%s_%s.orig.tar.gz' % (self.source, self.version)

    def __call__(self):
        import tempfile
        self.temp_dir = tempfile.mkdtemp(prefix='genorig', dir='debian')
        try:
            self.do_archive()
            self.do_tar()
        finally:
            shutil.rmtree(self.temp_dir)

    def do_archive(self):
        self.log("Create archive.\n")
        self.repo.do_archive(self)

    def do_tar(self):
        out = "../orig/%s" % self.orig_tar
        self.log("Generate tarball %s\n" % out)

        try:
            os.stat(out)
            raise RuntimeError("Destination already exists")
        except OSError: pass

        subprocess.check_call(('tar', '-C', self.temp_dir, '-czf', out, self.orig_dir))


if __name__ == '__main__':
    from optparse import OptionParser
    p = OptionParser(prog=sys.argv[0], usage='%prog [OPTION]... DIR')
    p.add_option('-c', '--component', dest='component')
    p.add_option('-t', '--tag', dest='tag')
    p.add_option('-v', '--version', dest='version')
    options, args = p.parse_args()
    if len(args) != 1:
        raise RuntimeError
    Main(options, *args)()
