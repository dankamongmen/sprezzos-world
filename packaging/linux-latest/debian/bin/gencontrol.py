#!/usr/bin/python

import sys
sys.path.append(sys.argv[1] + "/lib/python")

from debian_linux.config import ConfigCoreDump
from debian_linux.debian import Changelog, PackageDescription, VersionLinux
from debian_linux.gencontrol import Gencontrol as Base
from debian_linux.utils import Templates

import os.path, re, codecs

class Gencontrol(Base):
    def __init__(self, config):
        super(Gencontrol, self).__init__(ConfigCoreDump(fp = file(config)), Templates(["debian/templates"]))

        config_entry = self.config['version',]
        self.version = VersionLinux(config_entry['source'])
        self.abiname = config_entry['abiname']
        self.vars = {
            'upstreamversion': self.version.linux_upstream,
            'version': self.version.linux_version,
            'source_upstream': self.version.upstream,
            'abiname': self.abiname,
        }

        changelog_version = Changelog()[0].version
        self.package_version = u'%s+%s' % (self.version.linux_version, changelog_version.complete)

    def do_main_setup(self, vars, makeflags, extra):
        makeflags['GENCONTROL_ARGS'] = '-v%s' % self.package_version

    def do_main_packages(self, packages, vars, makeflags, extra):
        packages['source']['Build-Depends'].extend(
            [u'linux-support-%s' % self.abiname]
        )

        latest_source = self.templates["control.source.latest"]
        packages.extend(self.process_packages(latest_source, vars))

        latest_doc = self.templates["control.doc.latest"]
        packages.extend(self.process_packages(latest_doc, vars))

        latest_tools = self.templates["control.tools.latest"]
        packages.extend(self.process_packages(latest_tools, vars))

    def do_flavour_packages(self, packages, makefile, arch, featureset, flavour, vars, makeflags, extra):
        if self.version.linux_modifier is None:
            try:
                vars['abiname'] = u'-%s' % self.config['abi', arch]['abiname']
            except KeyError:
                vars['abiname'] = self.abiname
            makeflags['ABINAME'] = vars['abiname']

        config_base = self.config.merge('base', arch, featureset, flavour)
        config_description = self.config.merge('description', arch, featureset, flavour)
        config_image = self.config.merge('image', arch, featureset, flavour)

        vars['flavour'] = vars['localversion'][1:]
        vars['class'] = config_description['hardware']
        vars['longclass'] = config_description.get('hardware-long') or vars['class']

        templates = []

        templates.extend(self.templates["control.image.latest.type-standalone"])
        if self.config.get_merge('build', arch, featureset, flavour,
                                 'modules', True):
            templates.extend(self.templates["control.headers.latest"])

        image_fields = {'Description': PackageDescription()}

        desc_parts = self.config.get_merge('description', arch, featureset, flavour, 'parts')
        if desc_parts:
            # XXX: Workaround, we need to support multiple entries of the same name
            parts = list(set(desc_parts))
            parts.sort()
            desc = image_fields['Description']
            for part in parts:
                desc.append(config_description['part-long-' + part])
                desc.append_short(config_description.get('part-short-' + part, ''))

            if u'xen' in desc_parts:
                templates.extend(self.templates["control.xen-linux-system.latest"])

        packages_dummy = []

        packages_dummy.append(self.process_real_image(templates[0], image_fields, vars))
        packages_dummy.extend(self.process_packages(templates[1:], vars))

        for package in packages_dummy:
            name = package['Package']
            if packages.has_key(name):
                package = packages.get(name)
                package['Architecture'].add(unicode(arch))
            else:
                package['Architecture'] = unicode(arch)
                packages.append(package)

        makeflags['GENCONTROL_ARGS'] = '-v%s' % self.package_version

        cmds_binary_arch = []
        for i in packages_dummy:
            cmds_binary_arch += self.get_link_commands(i, ['NEWS'])
        cmds_binary_arch += ["$(MAKE) -f debian/rules.real install-dummy DH_OPTIONS='%s' %s" % (u' '.join([u"-p%s" % i['Package'] for i in packages_dummy]), makeflags)]
        makefile.add('binary-arch_%s_%s_%s_real' % (arch, featureset, flavour), cmds = cmds_binary_arch)

        for i in packages_dummy:
            if i['Package'].startswith(u'linux-image-'):
                bug_presubj = self.substitute(
                    self.templates["bug-presubj.image.latest"], vars)
                codecs.open("debian/%s.bug-presubj" % i['Package'], 'w', 'utf-8').write(bug_presubj)

    def do_extra(self, packages, makefile):
        templates_extra = self.templates["control.extra"]

        packages.extend(self.process_packages(templates_extra, {}))
        extra_arches = {}
        for package in templates_extra:
            arches = package['Architecture']
            for arch in arches:
                i = extra_arches.get(arch, [])
                i.append(package)
                extra_arches[arch] = i
        archs = extra_arches.keys()
        archs.sort()
        for arch in archs:
            cmds = []
            for i in extra_arches[arch]:
                if i.has_key(u'X-Version-Overwrite-Epoch'):
                    version = u'-v1:%s' % self.package_version
                else:
                    version = u'-v%s' % self.package_version
                cmds += self.get_link_commands(i, ['config', 'postinst', 'templates'])
                cmds.append("$(MAKE) -f debian/rules.real install-dummy ARCH='%s' DH_OPTIONS='-p%s' GENCONTROL_ARGS='%s'" % (arch, i['Package'], version))
            makefile.add('binary-arch_%s' % arch, [u'binary-arch_%s_extra' % arch])
            makefile.add("binary-arch_%s_extra" % arch, cmds = cmds)

    def process_real_image(self, entry, fields, vars):
        entry = self.process_package(entry, vars)
        for key, value in fields.iteritems():
            if key in entry:
                real = entry[key]
                real.extend(value)
            elif value:
                entry[key] = value
        return entry

    @staticmethod
    def get_link_commands(package, names):
        cmds = []
        for name in names:
            match = re.match(ur'^(linux-\w+)(-2.6)?(-.*)$', package['Package'])
            if not match:
                continue
            if match.group(2):
                source = 'debian/%s%s.%s' % (match.group(1), match.group(3),
                                             name)
            else:
                source = None
            if not (source and os.path.isfile(source)):
                source = 'debian/%s.%s' % (match.group(1), name)
            dest = 'debian/%s.%s' % (package['Package'], name)
            if (os.path.isfile(source) and
                (not os.path.isfile(dest) or os.path.islink(dest))):
                cmds.append('ln -sf %s %s' %
                            (os.path.relpath(source, 'debian'), dest))
        return cmds

if __name__ == '__main__':
    Gencontrol(sys.argv[1] + "/config.defines.dump")()
