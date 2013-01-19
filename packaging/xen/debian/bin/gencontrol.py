#!/usr/bin/env python

import os, sys
sys.path.append(os.path.join(sys.path[0], "../lib/python"))

from debian_xen.debian import VersionXen
from debian_linux.config import ConfigCoreHierarchy
from debian_linux.debian import Changelog, PackageArchitecture
from debian_linux.gencontrol import Gencontrol as Base
from debian_linux.utils import Templates

class Gencontrol(Base):
    config_schema = {
        'description': {
        }
    }

    def __init__(self):
        super(Gencontrol, self).__init__(ConfigCoreHierarchy(self.config_schema, ["debian/arch"]), Templates(["debian/templates"]))
        self.process_changelog()

    def do_main_setup(self, vars, makeflags, extra):
        makeflags.update({
            'VERSION': self.version.xen_version,
        })

    def do_arch_setup(self, vars, makeflags, arch, extra):
        config_entry = self.config.merge('base', arch)
        config_entry_description = self.config.merge('description', arch)

        for i in (
            ('xen-arch', 'XEN_ARCH'),
        ):
            makeflags[i[1]] = config_entry[i[0]]

    def do_arch_packages(self, packages, makefile, arch, vars, makeflags, extra):
        packages_main = self.process_packages(self.templates["control.main"], vars)
        packages_utils = self.process_packages(self.templates["control.utils"], vars)

        for package in packages_main + packages_utils:
            name = package['Package']
            if name in packages:
                package = packages.get(name)
            else:
                packages.append(package)

            arches = package.setdefault('Architecture', PackageArchitecture())
            if 'all' not in arches:
                arches.add(arch)

        package_utils_name = packages_utils[0]['Package']

        for i in ('postinst', 'prerm', 'lintian-overrides'):
            j = self.substitute(self.templates["xen-utils.%s" % i], vars)
            file("debian/%s.%s" % (package_utils_name, i), 'w').write(j)

        cmds_binary_arch = ["$(MAKE) -f debian/rules.real binary-arch-arch %s" % makeflags]
        cmds_build = ["$(MAKE) -f debian/rules.real build-arch-arch %s" % makeflags]
        cmds_setup = ["$(MAKE) -f debian/rules.real setup-arch %s" % makeflags]
        makefile.add('binary-arch_%s_real' % arch, cmds = cmds_binary_arch)
        makefile.add('build-arch_%s_real' % arch, cmds = cmds_build)
        makefile.add('setup_%s_real' % arch, cmds = cmds_setup)

    def do_flavour_setup(self, vars, makeflags, arch, featureset, flavour, extra):
        config_entry = self.config.merge('base', arch, featureset, flavour)
        config_description = self.config.merge('description', arch, featureset, flavour)

        vars['class'] = config_description['hardware']
        vars['longclass'] = config_description.get('hardware-long') or vars['class']

        for i in (
            ('xen-arch', 'XEN_ARCH'),
        ):
            if config_entry.has_key(i[0]):
                makeflags[i[1]] = config_entry[i[0]]

    def do_flavour_packages(self, packages, makefile, arch, featureset, flavour, vars, makeflags, extra):
        hypervisor = self.templates["control.hypervisor"]
        system_latest = self.templates["control.system.latest"]

        if not vars.has_key('desc'):
            vars['desc'] = ''

        packages_own = []
        packages_own.extend(self.process_packages(hypervisor, vars))
        packages_dummy = self.process_packages(system_latest, vars)

        for package in packages_own + packages_dummy:
            name = package['Package']
            package.setdefault('Architecture', PackageArchitecture()).add(arch)
            if name in packages:
                package = packages.get(name)
            else:
                packages.append(package)

            arches = package.setdefault('Architecture', PackageArchitecture())
            if 'all' not in arches:
                arches.add(arch)

        package_name = packages_own[0]['Package']

        for i in ('postinst', 'postrm'):
            j = self.substitute(self.templates["xen-hypervisor.%s" % i], vars)
            file("debian/%s.%s" % (package_name, i), 'w').write(j)

        cmds_binary_arch = ["$(MAKE) -f debian/rules.real binary-arch-flavour %s" % makeflags]
        cmds_build = ["$(MAKE) -f debian/rules.real build-arch-flavour %s" % makeflags]
        cmds_setup = ["$(MAKE) -f debian/rules.real setup-flavour %s" % makeflags]

        cmds_binary_arch += ["$(MAKE) -f debian/rules.real install-dummy DH_OPTIONS='%s' %s" % (u' '.join([u"-p%s" % i['Package'] for i in packages_dummy]), makeflags)]

        makefile.add("binary-arch_%s_%s_%s" % (arch, featureset, flavour), cmds = cmds_binary_arch)
        makefile.add("build-arch_%s_%s_%s" % (arch, featureset, flavour), cmds = cmds_build)
        makefile.add("setup_%s_%s_%s" % (arch, featureset, flavour), cmds = cmds_setup)

    def process_changelog(self):
        changelog = Changelog(version = VersionXen)
        self.version = changelog[0].version
        self.vars = {
            'version': self.version.xen_version,
        }

if __name__ == '__main__':
    Gencontrol()()
