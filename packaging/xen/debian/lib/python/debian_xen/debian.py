import re
from debian_linux.debian import Version


class VersionXen(Version):
    _version_xen_rules = ur"""
^
(?P<version>
    \d+\.\d+
)
(?:
    \.\d+
    (?:
        ~rc\d+
    )?
    (?:
        \+hg-\d+.[a-z0-9]+
    )?
    |
    ~hg-\d+.[a-z0-9]+
)
-
(?:[^-]+)
$
"""
    _version_xen_re = re.compile(_version_xen_rules, re.X)

    def __init__(self, version):
        super(VersionXen, self).__init__(version)
        match = self._version_xen_re.match(version)
        if match is None:
            raise ValueError("Invalid debian xen version")
        d = match.groupdict()
        self.xen_version = d['version']

