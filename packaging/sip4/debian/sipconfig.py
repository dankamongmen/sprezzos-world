# import the sipconfig.py for the normal or the debug build

import sys

if getattr(sys, "pydebug", False):
    try:
        from sipconfig_d import *
        from sipconfig_d import _pkg_config, _default_macros
    except ImportError, msg:
        raise ImportError, 'No module named sipconfig; package python-sip4-dbg not installed'
else:
    from sipconfig_nd import *
    from sipconfig_nd import _pkg_config, _default_macros
