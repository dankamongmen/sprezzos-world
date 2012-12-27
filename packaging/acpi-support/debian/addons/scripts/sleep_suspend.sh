#!/bin/sh

# This script HANDLES the sleep or suspend button (does not TRANSLATE it). It
# is part of the *suspend* side of acpi-support, not the special keys
# translation side. If this script is called, it is assumed to be the result of
# a suspend key press that can also be heard by other parts of the system. The
# only time that it actually does something is when it is determined that no
# other parts of the system are listening (this is what the CheckPolicy call
# does).

test -f /usr/share/acpi-support/key-constants || exit 0

. /etc/default/acpi-support
. /usr/share/acpi-support/policy-funcs

CheckPolicy && exit 0

[ x$1 != xsleep ] || [ x$ACPI_SLEEP = xtrue ] || exit 0

[ x$1 != xsuspend ] || [ x$ACPI_HIBERNATE = xtrue ] || exit 0

if [ x$LOCK_SCREEN = xtrue ]; then
	. /usr/share/acpi-support/screenblank
fi

if [ x$1 = xsleep ]; then
	pm-suspend
elif [ x$1 = xsuspend ]; then
	pm-hibernate
fi

