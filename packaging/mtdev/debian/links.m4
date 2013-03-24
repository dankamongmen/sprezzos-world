divert(-1)

define(`checkdef',`ifdef($1, , `errprint(`error: undefined macro $1
')m4exit(1)')')
define(`errexit',`errprint(`error: undefined macro `$1'
')m4exit(1)')

divert`'dnl
dnl --------------------------------------------------------------------------
ifelse(DIST,`Ubuntu',`dnl
usr/share/apport/package-hooks/source_mtdev.py usr/share/apport/package-hooks/source_utouch.py
usr/share/apport/package-hooks/source_mtdev.py usr/share/apport/package-hooks/source_utouch-grail.py
')dnl DIST
