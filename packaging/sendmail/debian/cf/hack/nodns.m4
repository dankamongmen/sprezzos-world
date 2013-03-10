divert(-1)dnl
#-----------------------------------------------------------------------------
# $Sendmail: ./nodns.m4,v 8.12.0 2001/08/24 12:00:00 cowboy Exp $
#
# Copyright (c) 1999-2001 Richard Nelson.  All Rights Reserved.
#
# hack/nodns.m4 m4 file for omitting DNS queries
#
# If you've a better idea, please let me know
#
#-----------------------------------------------------------------------------
divert(0)dnl
VERSIONID(`$Id: nodns.m4,v @sm_version@@sm_revision@ @sm_date@ @sm_time@ cowboy Exp $')
undefine(`confBIND_OPTS')dnl
define(`confSERVICE_SWITCH_FILE', `/etc/mail/service.switch-nodns')dnl
define(`confDONT_PROBE_INTERFACES', `True')dnl
FEATURE(nocanonify)dnl
