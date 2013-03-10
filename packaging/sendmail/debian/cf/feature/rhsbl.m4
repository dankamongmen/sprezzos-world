divert(-1)
#
# Copyright (c) 2002 Derek J. Balling
#	All rights reserved.
#
# Permission to use granted for all purposes. If modifications are made
# they are requested to be sent to <dredd@megacity.org> for inclusion in future
# versions 
#
# Allows (hopefully) for checking of access.db whitelisting now. This ONLY
# works on sendmail-8.12.x ... use on any other version may require tinkering
# by you the downloader.
#
# Incorporates many changes by Sergey S. Mokryshev <mokr@mokr.net>
#
#

divert(0)
ifdef(`_RHSBL_R_',`dnl',`dnl
VERSIONID(`$Id: rhsbl.m4,v 1.4 2002/06/01 14:05:06 dredd Exp $')
define(`_RHSBL_R_',`')
ifdef(`_DNSBL_R_',`dnl',`dnl
LOCAL_CONFIG
# map for DNS based blacklist lookups based on the sender RHS
Kdnsbl host -T<TMP>')')
divert(-1)
define(`_RHSBL_SRV_', `_ARG_')dnl
define(`_RHSBL_MSG_', `ifelse(len(X`'_ARG2_),`1',`"550 Mail from " $`'&{RHS} " refused by blackhole site '_RHSBL_SRV_`"',`_ARG2_')')dnl
define(`_RHSBL_MSG_TMP_', `ifelse(_ARG3_,`t',`"451 Temporary lookup failure of " $`'&{RHS} " at '_RHSBL_SRV_`"',`_ARG3_')')dnl
divert(8)
# DNS based RHS spam list _RHSBL_SRV_
R$+			$: <@> $>CanonAddr $&f
R<@> $*<@$+.>		$: <@> <@$2.> $| $>SearchList <+ rhs> $| <F:$1@$2> <D:$2> <>
R<@> $* $| <$={Accept}>	$: OKSOFAR
R<@> $*<@$+.> $| $*	$: <?> $(dnsbl $2._RHSBL_SRV_. $: OK $) $(macro {RHS} $@ $2 $)
R<@> $*			$: OKSOFAR
R<?> OK			$: OKSOFAR
ifelse(len(X`'_ARG3_),`1',
`R<?>$+<TMP>		$: TMPOK',
`R<?>$+<TMP>		$#error $@ 4.7.1 $: _RHSBL_MSG_TMP_')
R<?>$+			$#error $@ 5.7.1 $: _RHSBL_MSG_
