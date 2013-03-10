divert(-1)dnl
#-----------------------------------------------------------------------------
# $Sendmail: ./spamtrap.m4,v 8.12.0 2001/09/24 12:00:00 cowboy Exp $
#
# hack(spamtrap) config file for building Sendmail
#
# Courtesy of jeff@sdsc.edu (Jeff Makey)
# via c.m.s posting Sat, 8 Sep 2001 07:44:26 +0000 (UTC)
#
# Spamtrap addresses go into the access file like this:
#
# To:spamtrap@pandora.orbl.org	POISON
# To:Friend@public.com		POISON
#
#-----------------------------------------------------------------------------
#
divert(0)dnl
LOCAL_CONFIG
# provide access to macros as a map
Kmacro macro

LOCAL_RULESETS
######################################################################
### spamtrap: handle spamtrap(POISON) users
###
######################################################################
#
# This is not only redundant, but fails with feature(`delay_checks')
#
#SLocal_check_mail
# Clear the "poison recipient" indicator macro for this message.
#R$*				$: $(macro {PoisonRecipient} $) $1

SLocal_check_rcpt
#
# Spamtrap addresses go into the access file like this:
#	To:spamtrap@pandora.orbl.org    POISON
#
# Certain recipients are "poison" and cause the
# message to be rejected for all recipients.
R$*				$: $1 $| $1	create workspace to right of $|
R$* $| $* <$+> $*		$1 $| $3	focus on part in angle brackets
R$* $| $+			$: $1 $| $>SearchList <!To> $| <E:$2> <>
R$* $| <POISON>			$: $1 $| $(macro {PoisonRecipient} $@ POISON $)
R$* $| $*			$: $1		delete workspace

Scheck_eoh
R$*				$: $&{PoisonRecipient}
RPOISON				$#error $@ 5.7.1 $: 550 Mail sent to spam lists is not accepted here

#
# With a "To:Friend@public.com POISON" entry in the access file the
# following ruleset will block certain types of spam too:
#
# Certain To headers are sure-fire spam signatures.
HTo: $>CheckTo
SCheckTo
R$+ , $+			$@ $1 , $2	do not try to verify complex addresses
R$*				$: $1 $| $>SearchList <!To> $| <E:$1> <>
R$* $| <POISON>			$#error $@ 5.7.1 $: 550 Mail sent by spamware is not accepted here
R$* $| $*			$: $1		delete lookup result

