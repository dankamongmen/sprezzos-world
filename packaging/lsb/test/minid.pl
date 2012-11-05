#!/usr/bin/perl -w
# detach from tty
main:
$pid = fork;
exit if $pid;
die "fork: $!" unless defined $pid;
print "continue in child";
sleep(4000);
