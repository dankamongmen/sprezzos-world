#
# Remove any whose names that match the given pattern from the
# shlibs:Depends entry in the given substvars.
#

BEGIN { $^W = 1; }
use strict;
my $whoami = ($0 =~ m,([^/\\]*)$,) ? $1 : $0;

die "usage: $whoami substvars-file pattern" unless @ARGV == 2;
my ($file, $pattern) = @ARGV;
if (! -f $file)
{
    exit 0;
}

open(F, "<$file") or die "$whoami: can't open $file: $!\n";
my @in = (<F>);
close(F);
my @out = ();

for (@in)
{
    if (m/(shlibs:Depends=)(.*)/)
    {
	my $prefix = $1;
	my $contents = $2;
	my @items = split(',\s*', $contents);
	my @new = ();
	foreach my $i (@items)
	{
	    $i =~ m/^(\S+)/ or die;
	    my $pkg = $1;
	    if ($pkg !~ m/^${pattern}$/)
	    {
		push(@new, $i);
	    }
	}
	push(@out, $prefix . join(', ', @new) . "\n");
    }
    else
    {
	push(@out, $_);
    }
}

open(F, ">$file") or die "$whoami: can't open $file.new: $!\n";
foreach (@out)
{
    print F $_;
}
close(F);
