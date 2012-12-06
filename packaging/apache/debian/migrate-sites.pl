#! /usr/bin/perl

#
# Rename existing sites in $SITES_AVAILABLE to make sure they have a
# .conf suffix. update symlinks in $SITES_ENABLED if necessary
#
# Warning: This script does not work if you didn't use a2ensite/a2dissite to
# manage your sites
#

use strict;
use File::Copy;
use File::Spec;
use File::Basename;

my $SITES_AVAILABLE = "/etc/apache2/sites-available";
my $SITES_ENABLED = "/etc/apache2/sites-enabled";

my %SITES = (
	"$SITES_AVAILABLE" => [],
	"$SITES_ENABLED" => []
);

sub error
{
	my $reason = shift;
	print STDERR "$reason\n";
	exit 1;
}

foreach my $key (keys %SITES)
{
	error("No such directory: $key") unless -d $key;

	opendir(DIR, $key) || error("$key: $!");
	push $SITES{$key}, grep { m#^[^\.]# && $_ !~ m/default|default-ssl/ && $_ !~ m#\.conf$# } readdir(DIR);
	closedir(DIR);
}

foreach my $site (@{ $SITES{$SITES_AVAILABLE} })
{
		print("rename $site -> $site.conf\n");
		my $curname = $SITES_AVAILABLE . "/" . $site;
		my $newname = $curname . ".conf";
		my $curlink = $SITES_ENABLED . "/" . $site;
		my $newlink = $curlink . ".conf";

		if (-e $curname)
		{
			move($curname, $newname) || error("Could not rename file $curname: $!");
			if ( grep { $_ eq $site && -l $SITES_ENABLED . "/" . $_ } @{ $SITES{$SITES_ENABLED} } )
			{
				print("re-enable site: $site as $site.conf\n");
				symlink( File::Spec->abs2rel( $newname,	dirname($newlink)), $newlink ) || error("Could not create link $newlink: $1");
				if ( -l $curlink )
				{
					unlink($curlink)
				}
			}
		}
}

