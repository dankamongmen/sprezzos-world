package AptPkg::PkgRecords;

require 5.005_62;
use strict;
use warnings;
use AptPkg;

our $VERSION = 1.1;

sub lookup
{
    my ($self, $pack) = @_;
    my $xs = $$self;
    my %extra;
    unless (ref $pack)
    {
	my $p = do {
	    my $_p = $xs->cache->FindPkg($pack)	or return;
	    AptPkg::Cache::Package->new($_p);
	};

	my $v = ($p->{VersionList} || [])->[0]	or return;
	$pack = ($v->{FileList}    || [])->[0]	or return;
	$extra{$_} = $v->{$_} for qw/Section VerStr/;
    }

    my @r = $xs->Lookup($pack->_xs) or return;
    push @r, %extra;
    wantarray ? @r : { @r };
}

1;

__END__

=head1 NAME

AptPkg::PkgRecords - APT package description class

=head1 SYNOPSIS

use AptPkg::PkgRecords;

=head1 DESCRIPTION

The AptPkg::PkgRecords module provides an interface to the parsed
contents of package files.

=head2 AptPkg::PkgRecords

The AptPkg::PkgRecords package Implements the B<APT> pkgRecords class.

An instance of the AptPkg::PkgRecords class may be fetched using the
C<packages> method from an AptPkg::Cache object.

=head3 Methods

=over 4

=item lookup(I<PACK>)

Return a hash (or hash reference, depending on context) for the given
package.

I<PACK> may either be an AptPkg::Cache::VerFile object, or a package
name.

The hash contains the following keys:

=over 4

C<FileName>, C<MD5Hash>, C<SourcePkg>, C<Maintainer>, C<ShortDesc>,
C<LongDesc> and C<Name>.

=back

with values taken from the packages file.  If I<PACK> is a package
name, these additional values are set:

=over 4

C<Section> and C<VerStr>.

=back

=back

=head1 SEE ALSO

AptPkg::Cache(3pm), AptPkg(3pm).

=head1 AUTHOR

Brendan O'Dea <bod@debian.org>

=cut
