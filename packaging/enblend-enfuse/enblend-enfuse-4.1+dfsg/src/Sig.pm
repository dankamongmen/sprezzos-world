# name:         Sig.pm
# synopsis:     signature generation
# author:       Dr. Christoph L. Spiel
# perl version: 5.10.0


# This file is part of Enblend.
# Licence details can be found in the file COPYING.


package Sig;

use strict;
use warnings;

use DefaultSig;

our @ISA = qw(DefaultSig);


# See the base class, DefaultSig, for available methods and the
# default definition of signature().

# sub signature {
#     my $self = shift;
#
#     return "My signature ...";
# }


1;
