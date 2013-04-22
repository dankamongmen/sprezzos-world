# <Magick.pm> - ImageMagick compatibility wrapper for GraphicsMagick
# 
# This package exports GraphicsMagick Perl bindings in the traditional
# ImageMagick/PerlMagick namespace, which allows Perl scripts to change
# from ImageMagick to GraphicsMagick without any code changes.
# 
# Copyright (c) 2006 Daniel Kobras <kobras@debian.org>
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package Image::Magick;

use vars (@ISA);

require Graphics::Magick;

@ISA = qw(Graphics::Magick);

1;
__END__

=head1 NAME

Image::Magick - call GraphicsMagick routines with ImageMagick interface

=head1 SYNOPSIS

use Image::Magick

=head1 DESCRIPTION

This Perl wrapper provides GraphicsMagick bindings in the namespace
traditionally used by ImageMagick's PerlMagick extension. It is used
to turn GraphicsMagick into a drop-in replacement for ImageMagick in
Perl scripts. It is recommended that scripts are ported to use
GraphicsMagick natively instead of using this compatibility wrapper.

GraphicsMagick was forked from ImageMagick version 5.5.2, and has since been
developed with an emphasis on interface stability. Therefore, Perl scripts
supporting ImageMagick version 5.5.2 can usually be ported to GraphicsMagick
with little effort.

=head1 AUTHOR

Daniel Kobras <kobras@debian.org>

=head1 SEE ALSO

Graphics::Magick(3pm), perl(1).

=cut
