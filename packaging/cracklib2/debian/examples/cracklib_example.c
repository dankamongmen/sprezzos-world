/*
  copyright:
    Copyright (C) 1998, 1999 Jean Pierre LeJacq <jplejacq@quoininc.com>
    Cleaned up 2003 by Martin Pitt <martin@piware.de>
    Adapted to new upstream 2008 by Jan Dittberner <jan@dittberner.info>

    Distributed under the GNU GENERAL PUBLIC LICENSE.

  description:
    cracklib_example - an example of using cracklib

    Nothing fancy here.  Simply need to call FascistCheck() with the
    potential password and the path+prefix to the dictionary database.
    I'm using the path+prefix, CRACKLIB_DICTPATH, used by the
    utilities in the cracklib-runtime package.  FascistCheck() will
    return non-NULL if the password is not selected.
*/


#include <crack.h>
#include <stdio.h>

int main()
{
    char password[80U] = "";
    char const* msg;

    puts( "Example program using cracklib" );
    printf( "Enter potential password: " );
    scanf( "%79s", password );

    msg = FascistCheck( password, NULL );

    if( msg ) {
	puts( "Please use a different password." );
	printf( "The one you have chosen is unsuitable because:\n %s\n", msg );
    } else
	puts( "Good password." );

    return 0;
}
