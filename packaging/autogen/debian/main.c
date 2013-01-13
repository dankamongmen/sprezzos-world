#include "checkopt.h"

main( int argc, char** argv )
{
  {
    int optct = optionProcess( &checkOptions, argc, argv );
    argc -= optct;
    argv += optct;
  }
  if (ENABLED_OPT( SHOW_DEFS )) {
    int    dirct = STACKCT_OPT( CHECK_DIRS );
    char** dirs  = STACKLST_OPT( CHECK_DIRS );
    while (dirct-- > 0) {
      char* dir = *dirs++;
      /*
     ...
      */
    }
  }
}
