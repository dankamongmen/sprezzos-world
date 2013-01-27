// this script gets piped through cpp to sh
// cpp reads DBIXS.h and substitutes DBISTATE_VERSION, sh echoes it out
cat >/dev/null <<END-OF-DBI
#include "../DBIXS.h"
END-OF-DBI

echo DBISTATE_VERSION
