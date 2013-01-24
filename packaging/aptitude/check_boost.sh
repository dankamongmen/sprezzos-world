#!/bin/bash -e

# This script is invoked by "make check" to verify that we have
# correctly declared the exact set of Boost include files that
# "configure" should look for.
#
# Currently, this assumes that every occurrence of "boost/foo.hpp" is
# significant.  If stray strings that look like that find their way
# into the source code or configure, we'll have to look at #include
# lines and/or mark off the section of configure with a tag (and,
# e.g., use "awk" instead of "grep").

echo "Verifying that the configure check tests the correct set of header files."

RESULT=0

BOOST_PATTERN='boost/[a-zA-Z0-9_./-]*\.hpp'
# Exclude headers that are only used, e.g., to write configure tests.
BOOST_CONFIGURE_PATTERN="$BOOST_PATTERN"' dnl$'

SRCDIR=${srcdir:-.}

if [ ! -f $SRCDIR/configure.ac ]
then
  echo "Could not locate configure.ac."
  exit 2
fi

# Check that the source code and the configure check look for the same
# Boost headers.
SRC_OCCURRENCES=$((find $SRCDIR/src \( -name \*.cc -or -name \*.h \) -print0; find $SRCDIR/tests \( -name \*.cc -or -name \*.h \) -print0) \
                  | xargs -0 grep -h --only-matching "$BOOST_PATTERN" | sort -u)

if ! grep -h --only-matching "$BOOST_CONFIGURE_PATTERN" $SRCDIR/configure.ac | sed 's/ dnl$//' | sort -c
then
  echo "The list of Boost headers in configure.ac is not sorted."
  RESULT=2
fi

CONFIGURE_OCCURRENCES=$(grep -h --only-matching "$BOOST_CONFIGURE_PATTERN" $SRCDIR/configure.ac | sed 's/ dnl$//' | sort -u)

if ! cmp <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES") >/dev/null
then
    ONLY_SRC=$(comm -2 -3 <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES"))
    ONLY_CONFIG=$(comm -1 -3 <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES"))
    echo "Mismatch in Boost header file declarations."
    if [ -n "$ONLY_SRC" ]
    then
	echo "The following headers occur only in the source code:"
	echo "$ONLY_SRC"
    fi
    if [ -n "$ONLY_CONFIG" ]
    then
	echo "The following headers occur only in the configure script:"
	echo "$ONLY_CONFIG"
    fi

    echo "*** BOOST CONFIGURE CHECK MISMATCH."
    echo "*** Please update the configure script to match the source code."

    RESULT=1
fi

exit $RESULT