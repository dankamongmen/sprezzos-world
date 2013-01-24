dnl Process this file with autoconf to produce a configure script.
AC_INIT([aptitude], [0.6.9.1], [aptitude-devel@lists.alioth.debian.org])
AC_CONFIG_SRCDIR([src/main.cc])
AM_INIT_AUTOMAKE([dist-xz no-dist-gzip -Wno-portability])
AM_CONFIG_HEADER(config.h)

dnl Use C++
AC_LANG([C++])

ALL_LINGUAS="ar ast bs ca cs da de dz el es eu fi fr gl hu hr it ja km ku lt mr nb ne nl nn pl pt pt_BR ro ru sk sv th tl tr uk vi zh_CN zh_TW"
AM_GNU_GETTEXT([external])

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB
dnl Checks for programs used to generate the documentation.
AC_PATH_PROG(PO4A, po4a)
AC_PATH_PROG(XSLTPROC, xsltproc)
AC_PATH_PROG(RSVG_CONVERT, rsvg-convert)
AC_PATH_PROG(FOP, fop)

AM_CONDITIONAL(HAVE_PO4A, [test "x$PO4A" != "x"])
AM_CONDITIONAL(HAVE_XSLTPROC, [test "x$XSLTPROC" != "x"])
AM_CONDITIONAL(HAVE_RSVG_CONVERT, [test "x$RSVG_CONVERT" != "x"])
AM_CONDITIONAL(HAVE_FOP, [test "x$FOP" != "x"])

dnl C++ has const and inline!!!
ac_cv_c_const=yes
ac_cv_c_inline=yes

dnl Check for libraries only if compiling the program.
AS_IF([test "x$enable_aptitude" != xno],
  [

dnl Checks for libraries.
AC_CHECK_LIB(ncursesw, initscr, , [AC_MSG_ERROR([Can't find libncursesw -- please install libncursesw5-dev])])
AC_CHECK_LIB(apt-pkg, main, , [AC_MSG_ERROR([Can't find the APT libraries -- please install libapt-pkg-dev])])

AC_MSG_CHECKING([whether apt includes the automatic dependency removal patch (required)])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <apt-pkg/depcache.h>]],
                                   [[extern pkgDepCache::ActionGroup d;]])],
		  AC_MSG_RESULT([yes]),
		  AC_MSG_FAILURE([need libapt-pkg 0.7 or later]))

AC_CHECK_MEMBER([struct pkgCache::Version.MultiArch], [],
                [AC_MSG_ERROR([need libapt-pkg 0.8 or later])],
                [[#include <apt-pkg/pkgcache.h>]])

AC_CHECK_LIB(pthread, main,
	HAVE_LIBPTHREAD=1
	, [AC_MSG_ERROR([Can't find the POSIX thread libraries])])

AC_PATH_PROG(XAPIAN_CONFIG, xapian-config)
if test ! -x "$XAPIAN_CONFIG"
then
  AC_MSG_ERROR([Can't find xapian-config -- please install libxapian-dev])
fi
XAPIAN_CXXFLAGS="$($XAPIAN_CONFIG --cxxflags)"
XAPIAN_LIBS="$($XAPIAN_CONFIG --libs)"

OLD_CXXFLAGS="$CXXFLAGS"
OLD_LIBS="$LIBS"

CXXFLAGS="$CXXFLAGS $XAPIAN_CXXFLAGS"
LIBS="$LIBS $XAPIAN_LIBS"
AC_CHECK_HEADER([xapian.h], [],
    [AC_MSG_ERROR([Can't find the xapian headers -- please install libxapian-dev])])
CXXFLAGS="$OLD_CXXFLAGS"
LIBS="$OLD_LIBS"

########################################################################

PKG_CHECK_MODULES(SIGC, sigc++-2.0)

PKG_CHECK_MODULES(CWIDGET, cwidget)

dnl Check for Boost headers.  Place each one on a line by itself and
dnl write "dnl" at the end: the "dnl" deletes the newline, and without
dnl it you'll get an error when you run the configure script.
AC_CHECK_HEADERS( dnl
boost/algorithm/string.hpp dnl
boost/algorithm/string/join.hpp dnl
boost/array.hpp dnl
boost/compressed_pair.hpp dnl
boost/enable_shared_from_this.hpp dnl
boost/flyweight/hashed_factory.hpp dnl
boost/flyweight.hpp dnl
boost/format.hpp dnl
boost/functional/hash.hpp dnl
boost/function.hpp dnl
boost/fusion/adapted/mpl.hpp dnl
boost/fusion/algorithm/iteration/fold.hpp dnl
boost/fusion/algorithm/iteration/for_each.hpp dnl
boost/fusion/algorithm/transformation/clear.hpp dnl
boost/fusion/algorithm/transformation/join.hpp dnl
boost/fusion/algorithm/transformation/push_back.hpp dnl
boost/fusion/algorithm/transformation/push_front.hpp dnl
boost/fusion/container/generation/make_vector.hpp dnl
boost/fusion/container/list.hpp dnl
boost/fusion/container/vector/convert.hpp dnl
boost/fusion/container/vector.hpp dnl
boost/fusion/functional/adapter/fused.hpp dnl
boost/fusion/include/join.hpp dnl
boost/fusion/include/mpl.hpp dnl
boost/fusion/include/sequence.hpp dnl
boost/fusion/iterator/equal_to.hpp dnl
boost/fusion/sequence.hpp dnl
boost/fusion/support/is_sequence.hpp dnl
boost/iostreams/copy.hpp dnl
boost/iostreams/device/file.hpp dnl
boost/iostreams/filter/gzip.hpp dnl
boost/iostreams/filtering_stream.hpp dnl
boost/iostreams/invert.hpp dnl
boost/iostreams/operations.hpp dnl
boost/iostreams/read.hpp dnl
boost/iostreams/seek.hpp dnl
boost/iostreams/stream.hpp dnl
boost/iostreams/write.hpp dnl
boost/lambda/bind.hpp dnl
dnl Can't check this because of bugs in Boost, left here to fool my
dnl sanity-check of the config file:
dnl boost/lambda/construct.hpp dnl
boost/lambda/lambda.hpp dnl
boost/lexical_cast.hpp dnl
boost/make_shared.hpp dnl
boost/mpl/begin.hpp dnl
boost/mpl/fold.hpp dnl
boost/mpl/front.hpp dnl
boost/mpl/transform.hpp dnl
boost/multi_index_container.hpp dnl
boost/multi_index/hashed_index.hpp dnl
boost/multi_index/member.hpp dnl
boost/multi_index/mem_fun.hpp dnl
boost/multi_index/random_access_index.hpp dnl
boost/multi_index/sequenced_index.hpp dnl
boost/numeric/conversion/cast.hpp dnl
boost/optional.hpp dnl
boost/preprocessor/repetition/enum_binary_params.hpp dnl
boost/preprocessor/repetition/enum_params.hpp dnl
boost/preprocessor/repetition/repeat.hpp dnl
boost/random.hpp dnl
boost/range.hpp dnl
boost/ref.hpp dnl
boost/scoped_array.hpp dnl
boost/scoped_ptr.hpp dnl
boost/shared_ptr.hpp dnl
boost/test/unit_test.hpp dnl
boost/type_traits/is_same.hpp dnl
boost/unordered_map.hpp dnl
boost/unordered_set.hpp dnl
boost/utility.hpp dnl
boost/variant.hpp dnl
boost/weak_ptr.hpp dnl
                  , []
                  , [AC_MSG_FAILURE([Boost install not found, too old, or incomplete; install libboost-dev.])])

AX_BOOST_BASE()
AX_BOOST_IOSTREAMS

AC_MSG_CHECKING([the calling convertion of boost::fusion::fold])
# Test the order of arguments to the function passed into fold; this
# was randomly changed in Boost version 1.42 (because no-one needs a
# stable API, or something).
#
# For the test to work at compile-time, the types of the arguments to
# fold must be incompatible; the actual operations performed are
# irrelevant.  My fold operator takes an int and a class, and returns
# the empty string.
AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <boost/fusion/algorithm/iteration/fold.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/include/fold.hpp>
#include <boost/fusion/include/vector.hpp>

struct T
{
};

struct F
{
  T operator()(const T &t, int n) const
  {
    return T();
  }
  typedef T result_type;
};

int main(int argc, char **argv)
{
  boost::fusion::vector<int, int, int> v(5, 3, 1);
  boost::fusion::fold(v, T(), F());
}])],
                 [
                   AC_MSG_RESULT([State first, then value])
                   AC_DEFINE([BOOST_FUSION_FOLD_STATE_BEFORE_VALUE], [],
                             [Define if boost::fusion::fold passes the state as the first argument to the fold function.])
                 ],
                 [
                   AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <boost/fusion/algorithm/iteration/fold.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/include/fold.hpp>
#include <boost/fusion/include/vector.hpp>

struct T
{
};

struct F
{
  T operator()(int n, const T &t) const
  {
    return T();
  }
  typedef T result_type;
};

int main(int argc, char **argv)
{
  boost::fusion::vector<int, int, int> v(5, 3, 1);
  boost::fusion::fold(v, T(), F());
}
                   ])],
                   [AC_MSG_RESULT([Value first, then state])],
                   [AC_MSG_ERROR([Can't figure out how to invoke boost::fusion::fold])])
                 ])


AC_MSG_CHECKING([the name of the runtime library for Boost.Test])


##### Boost.Test check #####
OLDLIBS="$LIBS"
for libname in boost_unit_test_framework-mt boost_unit_test_framework
do
  LIBS="$LIBS -l$libname"

  AC_LINK_IFELSE([AC_LANG_SOURCE([
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_NO_MAIN

#include <boost/test/unit_test.hpp>

bool init_unit_test()
{
  return true;
}

int main(int argc, char **argv)
{
  return boost::unit_test::unit_test_main(init_unit_test, argc, argv);
}
])],
        BOOST_UNIT_TEST_LIBS="-l$libname")

  LIBS="$OLDLIBS"
done

if test -z "$BOOST_UNIT_TEST_LIBS"
then
  # TODO: just disable the unit tests?
  AC_MSG_FAILURE([Can't find the Boost unit test library; install libboost-test-dev.])
else
  AC_MSG_RESULT(["$BOOST_UNIT_TEST_LIBS"])
fi

AC_SUBST(BOOST_UNIT_TEST_LIBS)

##### End Boost.Test check #####

##### Check for Google Mock #####

dnl Google Mock's developers do not support prebuilt libraries (e.g.,
dnl they pay no attention to ABI compatibility) and as a result have
dnl asked Debian not to ship prebuilt libraries.  So, if we are
dnl running on a system where gmock isn't available in $LIBDIR, we
dnl guess that maybe there's source available that we can compile
dnl during our own build process.

AC_MSG_CHECKING([how to link gmock])

BUILD_LOCAL_GMOCK=0

OLD_LIBS="$LIBS"
LIBS="$LIBS -lgmock"

AC_LINK_IFELSE([AC_LANG_SOURCE([
#include <gmock/gmock.h>

int main(int argc, char **argv)
{
  return 0;
}])],
        [AC_MSG_RESULT([gmock is in in library path])]
        ,
        dnl Ok, check whether we can link it if we include the gmock
        dnl source code.  Note that it will require both pthreads and
        dnl gtest; no way around that.
        [LIBS="$OLD_LIBS -lpthread"
         OLD_CPPFLAGS="$CPPFLAGS"
         CPPFLAGS="$CPPFLAGS -I/usr/src/gtest -I/usr/src/gmock"
         AC_LINK_IFELSE([AC_LANG_SOURCE([
#include <src/gtest-all.cc>
#include <src/gmock-all.cc>

int main(int argc, char **argv)
{
  return 0;
}])],
           [AC_MSG_RESULT([source is in /usr/src/gmock])
            BUILD_LOCAL_GMOCK=1],
           [AC_MSG_ERROR([Can't figure out where Google Mock lives; either install the google-mock package or place the library in the link path])])
         CPPFLAGS="$OLD_CPPFLAGS"
         LIBS="$OLD_LIBS"])

##### End Google Mock check #####

PKG_CHECK_MODULES(SQLITE3, sqlite3)

  ]) dnl Check for libraries only if compiling the program.

AM_CONDITIONAL([BUILD_LOCAL_GMOCK], [test x$BUILD_LOCAL_GMOCK = x1])

########################################################################

WANT_HAVE_EPT=0
AC_ARG_ENABLE(ept,
	AS_HELP_STRING(--enable-ept, [enable use of ept library]),
	[if test x$enableval = xyes
	 then
	     WANT_HAVE_EPT=1
	 fi])

HAVE_EPT=0
AS_IF(
  [test x$WANT_HAVE_EPT = x1 && test "x$enable_aptitude" != xno],
  [HAVE_EPT=1
   PKG_CHECK_MODULES([ept], [libept], [],
     [AC_MSG_ERROR([Can't find the ept library -- please install libept-dev])])

   OLD_CXXFLAGS="$CXXFLAGS"

   CXXFLAGS="$CXXFLAGS $ept_CFLAGS"

   DEBTAGS_OK=0

   AC_MSG_CHECKING([whether ept::debtags::getTagsOfItem returns a set of strings.])
   AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  // This would always crash, but we only care if it type-checks.
  ept::debtags::Debtags DB;
  std::set<std::string> result = DB.getTagsOfItem("foo");

  return 0;
}])],
        [
        AC_MSG_RESULT(yes)
        AC_DEFINE([EPT_DEBTAGS_GETTAGSOFITEM_RETURNS_STRINGS], [],
            [Define if ept::debtags::Debtags::getTagsOfItem returns a set of strings])
        DEBTAGS_OK=1
        ],
        [
        AC_MSG_RESULT(no)
        ])

    if test x$DEBTAGS_OK = x0
    then
      AC_MSG_FAILURE([Can't figure out how to access the ept debtags database.])
    fi

    AC_MSG_CHECKING([whether ept::debtags::Vocabulary::tagData exists and returns ept::debtags::voc::TagData *])

    AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <ept/debtags/vocabulary.h>

int main(int argc, char **argv)
{
  ept::debtags::Vocabulary vocabulary;
  const ept::debtags::voc::TagData *td = vocabulary.tagData("foo");

  // Use td so the compiler doesn't complain:
  return td == NULL ? 1 : 0;
}])],
        [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_EPT_DEBTAGS_VOCABULARY_TAG_DATA], [], [Define if the ept debtags vocabulary object exists and supports tagData()])
        ],
        [AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([whether ept::debtags::Vocabulary::facetData exists and returns ept::debtags::voc::FacetData *])

    AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <ept/debtags/vocabulary.h>

int main(int argc, char **argv)
{
  ept::debtags::Vocabulary vocabulary;
  const ept::debtags::voc::FacetData *fd = vocabulary.facetData("foo");

  // Use fd so the compiler doesn't complain:
  return fd == NULL ? 1 : 0;
}])],
        [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_EPT_DEBTAGS_VOCABULARY_FACET_DATA], [], [Define if the ept debtags vocabulary object exists and supports facetData()])
        ],
        [AC_MSG_RESULT([no])])

   CXXFLAGS="$OLD_CXXFLAGS"
  ],

  [test "x$enable_aptitude" != xno],
  [AC_MSG_NOTICE([The ept library is not being used; use --enable-ept to enable it.])])

if test x$HAVE_EPT = x1
then
  AC_DEFINE([HAVE_EPT], [], [Define if the ept library is to be used.])
fi

########################################################################

WANT_HAVE_GTK=0
AC_ARG_ENABLE(gtk,
	AS_HELP_STRING(--enable-gtk, [compile the GTK+ frontend]),
	[if test x$enableval = xyes
	 then
	     if test "x$enable_aptitude" = xno
	     then
		 AC_MSG_ERROR([Can not build the GTK+ frontend because --disable-aptitude.])
	     fi
	     WANT_HAVE_GTK=1
	 else
	     WANT_HAVE_GTK=
	 fi])


dnl We always test all the libraries, even if some of them fail, so
dnl that the user gets to see which libraries are needed for GTK+.
HAVE_GTK=0
AS_IF(
 [test x$WANT_HAVE_GTK = x1],
 [AC_MSG_NOTICE([Testing for the libraries needed to build the GTK+ frontend.])

  HAVE_GTK=1
  PKG_CHECK_MODULES(GLIBMM, glibmm-2.4,,[HAVE_GTK=])
  PKG_CHECK_MODULES(GTHREAD, gthread-2.0,,[HAVE_GTK=])
  PKG_CHECK_MODULES(GTKMM, gtkmm-2.4,,[HAVE_GTK=])
  PKG_CHECK_MODULES(LIBGLADEMM, libglademm-2.4,,[HAVE_GTK=])
  PKG_CHECK_MODULES(VTE, vte,,[HAVE_GTK=])

  AS_IF([test x$HAVE_GTK != x1],
        [AC_MSG_ERROR([Unable to find the necessary GTK+ libraries.])],
        [AC_MSG_NOTICE([The GTK+ frontend will be built.])])],

 [test "x$enable_aptitude" != xno],
 [AC_MSG_NOTICE([GTK+ frontend not enabled; use --enable-gtk to enable it.])])

AM_CONDITIONAL([BUILD_GTK], [test x$HAVE_GTK = x1])
if test x$HAVE_GTK = x1
then
  AC_DEFINE([HAVE_GTK], [], [Define if the GTK+ frontend is included in the build.])
fi

CXXFLAGS="$CXXFLAGS $SIGC_CFLAGS $CWIDGET_CFLAGS $ept_CFLAGS $SQLITE3_CFLAGS $XAPIAN_CXXFLAGS"
LIBS="$LIBS $SIGC_LIBS $CWIDGET_LIBS $ept_LIBS $SQLITE3_LIBS $BOOST_IOSTREAMS_LIB $XAPIAN_LIBS"
CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"

if test x$HAVE_GTK = x1
then
  CXXFLAGS="$CXXFLAGS $GLIBMM_CFLAGS $GTHREAD_CFLAGS $GTKMM_CFLAGS $LIBGLADEMM_CFLAGS $VTE_CFLAGS"
  LIBS="$LIBS $GLIBMM_LIBS $GTHREAD_LIBS $GTKMM_LIBS $LIBGLADEMM_LIBS $VTE_LIBS"
fi


AC_DEFINE_UNQUOTED(SIGC_VERSION, ["$(pkg-config --modversion sigc++-2.0)"], [The version of libsigc++ with which the program was compiled])

########################################################################

WANT_HAVE_QT=0
AC_ARG_ENABLE(qt,
  AS_HELP_STRING(--enable-qt, [compile the Qt frontend]),
  [if test x$enableval = xyes
  then
    if test "x$enable_aptitude" = xno
    then
      AC_MSG_ERROR([Can not build the Qt frontend because --disable-aptitude.])
    fi
    WANT_HAVE_QT=1
  else
    WANT_HAVE_QT=
  fi])

HAVE_QT=0
AS_IF(
  [test x$WANT_HAVE_QT = x1],
  [AC_MSG_NOTICE([Testing for the libraries needed to build the Qt frontend.])

  # required version of Qt libraries
  QTCORE_REQUIRED=4.6.0
  QTGUI_REQUIRED=4.6.0

  HAVE_QT=1
  PKG_CHECK_MODULES(QtCore, QtCore >= $QTCORE_REQUIRED, ,[HAVE_QT=])
  PKG_CHECK_MODULES(QtGui, QtGui >= $QTGUI_REQUIRED, ,[HAVE_QT=])

  # test for qt4-moc
  AC_PATH_PROG(MOC, moc-qt4)
  if test ! -x "$MOC"; then
    AC_PATH_PROG(MOC, moc)
    if test ! -x "$MOC"; then
      AC_MSG_WARN([Qt MOC hasn't been found.])
      HAVE_QT=0
    else
      AC_MSG_NOTICE([Qt MOC has been found.])
    fi
  fi

  AS_IF([test x$HAVE_QT != x1],
        [AC_MSG_ERROR([Unable to find the necessary Qt libraries.])],
        [AC_MSG_NOTICE([The Qt frontend will be built.])])],

  [test "x$enable_aptitude" != xno],
  [AC_MSG_NOTICE([Qt frontend not enabled; use --enable-qt to enable it.])])

AM_CONDITIONAL([BUILD_QT], [test x$HAVE_QT = x1])
if test x$HAVE_QT = x1
then
  AC_DEFINE([HAVE_QT], [], [Define if the Qt frontend is included in the build.])
fi

if test x$HAVE_QT = x1
then
  CXXFLAGS="$CXXFLAGS -DQT_NO_KEYWORDS $QtCore_CFLAGS $QtGui_CFLAGS"
  LIBS="$LIBS $QtCore_LIBS $QtGui_LIBS"
fi

########################################################################


dnl Checks for header files.
dnl Check for header files only if compiling the program.
AS_IF([test "x$enable_aptitude" != xno],
  [

AC_HEADER_STDC

AC_CHECK_HEADERS(fcntl.h sys/ioctl.h unistd.h, , [AC_MSG_ERROR([Can't find needed header file.])])

AC_CHECK_HEADER(apt-pkg/init.h, , [AC_MSG_ERROR([Can't find the APT header files -- please install libapt-pkg-dev])])

AC_CHECK_HEADER(execinfo.h, [AC_DEFINE([HAVE_EXECINFO_H], [], [Define if the execinfo file (for self-backtracing) is available])])

AC_CHECK_HEADER(locale.h, [AC_DEFINE([HAVE_LOCALE_H], [], [Define if locale.h is available])])

AC_CHECK_DECL(setlocale, [AC_DEFINE([HAVE_SETLOCALE], [], [Define if setlocale is available in locale.h])], , [#include <locale.h>])

dnl All the hash-related stuff is commented out because g++ 4.3 will
dnl blow us up if we ever include hash_map -- yet it's present and I
dnl can't make the autoconf test fail.  The replacement is
dnl unordered_map, which is not just a nonstandard header: it also
dnl requires language extensions in order to work.  So, since aptitude
dnl has no real need for hashes anyway, I'm just removing all the
dnl hashes that it uses.

dnl HASH_INC=""
dnl HASH_CLS=""

dnl AC_CHECK_HEADER(hash_map,
dnl 		[HASH_INC="hash_map"
dnl 		 HASH_CLS="hash_map"
dnl 		 AC_DEFINE(HAVE_HASH_MAP, ,
dnl 			  [Define if the g++ extension hash_map is available])],
dnl 	  	[AC_CHECK_HEADER(ext/hash_map,
dnl 				 [HASH_INC="ext/hash_map"
dnl 				  HASH_CLS="hash_map"
dnl 				  AC_DEFINE(HAVE_EXT_HASH_MAP, , [Define if the g++ extension ext/hash_map is available])])])

dnl AC_CHECK_HEADER(hash_set,
dnl 		[HASH_INC="hash_set"
dnl 		 HASH_CLS="hash_set"
dnl 		 AC_DEFINE(HAVE_HASH_SET,  , [Define if the g++ extension hash_set is available])],
dnl 		[AC_CHECK_HEADER(ext/hash_set,
dnl 				[HASH_INC="ext/hash_set"
dnl 				 HASH_CLS="hash_set"
dnl 				 AC_DEFINE(HAVE_EXT_HASH_SET,  , [Define if the g++ extension ext/hash_set is available])])])

AC_MSG_CHECKING([whether apt supports ddtp])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <apt-pkg/pkgcache.h>]],
                                   [[pkgCache::DescIterator d;]])],
                  AC_MSG_RESULT([yes])
		  AC_DEFINE([HAVE_DDTP], [], [Set to true if apt is DDTP-enabled]),
		  AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether apt supports the Homepage: field])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <apt-pkg/pkgrecords.h>]],
				  [[  pkgRecords::Parser *p = NULL;]]
				  [[  if(0) p->Homepage();]])],
		   AC_MSG_RESULT([yes])
		   AC_DEFINE([APT_HAS_HOMEPAGE], [], [Set to true if apt supports the Homepage: field]),
		   AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether apt supports dpkg triggers])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <apt-pkg/pkgcache.h>]],
                                   [[pkgCache::State::TriggersAwaited;]]
                                   [[pkgCache::State::TriggersPending;]])],
		  AC_MSG_RESULT([yes])
		  AC_DEFINE([APT_HAS_TRIGGERS], [], [Set to true if apt supports dpkg trigger states]),
		  AC_MSG_RESULT([no]))

# Try to guess what namespace the hash stuff is in (maybe I should just
# use rbltrees instead in most places?)
if test x$HASH_INC != x
then
    AC_MSG_CHECKING([which namespace contains the hash extensions])

    HASH_NAMESPACE=""

    for NS in std __gnu_cxx
    do
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <$HASH_INC>],
	 			         [${NS}::${HASH_CLS} < int, int > foo;])],
			[HASH_NAMESPACE=$NS])
    done

    if test x$HASH_NAMESPACE = x
    then
       AC_MSG_ERROR([found hash classes, but they aren't in the std or GNU namespace.])
    fi

    AC_MSG_RESULT($HASH_NAMESPACE)
else
    # Use the std namespace if all else fails. (the code will fall back
    # to using map instead of hash_map, set instead of hash_set)
    HASH_NAMESPACE=std
fi

AC_DEFINE_UNQUOTED(HASH_NAMESPACE, $HASH_NAMESPACE, [If a hash header is available, define to the namespace in which the hash class resides])




# Try to guess how to extend string traits
TRAITS_CLASS=""

AC_MSG_CHECKING([for the name of the character traits template])

for T in char_traits string_char_traits
do
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <string>],
				     [std::basic_string<unsigned char,
						        std::$T < unsigned char > > s;])],
		    TRAITS_CLASS=$T)
done

if test x$TRAITS_CLASS = xstring_char_traits
then
    AC_MSG_WARN([Your compiler uses string_char_traits for its character traits.  Some compilers (eg, g++ 2.95) which use this name for the character traits template are known to have hideously broken implementations of the standard string class, which cause aptitude to fail to compile.  If you have a compiler with this problem, please upgrade it to a version that has a more compliant version of the STL (g++ >=3.0 is known to work).  You can specify which compiler this script should use via the CXX environment variable.])
fi

if test x$TRAITS_CLASS = x
then
  AC_MSG_ERROR([can't find the name of the character traits template])
else
  AC_DEFINE_UNQUOTED(TRAITS_CLASS, $TRAITS_CLASS, [The name of the class used by the STL to define character traits])

  AC_MSG_RESULT([$TRAITS_CLASS])
fi



if test x$HAVE_LIBPTHREAD = x1
then
  AC_CHECK_HEADER(pthread.h,
	CXXFLAGS="$CXXFLAGS -D_REENTRANT"
	LIBS="$LIBS -lpthread"
	[AC_DEFINE([HAVE_LIBPTHREAD], [] , [Define if pthread is available])]
	,
	[AC_MSG_ERROR([POSIX thread header not installed])])
fi

dnl Check for header files only if compiling the program.
  ])

WERROR="-Werror"

AC_ARG_ENABLE(private-glade-file,
	AS_HELP_STRING([--disable-private-glade-file], [Always read the .glade file from \$datadir instead of reading one in ".".  This can be useful when installing aptitude globally, to avoid accidentally picking up the wrong version of the .glade file.]),
	[if test x$enableval = xno
	 then
	   AC_DEFINE([DISABLE_PRIVATE_GLADE_FILE], [], [Define to disable the use of a .glade file found relative to the current working directory.])
	 fi]
	)

AC_ARG_ENABLE(dynamic-backtrace,
	AS_HELP_STRING([--enable-dynamic-backtrace], [Modify the executable so that it can generate a backtrace for uncaught exceptions.  Will double the size of the stripped binary.]),
	[if test x$enableval = xyes
	then
	  AC_DEFINE([ENABLE_DYNAMIC_BACKTRACE], [], [Define to enable dynamic generation of backtraces if HAVE_EXECINFO_H is defined])
	  LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
	fi]
	)
AC_ARG_ENABLE(werror,
	AS_HELP_STRING([--disable-werror], [do not compile with -Werror]),
	if test x$enableval = xno
	then
	  WERROR=""
	fi
	)
AC_ARG_ENABLE(period-beep,
	AS_HELP_STRING([--enable-period-beep], [periodically (once a second) produce an annoying beep]),
	if test x$enableval = xyes
	then
	  [AC_DEFINE(WITH_PERIOD_BEEP,  , [Define to produce an annoying beep periodically, for debugging])]
	fi
	)
AC_ARG_ENABLE(reload-cache,
	AS_HELP_STRING([--enable-reload-cache], [allow the user to reload the cache on demand. (debugging feature)]),
	if test x$enableval = xyes
	then
	  [AC_DEFINE(WITH_RELOAD_CACHE,  , [Allow the cache to be reloaded on demand])]
	fi
	)

AC_ARG_ENABLE(package-state-loc,
	AS_HELP_STRING([--with-package-state-loc], [use the given location for storing state (default /var/lib/aptitude)]),
	if test x$enableval = xyes
	then
	  [AC_MSG_ERROR([--with-package-state-loc needs an argument])]
	else
	  STATEDIR="$enableval"
	fi,
	STATEDIR="/var/lib/aptitude")

AC_ARG_ENABLE(lock-loc,
	AS_HELP_STRING([--with-lock-loc], [use the given location for storing state (default /var/lock/aptitude)]),
	if test x$enableval = xyes
	then
	  [AC_MSG_ERROR([--with-lock-loc needs an argument])]
	else
	  LOCKFILE="$enableval"
	fi,
	LOCKFILE="/var/lock/aptitude")

AC_ARG_ENABLE(aptitude,
	AS_HELP_STRING([--disable-aptitude], [don't compile the program itself]),
	if test x$enableval=xno
	then
	  SRCDIRS=
	else
	  SRCDIRS=src
	fi,
	SRCDIRS=src)

AC_ARG_ENABLE(docs,
	AS_HELP_STRING([--disable-docs], [don't generate the documentation]),
	if test x$enableval=xno
	then
	  DOCDIRS=
	else
	  DOCDIRS=doc
	fi,
	DOCDIRS=doc)

AC_ARG_ENABLE(tests,
        AS_HELP_STRING([--disable-tests], [don't run the test suite]),
        if test x$enableval=xno
        then
          TESTDIRS=
        else
          TESTDIRS=tests
        fi,
        TESTDIRS=tests)

AC_ARG_ENABLE(resolver-sanity-checks,
	AS_HELP_STRING([--enable-resolver-sanity-checks], [enable expensive internal sanity-checks in the dependency resolver]),
	if test x$enableval=xyes
	then
	  [AC_DEFINE(ENABLE_RESOLVER_SANITY_CHECKS,  ,  [Enable expensive internal sanity-checks in the dependency resolver.])]
	fi
	)

# We need -fno-strict-aliasing to avoid compiler errors that I can't
# figure out how to fix.
CXXFLAGS="$CXXFLAGS -fno-strict-aliasing -Wall $WERROR"

AC_SUBST(SRCDIRS)
AC_SUBST(DOCDIRS)
AC_SUBST(TESTDIRS)

AC_DEFINE_UNQUOTED(STATEDIR, "$STATEDIR", [The location of aptitude's state files])
AC_DEFINE_UNQUOTED(LOCKFILE, "$LOCKFILE", [The location of the lock to prevent simultaneous aptitude instances])
AC_SUBST(STATEDIR)

AC_TYPE_SIZE_T

dnl Checks for library functions.
AC_CHECK_FUNCS(strdup)

CPPFLAGS="$CPPFLAGS -DHELPDIR=\\\"\$(datadir)/aptitude\\\""
CPPFLAGS="$CPPFLAGS -DPKGDATADIR=\\\"\$(datadir)/aptitude\\\""
dnl FC_EXPAND_DIR(HELPDIR, [$datadir/aptitude])
dnl AC_DEFINE_UNQUOTED(HELPDIR, "$HELPDIR", [The location in which the online help data is placed])

dnl FC_EXPAND_DIR(PKGDATADIR, [$datadir/aptitude])
dnl AC_DEFINE_UNQUOTED(PKGDATADIR, "$PKGDATADIR", [The location in which the program data is placed])

AC_CONFIG_FILES([
	Doxyfile
	Makefile
	buildlib/Makefile
	doc/Makefile
	doc/cs/Makefile
	doc/cs/images/Makefile
	doc/de/Makefile
	doc/en/Makefile
	doc/en/images/Makefile
	doc/es/Makefile
	doc/es/images/Makefile
	doc/fi/Makefile
	doc/fi/images/Makefile
	doc/fr/Makefile
	doc/fr/images/Makefile
	doc/it/Makefile
	doc/it/images/Makefile
	doc/ja/Makefile
	doc/ja/images/Makefile
	doc/pl/Makefile
	doc/po4a/Makefile
	doc/po4a/po/Makefile
	doc/po4a/add_de/Makefile
	doc/po4a/add_es/Makefile
	doc/po4a/add_fr/Makefile
	doc/po4a/add_pl/Makefile
	m4/Makefile
	po/Makefile.in
	src/Makefile
	src/gtk/Makefile
        src/gtk/toplevel/Makefile
	src/gtk/util/Makefile
        src/gtk/view-impls/Makefile
	src/qt/Makefile
	src/cmdline/Makefile
        src/cmdline/mocks/Makefile
	src/generic/Makefile
	src/generic/apt/Makefile
	src/generic/apt/history/Makefile
	src/generic/apt/matching/Makefile
        src/generic/controllers/Makefile
	src/generic/problemresolver/Makefile
	src/generic/util/Makefile
        src/generic/util/mocks/Makefile
        src/generic/views/Makefile
        src/generic/views/mocks/Makefile
	src/mine/Makefile
	tests/Makefile
])

AC_OUTPUT
