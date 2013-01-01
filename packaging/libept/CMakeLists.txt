include( FindPkgConfig )
include( FindDoxygen )

add_custom_target( unit )

set( EPT_VERSION "1.0.9" )

# Get the soversion from libapt-pkg to include in our own
execute_process(
	COMMAND "cc" "-xc" "-shared" "-Wl,--no-as-needed" "/dev/null" "-lapt-pkg"
	)
execute_process(
	COMMAND "objdump" "-p" "a.out"
	COMMAND "awk" "/NEEDED.*libapt-pkg/ { sub(\".*\\\\.so\\\\.\", \"\"); print }"
	OUTPUT_VARIABLE LIBAPT_ABI_VERSION
	OUTPUT_STRIP_TRAILING_WHITESPACE
	)
set( LIBEPT_SOVERSION "1.aptpkg${LIBAPT_ABI_VERSION}" )
set( APT_INCLUDES "/usr/include/" CACHE STRING "apt include path" )
set( APT_LINKDIRS "/usr/lib/" CACHE STRING "apt library path" )
set( OPT_FLAGS "-O0 -g" CACHE STRING "optimization level" )

option( INTERNAL_TAGCOLL "use internal copy of tagcoll" )
option( INTERNAL_WIBBLE "use internal copy of wibble" )
option( RPM "using apt-rpm" )

if( INTERNAL_WIBBLE )
  add_subdirectory( wibble )
  set( WIBBLE_INCLUDE_DIRS ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR} )
  set( WIBBLE_LIBRARY_DIRS ${wibble_BINARY_DIR} )
  set( WIBBLE_LIBRARIES "wibble" )
  set( WIBBLE_TEST_CMAKE "${wibble_SOURCE_DIR}/test.cmake" )
else( INTERNAL_WIBBLE )
  pkg_check_modules( WIBBLE REQUIRED "libwibble >= 0.1.22" )
  find_program( WIBBLE_TEST_GENRUNNER wibble-test-genrunner )
  set( WIBBLE_TEST_CMAKE "${WIBBLE_PREFIX}/share/wibble/test.cmake" )
endif( INTERNAL_WIBBLE )

if( INTERNAL_TAGCOLL )
  set( TAGCOLL_FOUND ON )
  set( TAGCOLL_INCLUDE_DIRS ${CMAKE_SOURCE_DIR} )
  set( TAGCOLL_LIBRARY_DIRS ${tagcoll_BINARY_DIR} )
  set( TAGCOLL_LIBRARIES "tagcoll2" )
  add_subdirectory( tagcoll )
else( INTERNAL_TAGCOLL )
  pkg_check_modules( TAGCOLL REQUIRED "libtagcoll2 >= 2.0" )
endif( INTERNAL_TAGCOLL )

add_subdirectory( ept )
add_subdirectory( tools )
add_subdirectory( doc )
