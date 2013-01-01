project( ept )
include( ${WIBBLE_TEST_CMAKE} )

file( GLOB src *.cpp debtags/*.cc debtags/maint/*.cc
  popcon/*.cc popcon/maint/*.cc apt/*.cc axi/*.cc )

file( GLOB h_top *.h )
file( GLOB h_apt apt/*.h )
file( GLOB h_debtags debtags/*.h debtags/*.tcc )
file( GLOB h_debtags_maint debtags/maint/*.h debtags/maint/*.tcc )
file( GLOB h_popcon popcon/*.h )
file( GLOB h_popcon_maint popcon/maint/*.h )
file( GLOB h_axi axi/*.h )

file( GLOB debtagstesth debtags/*.test.h debtags/maint/*.test.h )
file( GLOB popcontesth popcon/*.test.h )
file( GLOB apttesth apt/*.test.h )
file( GLOB axitesth axi/*.test.h )
set( testh ${debtagstesth} ${popcontesth}
           ${apttesth} ${axitesth} )

include_directories( ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR} 
                     ${TAGCOLL_INCLUDE_DIRS} ${WIBBLE_INCLUDE_DIRS} )
link_libraries( ${WIBBLE_LIBRARIES} ${TAGCOLL_LIBRARIES} apt-pkg xapian -lpthread )
add_definitions( -fexceptions -fPIC -fvisibility=default )

add_library( ept SHARED ${src} )
add_library( ept-static STATIC ${src} )
set_target_properties( ept PROPERTIES SOVERSION ${LIBEPT_SOVERSION} CLEAN_DIRECT_OUTPUT 1)
set_target_properties( ept-static PROPERTIES SOVERSION ${LIBEPT_SOVERSION} OUTPUT_NAME "ept" CLEAN_DIRECT_OUTPUT 1)

set( TEST_ENV_DIR ${ept_BINARY_DIR}/test-env/ )

configure_file( ${ept_SOURCE_DIR}/config.h.cmake-in
  ${ept_BINARY_DIR}/config.h )

set( prefix "${CMAKE_INSTALL_PREFIX}" )
set( exec_prefix "${prefix}/bin" )
set( libdir "${prefix}/lib" )
set( includedir "${prefix}/include" )
configure_file( ${ept_SOURCE_DIR}/libept.pc.in
  ${ept_BINARY_DIR}/libept.pc @ONLY )

# regression testing
link_directories( ${CMAKE_CURRENT_BINARY_DIR}/../lib )
link_libraries( ept )
wibble_add_test( ept-test ${testh} )

set( datastamp ${CMAKE_CURRENT_BINARY_DIR}/data-stamp )
set( datadir ${CMAKE_CURRENT_SOURCE_DIR}/test-data )
set( listfile wherever_debian_._Packages )

wibble_check_target( ept-test ${datastamp} )

execute_process( COMMAND dpkg --print-architecture OUTPUT_VARIABLE ARCH )
string( REPLACE "\n" "" ARCH "${ARCH}" )

add_custom_command(
  OUTPUT ${datastamp}
  COMMAND mkdir -p test-env/etc test-env/state/lists/partial
		           test-env/cache test-env/debtags
			   test-env/cache/archives/partial test-env/desktop
  COMMAND sed -e s,i386,${ARCH}, < ${datadir}/packagelist > test-env/state/lists/${listfile}
  COMMAND cp -a ${datadir}/etc/sources.list test-env/etc/
  COMMAND sed -e s,i386,${ARCH}, < ${datadir}/dpkg-status > test-env/dpkg-status
  COMMAND cp -a ${datadir}/desktop/*.desktop test-env/desktop/
  COMMAND cp ${datadir}/debtags/package-tags test-env/debtags/test.tag
  COMMAND cp ${datadir}/debtags/vocabulary test-env/debtags/test.voc
  COMMAND mkdir -p test-env/debtags/empty
  COMMAND mkdir -p test-env/debtags/user
  COMMAND mkdir -p test-env/xapian/
  COMMAND mkdir -p test-env/popcon/
  COMMAND mkdir -p test-env/popcon/empty
  COMMAND cp -a ${datadir}/popcon/all-popcon-results.txt.gz test-env/popcon/
  COMMAND cp -a ${datadir}/popcon/popularity-contest test-env/popcon/
  COMMAND touch data-stamp )

install( TARGETS ept ept-static
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib )

install( FILES ${ept_BINARY_DIR}/libept.pc DESTINATION lib/pkgconfig )
install( FILES ${h_top} DESTINATION include/ept )
install( FILES ${h_apt} DESTINATION include/ept/apt )
install( FILES ${h_debtags} DESTINATION include/ept/debtags )
install( FILES ${h_debtags_maint} DESTINATION include/ept/debtags/maint )
install( FILES ${h_popcon} DESTINATION include/ept/popcon )
install( FILES ${h_popcon_maint} DESTINATION include/ept/popcon/maint )
install( FILES ${h_axi} DESTINATION include/ept/axi )
