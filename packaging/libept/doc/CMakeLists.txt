set( stamp "${CMAKE_CURRENT_BINARY_DIR}/doc-stamp" )

add_custom_command(
  DEPENDS ept ${CMAKE_CURRENT_BINARY_DIR}/libept.dox
  OUTPUT ${stamp}
  COMMAND ${DOXYGEN} libept.dox && touch ${stamp} )

add_custom_target( doc DEPENDS ${stamp} )

configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/libept.dox.in
  ${CMAKE_CURRENT_BINARY_DIR}/libept.dox @ONLY IMMEDIATE )
