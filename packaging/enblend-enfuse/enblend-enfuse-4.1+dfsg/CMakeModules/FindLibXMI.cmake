IF(NOT WIN32)
    FIND_PATH(LIBXMI_INCLUDE_DIR xmi.h
	  /usr/local/include
	  /usr/include
	)
	FIND_LIBRARY(LIBXMI_LIBRARIES xmi)
ELSE(NOT WIN32)
	include(FindLibraryWithDebug)
	FIND_PATH(LIBXMI_INCLUDE_DIR xmi.h
	  /usr/local/include
	  /usr/include
	  ${SOURCE_BASE_DIR}/libxmi-1.2
	)
	
	find_library_with_debug(LIBXMI_LIBRARIES
	  WIN32_DEBUG_PATH_SUFFIX Debug
	  WIN32_RELEASE_PATH_SUFFIX Release
	  NAMES libxmi
	  PATHS ${LIBXMI_INCLUDE_DIR}
	)
	
ENDIF(NOT WIN32)

MARK_AS_ADVANCED(
  LIBXMI_INCLUDE_DIR
  LIBXMI_LIBRARIES
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LIBXMI DEFAULT_MSG LIBXMI_INCLUDE_DIR LIBXMI_LIBRARIES)