# Try to find the glew libraries, setting these defines:
#  GLEW_FOUND - system has glew
#  GLEW_INCLUDE_DIR - glew include directory
#  GLEW_LIBRARIES - Libraries needed to use glew

IF(WIN32)
  include(FindLibraryWithDebug)
  FIND_PATH(GLEW_INCLUDE_DIR GL/glew.h PATHS ${SOURCE_BASE_DIR}/glew/include)
  
  find_library_with_debug(GLEW_LIBRARIES
    WIN32_DEBUG_POSTFIX d
	NAMES glew32s
	PATHS ${GLEW_LIB_DIR} ${SOURCE_BASE_DIR}/glew/lib
  )
ELSE(WIN32)
  FIND_PATH(GLEW_INCLUDE_DIR GL/glew.h PATHS /usr/include /usr/local/include)
  FIND_LIBRARY(GLEW_LIBRARIES GLEW PATHS ${SYSTEM_LIB_DIRS})
ENDIF(WIN32)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GLEW DEFAULT_MSG GLEW_INCLUDE_DIR GLEW_LIBRARIES)  

MARK_AS_ADVANCED(
  GLEW_INCLUDE_DIR
  GLEW_LIBRARIES
)
