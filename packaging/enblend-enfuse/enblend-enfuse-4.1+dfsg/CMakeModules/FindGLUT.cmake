# - try to find glut library and include files
#  GLUT_INCLUDE_DIR, where to find GL/glut.h, etc.
#  GLUT_LIBRARIES, the libraries to link against
#  GLUT_FOUND, If false, do not try to use GLUT.
# Also defined, but not for general use are:
#  GLUT_glut_LIBRARY = the full path to the glut library.
#  GLUT_Xmu_LIBRARY  = the full path to the Xmu library.
#  GLUT_Xi_LIBRARY   = the full path to the Xi Library.

IF (WIN32)
  include(FindLibraryWithDebug)
  
  FIND_PATH(GLUT_ROOT_PATH 
    NAMES include/GL/glut.h
    PATHS 
	  ${SOURCE_BASE_DIR}/glut
	  ${SOURCE_BASE_DIR}/glut-3.7.6
  )
  
  FIND_PATH(GLUT_INCLUDE_DIR
    NAMES GL/glut.h
	PATHS ${GLUT_ROOT_PATH}/include
  )
  
  find_library_with_debug( GLUT_glut_LIBRARY 
    WIN32_DEBUG_PATH_SUFFIX Debug
	WIN32_RELEASE_PATH_SUFFIX Release
    NAMES 
	  glut 
	  glut32
    PATHS 
	  ${OPENGL_LIBRARY_DIR}
      ${GLUT_ROOT_PATH}/lib/glut
  )
  
ELSE (WIN32)
  
  IF (APPLE)
    # These values for Apple could probably do with improvement.
    FIND_PATH( GLUT_INCLUDE_DIR glut.h
      /System/Library/Frameworks/GLUT.framework/Versions/A/Headers
      ${OPENGL_LIBRARY_DIR}
      )
    SET(GLUT_glut_LIBRARY "-framework GLUT" CACHE STRING "GLUT library for OSX") 
    SET(GLUT_cocoa_LIBRARY "-framework Cocoa" CACHE STRING "Cocoa framework for OSX")
  ELSE (APPLE)
    
    FIND_PATH( GLUT_INCLUDE_DIR GL/glut.h
      /usr/include/GL
      /usr/openwin/share/include
      /usr/openwin/include
      /opt/graphics/OpenGL/include
      /opt/graphics/OpenGL/contrib/libglut
      )
  
    FIND_LIBRARY( GLUT_glut_LIBRARY glut
      /usr/openwin/lib
      )
    
    FIND_LIBRARY( GLUT_Xi_LIBRARY Xi
      /usr/openwin/lib ${SYSTEM_LIB_DIRS}
      )
    
    FIND_LIBRARY( GLUT_Xmu_LIBRARY Xmu
      /usr/openwin/lib
      )
    
  ENDIF (APPLE)
  
ENDIF (WIN32)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GLUT DEFAULT_MSG GLUT_INCLUDE_DIR GLUT_glut_LIBRARY)  

IF(GLUT_FOUND)
  # Is -lXi and -lXmu required on all platforms that have it?
  # If not, we need some way to figure out what platform we are on.
  SET( GLUT_LIBRARIES
    ${GLUT_glut_LIBRARY}
    ${GLUT_Xmu_LIBRARY}
    ${GLUT_Xi_LIBRARY} 
    ${GLUT_cocoa_LIBRARY}
  )
ENDIF(GLUT_FOUND)

MARK_AS_ADVANCED(
  GLUT_INCLUDE_DIR
  GLUT_ROOT_PATH
  GLUT_glut_LIBRARY
  GLUT_Xmu_LIBRARY
  GLUT_Xi_LIBRARY
  )
