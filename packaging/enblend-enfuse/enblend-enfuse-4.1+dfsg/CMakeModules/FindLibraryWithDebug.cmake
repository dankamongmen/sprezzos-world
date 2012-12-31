#
#  FIND_LIBRARY_WITH_DEBUG
#  -> enhanced FIND_LIBRARY to allow the search for an
#     optional debug library with a WIN32_DEBUG_POSTFIX similar
#     to CMAKE_DEBUG_POSTFIX when creating a shared lib
#     it has to be the second and third argument

# Copyright (c) 2007, Christian Ehrlicher, <ch.ehrlicher@gmx.de>
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

MACRO(FIND_LIBRARY_WITH_DEBUG var_name)
  IF(NOT WIN32)
    FIND_LIBRARY(${var_name} ${ARGN})
  ELSE(NOT WIN32)
    PARSE_ARGUMENTS(FIND_LIB_WITH_DEBUG "WIN32_DEBUG_POSTFIX;WIN32_DEBUG_PATH_SUFFIX;WIN32_RELEASE_PATH_SUFFIX;NAMES;PATHS" "" ${ARGN})

    IF(NOT FIND_LIB_WITH_DEBUG_NAMES)
      LIST(GET FIND_LIB_WITH_DEBUG_DEFAULT_ARGS 0 FIND_LIB_WITH_DEBUG_NAMES)
	  LIST(REMOVE_AT FIND_LIB_WITH_DEBUG_DEFAULT_ARGS 0)
    ENDIF(NOT FIND_LIB_WITH_DEBUG_NAMES)
  
    IF(NOT FIND_LIB_WITH_DEBUG_PATHS)
      LIST(GET FIND_LIB_WITH_DEBUG_DEFAULT_ARGS 0 FIND_LIB_WITH_DEBUG_PATHS)
    ENDIF(NOT FIND_LIB_WITH_DEBUG_PATHS)
	
	IF(NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_POSTFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX)
	  FIND_LIBRARY(${var_name} NAMES ${FIND_LIB_WITH_DEBUG_NAMES} PATHS ${FIND_LIB_WITH_DEBUG_PATHS})
	ELSE(NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_POSTFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX)
	  SET(libpaths_release "")
	  SET(libpaths_debug "")
	  
	  SET(libnames_release "")
	  SET(libnames_debug "")
	  
	  FOREACH(libpath ${FIND_LIB_WITH_DEBUG_PATHS})
        FOREACH(rel_suffix ${FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX})
		  LIST(APPEND libpaths_release "${libpath}/${rel_suffix}")
	    ENDFOREACH(rel_suffix ${FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX})
		IF(NOT DEFINED ${FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX})
          LIST(APPEND libpaths_release "${libpath}")
		ENDIF(NOT DEFINED ${FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX})

		FOREACH(deb_suffix ${FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX})
		  LIST(APPEND libpaths_debug "${libpath}/${deb_suffix}")
		ENDFOREACH(deb_suffix ${FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX})
		IF(NOT DEFINED ${FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX})
		  LIST(APPEND libpaths_debug "${libpath}")
		ENDIF(NOT DEFINED ${FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX})
	  ENDFOREACH(libpath ${FIND_LIB_WITH_DEBUG_PATHS})
	  
 	  FOREACH(libname ${FIND_LIB_WITH_DEBUG_NAMES})
	    LIST(APPEND libnames_release "${libname}")
		LIST(APPEND libnames_debug "${libname}${FIND_LIB_WITH_DEBUG_WIN32_DEBUG_POSTFIX}")
 	  ENDFOREACH(libname ${FIND_LIB_WITH_DEBUG_NAMES})
	  
      # search the release lib
      find_library_for_cpu(${var_name}_RELEASE
                   NAMES ${libnames_release}
				   PATHS ${libpaths_release}
      )

      # search the debug lib
      find_library_for_cpu(${var_name}_DEBUG
                   NAMES ${libnames_debug}
                   PATHS ${libpaths_debug}
      )

      IF(${var_name}_RELEASE AND ${var_name}_DEBUG)
        # both libs found
        SET(${var_name} optimized ${${var_name}_RELEASE}
                        debug     ${${var_name}_DEBUG})
      ELSE(${var_name}_RELEASE AND ${var_name}_DEBUG)
        IF(${var_name}_RELEASE)
          # only release found
          SET(${var_name} ${${var_name}_RELEASE})
        ELSE(${var_name}_RELEASE)
          # only debug (or nothing) found
          SET(${var_name} ${${var_name}_DEBUG})
        ENDIF(${var_name}_RELEASE)
      ENDIF(${var_name}_RELEASE AND ${var_name}_DEBUG)

      MARK_AS_ADVANCED(${var_name}_RELEASE)
      MARK_AS_ADVANCED(${var_name}_DEBUG)
	  
	ENDIF(NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_POSTFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_DEBUG_PATH_SUFFIX AND NOT FIND_LIB_WITH_DEBUG_WIN32_RELEASE_PATH_SUFFIX)
  ENDIF(NOT WIN32)
	
ENDMACRO(FIND_LIBRARY_WITH_DEBUG)
