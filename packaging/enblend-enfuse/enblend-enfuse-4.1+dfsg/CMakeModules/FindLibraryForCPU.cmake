#
#  FIND_LIBRARY_WITH_CPU
#  -> enhanced FIND_LIBRARY to allow searching for platform
#     specific versions of libraries on Win32. This is common
#     for MSVC based builds that may output a file into a directory
#     particular for that CPU architecture (traditionally, 
#     "Win32" and "x64" for 32-bit and 64-bit builds)
#
#    
# Based in part on FIND_LIBRARY_WITH_DEBUG
# Copyright (c) 2009, Ryan Sleevi, <ryan@sleevi.com>
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
MACRO(FIND_LIBRARY_FOR_CPU var_name)
	IF(NOT MSVC)
		FIND_LIBRARY(${var_name} ${ARGN})
	ELSE(NOT MSVC)
		SET(args ${ARGN})
		SET(names "")
		SET(newpaths "")
		SET(found_paths 0)
		IF(CMAKE_CL_64)
			SET(msvc_platform "x64")
		ELSE(CMAKE_CL_64)
			SET(msvc_platform "Win32")
		ENDIF(CMAKE_CL_64)
		
		FOREACH(val ${args})
			IF(found_paths)
				# Prefer the CPU-specific path over the generic/original path
				LIST(APPEND newpaths "${val}/${msvc_platform}")
				LIST(APPEND newpaths "${val}")
			ELSE(found_paths)
				IF("${val}" STREQUAL "PATHS")
					SET(found_paths 1)
				ELSE("${val}" STREQUAL "PATHS")
					LIST(APPEND names "${val}")
				ENDIF("${val}" STREQUAL "PATHS")
			ENDIF(found_paths)
		ENDFOREACH(val)
		
		FIND_LIBRARY(${var_name} ${names} PATHS ${newpaths})
	ENDIF(NOT MSVC)
ENDMACRO(FIND_LIBRARY_FOR_CPU) 