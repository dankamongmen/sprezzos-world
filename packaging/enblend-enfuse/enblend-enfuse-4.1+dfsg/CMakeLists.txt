# This file is part of enblend/enfuse.
# Licence details can be found in the file COPYING.
#
# Copyright(c) 2009-2010, Kornel Benko <Kornel.Benko@berlin.de>
#                  , Ryan Sleevi <ryan+hugin@sleevi.com>
#                  , Harry van der Wolf <hvdwolf@gmail.com>
# Original file from lyx-project, heavily modified
#
cmake_minimum_required(VERSION 2.4)

if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 OLD)
  cmake_policy(SET CMP0005 OLD)
  cmake_minimum_required(VERSION 2.6)
endif(COMMAND cmake_policy)

set(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS true)

project(enblend)
IF(NOT MSVC)
  SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel" FORCE)
ENDIF(NOT MSVC)
# where to look first for cmake modules, before ${CMAKE_ROOT}/Modules/
# is checked
# For now, reuse the hugin modules directory
#
# Try to find ...
set(CMAKE_MODULE_PATH) # overwrite cache
find_file(CMAKE_MODULE_PATH CMakeModules
  "${CMAKE_SOURCE_DIR}"
  "${CMAKE_SOURCE_DIR}/../hugin"
  "${CMAKE_SOURCE_DIR}/../hugin-trunk"
  "${CMAKE_SOURCE_DIR}/../../hugin/hugin"
  "${CMAKE_SOURCE_DIR}/../../hugin/trunk"
  "$ENV{HOME}/hugin")
message(STATUS "found hugin modules directory in ${CMAKE_MODULE_PATH}")

set(ARCH_TRIPLET)
if(UNIX)
  FIND_PROGRAM(DPKG_ARCHITECTURE_EXECUTABLE dpkg-architecture)
  if(DPKG_ARCHITECTURE_EXECUTABLE)
    EXECUTE_PROCESS(COMMAND dpkg-architecture -qDEB_HOST_MULTIARCH
      OUTPUT_VARIABLE ARCH_TRIPLET
      ERROR_VARIABLE ERROR_ARCH_TRIPLET
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
endif()
if(ARCH_TRIPLET)
  set(SYSTEM_LIB_DIRS /usr/lib /usr/lib/${ARCH_TRIPLET} /usr/local/lib)
else()
  set(SYSTEM_LIB_DIRS /usr/lib /usr/local/lib)
endif()

include(HuginMacros)
include(FindLibraryForCPU)

set(EXECUTABLE_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/bin)

# Regenerate when calling make
set(CMAKE_SUPPRESS_REGENERATION FALSE)
set(VERSION_INFO "CMake Build")

macro(setstripped _varname _value)
  string(STRIP ${_value} _v)
  if(USE_POSIX_PACKAGING)
    string(TOLOWER ${_v} ${_varname})
  else()
    set(${_varname} ${_v})
  endif()
endmacro(setstripped)

# Try to get the package version from file
set(TOP_SRC_DIR ${CMAKE_SOURCE_DIR})
file(STRINGS "${TOP_SRC_DIR}/VERSION" _version_lines)
foreach(_v_l ${_version_lines})
  if(_v_l MATCHES "^\([0-9]+\)\\.\([0-9]+\).*-\([0-9a-z]+\)")
    setstripped(ENBLEND_MAJOR_VERSION ${CMAKE_MATCH_1})
    setstripped(ENBLEND_MINOR_VERSION ${CMAKE_MATCH_2})
    setstripped(PACKAGE_REVISION ${CMAKE_MATCH_3})
    message(STATUS "Extracted enblend version = ${ENBLEND_MAJOR_VERSION}.${ENBLEND_MINOR_VERSION}")
    message(STATUS "Extracted revision = ${PACKAGE_REVISION}")
    set(PACKAGE_VERSION "${ENBLEND_MAJOR_VERSION}.${ENBLEND_MINOR_VERSION}-${PACKAGE_REVISION}")
  endif()
endforeach(_v_l)

# For Win32 builds, assume that all of the libraries are located
# one directory above the current CMakeLists directory
IF(WIN32)
  STRING(REGEX REPLACE "(.*)/[^/]+$" "\\1" work "${CMAKE_SOURCE_DIR}")
  # create the cache entry
  SET(SOURCE_BASE_DIR ${work} CACHE FILEPATH "parent dir of hugin source root")

  LIST(APPEND CMAKE_PREFIX_PATH ${SOURCE_BASE_DIR})
ENDIF(WIN32)

# First include so it picks up(our) config.h
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

SET(library_type STATIC)
set(common_libs)
IF(WIN32)
# we are using static vigra lib on Windows
ADD_DEFINITIONS(-DVIGRA_STATIC_LIB)
ENDIF(WIN32)

# Configurable options
OPTION(ENABLE_OPENMP "OpenMP Support" OFF)
OPTION(ENABLE_IMAGECACHE "Image Cache Support" ON)
OPTION(ENABLE_GPU "GPU Acceleration Support" OFF)
OPTION(DOC "Create Documentation" OFF)
OPTION(NOSPLIT "Do not split html-documents" OFF)
OPTION(A4 "Use A4 for pdf-doc's" ON)

IF(NOT CMAKE_CL_64)
  OPTION(ENABLE_SSE2 "SSE2 Support(Release builds only)" OFF)
ELSE(NOT CMAKE_CL_64)
  SET(ENABLE_SSE2 OFF CACHE INTERNAL "")
ENDIF(NOT CMAKE_CL_64)
IF(NOT WIN32)
  OPTION(ENABLE_DMALLOC "Use the dmalloc debugging package" OFF)
  OPTION(ENABLE_DMALLOC_FUNC_CHECK "Enable dmalloc function checking" OFF)
ELSE(NOT WIN32)
  SET(ENABLE_DMALLOC OFF CACHE INTERNAL "")
  SET(ENABLE_DMALLOC_FUNC_CHECK OFF CACHE INTERNAL "")
ENDIF(NOT WIN32)

# Compiler specific tweaks and optimizations
IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -ffast-math -DNDEBUG -Wall")
  IF(APPLE AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION LESS 5)
    ## Add needed Compiler and Linker flags for OSX
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -flat_namespace -undefined suppress")
    SET(CMAKE_CXX_FLAGS "{$CMAKE_CXX_FLAGS} -flat_namespace -undefined suppress")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -flat_namespace -undefined suppress")
    SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -flat_namespace -undefined suppress")
    SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -flat_namespace -undefined suppress")
  ENDIF(APPLE AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION LESS 5)
ELSEIF(WIN32)
  IF(MSVC)
    # Quiet some compiler noise
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
    ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
    ADD_DEFINITIONS(-D_CRT_NONSTDC_NO_DEPRECATE)

    # Due to the number of templates and the added overhead in .obj files from 64-bit builds
    # add /bigobj support
    # details: http://msdn.microsoft.com/en-us/library/ms173499.aspx
    IF(CMAKE_CL_64)
      SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /bigobj")
      SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj")
    ENDIF(CMAKE_CL_64)

    # compile everything for the static C runtime
    FOREACH(TYPE C CXX)
      # makefiles use blank configuration
      FOREACH(CFG "_DEBUG" "_MINSIZEREL" "_RELEASE" "_RELWITHDEBINFO")
	#MESSAGE("Replacing CMAKE_${TYPE}_FLAGS${CFG}: ${CMAKE_${TYPE}_FLAGS${CFG}}")
	SET(NEW_FLAGS "${CMAKE_${TYPE}_FLAGS${CFG}}")
	# fix up static libc flags
	STRING(REPLACE "/MD" "/MT" NEW_FLAGS "${NEW_FLAGS}")
	# *FORCE* to override whats already placed into the cache
	SET(CMAKE_${TYPE}_FLAGS${CFG} "${NEW_FLAGS}" CACHE STRING
	  "CMAKE_${TYPE}_FLAGS${CFG} (overwritten to ensure static build)" FORCE)
	#MESSAGE("New       CMAKE_${TYPE}_FLAGS${CFG}: ${CMAKE_${TYPE}_FLAGS${CFG}}")
      ENDFOREACH(CFG)
    ENDFOREACH(TYPE)
    # link only with the static C runtime !! THIS IS A MUST !!
    SET(NEW_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
    # ensure that the flags are not duplicated on subsequent runs
    STRING(REPLACE " /NODEFAULTLIB:\"MSVCRT.lib\" /NODEFAULTLIB:\"MSVCRTd.lib\" /LTCG /LARGEADDRESSAWARE" "" NEW_FLAGS "${NEW_FLAGS}")
    # note that flag names (NODEFAULTLIB) etc.) are case sensitive
    SET( CMAKE_EXE_LINKER_FLAGS
      "${NEW_FLAGS} /NODEFAULTLIB:\"MSVCRT.lib\" /NODEFAULTLIB:\"MSVCRTd.lib\" /LTCG /LARGEADDRESSAWARE"
      CACHE STRING "for MSVC" FORCE
      )

    # Maximize speed
    STRING(REPLACE " /O2 " " /Ox /GL " CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
    STRING(REPLACE " /O2 " " /Ox /GL " CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")

    SET(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /GL")
  ENDIF(MSVC)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

# Required Libraries first
FIND_PACKAGE(LCMS2 REQUIRED)
FIND_PACKAGE(TIFF REQUIRED)
FIND_PACKAGE(Perl REQUIRED)
# this one is needed in doc. (Especially the program "convert")
FIND_PACKAGE(ImageMagick REQUIRED)
FIND_PACKAGE(GSL REQUIRED)
include_directories(${GSL_INCLUDE_DIR})
LIST(APPEND common_libs ${LCMS2_LIBRARIES} ${TIFF_LIBRARIES} ${GSL_LIBRARIES})
include_directories(${TIFF_INCLUDE_DIR} ${LCMS2_INCLUDE_DIR})

# Platform specifics required libraries
IF(WIN32)
  SET(Boost_USE_STATIC_LIBS ON)
  # path for getopt library
  INCLUDE_DIRECTORIES(${TOP_SRC_DIR}/src/win32helpers)
ENDIF(WIN32)

# Boost headers are required, but filesystem is optional
if(WIN32)
  IF( NOT Boost_root_suffix )
    SET( Boost_root_suffix _1_51_0 CACHE STRING "suffix of boost root dir." FORCE )
  ENDIF( NOT Boost_root_suffix )
  SET(Boost_USE_STATIC_LIBS ON)
  if (NOT BOOST_ROOT AND NOT $ENV{BOOST_ROOT} STREQUAL "") 
    set(BOOST_ROOT $ENV{BOOST_ROOT}) 
  else(NOT BOOST_ROOT AND NOT $ENV{BOOST_ROOT} STREQUAL "") 
    set(BOOST_ROOT ${SOURCE_BASE_DIR}/boost${Boost_root_suffix}) 
  endif(NOT BOOST_ROOT AND NOT $ENV{BOOST_ROOT} STREQUAL "") 
endif()

find_package(Boost COMPONENTS filesystem system)

set(Boost_filesystem_FOUND ${Boost_FILESYSTEM_FOUND})
set(Boost_system_FOUND ${Boost_SYSTEM_FOUND})
message(STATUS "Boost_LIBRARIES = ${Boost_LIBRARIES}")

IF(Boost_filesystem_FOUND)
  ADD_DEFINITIONS(-DHAVE_BOOST_FILESYSTEM)
ELSE(Boost_filesystem_FOUND)
  MESSAGE(STATUS "Boost filesystem library found:     ${Boost_filesystem_FOUND}")
  MESSAGE(STATUS "Boost system library found:         ${Boost_system_FOUND}")
  MESSAGE(STATUS "Disabling Boost filesystem support")
ENDIF(Boost_filesystem_FOUND)

LIST(APPEND common_libs ${Boost_LIBRARIES})
link_directories(${Boost_LIBRARY_DIRS})

IF(NOT MSVC)
  # MSVC supports auto-linking and will always choose the appropriate
  # library for the config. Use explicit specifiers for the other platforms
  LIST(APPEND common_libs ${Boost_LIBRARIES})
ENDIF(NOT MSVC)

include_directories(${Boost_INCLUDE_DIR})

# Optional Libraries
FIND_PACKAGE(ZLIB)
FIND_PACKAGE(JPEG)
FIND_PACKAGE(PNG)
FIND_PACKAGE(OpenEXR)
FIND_PACKAGE(Threads)

# VIGRA uses Has* pre-processor definitions for config.h
ADD_DEFINITIONS(-DHasTIFF)
FIND_PACKAGE(VIGRA REQUIRED)
include_directories(${VIGRA_INCLUDE_DIR})
list(APPEND common_libs ${VIGRA_LIBRARIES})

IF(ZLIB_FOUND)
  LIST(APPEND common_libs ${ZLIB_LIBRARIES})
  include_directories(${ZLIB_INCLUDE_DIR})
ELSEIF(WIN32)
  IF(PNG_FOUND)
    MESSAGE(STATUS "Disabling support for PNG due to lack of zlib")
    SET(PNG_FOUND 0)
  ENDIF(PNG_FOUND)
ENDIF(ZLIB_FOUND)

IF(JPEG_FOUND)
  ADD_DEFINITIONS(-DHasJPEG)
  list(APPEND common_libs ${JPEG_LIBRARIES})
  include_directories(${JPEG_INCLUDE_DIR})
ELSE(JPEG_FOUND)
  MESSAGE(STATUS "Compiling without support for JPEG files")
ENDIF(JPEG_FOUND)

IF(PNG_FOUND)
  ADD_DEFINITIONS(-DHasPNG)
  list(APPEND common_libs ${PNG_LIBRARIES})
  include_directories(${PNG_INCLUDE_DIR})
ELSE(PNG_FOUND)
  MESSAGE(STATUS "Compiling without support for PNG files")
ENDIF(PNG_FOUND)

IF(OPENEXR_FOUND)
  ADD_DEFINITIONS(-DHasEXR)
  list(APPEND common_libs ${OPENEXR_LIBRARIES})
  include_directories(${OPENEXR_INCLUDE_DIR})
ELSE(OPENEXR_FOUND)
  MESSAGE(STATUS "OpenEXR support disabled")
ENDIF(OPENEXR_FOUND)

# Now that packages are / are not detected, handle config options

IF(ENABLE_OPENMP)
  FIND_PACKAGE(OpenMP REQUIRED)
  add_definitions(${OpenMP_CXX_FLAGS})
ENDIF(ENABLE_OPENMP)

IF(ENABLE_IMAGECACHE AND ENABLE_OPENMP)
  MESSAGE(STATUS "image cache and OpenMP support are mutually exclusive. only configure like this if you want to develop a reentrant image cache")
  LIST(APPEND WARNINGS "WARNING: Image cache and OpenMP are both enabled!  You are a developer, aren't you?")
ENDIF(ENABLE_IMAGECACHE AND ENABLE_OPENMP)

IF(ENABLE_IMAGECACHE)
  set(CACHE_IMAGES 1)
ENDIF(ENABLE_IMAGECACHE)

IF(ENABLE_GPU)
  IF(WIN32)
    IF(NOT GLUT_ROOT_PATH)
      SET(GLUT_ROOT_PATH ${SOURCE_BASE_DIR}/glut)
    ENDIF(NOT GLUT_ROOT_PATH)
  ENDIF(WIN32)
  FIND_PACKAGE(GLUT REQUIRED)
  FIND_PACKAGE(GLEW REQUIRED)

  FIND_PACKAGE(OpenGL)
  IF(OPENGL_FOUND)
    if(APPLE)
            ADD_DEFINITIONS(-DHAVE_APPLE_OPENGL_FRAMEWORK)
            include_directories(${OPENGL_INCLUDE_DIR}})
    endif(APPLE)
    LIST(APPEND common_libs ${OPENGL_LIBRARIES})
  ENDIF(OPENGL_FOUND)


  ADD_DEFINITIONS(-DHAVE_LIBGLEW)
  include_directories(${GLEW_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
  LIST(APPEND common_libs ${GLEW_LIBRARIES} ${GLUT_LIBRARIES})
ENDIF(ENABLE_GPU)

IF(ENABLE_SSE2)
  IF(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -msse2 -mtune=pentium4")
  ELSEIF(MSVC)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /arch:SSE2")
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(ENABLE_SSE2)

IF(ENABLE_DMALLOC)
  ADD_DEFINITIONS(-DDMALLOC)
  IF(ENABLE_DMALLOC_FUNC_CHECK)
    ADD_DEFINITIONS(-DDMALLOC_FUNC_CHECK)
  ENDIF(ENABLE_DMALLOC_FUNC_CHECK)

  IF(CMAKE_USE_PTHREADS_INIT)
    FIND_LIBRARY(dmalloc_LIBRARIES dmallocthcxx)
  ELSE(CMAKE_USE_PTHREADS_INIT)
    FIND_LIBRARY(dmalloc_LIBRARIES dmalloccxx)
  ENDIF(CMAKE_USE_PTHREADS_INIT)

  IF(NOT dmalloc_LIBRARIES)
    MESSAGE(FATAL_ERROR "dmalloc support requested, but was unable to locate dmalloc library")
  ENDIF(NOT dmalloc_LIBRARIES)

  add_definitions("-g")
  LIST(APPEND common_libs ${dmalloc_LIBRARIES})
ENDIF(ENABLE_DMALLOC)


# Try to create config.h
set(ENBLEND_VERSION_ONLY "${ENBLEND_MAJOR_VERSION}.${ENBLEND_MINOR_VERSION}-${PACKAGE_REVISION}")
set(PACKAGE_STRING "enblend-enfuse ${ENBLEND_VERSION_ONLY}")
set(ENBLEND_VERSION "${PACKAGE_STRING}")
include(ConfigureChecks.cmake)
configure_file(config.h.cmake ${CMAKE_BINARY_DIR}/config.h)
add_definitions(-DHAVE_CONFIG_H)

include_directories(${CMAKE_BINARY_DIR} ${TOP_SRC_DIR}/include)

MESSAGE(STATUS "")
MESSAGE(STATUS "")
MESSAGE(STATUS "Enblend-enfuse now configured for ${CMAKE_SYSTEM}")
MESSAGE(STATUS "Source Directory:        ${CMAKE_SOURCE_DIR}")
message(STATUS "Build Directory:         ${CMAKE_BINARY_DIR}")
message(STATUS "Installation Directory:  ${CMAKE_INSTALL_PREFIX}")
MESSAGE(STATUS "C++ Compiler:            ${CMAKE_CXX_COMPILER}")
MESSAGE(STATUS "CFLAGS:                  ${CMAKE_C_FLAGS}")
MESSAGE(STATUS "CXXFLAGS:                ${CMAKE_CXX_FLAGS}")
MESSAGE(STATUS "Libraries:               ${common_libs}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Features Enabled:")
IF(NOT WIN32)
MESSAGE(STATUS "enable malloc debugging: ${ENABLE_DMALLOC}")
ENDIF(NOT WIN32)
MESSAGE(STATUS "use image cache:         ${ENABLE_IMAGECACHE}")
MESSAGE(STATUS "build GPU acceleration:  ${ENABLE_GPU}")
MESSAGE(STATUS "use OpenMP:              ${ENABLE_OPENMP}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Image Formats Enabled:")
MESSAGE(STATUS "OpenEXR:                 ${OPENEXR_FOUND}")
MESSAGE(STATUS "JPEG:                    ${JPEG_FOUND}")
MESSAGE(STATUS "TIFF:                    ${TIFF_FOUND}")
MESSAGE(STATUS "PNG:                     ${PNG_FOUND}")
MESSAGE(STATUS "")
MESSAGE(STATUS ${WARNINGS})
MESSAGE(STATUS "")


add_subdirectory(src)

# you really should have perl installed, when creating doc's
if (PERL_FOUND AND IMAGEMAGICK_FOUND AND DOC)
  # this should be set from comman line
  # Possible values like described in README: @smallbook @afivepaper @afourpaper @afourwide @afourlatex
  # @letter is default
  set(EXTRATEXI2DVIFLAGS "@afourpaper" CACHE STRING "Papersize: @letter @smallbook @afivepaper @afourpaper @afourwide @afourlatex")
  add_subdirectory(doc)
endif()

# Windows installer packaging related stuff
# prepare files here...
IF(WIN32)
  # install into place in build-dir
  SET( CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/INSTALL/FILES CACHE FILEPATH "install prefix" FORCE)
  SET( INSTALL_WIN_FILES AUTHORS ChangeLog COPYING NEWS README )
  INSTALL(FILES ${INSTALL_WIN_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX})
  # install Eriks droplets
  SET( DROPLETS_FILES contrib/enfuse_droplet/enfuse_droplet.bat
    contrib/enfuse_droplet/enfuse_droplet_360.bat contrib/enfuse_droplet/enfuse_droplet_readme.txt
    contrib/enfuse_droplet/LICENCE.txt)
  INSTALL(FILES ${DROPLETS_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/droplets)
ELSE(WIN32)
  foreach(_manual enblend enfuse)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/src/${_manual}.1
      DESTINATION "share/man/man1" CONFIGURATIONS Release RelWithDebInfo MinSizeRel)
  endforeach()
ENDIF(WIN32)

SET(CPACK_PACKAGE_VERSION_MAJOR ${ENBLEND_MAJOR_VERSION})
SET(CPACK_PACKAGE_VERSION_MINOR ${ENBLEND_MINOR_VERSION})

# Use dpkg-shlibs to get the dependences
SET(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)

# needed by rpm
SET(CPACK_SET_DESTDIR "ON")

# The next ones are required by debian
SET(CPACK_PACKAGE_CONTACT "hugin-ptx@googlegroups.com")

include(CPack)

