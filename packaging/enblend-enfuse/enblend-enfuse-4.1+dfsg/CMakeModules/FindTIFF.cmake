# - Find TIFF for Hugin 0.7 01Nov2007 TKSharpless
# Added to support Windows build but should work anywhere.
# After looking in UNIX standard places, tries wxWidgets build 
# tree, which should have this package.
#
# Call FIND_PACKAGE(wxWidgets REQUIRED) before calling this!
# 
# reads cache variables
#  wxWidgets_ROOT_DIR
#  wxWidgets_LIB_DIR
# defines cache variables
#  TIFF_INCLUDE_DIR, where to find headers
#  TIFF_LIBRARIES, list of link libraries for release
#  TIFF_DEBUG_LIBRARIES ditto for debug
#  TIFF_FOUND, If != "YES", do not try to use TIFF.

FIND_PATH(TIFF_INCLUDE_DIR tiff.h
  /usr/local/include
  /usr/include
  ${SOURCE_BASE_DIR}/tiff-4.0.3/libtiff
  ${SOURCE_BASE_DIR}/tiff-4.0.0beta5/libtiff
  ${SOURCE_BASE_DIR}/tiff-3.9.2/libtiff
  ${SOURCE_BASE_DIR}/tiff-3.8.2/libtiff
)

include(FindLibraryWithDebug)

find_library_with_debug(TIFF_LIBRARIES
  WIN32_DEBUG_POSTFIX d
  NAMES tiff libtiff 
  PATHS ${SYSTEM_LIB_DIRS}
        ${SOURCE_BASE_DIR}/tiff-4.0.3/libtiff
        ${SOURCE_BASE_DIR}/tiff-4.0.0beta5/libtiff
        ${SOURCE_BASE_DIR}/tiff-3.9.2/libtiff
        ${SOURCE_BASE_DIR}/tiff-3.8.2/libtiff
)


include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(TIFF DEFAULT_MSG 
                                  TIFF_INCLUDE_DIR TIFF_LIBRARIES)

MARK_AS_ADVANCED(TIFF_INCLUDE_DIR TIFF_LIBRARIES)

