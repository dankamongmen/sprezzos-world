# - Find JPEG for Hugin 0.7 01Nov2007 TKSharpless
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
#  JPEG_INCLUDE_DIR, where to find headers
#  JPEG_LIBRARIES, list of release link libraries
#  JPEG_DEBUG_LIBRARIES, list of debug link libraries
#  JPEG_FOUND, If != "YES", do not try to use JPEG

FIND_PATH(JPEG_INCLUDE_DIR jpeglib.h
  /usr/local/include
  /usr/include
  ${SOURCE_BASE_DIR}/jpeg-8d
  ${SOURCE_BASE_DIR}/jpeg-7
)

include(FindLibraryForCPU)

find_library_for_cpu(JPEG_LIBRARIES
  WIN32_DEBUG_POSTFIX d
  NAMES jpeg libjpeg
  PATHS 
    ${SYSTEM_LIB_DIRS}
    ${SOURCE_BASE_DIR}/jpeg-8d/lib
    ${SOURCE_BASE_DIR}/jpeg-7/lib
)


include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(JPEG DEFAULT_MSG 
                                  JPEG_INCLUDE_DIR JPEG_LIBRARIES)

MARK_AS_ADVANCED(JPEG_INCLUDE_DIR JPEG_LIBRARIES )

