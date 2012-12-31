
IF(NOT WIN32)
    FIND_LIBRARY(LCMS2_LIBRARIES lcms2 HINTS /usr/lib/x86_64-linux-gnu /usr/lib32)
ELSE(NOT WIN32)
    FIND_PATH(LCMS2_ROOT_DIR
      NAMES include/lcms2.h
      PATHS /usr/local
        /usr
        ${SOURCE_BASE_DIR}
      PATH_SUFFIXES
        lcms2-2.5
        lcms2-2.4
        lcms2-2.3
    )
    
    FIND_PATH(LCMS2_INCLUDE_DIR 
      NAMES lcms2.h
      PATHS
        /usr/local/include
        /usr/include
        ${LCMS2_ROOT_DIR}/include
    )

    FIND_LIBRARY(LCMS2_LIBRARIES
      WIN32_DEBUG_POSTFIX d    
      NAMES lcms2 lcms2_static
      PATHS /usr/local/include /usr/include ${LCMS2_ROOT_DIR}/Lib/MS
    )
    
    MARK_AS_ADVANCED(
      LCMS_ROOT_DIR
      LCMS_LIBRARIES
      LCMS_INCLUDE_DIR
      )        
ENDIF(NOT WIN32) 