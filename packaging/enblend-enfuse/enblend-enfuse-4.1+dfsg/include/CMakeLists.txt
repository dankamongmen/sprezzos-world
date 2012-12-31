
include_directories(${KDE4_INCLUDES} ${KDE4_INCLUDE_DIR} ${QT_INCLUDES} )

add_subdirectory(vigra)
add_subdirectory(vigra_ext)


########### install files ###############




#original Makefile.am contents follow:

#SUBDIRS = vigra vigra_ext 
