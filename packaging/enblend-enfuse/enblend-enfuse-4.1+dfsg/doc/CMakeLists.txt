# This file is part of enblend.
# Licence details can be found in the file COPYING.
#
# Copyright (c) 2009-2012, Kornel Benko <Kornel.Benko@berlin.de>
#

include(GetPrerequisites)	# get function gp_append_unique()
IF(WIN32)
set(DocumentsDirectory "doc")
ELSE(WIN32)
set(DocumentsDirectory "doc/enblend")
ENDIF(WIN32)

include(${TOP_SRC_DIR}/doc/macros.cmake)
set(main_texi_list enblend enfuse)

set(_docs)
set(_enblend_Bases auxmac.texi auxmac.tex
    fdl.texi
    mask-template-characters.texi
    helpful-programs.texi
    seam-generators.texi
    color-profiles.texi
    tuning-memory-usage.texi
    understanding-masks.texi
    workflow.texi filespec.texi
    external-masks.texi
    bug-reports.texi authors.texi
    photographic-workflow.fig
    external-mask-workflow.fig
    seam-line-visualization.tif
    default.css)

set(_enfuse_Bases auxmac.texi auxmac.tex
    fdl.texi
    mask-template-characters.texi
    helpful-programs.texi
    color-profiles.texi
    tuning-memory-usage.texi
    understanding-masks.texi
    workflow.texi filespec.texi
    external-masks.texi
    bug-reports.texi authors.texi
    config-edge.gp config.gp
    entropy-cutoff.gp.in
    exposure-cutoff.gp.in
    entropy.gp.in gaussian.gp.in
    laplacian-of-gaussian.gp.in
    sharp-edge.gp.in sharp-edge.data
    smooth-edge.gp.in smooth-edge.data
    local-analysis-window.fig
    photographic-workflow.fig
    focus-stack-decision-tree.fig
    external-mask-workflow.fig
    default.css)

set(_enblend_TEXINFOS)
foreach(_e ${_enblend_Bases})
  list(APPEND _enblend_TEXINFOS "${TOP_SRC_DIR}/doc/${_e}")
endforeach(_e)

set(_enfuse_TEXINFOS)
foreach(_e ${_enfuse_Bases})
  list(APPEND _enfuse_TEXINFOS "${TOP_SRC_DIR}/doc/${_e}")
endforeach(_e)

set(_enblendenfuse_fig_images)
set(_enblendenfuse_tif_images)
set(_enblendenfuse_gp_images)

foreach(_cmd ${main_texi_list} )
  doc_create_selection(_${_cmd}_Bases "fig" _${_cmd}_fig_images)
  doc_create_selection(_${_cmd}_Bases "tif" _${_cmd}_tif_images)
  doc_create_selection(_${_cmd}_Bases "gp\\\\.in" _${_cmd}_gp_images)
  set(_${_cmd}_images ${_${_cmd}_fig_images} ${_${_cmd}_tif_images} ${_${_cmd}_gp_images})
  foreach(_i ${_${_cmd}_fig_images})
    gp_append_unique(_enblendenfuse_fig_images ${_i})
  endforeach(_i)
  foreach(_i ${_${_cmd}_tif_images})
    gp_append_unique(_enblendenfuse_tif_images ${_i})
  endforeach(_i)
  foreach(_i ${_${_cmd}_gp_images})
    gp_append_unique(_enblendenfuse_gp_images ${_i})
  endforeach(_i)
endforeach(_cmd)

project(doc0)

set(_helpfiles)

#create <build>/config.h.texi
doc_create_configh_texi(_helpfiles config-h.texi)

# create <build>/vers*.texi
foreach(_cmd ${main_texi_list})
  #message(STATUS "doc_verscopy_file vers${_cmd}.texi")
  doc_verscopy_file(_helpfiles "vers${_cmd}.texi")
endforeach(_cmd)

# create <build>/vars*.texi
foreach(_cmd ${main_texi_list})
  #message(STATUS "doc_varscreate_file ${_cmd}")
  doc_varscreate_file(_helpfiles ${_cmd})
endforeach(_cmd)

# %.txt: %.fig
foreach(_fig ${_enblendenfuse_fig_images})
  #message(STATUS "doc_create_txt_from_fig ${_tif}")
  doc_create_txt_from_fig(_helpfiles ${_fig})
endforeach(_fig)

# %.gif: %.tif
# %.png: %.tif
# %.eps: %.tif
foreach(_tif ${_enblendenfuse_tif_images})
  #message(STATUS "doc_create_tif_images ${_tif}")
  doc_create_tif_images(_helpfiles ${_tif})
endforeach(_tif)

# <source>/%.gp.in: <build>/%.gp
set(srcdir "${TOP_SRC_DIR}/doc")
set(RASTER_DIR "${CMAKE_CURRENT_BINARY_DIR}")
foreach(_gp ${_enblendenfuse_gp_images})
  message(STATUS "Handling ${_gp}.gp.in")
  # (@srcdir@ and @RASTER_DIR@) will be substituted in *.gp.in
  doc_create_gp_in_files(_helpfiles ${_gp})
endforeach()

#message(STATUS "_helpfiles = " ${_helpfiles})
add_custom_target(doc0 DEPENDS ${_helpfiles})

project(doc1)

# search for gnuplot and
# generate images (.txt, .pdf .eps and .png)
find_package(Gnuplot)
set(_depend_gp)
if (NOT DOC OR NOT GNUPLOT_FOUND)
  # there is no gnuplot command, so simply copy the needed files
  file(GLOB _copy_files RELATIVE "${TOP_SRC_DIR}/doc"
    "${TOP_SRC_DIR}/doc/*.txt"
    "${TOP_SRC_DIR}/doc/*.eps"
    "${TOP_SRC_DIR}/doc/*.png"
    "${TOP_SRC_DIR}/doc/*.pdf")
  foreach(_cf ${_copy_files})
    if(NOT ${_cf} MATCHES "en(blend|fuse)") # This is done below
      doc_copybin(_depend_gp "${_cf}")
      list(APPEND _docs "${_cf}")
    endif()
  endforeach(_cf)
else()
  #
  foreach(_gp ${_enblendenfuse_gp_images})
    doc_create_gp_images(_depend_gp ${_gp})
    list(APPEND _docs ${_created})
  endforeach(_gp)
endif()

add_custom_target(doc1 DEPENDS doc0 ${_depend_gp})
project(doc2)

# Search for fig2dev and convert some images
find_program(FIG2DEV_EXE fig2dev)
if (${FIG2DEV_EXE} MATCHES "-NOTFOUND")
  # here the creation of html and pdf will be disabled, because .gif-prerequisites are not found
  # Automake uses here sed and the like
  set(DOC3:BOOL "FALSE")
else()
  #message(STATUS "_enblendenfuse_fig_images =  ${_enblendenfuse_fig_images}")
  foreach(_img ${_enblendenfuse_fig_images})
    foreach(_ext eps gif pdf png)
      doc_create_fig2dev(_depend_gp ${_img} ${_ext})
      list(APPEND _docs ${_img}.${_ext})
    endforeach(_ext)
    if(NOSPLIT)
      install(FILES ${_img}.png DESTINATION ${DocumentsDirectory})
    endif(NOSPLIT)
  endforeach(_img)
  set(DOC3:BOOL ${DOC})
endif()
add_custom_target(doc2 DEPENDS doc1 ${_depend_gp})

project(doc3)
# Search for makeinfo command and
# generate info and pdf for enblend + enfuse
find_program(MAKEINFO_EXE "makeinfo")
if (DOC3:BOOL)
  if ( NOT ${MAKEINFO_EXE} MATCHES "-NOTFOUND")
    # Check for sufficient version, should be >= 4.11
    EXECUTE_PROCESS(COMMAND ${MAKEINFO_EXE} "--version" OUTPUT_VARIABLE _makeinfo_version OUTPUT_STRIP_TRAILING_WHITESPACE)
    foreach(_v_l ${_makeinfo_version})
      #message(STATUS "_v_l = ${_v_l}")
      if(_v_l MATCHES "\\(GNU texinfo\\) *\([0-9]+\)\\.\([0-9]+\)")
	set(MAKEINFO_MAJOR ${CMAKE_MATCH_1})
	set(MAKEINFO_MINOR1 "00000${CMAKE_MATCH_2}")
	if(MAKEINFO_MINOR1 MATCHES "^[0]+\([0-9][0-9][0-9][0-9]\)\$")
	  set(MAKEINFO_MINOR ${CMAKE_MATCH_1})
	endif()
	set(MAKEINFO_VERSION "${MAKEINFO_MAJOR}.${MAKEINFO_MINOR}")
      endif()
    endforeach()
    string(COMPARE GREATER "${MAKEINFO_VERSION}" "4.0010" DOC3:BOOL)
    #message(STATUS "MAKEINFO_VERSION = ${MAKEINFO_VERSION}")
    if (NOT DOC3:BOOL)
      message(STATUS "Sorry, but you need makeinfo version >= 4.11")
    endif()
  endif()
endif()

find_program(TEXI2DVI_EXE "texi2dvi")
if(${EXTRATEXI2DVIFLAGS} MATCHES "^\\@")
  set(TEXI2DVIFLAGS "--command=${EXTRATEXI2DVIFLAGS}")
  message(STATUS "EXTRATEXI2DVIFLAGS = ${EXTRATEXI2DVIFLAGS}")
  message(STATUS "Setting TEXI2DVIFLAGS to ${TEXI2DVIFLAGS}")
else()
  set(TEXI2DVIFLAGS)
  message(STATUS "Resetting TEXI2DVIFLAGS")
endif()

set(_auxdepends)

FILE(GLOB tmp_texi RELATIVE "${TOP_SRC_DIR}/doc" "${TOP_SRC_DIR}/doc/*.tex*")
foreach(_cmd ${main_texi_list})
  # These are already created with doc_varscreate_file()
  list(REMOVE_ITEM tmp_texi vars${_cmd}.texi)
endforeach()

foreach(_f ${tmp_texi})
  if (NOT ${_f} MATCHES "vers.*")
    add_custom_command(
      OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${_f}"
      COMMAND ${CMAKE_COMMAND} -E copy "${TOP_SRC_DIR}/doc/${_f}" "${CMAKE_CURRENT_BINARY_DIR}/${_f}"
      DEPENDS "${TOP_SRC_DIR}/doc/${_f}"
      )
    list(APPEND _auxdepends "${CMAKE_CURRENT_BINARY_DIR}/${_f}")
  endif()
endforeach()

foreach (_cmd ${main_texi_list})
  set(_created "${CMAKE_CURRENT_BINARY_DIR}/${_cmd}")
  if (NOT DOC3:BOOL OR ${MAKEINFO_EXE} MATCHES "-NOTFOUND" OR ${TEXI2DVI_EXE} MATCHES "-NOTFOUND")
    # there is no makeinfo or no texi2dvi command,
    # this implies insufficient latex installation.
    # Therefore simply copy the data from source tree.
    foreach(_ext "info")
      doc_copybin(_depend_gp "${_cmd}.${_ext}")
      list(APPEND _docs "${_cmd}.${_ext}" "${_created}.${_ext}")
    endforeach()
    #
    # Disabled, because not part of repo.
    # They would have only been created with "make pdf" or "make html" in source doc directory
    # under automake
    if (EXISTS "${TOP_SRC_DIR}/doc/${_cmd}.pdf" AND NOT IS_DIRECTORY "${TOP_SRC_DIR}/doc/${_cmd}.pdf")
      copybin(_depend_gp "${_cmd}.pdf")
      list(APPEND _docs "${_cmd}.pdf" "${_created}.pdf")
    else()
      message(STATUS "Sorry, but there is no file ${TOP_SRC_DIR}/doc/${_cmd}.pdf to copy. Leave it out")
    endif()
    if (EXISTS "${TOP_SRC_DIR}/doc/${_cmd}.html" AND IS_DIRECTORY "${TOP_SRC_DIR}/doc/${_cmd}.html")
      doc_copybin(_depend_gp "${_cmd}.html" "DIR")
      list(APPEND _docs "${_cmd}.html" "${_created}.html")
    else()
      message(STATUS "Sorry, but there is no directory ${TOP_SRC_DIR}/doc/${_cmd}.html to copy. Leave it out")
    endif()
  else()
    # here fig2dev, makeinfo and texi2dvi exist, option DOC set to ON
    #message(STATUS "create ${_created}.info")
    add_custom_command(
      OUTPUT "${_created}.info"
      COMMAND ${MAKEINFO_EXE} "--css-include=${TOP_SRC_DIR}/doc/default.css"
      "-I" "${TOP_SRC_DIR}/doc"
      -o "${_created}.info" "${TOP_SRC_DIR}/doc/${_cmd}.texi"
      DEPENDS "${TOP_SRC_DIR}/doc/${_cmd}.texi"
      ${_${_cmd}_TEXINFOS}
      ${_depend_gp}
      "${CMAKE_CURRENT_BINARY_DIR}/config-h.texi"
      COMMENT "makeinfo: Creating ${_created}.info from ${TOP_SRC_DIR}/doc/${_cmd}.texi"
      )
    install(FILES "${_created}.info" DESTINATION ${DocumentsDirectory})
    add_custom_command(
      OUTPUT "${_created}.texi"
      COMMAND ${CMAKE_COMMAND} -E copy "${TOP_SRC_DIR}/doc/${_cmd}.texi" "${_created}.texi"
      DEPENDS "${TOP_SRC_DIR}/doc/${_cmd}.texi"
      )
    #message(STATUS "${TEXI2DVI_EXE} ${TEXI2DVIFLAGS} --pdf --batch ${_created}.texi")
    file(RELATIVE_PATH _tmp_created "${CMAKE_CURRENT_BINARY_DIR}" "${_created}.texi")
    add_custom_command(
      OUTPUT "${_created}.pdf"
      COMMAND ${TEXI2DVI_EXE} ${TEXI2DVIFLAGS} "--dvipdf" "--batch" "--build=local" "${_tmp_created}"
      DEPENDS "${_created}.texi"
      ${_${_cmd}_TEXINFOS} ${_depend_gp}
      "${CMAKE_CURRENT_BINARY_DIR}/config-h.texi" ${_auxdepends}
      COMMENT "texi2dvi: Creating ${_created}.pdf from ${TOP_SRC_DIR}/doc/${_cmd}.texi, cmdline="${TEXI2DVIFLAGS}
      )
    install(FILES "${_created}.pdf" DESTINATION ${DocumentsDirectory})
    #message(STATUS "create ${_created}.html")
    if(NOSPLIT)
      set(_created_output "${_created}_file.html")
      set(_output_param "${_created}_file.html")
      set(_html_param "--no-split")
    else()
      set(_created_output "${_created}.html/index.html")
      set(_output_param "${_created}.html")
      set(_html_param)
    endif(NOSPLIT)
    add_custom_command(
      OUTPUT "${_created_output}"
      COMMAND ${MAKEINFO_EXE} "--html" ${_html_param} "--css-include=${TOP_SRC_DIR}/doc/default.css"
      "-I" "${TOP_SRC_DIR}/doc"
      -o "${_output_param}" "${TOP_SRC_DIR}/doc/${_cmd}.texi"
      DEPENDS "${TOP_SRC_DIR}/doc/${_cmd}.texi" ${_${_cmd}_TEXINFOS} ${_depend_gp}
      COMMENT "makeinfo: Creating ${_created}.html from ${TOP_SRC_DIR}/doc/${_cmd}.texi"
      )
    if(NOSPLIT)
      install(FILES "${_created}_file.html" DESTINATION ${DocumentsDirectory} RENAME "${_cmd}.html")
    else(NOSPLIT)
      foreach(_img ${_${_cmd}_images})
	set(_needed_image "${_created}.html/${_img}.png")
	set(_parent_image "${CMAKE_CURRENT_BINARY_DIR}/${_img}.png")
	#message(STATUS "_needed_image = ${_needed_image}")
	add_custom_command(
	  OUTPUT "${_needed_image}"
	  COMMAND ${CMAKE_COMMAND} -E copy "${_parent_image}" "${_needed_image}"
	  DEPENDS "${_created}.html/index.html" "${_parent_image}"
	  COMMENT "Copying ${_parent_image} to ${_needed_image}"
	  )
	list(APPEND _docs "${_needed_image}")
      endforeach(_img)
      install(DIRECTORY "${_created}.html" DESTINATION ${DocumentsDirectory})
    endif(NOSPLIT)
    list(APPEND _docs "${_created}.info" "${_created}.pdf" "${_created_output}")
  endif()
endforeach(_cmd)

add_custom_target(doc3 ALL DEPENDS ${_docs})

if(DOC3:BOOL)
  # set make order
  add_dependencies(doc3 doc2 doc1 doc0 enblendenfuse)
endif()


