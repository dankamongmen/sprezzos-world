# used to copy files or directories for <source> to <build>- directory
# copied files will also be installed
macro(doc_copybin _depend_list _basename)
  set_source_files_properties(${_basename} GENERATED)
  if (${ARGV1} MATCHES "DIR")
    set(cp_command "copy_directory")
    install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${_basename}" DESTINATION ${DocumentsDirectory})
  else()
    set(cp_command "copy")
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${_basename}" DESTINATION ${DocumentsDirectory})
  endif()
  add_custom_command(
    OUTPUT ${_basename}
    COMMAND ${CMAKE_COMMAND} -E ${cp_command} "${TOP_SRC_DIR}/doc/${_basename}" ${_basename}
    DEPENDS "${TOP_SRC_DIR}/doc/${_basename}"
    COMMENT "Copy ${TOP_SRC_DIR}/doc/${_basename} ${_basename}"
    )
  list(APPEND ${_depend_list} ${_basename})
endmacro(doc_copybin)

# used to create config-h.texi
macro(doc_create_configh_texi _depend_list _basename2)
  add_custom_command(
    OUTPUT  "${_basename2}"
    COMMAND ${PERL_EXECUTABLE} "${TOP_SRC_DIR}/doc/define2set.pl"
    "${CMAKE_BINARY_DIR}/config.h" ">" "${_basename2}"
    DEPENDS "${CMAKE_BINARY_DIR}/config.h"
    COMMENT "Creating ${_basename2}"
    )
  list(APPEND ${_depend_list} "${_basename2}")
endmacro()

# used to create "eps", "gif", "pdf" and "png" from fig
macro(doc_create_fig2dev _depend_list _basename _ext)
  add_custom_command(
    OUTPUT "${_basename}.${_ext}"
    COMMAND ${FIG2DEV_EXE} -L ${_ext} "${TOP_SRC_DIR}/doc/${_basename}.fig" ">" "${_basename}.${_ext}"
    DEPENDS "${TOP_SRC_DIR}/doc/${_basename}.fig"
    COMMENT "fig2dev: Creating ${_basename}.${_ext} from ${TOP_SRC_DIR}/doc/${_img}.fig"
    )
  list(APPEND ${_depend_list} ${_basename}.${_ext})
endmacro()

# used to create "txt", "pdf", "eps" and "svg" from "gp" via gnuplot
macro(doc_create_gp_images _depend_list _basename)
  set(_created)
  foreach(_ext "txt" "pdf" "eps" "svg")
    list(APPEND _created "${CMAKE_CURRENT_BINARY_DIR}/${_basename}.${_ext}")
  endforeach(_ext)
  add_custom_command(
    OUTPUT ${_created}
    COMMAND ${GNUPLOT_EXECUTABLE} ${_basename}.gp
    DEPENDS ${_basename}.gp
    COMMENT "gnuplot: Creating ${_created} from ${CMAKE_CURRENT_BINARY_DIR}/${_basename}.gp"
    )
  install(FILES ${_created} DESTINATION ${DocumentsDirectory})
  list(APPEND ${_depend_list} ${_created})
endmacro(doc_create_gp_images)

# used to create "gp" from "gp.in"
macro(doc_create_gp_in_files _depend_list _basename)
  set(_basename1 ${_basename}.gp.in)
  set(_basename2 ${_basename}.gp)
  add_custom_command(
    OUTPUT  "${_basename2}"
    COMMAND ${PERL_EXECUTABLE} "${TOP_SRC_DIR}/doc/ReplaceValues.pl"
    "srcdir=${srcdir}" "RASTER_DIR=${RASTER_DIR}" "${TOP_SRC_DIR}/doc/${_basename1}" ">" "${_basename2}"
    DEPENDS "${TOP_SRC_DIR}/doc/${basename1}"
    COMMENT "Creating ${_basename2}, basename1=${_basename1}"
    )
  list(APPEND ${_depend_list} "${_basename2}")
endmacro()

# Select files matching the extension and add the names without the extension
# to the destination list
macro(doc_create_selection _list _ext _destlist)
  set(${_destlist})		# empty destination list first
  foreach(_src ${${_list}})
    if(${_src} MATCHES "^(.+)\\.${_ext}$") #select from the list those with expected extension
      list(APPEND ${_destlist} ${CMAKE_MATCH_1}) # Add the base name without extension only
    endif()
  endforeach(_src)
endmacro(doc_create_selection)

# used to create "gif", "eps" and "png" from "tif"
macro(doc_create_tif_images _depend_list _basename)
  foreach(_type "gif" "eps" "png")
    set(_basename2 ${_basename}.${_type})
    add_custom_command(
      OUTPUT "${_basename}.${_type}"
      COMMAND ${IMAGEMAGICK_CONVERT_EXECUTABLE} "${TOP_SRC_DIR}/doc/${_basename}.tif" "${_basename2}"
      DEPENDS "${TOP_SRC_DIR}/doc/${_basename}.tif"
      COMMENT "convert: creating ${_basename2}"
      )
    list(APPEND ${_depend_list} "${_basename2}")
  endforeach()
endmacro()

# used to create "txt" from <source>/"fig"
macro(doc_create_txt_from_fig _depend_list _basename)
    add_custom_command(
    OUTPUT "${_basename}.txt"
    COMMAND ${PERL_EXECUTABLE} "${TOP_SRC_DIR}/doc/fig2txt.pl" "${TOP_SRC_DIR}/doc/${_basename}.fig" ">" "${_basename}.txt"
    DEPENDS "${TOP_SRC_DIR}/doc/${_basename}.fig"
    COMMENT "perl: creating ${_basename}.txt"
    )
  list(APPEND  ${_depend_list} "${_basename}.txt")
endmacro()

# create vars*.texi files
macro(doc_varscreate_file _depend_list _basename)
  set(_depends "${TOP_SRC_DIR}/src/${_basename}.cc"
    "${TOP_SRC_DIR}/src/bounds.h" "${TOP_SRC_DIR}/src/common.h"
    "${TOP_SRC_DIR}/src/global.h")
  set(_basename2 "vars${_basename}.texi")
  add_custom_command(
    OUTPUT "${_basename2}"
    COMMAND ${PERL_EXECUTABLE} "${TOP_SRC_DIR}/doc/docstrings" ${_depends} ">" "${_basename2}"
    DEPENDS ${_depends}
    COMMENT "perl: creating ${_basename2}"
    )
  list(APPEND ${_depend_list} "${_basename2}")
endmacro()

# used to create vers*.texi
macro(doc_verscopy_file _depend_list _basename)
  add_custom_command(
    OUTPUT "${_basename}"
    COMMAND COMMAND ${PERL_EXECUTABLE} "${TOP_SRC_DIR}/doc/CreateVersTexi.pl" "${TOP_SRC_DIR}/doc/${_basename}" "${_version_lines}" ${A4} > "${_basename}"
    DEPENDS "${TOP_SRC_DIR}/doc/${_basename}"
    COMMENT "perl: ${TOP_SRC_DIR}/doc/${_basename} -> ${_basename}"
    )
  list(APPEND ${_depend_list} ${_basename})
endmacro()

# copy from source-dir to build-dir
macro(doc_copy_file _basename _basename2)
  add_custom_command(
    OUTPUT "${_basename2}"
    COMMAND ${CMAKE_COMMAND} -E copy "${TOP_SRC_DIR}/doc/${_basename}" "${_basename2}"
    DEPENDS "${TOP_SRC_DIR}/doc/${_basename}"
    COMMENT "Copy ${TOP_SRC_DIR}/doc/${_basename} ${_basename2}"
    )
  list(APPEND copy_cmake_depends ${_basename2})
endmacro()
