include(LibAddMacros)

# - adds a binding if it is included in BINDINGS and not exluded.
#   If the binding is exluded the reason is printed along with the binding name.
#   This is function is intended to be used in a CMakeLists.txt in the directory
#   above or within the actual binding directory.
#
# try_add_binding (BINDING_NAME OUTVARIABLE)
#
# BINDING_NAME:
#   name of the binding
#
# OUTVARIABLE:
#   variable that is set to true if binding was added
#
# SILENT:
#   suppress exclusion message
#
# Additional options are passed to check_item_is_excluded ().
# Default for SUBDIRECTORY is BINDING_NAME
#
# Example:
#   try_add_binding ("swig_python" IS_INCLUDED SUBDIRECTORY "python")
#   if (IS_INCLUDED)
#     add_subdirectory (python)
#   endif ()
#
function (try_add_binding BINDING_NAME OUTVARIABLE)
	cmake_parse_arguments (ARG
		"SILENT" # optional keywords
		"" # one value keywords
		"" # multi value keywords
		${ARGN}
	)

	check_item_is_excluded (IS_EXCLUDED BINDINGS ${BINDING_NAME} SUBDIRECTORY ${BINDING_NAME} ${ARGN})
	if (IS_EXCLUDED)
		if (ARG_SILENT)
			# make sure that the exclusion message is not printed
			set (IS_EXCLUDED "silent")
		endif ()
		remove_binding (${BINDING_NAME} ${IS_EXCLUDED} REMOVE_NOT_NECESSARY)
		set (${OUTVARIABLE} "" PARENT_SCOPE)
	else ()
		add_binding_manual (${BINDING_NAME})
		set (${OUTVARIABLE} "YES" PARENT_SCOPE)
	endif ()
endfunction (try_add_binding)

# - add a binding to list of bindings that will be built.
#   Can be used if a binding depends on another binding and you want to
#   silently include it rather than display an error.
#
# add_binding_manual (BINDING_NAME)
#
# BINDING_NAME:
#   name of the binding
#
# Example:
#   add_binding_manual ("anynameyouwant")
#
function (add_binding_manual BINDING_NAME)
	if (ADDED_BINDINGS)
		set (TMP "${ADDED_BINDINGS};${BINDING_NAME}")
		list (SORT TMP)
		list (REMOVE_DUPLICATES TMP)
		set (ADDED_BINDINGS "${TMP}" CACHE STRING "${ADDED_BINDINGS_DOC}" FORCE)
	else ()
		set (ADDED_BINDINGS "${BINDING_NAME}" CACHE STRING "${ADDED_BINDINGS_DOC}" FORCE)
	endif ()
endfunction (add_binding_manual)

# - check if a binding is included in the BINDINGS list given by the user.
#   If it is included it is not guaranteed that the binding will be built
#   (dependencies can be missing, etc.)
#   This is function can be used anywhere since the base directory is fixed to
#   the bindings directory. However if the binding uses subdirectories
#   (like swig, gi or io) you need to supply the correct SUBDIRECTORY option
#   (see examples).
#
# check_binding_included (BINDING_NAME OUTVARIABLE)
#
# BINDING_NAME:
#   name of the binding
#
# OUTVARIABLE:
#   variable that is set to true if binding was added
#
# Additional options are passed to check_item_is_excluded ().
# Default for SUBDIRECTORY is BINDING_NAME
#
# Example:
#   check_binding_included ("swig_python" IS_INCLUDED SUBDIRECTORY "swig/python")
#   if (IS_INCLUDED)
#     add_subdirectory (python)
#   endif ()
#
function (check_binding_included BINDING_NAME OUTVARIABLE)
	check_item_is_excluded (IS_EXCLUDED BINDINGS ${BINDING_NAME} SUBDIRECTORY ${BINDING_NAME} BASEDIRECTORY "${CMAKE_SOURCE_DIR}/src/bindings" ${ARGN})
	if (IS_EXCLUDED)
		set (${OUTVARIABLE} "" PARENT_SCOPE)
	else ()
		set (${OUTVARIABLE} "YES" PARENT_SCOPE)
	endif ()
endfunction (check_binding_included)

# - check if a binding will be built.
#   Can only be used run after bindins have been processed (e.g. in
#   ADDTESTING_PHASE of plugins)
#   This is function can be used anywhere.
#
# check_binding_was_added (BINDING_NAME OUTVARIABLE)
#
# BINDING_NAME:
#   name of the binding
#
# OUTVARIABLE:
#   variable that is set to true if binding was added
#
# Example:
#   check_binding_was_added ("swig_python" WAS_ADDED)
#   if (NOT WAS_ADDED)
#     message (WARNING "swig_python bindings are required for testing, test deactivated")
#   endif ()
#
function (check_binding_was_added BINDING_NAME OUTVARIABLE)
	list (FIND ADDED_BINDINGS ${BINDING_NAME} FINDEX)
	if (FINDEX GREATER -1)
		set (${OUTVARIABLE} "YES" PARENT_SCOPE)
	else ()
		set (${OUTVARIABLE} "" PARENT_SCOPE)
	endif ()
endfunction (check_binding_was_added)

#- Determines if ITEM_NAME is not included from LIST by
#  explicit mention or by category
#
#  check_item_is_excluded (OUTVARIABLE LIST ITEM_NAME)
#
# OUTVARIABLE:
#   name of variable in which exclusion reason are stored
#
# LIST:
#   name of variable with list of items or categories
#
# ITEM_NAME:
#   name of item in list
#
# SUBDIRECTORY:
#   (optional) set subdirectory for README.md. Defaults to current directory
#
# NO_CATEGORIES:
#   (optional) disable categories
#
# ENABLE_PROVIDES:
#   (optional) add "infos/provides" from README.md to categories and expand the
#   list with split_plugin_providers()
#
#
# example:
# 	# BINDINGS="ALL;-EXPERIMENTAL"
# 	check_item_is_excluded (IS_EXCLUDED BINDINGS "io_uv")
# 	if (IS_EXCLUDED)
# 		remove_something ("io_uv" ${IS_EXCLUDED})
# 	endif ()
#
function (check_item_is_excluded OUTVARIABLE LIST ITEM_NAME)
	cmake_parse_arguments (ARG
		"ENABLE_PROVIDES;NO_CATEGORIES" # optional keywords
		"SUBDIRECTORY;BASEDIRECTORY" # one value keywords
		"" # multi value keywords
		${ARGN}
	)
	set (${OUTVARIABLE} "" PARENT_SCOPE)

	list (FIND ${LIST} "-${ITEM_NAME}" FOUND_EXCLUDE_NAME)
	if (FOUND_EXCLUDE_NAME GREATER -1)
		set (${OUTVARIABLE} "explicitly excluded" PARENT_SCOPE)
		# let explicit exclusion win

		return ()
	endif ()

	list (FIND ${LIST} "${ITEM_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		set (${OUTVARIABLE} "silent" PARENT_SCOPE)
		# maybe it is included by category
	else ()
		# plugin is given explicit
		return ()
	endif ()

	if (ARG_NO_CATEGORIES)
		# we are done if categories are disabled
		return ()
	endif ()

	if (NOT ARG_BASEDIRECTORY)
		set (ARG_BASEDIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
	endif ()
	if (NOT ARG_SUBDIRECTORY)
		set (README_FILE ${ARG_BASEDIRECTORY}/README.md)
	else ()
		set (README_FILE ${ARG_BASEDIRECTORY}/${ARG_SUBDIRECTORY}/README.md)
	endif ()
	if (NOT EXISTS ${README_FILE})
		# we need README.md for extracting categories
		message (WARNING "readme file does not exist at ${README_FILE}")
	else ()
		FILE(READ ${README_FILE} contents)
		STRING (REGEX MATCH "- +infos/status *= *([-a-zA-Z0-9 ]*)" CATEGORIES "${contents}")
		STRING (REGEX REPLACE "- +infos/status *= *([-a-zA-Z0-9 ]*)" "\\1" CATEGORIES "${CATEGORIES}")
		STRING (REGEX REPLACE " " ";" CATEGORIES "${CATEGORIES}")

		if (ARG_ENABLE_PROVIDES)
			STRING (REGEX MATCH "- +infos/provides *= *([a-zA-Z0-9/ ]*)" PROVIDES "${contents}")
			STRING (REGEX REPLACE "- +infos/provides *= *([a-zA-Z0-9/ ]*)" "\\1" PROVIDES "${PROVIDES}")
			STRING (REGEX REPLACE " " ";" PROVIDES "${PROVIDES}")
			split_plugin_providers (PROVIDES)
			list (APPEND CATEGORIES "${PROVIDES}")
		endif()
	endif()
	list (APPEND CATEGORIES "ALL")

	STRING (TOUPPER "${CATEGORIES}" CATEGORIES)
	#message (STATUS "CATEGORIES FOUND FOR ${ITEM_NAME}: ${CATEGORIES}")

	foreach (CAT ${CATEGORIES})
		list (FIND ${LIST} "-${CAT}" FOUND_EXCLUDE_CATEGORY)
		if (FOUND_EXCLUDE_CATEGORY GREATER -1)
			set (${OUTVARIABLE} "excluded by category ${CAT}" PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()

	foreach (CAT ${CATEGORIES})
		list (FIND ${LIST} "${CAT}" FOUND_CATEGORY)
		if (FOUND_CATEGORY GREATER -1)
			set (${OUTVARIABLE} "" PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()
endfunction ()
