include (LibAddMacros)
# ~~~
# QUICK INTRODUCTION:
# > To add a binding the following steps have to be performed:
#   - call "check_binding_included ()": this prints exclusion messages based on categories
#   - based on dependency checks
#    a) call "exclude_binding ()": this prints a custom exclusion message
#    b) call "add_binding ()": this prints the inclusion message
#
# > To check if a binding is built there are two options:
#   - General check (i.e. is this binding included by categories): "check_binding_included ()"
#   - Check if "add_binding ()" was called: "check_binding_was_added ()"
#     Note that this only works after the CMake has processed the scripts in
#     "bindings" directory. As a result this only works for CMake scripts in the
#     "tools", "include" and "plugins" (ADDTESTING_PHASE) directories.
#
# ~~~

# ~~~
# - Adds a binding if it is included in BINDINGS and not excluded by name or
#   category (infos/status or infos/provides from it's README.md).
#
#   If the binding is excluded the reason is printed along with the binding name.
#
#   This is function can be used anywhere since the base directory is fixed to
#   the bindings source directory. However if the binding uses subdirectories
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
# SILENT:
#   suppress exclusion message
#
# Additional options are passed to check_item_is_excluded ().
# Default for SUBDIRECTORY is BINDING_NAME
#
# Example:
#   check_binding_included ("python" IS_INCLUDED SUBDIRECTORY "swig/python")
#   if (IS_INCLUDED)
#     add_subdirectory (python)
#   endif ()
# ~~~
function (check_binding_included BINDING_NAME OUTVARIABLE)
	cmake_parse_arguments (
		ARG
		"SILENT" # optional keywords
		"" # one value keywords
		"" # multi value keywords
		${ARGN})

	check_item_is_excluded (
		IS_EXCLUDED
		BINDINGS
		${BINDING_NAME}
		SUBDIRECTORY
		${BINDING_NAME}
		BASEDIRECTORY
		"${CMAKE_SOURCE_DIR}/src/bindings"
		ENABLE_PROVIDES
		${ARGN})
	if (IS_EXCLUDED)
		if (ARG_SILENT) # make sure that the exclusion message is not printed
			set (IS_EXCLUDED "silent")
		endif ()
		exclude_binding (${BINDING_NAME} ${IS_EXCLUDED})
		set (
			${OUTVARIABLE}
			"NO"
			PARENT_SCOPE)
	else ()
		set (
			${OUTVARIABLE}
			"YES"
			PARENT_SCOPE)
	endif ()
endfunction (check_binding_included)

# ~~~
# - Add a binding to list of bindings that will be built and print corresponding
#  message.
#
#  add_binding (BINDING_NAME)
#
#  BINDING_NAME:
#    name of the binding
#
#  example:
#    add_binding ("anynameyouwant")
# ~~~
function (add_binding BINDING_NAME)
	cmake_parse_arguments (
		ARG
		"ONLY_SHARED" # optional keywords
		"" # one value keywords
		"" # multi value keywords
		${ARGN})

	if (ADDED_BINDINGS)
		set (TMP "${ADDED_BINDINGS};${BINDING_NAME}")
		list (SORT TMP)
		list (REMOVE_DUPLICATES TMP)
		set (
			ADDED_BINDINGS
			"${TMP}"
			CACHE STRING "${ADDED_BINDINGS_DOC}" FORCE)
	else ()
		set (
			ADDED_BINDINGS
			"${BINDING_NAME}"
			CACHE STRING "${ADDED_BINDINGS_DOC}" FORCE)
	endif ()

	set (STATUS_MESSAGE "Include binding ${BINDING_NAME}")
	if (ARG_ONLY_SHARED)
		set (STATUS_MESSAGE "${STATUS_MESSAGE} for shared builds")
	endif (ARG_ONLY_SHARED)
	message (STATUS "${STATUS_MESSAGE}")
endfunction (add_binding)

# ~~~
# - Remove a binding from the global cache
#
#  exclude_binding (name reason)
#
#  name
#    binding name
#
#  reason
#    reason for exclusion
#
#  REMOVE
#    (optional) remove already added with add_binding.
#    Consistency check: If not given it is made sure that the binding is not on
#    the list.
#
# example:
#  exclude_binding (fstab "mntent is missing")
# ~~~
function (exclude_binding name reason)
	cmake_parse_arguments (
		ARG
		"REMOVE" # optional keywords
		"" # one value keywords
		"" # multi value keywords
		${ARGN})

	if (NOT ${reason} STREQUAL "silent")
		message (STATUS "Exclude binding ${name} because ${reason}")
	endif ()
	if (ARG_REMOVE)
		if (ADDED_BINDINGS)
			set (TMP ${ADDED_BINDINGS})
			list (REMOVE_ITEM TMP ${name})
			set (
				ADDED_BINDINGS
				${TMP}
				CACHE STRING ${ADDED_BINDINGS_DOC} FORCE)
		endif ()
	else ()
		list (FIND ADDED_BINDINGS "${name}" FOUND_NAME)
		if (FOUND_NAME GREATER -1)
			message (WARNING "Internal inconsistency: REMOVE_NOT_NECESSARY given but ${name} is present in bindings!")
		endif ()
	endif ()
endfunction (exclude_binding)

# ~~~
# - Check if a binding will be built.
#   Can only be used run after bindings have been processed (e.g. in
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
#   check_binding_was_added ("python" WAS_ADDED)
#   if (NOT WAS_ADDED)
#     message (WARNING "python bindings are required for testing, test deactivated")
#   endif ()
# ~~~
function (check_binding_was_added BINDING_NAME OUTVARIABLE)
	list (FIND ADDED_BINDINGS ${BINDING_NAME} FINDEX)
	if (FINDEX GREATER -1)
		set (
			${OUTVARIABLE}
			"YES"
			PARENT_SCOPE)
	else ()
		set (
			${OUTVARIABLE}
			"NO"
			PARENT_SCOPE)
	endif ()
endfunction (check_binding_was_added)

# ~~~
# - Determines if ITEM_NAME is not included from LIST by
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
# ~~~
function (check_item_is_excluded OUTVARIABLE LIST ITEM_NAME)
	cmake_parse_arguments (
		ARG
		"ENABLE_PROVIDES;NO_CATEGORIES" # optional keywords
		"SUBDIRECTORY;BASEDIRECTORY" # one value keywords
		"" # multi value keywords
		${ARGN})
	set (
		${OUTVARIABLE}
		"NO"
		PARENT_SCOPE)

	list (FIND ${LIST} "-${ITEM_NAME}" FOUND_EXCLUDE_NAME)
	if (FOUND_EXCLUDE_NAME GREATER -1)
		set (
			${OUTVARIABLE}
			"explicitly excluded"
			PARENT_SCOPE) # let explicit exclusion win

		return ()
	endif ()

	list (FIND ${LIST} "${ITEM_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		set (
			${OUTVARIABLE}
			"silent"
			PARENT_SCOPE) # maybe it is included by category

	else ()

		return () # plugin is given explicit
	endif ()

	if (ARG_NO_CATEGORIES)

		return () # we are done if categories are disabled
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
		file (READ ${README_FILE} contents)
		string (REGEX MATCH "- +infos/status *= *([-a-zA-Z0-9/ ]*)" CATEGORIES "${contents}")
		string (REGEX REPLACE "- +infos/status *= *([-a-zA-Z0-9/ ]*)" "\\1" CATEGORIES "${CATEGORIES}")
		string (REGEX REPLACE "[/ ]" ";" CATEGORIES "${CATEGORIES}")

		if (ARG_ENABLE_PROVIDES)
			string (REGEX MATCH "- +infos/provides *= *([a-zA-Z0-9/ ]*)" PROVIDES "${contents}")
			string (REGEX REPLACE "- +infos/provides *= *([a-zA-Z0-9/ ]*)" "\\1" PROVIDES "${PROVIDES}")
			string (REGEX REPLACE " " ";" PROVIDES "${PROVIDES}")
			split_plugin_providers (PROVIDES)
			list (APPEND CATEGORIES "${PROVIDES}")
		endif ()
	endif ()
	list (APPEND CATEGORIES "ALL")

	string (TOUPPER "${CATEGORIES}" CATEGORIES) # message (STATUS "CATEGORIES FOUND FOR ${ITEM_NAME}: ${CATEGORIES}")

	foreach (CAT ${CATEGORIES})
		list (FIND ${LIST} "-${CAT}" FOUND_EXCLUDE_CATEGORY)
		if (FOUND_EXCLUDE_CATEGORY GREATER -1)
			set (
				${OUTVARIABLE}
				"excluded by category ${CAT}"
				PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()

	foreach (CAT ${CATEGORIES})
		list (FIND ${LIST} "${CAT}" FOUND_CATEGORY)
		if (FOUND_CATEGORY GREATER -1)
			set (
				${OUTVARIABLE}
				""
				PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()
endfunction ()
