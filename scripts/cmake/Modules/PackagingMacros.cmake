# ~~~
# check_component_dependencies
#
# check and excludes a component from packaging if
# the plugin/binding/tool the component depends on
# is missing
#
# dependency_name  name of the dependency the component depends on
# component_name   name of the component
# PLUGIN           whether the dependency is a plugin
# BINDING          whether the dependency is a binding
# TOOL             whether the dependency is a tool
# ADDITIONAL_DEPENDENCIES additional dependencies if the component
# 				          has multiple dependencies
# ~~~
macro (check_component_dependencies dependency_name component_name)
	cmake_parse_arguments (
		ARG
		"PLUGIN;BINDING;TOOL" # optional keywords
		"" # one value keywords
		"ADDITIONAL_DEPENDENCIES" # multi value keywords
		${ARGN})

	set (TMP_DEPENDENCY_NAMES ${dependency_name})
	if (ARG_ADDITIONAL_DEPENDENCIES)
		list (APPEND TMP_DEPENDENCY_NAMES "${ARG_ADDITIONAL_DEPENDENCIES}")
	endif ()

	set (TMP_UNFULFILLED_DEPENDENCIES "")

	foreach (name ${TMP_DEPENDENCY_NAMES})

		if (ARG_PLUGIN)
			if (NOT ${name} IN_LIST ADDED_PLUGINS)
				set (TMP_DEPENDENCY_TYPE "plugin")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()
		if (ARG_BINDING)
			if (NOT ${name} IN_LIST ADDED_BINDINGS)
				set (TMP_DEPENDENCY_TYPE "binding")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()
		if (ARG_TOOL)
			if (NOT ${name} IN_LIST TOOLS)
				set (TMP_DEPENDENCY_TYPE "tool")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()

	endforeach ()

	list (LENGTH TMP_UNFULFILLED_DEPENDENCIES UNFULFILLED_DEPENDENCIES_AMOUNT)
	list (LENGTH TMP_DEPENDENCY_NAMES EXPECTED_DEPENDENCIES_AMOUNT)
	if (${UNFULFILLED_DEPENDENCIES_AMOUNT} GREATER 1)
		set (TMP_DEPENDENCY_TYPE "${TMP_DEPENDENCY_TYPE}s")
	endif ()
	string (REPLACE ";" ", " TMP_UNFULFILLED_DEPENDENCIES_STR "${TMP_UNFULFILLED_DEPENDENCIES}")
	if (TMP_UNFULFILLED_DEPENDENCIES AND ${UNFULFILLED_DEPENDENCIES_AMOUNT} EQUAL ${EXPECTED_DEPENDENCIES_AMOUNT})
		# all dependencies of component are missing
		message (
			STATUS
				"Exclude component ${component_name} because ${TMP_UNFULFILLED_DEPENDENCIES_STR} ${TMP_DEPENDENCY_TYPE} excluded."
		)
		list (APPEND EXCLUDED_COMPONENTS ${component_name})
	elseif (TMP_UNFULFILLED_DEPENDENCIES AND ${UNFULFILLED_DEPENDENCIES_AMOUNT} LESS ${EXPECTED_DEPENDENCIES_AMOUNT})
		# at least one but not all dependencies of a component are missing
		message (
			STATUS
				"Component ${component_name} is missing ${TMP_UNFULFILLED_DEPENDENCIES_STR} ${TMP_DEPENDENCY_TYPE}. This package will still be generated."
		)
	endif ()

endmacro (check_component_dependencies)
