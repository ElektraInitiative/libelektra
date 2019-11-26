include (CheckSymbolExists)
include (CMakePushCheckState)

# ~~~
# Check if a symbol is exported
#
# Same as check_symbol_exists but works around the problem of not detecting
# symbols when -Wpedantic is on.
#
# Also automatically adds the definitions of the current directory to
# CMAKE_REQUIRED_DEFINITIONS.
#
# https://issues.libelektra.org/2218
# ~~~

macro (safe_check_symbol_exists SYMBOL FILES VARIABLE)
	set (CMAKE_C_FLAGS_OLD ${CMAKE_C_FLAGS})
	string (REPLACE "-Wpedantic" "" CMAKE_C_FLAGS ${CMAKE_C_FLAGS})

	cmake_push_check_state ()
	get_directory_property (DEFS COMPILE_DEFINITIONS)
	prepend (DEFS "-D" ${DEFS})
	list (APPEND CMAKE_REQUIRED_DEFINITIONS ${DEFS})

	check_symbol_exists ("${SYMBOL}" "${FILES}" "${VARIABLE}")
	cmake_pop_check_state ()
	set (CMAKE_C_FLAGS ${CMAKE_C_FLAGS_OLD})
endmacro ()

# Prepends prefix onto all argn list members and saves the result to var Can be replaced by list(TRANSFORM .. PREPEND ..) once we use CMake
# 3.12
function (prepend var prefix)
	set (temp "")
	foreach (f ${ARGN})
		list (APPEND temp "${prefix}${f}")
	endforeach (f)
	set (
		${var}
		"${temp}"
		PARENT_SCOPE)
endfunction (prepend)
