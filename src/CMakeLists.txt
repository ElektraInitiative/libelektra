set (ADDTESTING_PHASE OFF)
add_subdirectory (plugins)

add_subdirectory (libs)

add_subdirectory (bindings)

add_subdirectory (tools)

add_subdirectory (include)

if (BUILD_TESTING)
	set (ADDTESTING_PHASE ON)
	add_subdirectory (plugins plugins_tests)
endif (BUILD_TESTING)
