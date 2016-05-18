add_subdirectory (error)


set (ADDTESTING_PHASE OFF)
add_subdirectory (plugins)

add_subdirectory (libs)

add_subdirectory (bindings)

add_subdirectory (tools)

add_subdirectory (include)

set (ADDTESTING_PHASE ON)
add_subdirectory (plugins plugins_tests)
