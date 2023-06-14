include (LibAddMacros)

include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/include" "${CMAKE_CURRENT_SOURCE_DIR}/../include")

include (LibAddTest)

list (FIND REMOVED_PLUGINS error error_index)
set (ERROR_PLUGIN_AVAILABLE ON)
if (error_index GREATER -1)
	set (ERROR_PLUGIN_AVAILABLE OFF)
endif (error_index GREATER -1)

file (GLOB TESTS testtool_*.cpp)

# ~~~
# TODO: Reenable the following warning after we add a virtual destructor to `PluginDatabase`, and its subclasses.
# See also:
# - https://github.com/ElektraInitiative/libelektra/pull/1841
# - https://github.com/ElektraInitiative/libelektra/commit/15d67328
# ~~~
if (CMAKE_C_COMPILER_ID MATCHES "Clang")
	set_source_files_properties (testtool_specreader.cpp testtool_backendbuilder.cpp testtool_backendparser.cpp
				     PROPERTIES COMPILE_FLAGS -Wno-delete-non-virtual-dtor)
endif (CMAKE_C_COMPILER_ID MATCHES "Clang")

foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	if (NOT (name MATCHES testtool_mergingkdb AND (NOT ERROR_PLUGIN_AVAILABLE)))
		add_gtest (${name} LINK_TOOLS)
	endif (NOT (name MATCHES testtool_mergingkdb AND (NOT ERROR_PLUGIN_AVAILABLE)))
endforeach (file ${TESTS})

if (BUILD_TESTING
    AND ENABLE_TESTING
    AND ERROR_PLUGIN_AVAILABLE)
	set_property (TEST testtool_mergingkdb PROPERTY LABELS kdbtests)
endif ()
