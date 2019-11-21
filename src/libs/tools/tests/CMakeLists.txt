include (LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

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
	set_source_files_properties (testtool_specreader.cpp testtool_backendbuilder.cpp testtool_backendparser.cpp PROPERTIES
				     COMPILE_FLAGS -Wno-delete-non-virtual-dtor)
endif (CMAKE_C_COMPILER_ID MATCHES "Clang")

foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_TOOLS)
endforeach (file ${TESTS})
