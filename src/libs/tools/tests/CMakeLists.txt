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
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	if (NOT (name MATCHES testtool_mergingkdb AND (NOT ERROR_PLUGIN_AVAILABLE)))
		add_gtest (${name} LINK_TOOLS)
	endif (NOT (name MATCHES testtool_mergingkdb AND (NOT ERROR_PLUGIN_AVAILABLE)))
endforeach (file ${TESTS})

if (BUILD_TESTING AND ENABLE_TESTING AND ERROR_PLUGIN_AVAILABLE)
	set_property(TEST testtool_mergingkdb PROPERTY LABELS kdbtests)
endif ()
