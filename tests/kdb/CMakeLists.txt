find_package (Threads QUIET)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

include (LibAddTest)

function (add_kdb_test suffix)
	set (multiValueArgs "REQUIRED_PLUGINS;LINK_ELEKTRA")
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS
			   ${plugin}
			   plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	set (name testkdb_${suffix})
	add_gtest (${name} KDBTESTS LINK_TOOLS LINK_ELEKTRA ${ARG_LINK_ELEKTRA})
endfunction (add_kdb_test suffix)

add_kdb_test (allplugins)
add_kdb_test (conflict REQUIRED_PLUGINS error)
add_kdb_test (error REQUIRED_PLUGINS error list spec)
add_kdb_test (nested REQUIRED_PLUGINS error)
add_kdb_test (simple REQUIRED_PLUGINS error)
add_kdb_test (ensure REQUIRED_PLUGINS tracer list spec)

if (NOT
    (APPLE
     AND XCODE
     AND XCODE_VERSION
	 VERSION_EQUAL
	 10.1))
	set_source_files_properties (testkdb_highlevel PROPERTIES COMPILE_FLAGS -Wno-sign-promo)
	add_kdb_test (highlevel LINK_ELEKTRA elektra-highlevel REQUIRED_PLUGINS error)
endif ()
