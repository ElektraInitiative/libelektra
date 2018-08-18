find_package (Threads)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

include (LibAddTest)

function (add_kdb_test suffix)
	set (multiValueArgs REQUIRED_PLUGINS)
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
	add_gtest (${name} KDBTESTS LINK_TOOLS)
	target_link_elektra (${name} elektra-kdb)
endfunction (add_kdb_test suffix)

add_kdb_test (allplugins)
add_kdb_test (conflict REQUIRED_PLUGINS error)
add_kdb_test (error REQUIRED_PLUGINS error list spec)
add_kdb_test (nested REQUIRED_PLUGINS error)
add_kdb_test (simple REQUIRED_PLUGINS error)
