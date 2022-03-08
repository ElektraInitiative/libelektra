# all .in files are processed with configure_file and then installed. all other files are directly installed as-is.

file (GLOB IN_FILES *.in)
foreach (file ${IN_FILES})
	get_filename_component (file_we "${file}" NAME_WE)
	configure_file ("${file}" "${file_we}" @ONLY)
	if (file_we STREQUAL "mount-java")
		if (NOT BUILD_SHARED
		    OR NOT "jna" IN_LIST ADDED_BINDINGS
		    OR NOT "process" IN_LIST ADDED_PLUGINS)
			continue ()
		endif ()
	endif ()
	install (
		PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/${file_we}"
		DESTINATION "${TARGET_TOOL_EXEC_FOLDER}"
		COMPONENT elektra-bin)
endforeach (file ${IN_FILES})

set (COMPONENT_BIN_EXTRA_FILES "find-tools" "mount-list-all-files" "mountpoint-info")

file (GLOB ALL_FILES *)
list (REMOVE_ITEM ALL_FILES ${IN_FILES})
list (REMOVE_ITEM ALL_FILES "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
foreach (file ${ALL_FILES})
	get_filename_component (file_we "${file}" NAME_WE)
	if ("${file_we}" IN_LIST COMPONENT_BIN_EXTRA_FILES)
		set (HAS_COMPONENT elektra-bin-extra)
	else ()
		set (HAS_COMPONENT elektra-bin)
	endif ()
	install (
		PROGRAMS "${file}"
		DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
		COMPONENT ${HAS_COMPONENT})
endforeach (file ${ALL_FILES})
