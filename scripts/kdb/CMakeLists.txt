# all .in files are processed with configure_file and then installed. all other files are directly installed as-is.

file (GLOB IN_FILES *.in)
foreach (file ${IN_FILES})
	get_filename_component (file_we "${file}" NAME_WE)
	configure_file ("${file}" "${file_we}")
	install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/${file_we}" DESTINATION "${TARGET_TOOL_EXEC_FOLDER}")
endforeach (file ${IN_FILES})

file (GLOB ALL_FILES *)
list (REMOVE_ITEM ALL_FILES ${IN_FILES})
list (REMOVE_ITEM ALL_FILES "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
foreach (file ${ALL_FILES})
	install (PROGRAMS "${CMAKE_CURRENT_SOURCE_DIR}/${file}" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
endforeach (file ${ALL_FILES})
