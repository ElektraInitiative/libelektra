install (
	PROGRAMS xfconf-populate
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/xfconf-system-lib-replace.in" "${CMAKE_CURRENT_BINARY_DIR}/xfconf-system-lib-replace" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/xfconf-system-lib-replace"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/xfconf-system-lib-restore.in" "${CMAKE_CURRENT_BINARY_DIR}/xfconf-system-lib-restore" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/xfconf-system-lib-restore"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/xfconf-user-lib-replace.in" "${CMAKE_CURRENT_BINARY_DIR}/xfconf-user-lib-replace" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/xfconf-user-lib-replace"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)
