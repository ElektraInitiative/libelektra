install (
	PROGRAMS xfconf-populate
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

install (
	PROGRAMS xfconf-system-lib-replace
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

install (
	PROGRAMS xfconf-system-lib-restore
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

install (
	PROGRAMS xfconf-user-lib-replace
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)