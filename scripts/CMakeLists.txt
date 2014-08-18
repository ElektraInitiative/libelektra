install (PROGRAMS elektra-merge DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS elektra-mount DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS elektra-umount DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

IF (IS_DIRECTORY /etc/bash_completion.d)
	install (FILES kdb-bash-completion
			DESTINATION /etc/bash_completion.d 
			RENAME kdb)
ENDIF()

configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/make-source-package.in"
	"${CMAKE_CURRENT_BINARY_DIR}/make-source-package")

ADD_CUSTOM_TARGET(source-package
		COMMAND "${CMAKE_CURRENT_BINARY_DIR}/make-source-package"
		COMMENT "Make a elektra source package by using git"
		VERBATIM
		WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
		)
