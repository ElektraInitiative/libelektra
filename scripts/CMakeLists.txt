install (PROGRAMS elektra-merge DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

IF (IS_DIRECTORY /etc/bash_completion.d)
	install (PROGRAMS kdb-bash-completion 
			DESTINATION /etc/bash_completion.d 
			RENAME kdb
			PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
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
