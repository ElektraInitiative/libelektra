install (PROGRAMS benchmark-createtree DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS elektra-merge DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS elektra-mount DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS elektra-umount DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS convert-fstab DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS convert-hosts DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS convert-inittab DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS convert-users DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS example-xorg DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS mount-augeas DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS mount-info DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS mount-kde DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS mount-openicc DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS find-tools DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
generate_manpage (kdb-find-tools FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)

install (PROGRAMS upgrade-bootstrap DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS umount-all DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

if (INSTALL_SYSTEM_FILES)
	set (BASH_COMPLETION_COMPLETIONSDIR "/usr/share/bash-completion/completions" CACHE INTERNAL "bash completions dir")
	find_package (bash-completion QUIET)
	unset (bash-completion_DIR CACHE)
	if (NOT BASH_COMPLETION_FOUND)
		find_package (PkgConfig REQUIRED)
		pkg_check_modules (BASH_COMPLETION bash-completion QUIET)
		if (BASH_COMPLETION_FOUND)
			include (LibFindMacros)
			pkg_get_variable (BASH_COMPLETION_COMPLETIONSDIR bash-completion completionsdir)
		endif ()
	endif ()
	install (FILES kdb-bash-completion
			DESTINATION ${BASH_COMPLETION_COMPLETIONSDIR}
			RENAME kdb)
	install (FILES kdb_zsh_completion
			DESTINATION "/usr/share/zsh/vendor-completions"
			RENAME "_kdb")
endif()

configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/elektrify-getenv.in"
	"${CMAKE_CURRENT_BINARY_DIR}/elektrify-getenv")
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektrify-getenv" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/make-source-package.in"
	"${CMAKE_CURRENT_BINARY_DIR}/make-source-package")

configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/list-tools.in"
	"${CMAKE_CURRENT_BINARY_DIR}/list-tools")
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/list-tools" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

ADD_CUSTOM_TARGET(source-package
		COMMAND "${CMAKE_CURRENT_BINARY_DIR}/make-source-package"
		COMMENT "Make a elektra source package by using git"
		VERBATIM
		WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
		)

