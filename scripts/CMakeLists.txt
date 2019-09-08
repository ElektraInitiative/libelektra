include (ProcessorCount)

processorcount (PROCESSOR_COUNT)
if (PROCESSOR_COUNT EQUAL 0)
	set (PROCESSOR_COUNT 1)
endif (PROCESSOR_COUNT EQUAL 0)

set (CMAKE_MAKE_PROGRAM_BASENAME ${CMAKE_MAKE_PROGRAM})
string (REGEX
	REPLACE ".*/([^/]+)$"
		"\\1"
		CMAKE_MAKE_PROGRAM_BASENAME
		${CMAKE_MAKE_PROGRAM_BASENAME})

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

install (PROGRAMS upgrade-bootstrap DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS mountpoint-info DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS mount-list-all-files DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS check-env-dep DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

file (READ "kdb_zsh_completion" CONTENT_OF_ZSH_COMP)
file (READ "kdb-bash-completion" CONTENT_OF_BASH_COMP)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/install-sh-completion.in" "${CMAKE_CURRENT_BINARY_DIR}/install-sh-completion" @ONLY)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run_all.in" "${CMAKE_CURRENT_BINARY_DIR}/run_all" @ONLY)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run_memcheck.in" "${CMAKE_CURRENT_BINARY_DIR}/run_memcheck" @ONLY)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run_nokdbtests.in" "${CMAKE_CURRENT_BINARY_DIR}/run_nokdbtests" @ONLY)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run_checkshell.in" "${CMAKE_CURRENT_BINARY_DIR}/run_checkshell" @ONLY)
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run_nocheckshell.in" "${CMAKE_CURRENT_BINARY_DIR}/run_nocheckshell" @ONLY)

install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/install-sh-completion" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

if (INSTALL_SYSTEM_FILES)
	if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		set (BASH_COMPLETION_COMPLETIONSDIR "/usr/local/share/bash-completion/completions" CACHE INTERNAL "bash completions dir")
	else ()
		set (BASH_COMPLETION_COMPLETIONSDIR "/usr/share/bash-completion/completions" CACHE INTERNAL "bash completions dir")
	endif ()
	find_package (bash-completion QUIET)
	unset (bash-completion_DIR CACHE)
	if (NOT BASH_COMPLETION_FOUND)
		find_package (PkgConfig QUIET)
		if (PKG_CONFIG_FOUND)
			pkg_check_modules (BASH_COMPLETION bash-completion QUIET)
			if (BASH_COMPLETION_FOUND)
				include (LibFindMacros)
				pkg_get_variable (BASH_COMPLETION_COMPLETIONSDIR bash-completion completionsdir)
			endif ()
		endif ()
	endif ()
	install (FILES kdb-bash-completion DESTINATION ${BASH_COMPLETION_COMPLETIONSDIR} RENAME kdb)
	if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		set (FISH_COMPLETION_COMPLETIONSDIR "/usr/local/share/fish/completions")
	else ()
		set (FISH_COMPLETION_COMPLETIONSDIR "/usr/share/fish/vendor_completions.d")
	endif ()
	find_package (PkgConfig QUIET)
	if (PKG_CONFIG_FOUND)
		pkg_check_modules (FISH_FOUND fish QUIET)
		if (FISH_FOUND)
			include (LibFindMacros)
			pkg_get_variable (FISH_COMPLETION_COMPLETIONSDIR fish completionsdir)
		endif ()
	endif ()
	install (FILES kdb.fish DESTINATION ${FISH_COMPLETION_COMPLETIONSDIR})

	if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

		# On macOS we need to copy this into another folder. Because of "System Integrity Protection", we need to use a /usr/local
		# path
		install (FILES kdb_zsh_completion DESTINATION /usr/local/share/zsh/site-functions RENAME _kdb)
	else ()

		# This Folder is at least the valid folder for Debian
		install (FILES kdb_zsh_completion DESTINATION /usr/share/zsh/vendor-completions RENAME _kdb)
	endif ()
endif ()

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektrify-getenv.in" "${CMAKE_CURRENT_BINARY_DIR}/elektrify-getenv")
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektrify-getenv" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektrify-open.in" "${CMAKE_CURRENT_BINARY_DIR}/elektrify-open")
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektrify-open" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/make-source-package.in" "${CMAKE_CURRENT_BINARY_DIR}/make-source-package")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/list-tools.in" "${CMAKE_CURRENT_BINARY_DIR}/list-tools")
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/list-tools" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/configure-firefox.in" "${CMAKE_CURRENT_BINARY_DIR}/configure-firefox" @ONLY)
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/configure-firefox" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
add_subdirectory (ffconfig)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/change-resolver-symlink.in" "${CMAKE_CURRENT_BINARY_DIR}/change-resolver-symlink" @ONLY)
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/change-resolver-symlink" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/change-storage-symlink.in" "${CMAKE_CURRENT_BINARY_DIR}/change-storage-symlink" @ONLY)
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/change-storage-symlink" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/backup.in" "${CMAKE_CURRENT_BINARY_DIR}/backup" @ONLY)
install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/backup" DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS stash DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS restore DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

install (PROGRAMS reset DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS reset-elektra DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install (PROGRAMS umount-all DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

add_custom_target (source-package
		   COMMAND "${CMAKE_CURRENT_BINARY_DIR}/make-source-package"
		   COMMENT "Make a elektra source package by using git"
		   VERBATIM
		   WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/randoop/randoop.in" "${CMAKE_CURRENT_BINARY_DIR}/randoop/randoop" @ONLY)
file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/randoop/randoopClasses.txt" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/randoop/")

add_subdirectory (admin)
