find_package (Python2Interp 2.7 REQUIRED)

if (PYTHON2_EXECUTABLE)
	#add_custom_command(
	#	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/timestamp
	#	COMMAND ${PYTHON2_EXECUTABLE} setup.py build
	#	COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/timestamp
	#	DEPENDS ${DEPS}
	#	)
	#add_custom_target(target ALL DEPENDS ${OUTPUT}/timestamp)

	set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
	configure_file(setup.py.in ${SETUP_PY})

	install(CODE "execute_process(COMMAND
			${PYTHON2_EXECUTABLE} ${SETUP_PY}
			--no-user-cfg
			--quiet
			install
			--prefix=${CMAKE_INSTALL_PREFIX}
			--root=\$ENV{DESTDIR}
			--install-scripts=${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}
			--install-layout=deb
			)"
		)

endif()
