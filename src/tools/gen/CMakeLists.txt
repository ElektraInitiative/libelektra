find_program(PYTHON "python")

if (PYTHON)
	#add_custom_command(
	#	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/timestamp
	#	COMMAND ${PYTHON} setup.py build
	#	COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/timestamp
	#	DEPENDS ${DEPS}
	#	)
	#add_custom_target(target ALL DEPENDS ${OUTPUT}/timestamp)

	set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
	configure_file(setup.py.in ${SETUP_PY})

	install(CODE "execute_process(COMMAND
		${PYTHON} ${SETUP_PY} install
			--prefix=${CMAKE_INSTALL_PREFIX}
			--root=\$ENV{DESTDIR}
			--install-scripts=${CMAKE_INSTALL_PREFIX}/${TARGET_TOOLS_EXEC_FOLDER}
			)"
		)

endif()
