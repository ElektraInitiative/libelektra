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

	function(gen ARGS)
		#scan args for files
		file(GLOB DEPS
			RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
			${ARGS})
		#call the gen tool
		add_custom_command(
			OUTPUT ${CMAKE_BINARY_DIR}/src/tools/x
			COMMAND ${PYTHON2_EXECUTABLE}
			WORKING_DIRECTORY
				${CMAKE_BINARY_DIR}/src/tools/gen
			${CMAKE_SOURCE_DIR}/src/tools/gen/gen ${ARGS}
			DEPENDS ${DEPS}
			)
	endfunction(gen)


	# for future cmakification
	#add_subdirectory(support)
	#add_subdirectory(template)
	#add_subdirectory(util)
	#add_subdirectory(tests)

endif()
