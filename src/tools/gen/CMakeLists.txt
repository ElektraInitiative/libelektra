find_package (Python2Interp 2.7)

if (PYTHON2INTERP_FOUND)
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
			${PYTHON2_EXECUTABLE}
			${CMAKE_CURRENT_SOURCE_DIR}/gen
			-p tests/lift.ini
			${CMAKE_CURRENT_SOURCE_DIR}/util/util.c
			-o ${CMAKE_CURRENT_BINARY_DIR}/util.py)")

	install(CODE "execute_process(COMMAND
			${PYTHON2_EXECUTABLE}
			${CMAKE_CURRENT_SOURCE_DIR}/gen
			-p tests/lift.ini
			${CMAKE_CURRENT_SOURCE_DIR}/util/util.cpp
			-o ${CMAKE_CURRENT_BINARY_DIR}/cpp_util.py)")

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

	function(gen_util TEMPLATE OUTPUT)
		#file(GLOB SUPPORT RELATIVE support "*.py")
		#call the gen tool
		set(OUTPUT
			"${CMAKE_BINARY_DIR}/src/tools/gen/${OUTPUT}")
		message (STATUS "doing command ${TEMPLATE} ${OUTPUT}")
		#add_custom_command(
		#add_custom_target(${OUTPUT}
		#	OUTPUT ${OUTPUT}
		#	COMMAND ${PYTHON2_EXECUTABLE}
		#		${CMAKE_SOURCE_DIR}/src/tools/gen/gen
		#		-p
		#		tests/lift.ini
		#		${TEMPLATE}
		#		-o
		#		${OUTPUT}
		#	WORKING_DIRECTORY
		#		${CMAKE_BINARY_DIR}/src/tools/gen
		#	MAIN_DEPENDENCY ${TEMPLATE} 
		#	DEPENDS ${SUPPORT}
		#	COMMENT "build the util ${TEMPLATE}"
		#	)
	endfunction(gen_util)

	generate_manpage (kdb-gen FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)

	#add_subdirectory(util)

	# for future cmakification
	#add_subdirectory(support)
	#add_subdirectory(template)
	#add_subdirectory(tests)

else()
	remove_tool(gen "Did not find python2 interpreter")
endif()
