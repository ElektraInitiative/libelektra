remove_tool (pythongen "Deprecated and will be removed soon")
return ()

find_package (Python2Interp 2.7 QUIET)

if (PYTHON2INTERP_FOUND)

	set (SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
	configure_file (setup.py.in ${SETUP_PY})

	install (
		CODE "execute_process(COMMAND
			${PYTHON2_EXECUTABLE}
			\"${CMAKE_CURRENT_SOURCE_DIR}/pythongen\"
			-p tests/lift.ini
			\"${CMAKE_CURRENT_SOURCE_DIR}/util/util.c\"
			-o \"${CMAKE_CURRENT_BINARY_DIR}/util.py\")")

	install (
		CODE "execute_process(COMMAND
			${PYTHON2_EXECUTABLE}
			\"${CMAKE_CURRENT_SOURCE_DIR}/pythongen\"
			-p tests/lift.ini
			\"${CMAKE_CURRENT_SOURCE_DIR}/util/util.cpp\"
			-o \"${CMAKE_CURRENT_BINARY_DIR}/cpp_util.py\")")

	install (
		CODE "execute_process(COMMAND
			${PYTHON2_EXECUTABLE} \"${SETUP_PY}\"
			--no-user-cfg
			--quiet
			install
			--prefix=${CMAKE_INSTALL_PREFIX}
			--root=\$ENV{DESTDIR}
			--install-scripts=${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}
			${INSTALL_OPTIONS}
			)")

	function (pythongen_util TEMPLATE OUTPUT)

		# file(GLOB SUPPORT RELATIVE support "*.py")

		# call the gen tool
		set (OUTPUT "${CMAKE_BINARY_DIR}/src/tools/pythongen/${OUTPUT}")
		message (STATUS "doing command ${TEMPLATE} ${OUTPUT}")

	endfunction (pythongen_util)

	generate_man_page (kdb-gen FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)

	# add_subdirectory(util)

	# ~~~
	# for future cmakification
	# add_subdirectory(support)
	# add_subdirectory(template)
	# add_subdirectory(tests)
	# ~~~

else ()
	remove_tool (pythongen "Did not find python2 interpreter")
endif ()
