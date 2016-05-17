if (DEPENDENCY_PHASE)
	find_package(PythonLibs 3)
	find_swig()

	if (PYTHONLIBS_FOUND AND SWIG_FOUND)
		include (LibAddMacros)

		add_custom_command (OUTPUT runtime.h
			COMMAND ${SWIG_EXECUTABLE} -c++ -python -py3 -external-runtime runtime.h
		)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")
		set_source_files_properties ("python.cpp" PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")
	elseif (NOT PYTHONLIBS_FOUND)
		remove_plugin (python "python 3 libs not found")
	else ()
		remove_plugin (python "swig not found")
	endif ()
endif ()

add_plugin(python
	CPP
	SOURCES
		python.hpp python.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
	INCLUDE_DIRECTORIES
		${PYTHON_INCLUDE_DIR}
	LINK_LIBRARIES
		${PYTHON_LIBRARIES}
	COMPILE_DEFINITIONS
		SWIG_TYPE_TABLE=kdb
		SWIG_RUNTIME=\"runtime.h\"
		PYTHON_PLUGIN_NAME=python
		PYTHON_PLUGIN_SYMBOL_NAME=Python
)

if (ADDTESTING_PHASE)
	# bindings are required for tests
	list (FIND BINDINGS "swig_python" FINDEX)
	if (NOT BUILD_SHARED AND BUILD_TESTING AND FINDEX GREATER -1)
		# test environment clears env so setting PYTHONPATH is no option
		# we workaround this by changing the current directory to our bindings
		# output directory + our test adds the current directory to pythons sys.path
		add_plugintest (python
			MEMLEAK
			WORKING_DIRECTORY
				${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/python/
		)

		if (INSTALL_TESTING)
			install (DIRECTORY python DESTINATION ${TARGET_TEST_DATA_FOLDER})
		endif ()
	else ()
		message (WARNING "swig_python bindings are required for testing, test deactivated")
	endif ()
endif ()
