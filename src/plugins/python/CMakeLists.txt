include (LibAddBinding)

if (DEPENDENCY_PHASE)
	find_package (PythonInterp 3 QUIET)
	find_package (PythonLibs 3 QUIET)
	find_package (Pluginprocess)
	find_swig ()
	check_binding_included ("python" BINDING_WAS_ADDED SUBDIRECTORY "swig/python" SILENT)

	if (PYTHONLIBS_FOUND
	    AND SWIG_FOUND
	    AND BINDING_WAS_ADDED
	    AND PLUGINPROCESS_FOUND)
		include (LibAddMacros)

		add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
				    COMMAND ${SWIG_EXECUTABLE} -c++ -python -py3 -external-runtime ${CMAKE_CURRENT_BINARY_DIR}/runtime.h)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable -Wno-missing-field-initializers")
		# Unfortunately compiling the generated code produces warnings about casts between incompatible function types.
		if (CMAKE_COMPILER_IS_GNUCXX)
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-cast-function-type")
		endif ()

		set_source_files_properties ("python.cpp" PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")

		set (PYTHON_PLUGIN_FOLDER "${CMAKE_INSTALL_PREFIX}/${TARGET_TEST_DATA_FOLDER}/python")
		set (PYTHON_GET_MODULES_DIR_COMMAND
		     "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
		execute_process (
			COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
			OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
			OUTPUT_STRIP_TRAILING_WHITESPACE)

		configure_file (config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
	elseif (NOT PYTHONLIBS_FOUND)
		remove_plugin (python "python 3 libs (libpython3-dev) not found")
	elseif (NOT BINDING_WAS_ADDED)
		remove_plugin (python "python binding is required")
	elseif (NOT PLUGINPROCESS_FOUND)
		remove_plugin (python "Elektra's pluginprocess library is required")
	else ()
		remove_plugin (python "swig not found")
	endif ()
endif ()

add_plugin (
	python CPP
	SOURCES python.hpp python.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h ${CMAKE_CURRENT_BINARY_DIR}/config.h
	INCLUDE_DIRECTORIES ${PYTHON_INCLUDE_DIRS}
	LINK_LIBRARIES ${PYTHON_LIBRARIES}
	LINK_ELEKTRA elektra-pluginprocess
	COMPILE_DEFINITIONS SWIG_TYPE_TABLE=kdb SWIG_RUNTIME=\"runtime.h\" PYTHON_PLUGIN_NAME=python PYTHON_PLUGIN_SYMBOL_NAME=Python
			    COMPONENT libelektra${SO_VERSION}-python)

if (ADDTESTING_PHASE)
	# bindings are required for tests
	check_binding_was_added ("python" BINDING_WAS_ADDED)
	if (BUILD_TESTING AND BINDING_WAS_ADDED)

		# test environment clears env so setting PYTHONPATH is no option we workaround this by changing the current directory to our
		# bindings output directory + our test adds the current directory to pythons sys.path
		add_plugintest (python MEMLEAK WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/python/)

		if (INSTALL_TESTING)
			# TODO: make clear via repo structure that this happens
			install (
				DIRECTORY ${CMAKE_SOURCE_DIR}/src/plugins/python/python/
				DESTINATION "${TARGET_TEST_DATA_FOLDER}/python"
				COMPONENT elektra-tests)
		endif ()

	else ()
		message (WARNING "The python binding is required for testing, test deactivated")
	endif ()
endif ()
