include (LibAddBinding)

if (DEPENDENCY_PHASE)
	find_package (Python2Libs 2.7 QUIET)
	find_swig ()
	check_binding_included ("swig_python2" BINDING_WAS_ADDED SUBDIRECTORY "swig/python2" SILENT)

	if (PYTHON2LIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
		remove_plugin (python2 "python2 is for python 2.7 only and not ${PYTHON2LIBS_VERSION_STRING}")
	elseif (PYTHON2LIBS_FOUND AND SWIG_FOUND AND BINDING_WAS_ADDED)
		include (LibAddMacros)

		add_custom_command (OUTPUT runtime.h
				    COMMAND ${SWIG_EXECUTABLE} -c++ -python -external-runtime runtime.h)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")
		if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-deprecated-register")
		elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-cast-function-type")
		endif ()
		set_source_files_properties ("python.cpp" PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")
	elseif (NOT PYTHON2LIBS_FOUND)
		remove_plugin (python2 "python 2 libs (libpython-dev) not found")
	elseif (NOT BINDING_WAS_ADDED)
		remove_plugin (python2 "swig_python2 binding is required")
	else ()
		remove_plugin (python2 "swig not found")
	endif ()
endif ()

add_plugin (python2
	    CPP
	    SOURCES python.hpp
		    python.cpp
		    ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
	    INCLUDE_DIRECTORIES ${PYTHON2_INCLUDE_DIR}
	    LINK_LIBRARIES ${PYTHON2_LIBRARIES}
	    COMPILE_DEFINITIONS SWIG_TYPE_TABLE=kdb
				SWIG_RUNTIME=\"runtime.h\"
				PYTHON_PLUGIN_NAME=python2
				PYTHON_PLUGIN_SYMBOL_NAME=Python2)

if (DEPENDENCY_PHASE)

	# generate readme from pythons3 README.md it will overwrite previously generated file of add_plugin directly above
	set (CMAKE_CURRENT_SOURCE_DIR_SAFE ${CMAKE_CURRENT_SOURCE_DIR})
	if (CMAKE_VERSION VERSION_LESS "2.8.12")
		get_filename_component (CMAKE_CURRENT_SOURCE_PARENT_DIR ${CMAKE_CURRENT_SOURCE_DIR} PATH)
	else ()
		get_filename_component (CMAKE_CURRENT_SOURCE_PARENT_DIR ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
	endif ()
	set (CMAKE_CURRENT_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_PARENT_DIR}/python")
	generate_readme (python2)
	set (CMAKE_CURRENT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR_SAFE})

endif ()

if (ADDTESTING_PHASE)
	if (ENABLE_BROKEN_TESTS)
		check_binding_was_added ("swig_python2" BINDING_WAS_ADDED) # bindings are required for tests

		if (BUILD_TESTING AND BINDING_WAS_ADDED)

			# test environment clears env so setting PYTHONPATH is no option we workaround this by changing the current
			# directory to our bindings output directory + our test adds the current directory to pythons sys.path
			add_plugintest (python2 MEMLEAK WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/python2/)

			if (INSTALL_TESTING)
				install (DIRECTORY ${CMAKE_SOURCE_DIR}/src/plugins/python/python/
					 DESTINATION "${TARGET_TEST_DATA_FOLDER}/python2")
			endif ()

		else ()
			message (WARNING "BUILD_STATIC or BUILD_FULL and swig_python2 bindings are required for testing, test deactivated")
		endif ()
	endif (ENABLE_BROKEN_TESTS)
endif ()
