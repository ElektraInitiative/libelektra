message(DEPRECATION "Python 2.7 plugin has been deprecated")

find_package(Python2Libs 2.7)
find_swig()

if (PYTHON2LIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
	remove_plugin (python2 "python2 is for python 2.7 only and not ${PYTHON2LIBS_VERSION_STRING}")
elseif (PYTHON2LIBS_FOUND AND SWIG_FOUND)
	include (LibAddMacros)

	execute_process (
		COMMAND ${SWIG_EXECUTABLE} -c++ -python -external-runtime runtime.h
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)

	add_plugin(python2
		CPP
		SOURCES
			python.hpp python.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
		INCLUDE_DIRECTORIES
			${PYTHON2_INCLUDE_DIR}
		LINK_LIBRARIES
			${PYTHON2_LIBRARIES}
		COMPILE_DEFINITIONS
			SWIG_TYPE_TABLE=kdb
			SWIG_RUNTIME=\"runtime.h\"
			PYTHON_PLUGIN_NAME=python2
			PYTHON_PLUGIN_SYMBOL_NAME=Python2
	)

	# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
	set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")
	set_source_files_properties (python.cpp PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")

	# bindings are required for tests
	list (FIND BINDINGS "swig_python2" FINDEX)
	if (BUILD_TESTING AND FINDEX GREATER -1)
		# test environment clears env so setting PYTHONPATH is no option
		# we workaround this by changing the current directory to our bindings
		# output directory + our test adds the current directory to pythons sys.path
		add_plugintest (python2
			MEMLEAK
			INCLUDE_DIRECTORIES
				${PYTHON2_INCLUDE_DIR}
			LINK_LIBRARIES
				${PYTHON2_LIBRARIES}
			WORKING_DIRECTORY
				${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/python2/
			COMPILE_DEFINITIONS
				ELEKTRA_PLUGIN_NAME=\"python2\"
		)
	else ()
		message (WARNING "swig_python2 bindings are required for testing, test deactivated")
	endif ()
elseif (NOT PYTHON2LIBS_FOUND)
	remove_plugin (python2 "python 2 libs not found")
else ()
	remove_plugin (python2 "swig not found")
endif ()
