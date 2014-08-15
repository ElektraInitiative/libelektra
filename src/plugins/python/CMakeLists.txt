find_package(PythonLibs REQUIRED)
find_package(SWIG 3 REQUIRED)

if (PYTHONLIBS_FOUND AND SWIG_FOUND)
	execute_process (
		COMMAND ${SWIG_EXECUTABLE} -c++ -python -py3 -external-runtime runtime.h
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)

	include (LibAddMacros)

	set (SOURCES python.hpp python.cpp)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)
	add_cppheaders (SOURCES)

	set (PLUGIN_NAME elektra-python)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})

	set_target_properties (${PLUGIN_NAME} PROPERTIES COMPILE_FLAGS "-DSWIG_TYPE_TABLE=kdb")

	install (
		TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}
	)

	include_directories (${PYTHON_INCLUDE_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	target_link_libraries (${PLUGIN_NAME} elektra)
	target_link_libraries (${PLUGIN_NAME} ${PYTHON_LIBRARIES})

  add_includes (elektra-full ${PYTHON_INCLUDE_DIR})

	add_libraries (elektra-full ${PYTHON_LIBRARIES})

	add_plugintest (python)
	configure_file (test_elektra.py ${CMAKE_CURRENT_BINARY_DIR}/test_elektra.py)
	configure_file (test_elektra2.py ${CMAKE_CURRENT_BINARY_DIR}/test_elektra2.py)
else (PYTHONLIBS_FOUND AND SWIG_FOUND)
	remove_plugin (python "python libraries or swig not found")
endif (PYTHONLIBS_FOUND AND SWIG_FOUND)
