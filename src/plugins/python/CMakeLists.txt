find_package(PythonLibs REQUIRED)

if (PYTHONLIBS_FOUND)
	include (LibAddMacros)

	set (SOURCES python.h python.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-python)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})

	install (TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	include_directories (${PYTHON_INCLUDE_PATH})

	target_link_libraries (${PLUGIN_NAME} elektra)
	target_link_libraries (${PLUGIN_NAME} ${PYTHON_LIBRARIES})

	add_includes (elektra-full ${PYTHON_INCLUDE_PATH})

	add_libraries (elektra-full ${PYTHON_LIBRARIES})

	add_plugintest (python)
	configure_file (test_elektra.py ${CMAKE_CURRENT_BINARY_DIR}/test_elektra.py)
	configure_file (test_elektra2.py ${CMAKE_CURRENT_BINARY_DIR}/test_elektra2.py)
else (PYTHONLIBS_FOUND)
	remove_plugin (python "python libraries not found")
endif (PYTHONLIBS_FOUND)
