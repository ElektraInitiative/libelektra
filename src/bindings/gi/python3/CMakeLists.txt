find_package (PythonInterp 3)

macro (do_gi_python_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND ${PYTHON_EXECUTABLE} -B ${source}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)

	set_property(
		TEST ${name}
		PROPERTY ENVIRONMENT
			"LD_LIBRARY_PATH=${GELEKTRA_LIBRARY_DIR}"
			"GI_TYPELIB_PATH=${GI_TYPELIB_DIR}"
	)
endmacro (do_gi_python_test)

if (PYTHONINTERP_FOUND)
	set (PYTHON_GET_MODULES_DIR_COMMAND "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	execute_process (
		COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
		OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)

	install (DIRECTORY python/gi DESTINATION ${PYTHON_SITE_PACKAGES})

	file (GLOB TESTS testgi_*.py)
	foreach (file ${TESTS})
		do_gi_python_test (${file})
	endforeach (file ${TESTS})
else (PYTHONINTERP_FOUND)
	message (WARNING "No Python interpreter found. Python specific GObject Introspection overrides won't be installed")
endif (PYTHONINTERP_FOUND)
