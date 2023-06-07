set (PYTHON_MODULEPATH "${CMAKE_SWIG_OUTDIR}/..")

function (do_python_test source)
	cmake_parse_arguments (ARG "KDB_TEST" "" "REQUIRED_PLUGINS" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	get_filename_component (name ${source} NAME)
	add_test (NAME ${name} COMMAND ${PYTHON_EXECUTABLE} -B "${CMAKE_CURRENT_SOURCE_DIR}/${source}")

	set_property (TEST ${name} PROPERTY ENVIRONMENT "PYTHONPATH=${PYTHON_MODULEPATH}" "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	set_property (TEST ${name} PROPERTY LABELS memleak bindings)

	if (ARG_KDB_TEST)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY LABELS kdbtests)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY RUN_SERIAL TRUE)
	endif ()
endfunction (do_python_test)

if (NOT (APPLE AND ENABLE_ASAN))
	do_python_test (test_record_simple.py)
endif (NOT (APPLE AND ENABLE_ASAN))
