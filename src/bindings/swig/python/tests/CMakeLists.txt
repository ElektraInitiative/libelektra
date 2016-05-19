set(PYTHON_MODULEPATH "${CMAKE_SWIG_OUTDIR}")

macro (do_python_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND ${PYTHON_EXECUTABLE} -B ${source}
	)

	set_property (
		TEST ${name}
		PROPERTY ENVIRONMENT
			"PYTHONPATH=${PYTHON_MODULEPATH}"
			"LD_LIBRARY_PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
	)

	set_property (
		TEST ${name}
		PROPERTY LABELS memleak bindings
	)

	if (${name} MATCHES ".*_kdb")
		set_property (
			TEST ${name}
			APPEND PROPERTY LABELS kdbtests
		)
	endif ()
endmacro (do_python_test)

file (GLOB TESTS *.py)
foreach (file ${TESTS})
	do_python_test (${file})
endforeach (file ${TESTS})
