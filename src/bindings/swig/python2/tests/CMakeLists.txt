set(PYTHON_MODULEPATH "${CMAKE_CURRENT_BINARY_DIR}/..")

macro (do_python_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND ${PYTHON_EXECUTABLE} -B ${source}
		)
	set_tests_properties (${name} PROPERTIES ENVIRONMENT "PYTHONPATH=${PYTHON_MODULEPATH}")

	# python is a huge memleaker
	set_property(TEST ${name} PROPERTY LABELS memleak)
endmacro (do_python_test)

file (GLOB TESTS *.py)
foreach (file ${TESTS})
	do_python_test (${file})
endforeach (file ${TESTS})
