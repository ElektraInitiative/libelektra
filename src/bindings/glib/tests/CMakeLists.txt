set (TEST_FILES $<TARGET_OBJECTS:cframework>)

macro (do_glib_test file)
	get_filename_component (name ${file} NAME_WE)
	set (SOURCES ${file} ${GELEKTRA_HDR_FILES} ${TEST_FILES})

	add_executable (${name} ${SOURCES})

	set_target_properties (${name} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

	target_link_libraries (${name} PRIVATE ${GELEKTRA_LIBRARY})

	target_include_directories (${name} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")

	add_test (NAME ${name} COMMAND "${CMAKE_BINARY_DIR}/bin/${name}" "${CMAKE_CURRENT_SOURCE_DIR}")

	set_property (TEST ${name} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	set_property (TEST ${name} PROPERTY LABELS memleak bindings)

	if (${name} MATCHES ".*_kdb")
		set_property (
			TEST ${name}
			APPEND
			PROPERTY LABELS kdbtests)
		set_property (TEST ${name} PROPERTY RUN_SERIAL TRUE)
	endif ()
endmacro (do_glib_test)

file (GLOB TESTS testglib_*.c)
foreach (file ${TESTS})
	do_glib_test (${file})
endforeach (file ${TESTS})
