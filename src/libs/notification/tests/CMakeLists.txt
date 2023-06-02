include (LibAddMacros)

# Clang with enabled ASAN reports that this test leaks memory: https://travis-ci.org/sanssecours/elektra/jobs/383014685#L2197
if (BUILD_TESTING AND NOT ENABLE_ASAN)

	add_headers (HDR_FILES)

	set (TESTS testlib_notification.c)

	foreach (file ${TESTS})
		get_filename_component (name ${file} NAME_WE)

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
		list (APPEND TEST_SOURCES ${name})
		list (APPEND TEST_SOURCES ${HDR_FILES})

		add_executable (${name} ${TEST_SOURCES})

		target_include_directories (${name} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")

		target_link_elektra (${name} elektra-kdb elektra-notification)

		add_test (NAME ${name} COMMAND "${CMAKE_BINARY_DIR}/bin/${name}" "${CMAKE_CURRENT_SOURCE_DIR}")
		set_property (TEST ${name} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	endforeach (file ${TESTS})

endif (BUILD_TESTING AND NOT ENABLE_ASAN)
