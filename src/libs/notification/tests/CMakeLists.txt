include (LibAddMacros)

if (BUILD_TESTING)

	add_headers (HDR_FILES)

	set (TESTS
		testlib_notification.c
	)

	foreach (file ${TESTS})
		get_filename_component (name ${file} NAME_WE)

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
		list (APPEND TEST_SOURCES ${name})
		list (APPEND TEST_SOURCES ${HDR_FILES})

		add_executable (${name} ${TEST_SOURCES})
		add_dependencies (${name} kdberrors_generated)

		target_include_directories (${name} PUBLIC "${CMAKE_SOURCE_DIR}/tests/cframework")

		target_link_elektra (${name} elektra-kdb elektra-notification)

		add_test (NAME ${name}
			COMMAND "${CMAKE_BINARY_DIR}/bin/${name}" "${CMAKE_CURRENT_SOURCE_DIR}"
		)

	endforeach (file ${TESTS})

endif (BUILD_TESTING)
