include(LibAddMacros)

find_package (PkgConfig)
pkg_check_modules (LIBUV QUIET libuv)

if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	remove_binding(io_uv "cannot be built on Windows")
elseif(NOT LIBUV_FOUND)
	remove_binding (io_uv "libuv not found")
else()
	message(STATUS "Include Binding io_uv")

	set (BINDING_VARIANT uv)

	## Build library

	file (GLOB IO_UV_HDR_FILES *.h)
	file (GLOB IO_UV_SRC_FILES *.c)
	# Remove test from sources
	list (REMOVE_ITEM IO_UV_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${IO_UV_SRC_FILES} ${IO_UV_HDR_FILES} ${ELEKTRA_HEADERS})

	set (IO_UV_LIBRARY elektra-io-${BINDING_VARIANT})

	add_library (${IO_UV_LIBRARY} SHARED ${SOURCES})
	target_include_directories (${IO_UV_LIBRARY} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
	target_include_directories (${IO_UV_LIBRARY} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

	target_compile_options (${IO_UV_LIBRARY} PUBLIC ${LIBUV_CFLAGS_OTHER})
	target_link_elektra(${IO_UV_LIBRARY} elektra-io ${LIBUV_LDFLAGS})

	if (LIBUV_VERSION VERSION_LESS "1.0")
		set_target_properties (${IO_UV_LIBRARY} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV0")
	else()
		set_target_properties (${IO_UV_LIBRARY} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV1")
	endif()

	install (
		TARGETS ${IO_UV_LIBRARY}
		LIBRARY DESTINATION lib${LIB_SUFFIX}
	)

	install (
		FILES ${IO_UV_HDR_FILES}
		DESTINATION include/${TARGET_INCLUDE_FOLDER}
	)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/${IO_UV_LIBRARY}.pc.in"
		"${CMAKE_CURRENT_BINARY_DIR}/${IO_UV_LIBRARY}.pc"
		@ONLY
	)

	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/${IO_UV_LIBRARY}.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	)

	## Build test
	set (testexename testio_${BINDING_VARIANT})

	set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
	add_headers(TEST_SOURCES)
	file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
	list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
	list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

	add_executable (${testexename} ${TEST_SOURCES})
	add_dependencies (${testexename} kdberrors_generated)

	target_include_directories (${testexename} PUBLIC "${CMAKE_SOURCE_DIR}/tests/cframework")

	target_link_elektra(${testexename} elektra-kdb elektra-plugin elektra-io ${IO_UV_LIBRARY} m)

	if (LIBUV_VERSION VERSION_LESS "1.0")
		set_target_properties (${testexename} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV0")
	else()
		set_target_properties (${testexename} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV1")
	endif()

	add_test (NAME ${testexename}
		COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
		WORKING_DIRECTORY "${WORKING_DIRECTORY}"
	)
endif()
