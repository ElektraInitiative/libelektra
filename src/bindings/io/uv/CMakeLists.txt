include (LibAddMacros)

find_package (PkgConfig)
pkg_check_modules (LIBUV QUIET libuv>=1)

if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	exclude_binding (io_uv "cannot be built on Windows")
elseif (NOT LIBUV_FOUND)
	exclude_binding (io_uv "libuv not found")
else ()
	add_binding (io_uv)

	## Build library
	set (BINDING_VARIANT uv)

	set (IO_UV_SRC_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/io_uv.c"
	)

	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${IO_UV_SRC_FILES} ${ELEKTRA_HEADERS})

	set (IO_UV_LIBRARY elektra-io-${BINDING_VARIANT})

	# create object library so that executables can use
	# $<TARGET_OBJECTS:OBJ_${IO_UV_LIBRARY}>
	add_library(OBJ_${IO_UV_LIBRARY} OBJECT ${SOURCES})

	add_lib (io-${BINDING_VARIANT}
		SOURCES ${SOURCES}
		LINK_ELEKTRA
			elektra-io
		LINK_LIBRARIES
			${LIBUV_LIBRARIES}
	)

	if (BUILD_FULL OR BUILD_STATIC)
		add_includes ("elektra-full" ${LIBUV_INCLUDE_DIRS})
	else ()
		target_include_directories (${IO_UV_LIBRARY} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})
	endif ()

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
	add_headers (TEST_SOURCES)
	file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
	list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
	list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

	if (BUILD_FULL OR BUILD_STATIC)
		# add sources for elektra-io-uv for static and full builds
		list (APPEND TEST_SOURCES $<TARGET_OBJECTS:OBJ_${IO_UV_LIBRARY}>)
	endif ()

	add_executable (${testexename} ${TEST_SOURCES})
	add_dependencies (${testexename} kdberrors_generated)

	target_include_directories (${testexename} PUBLIC "${CMAKE_SOURCE_DIR}/tests/cframework")

	target_link_elektra (${testexename} elektra-kdb elektra-plugin elektra-io ${IO_UV_LIBRARY} m)
	if (BUILD_FULL OR BUILD_STATIC)
		target_link_libraries (${testexename} "${LIBUV_LIBRARIES};m")
	endif ()

	add_test (NAME ${testexename}
		COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
		WORKING_DIRECTORY "${WORKING_DIRECTORY}"
	)

	add_subdirectory (example)
endif ()
