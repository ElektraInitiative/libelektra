include (LibAddMacros)
include (FindLibEv QUIET)

if (NOT LIBEV_FOUND)
	exclude_binding (io_ev "libev not found")
else () # Build library
	add_binding (io_ev)

	set (BINDING_VARIANT ev)

	set (IO_VARIANT_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/io_ev.c")

	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${IO_VARIANT_SRC_FILES} ${ELEKTRA_HEADERS})

	set (IO_VARIANT_LIBRARY elektra-io-${BINDING_VARIANT})

	add_lib (
		io-${BINDING_VARIANT}
		SOURCES
		${SOURCES}
		LINK_ELEKTRA
		elektra-io
		LINK_LIBRARIES
		${LIBEV_LIBRARIES}
		INCLUDE_SYSTEM_DIRECTORIES
		${LIBEV_INCLUDE_DIRS}
		COMPONENT
		io-ev-elektra)

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${IO_VARIANT_LIBRARY}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc"
			@ONLY)

	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
		COMPONENT io-ev-elektra)

	if (BUILD_TESTING)
		# Build test
		set (testexename testio_${BINDING_VARIANT})

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
		add_headers (TEST_SOURCES)
		file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
		list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
		list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

		if (BUILD_FULL OR BUILD_STATIC) # add sources for elektra-io-ev for static and full builds
			list (APPEND TEST_SOURCES $<TARGET_OBJECTS:${IO_VARIANT_LIBRARY}-objects>)
		endif ()

		add_executable (${testexename} ${TEST_SOURCES})

		target_include_directories (${testexename} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")
		target_include_directories (${testexename} PRIVATE SYSTEM ${LIBEV_INCLUDE_DIRS})

		target_link_elektra (${testexename} elektra-kdb elektra-plugin elektra-io ${IO_VARIANT_LIBRARY} m)
		if (BUILD_FULL OR BUILD_STATIC)
			target_link_libraries (${testexename} ${LIBEV_LIBRARIES})
		endif ()

		add_test (
			NAME ${testexename}
			COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
			WORKING_DIRECTORY "${WORKING_DIRECTORY}")

		add_subdirectory (example)
	endif ()
endif ()
