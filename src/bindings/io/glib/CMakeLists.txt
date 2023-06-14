include (LibAddMacros)

find_package (GLib QUIET)

if (NOT GLib_FOUND)
	exclude_binding (io_glib "glib-2.0 not found")
elseif (ENABLE_ASAN)
	exclude_binding (io_glib "io bindings are not compatible with ENABLE_ASAN")
else ()

	# Build library
	add_binding (io_glib)

	set (BINDING_VARIANT glib)

	set (IO_VARIANT_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/io_glib.c")

	set (SOURCES ${IO_VARIANT_SRC_FILES} ${ELEKTRA_HEADERS})
	add_compile_options ("-Wno-pedantic")

	set (IO_VARIANT_LIBRARY elektra-io-${BINDING_VARIANT})

	add_lib (
		io-${BINDING_VARIANT}
		SOURCES
		${SOURCES}
		LINK_ELEKTRA
		elektra-io
		LINK_LIBRARIES
		${GLib_LIBRARY}
		INCLUDE_SYSTEM_DIRECTORIES
		${GLib_INCLUDE_DIRS}
		COMPONENT
		io-glib-elektra)

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${IO_VARIANT_LIBRARY}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc"
			@ONLY)

	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
		COMPONENT io-glib-elektra)

	# Build test
	if (BUILD_TESTING)
		set (testexename testio_${BINDING_VARIANT})

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
		file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
		list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
		list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

		if (BUILD_FULL OR BUILD_STATIC) # add sources for elektra-io-glib for static and full builds
			list (APPEND TEST_SOURCES $<TARGET_OBJECTS:${IO_VARIANT_LIBRARY}-objects>)
		endif ()

		add_executable (${testexename} ${TEST_SOURCES})

		target_include_directories (${testexename} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")
		target_include_directories (${testexename} PRIVATE SYSTEM ${GLib_INCLUDE_DIRS})

		target_link_elektra (${testexename} elektra-kdb elektra-plugin elektra-io ${IO_VARIANT_LIBRARY} m)
		if (BUILD_FULL OR BUILD_STATIC)
			target_link_libraries (${testexename} ${GLib_LIBRARY})
		endif ()

		add_test (
			NAME ${testexename}
			COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
			WORKING_DIRECTORY "${WORKING_DIRECTORY}")
		set_property (TEST ${testexename} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		add_subdirectory (example)
	endif ()
endif ()
