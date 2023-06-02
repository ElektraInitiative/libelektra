include (LibAddMacros)

message (STATUS "Include binding io_doc")

set (BINDING_VARIANT doc)

# Build library

set (IO_VARIANT_HDR_FILES kdbio_doc.h)
set (IO_VARIANT_SRC_FILES io_doc.c)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${IO_VARIANT_SRC_FILES} ${IO_VARIANT_HDR_FILES} ${ELEKTRA_HEADERS})

set (IO_VARIANT_LIBRARY elektra-io-${BINDING_VARIANT})

add_library (${IO_VARIANT_LIBRARY} SHARED ${SOURCES})
target_include_directories (${IO_VARIANT_LIBRARY} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# ~~~
# If needed add a library (e.g. found with pkg_check_modules)
# target_include_directories (${IO_VARIANT_LIBRARY} SYSTEM PUBLIC ${SOMELIB_INCLUDE_DIRS})
# target_compile_options (${IO_VARIANT_LIBRARY} PUBLIC ${SOMELIB_CFLAGS_OTHER})
# target_link_elektra (${IO_VARIANT_LIBRARY} elektra-io ${SOMELIB_LDFLAGS})
# ~~~
target_link_elektra (${IO_VARIANT_LIBRARY} elektra-io)

install (TARGETS ${IO_VARIANT_LIBRARY} LIBRARY DESTINATION lib${LIB_SUFFIX})

install (FILES ${IO_VARIANT_HDR_FILES} DESTINATION include/${TARGET_INCLUDE_FOLDER})

# configure_file ( "${CMAKE_CURRENT_SOURCE_DIR}/${IO_VARIANT_LIBRARY}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc" @ONLY )

# install ( FILES "${CMAKE_CURRENT_BINARY_DIR}/${IO_VARIANT_LIBRARY}.pc" DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER} )

if (BUILD_TESTING)
	# Build test
	set (testexename testio_${BINDING_VARIANT})
	set (ADDITIONAL_TEST_LIBRARIES "m")

	set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
	add_headers (TEST_SOURCES)
	file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
	list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
	list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${BINDING_VARIANT}.c")

	add_executable (${testexename} ${TEST_SOURCES})

	target_include_directories (${testexename} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")

	target_link_elektra (${testexename} elektra-kdb elektra-plugin elektra-io ${IO_VARIANT_LIBRARY} ${ADDITIONAL_TEST_LIBRARIES})

	add_test (
		NAME ${testexename}
		COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
		WORKING_DIRECTORY "${WORKING_DIRECTORY}")
	set_property (TEST ${testexename} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
endif ()
