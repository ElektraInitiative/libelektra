include(LibAddMacros)

set (SOURCES iconv.h iconv.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-iconv MODULE ${SOURCES})
install (TARGETS elektra-iconv DESTINATION lib/elektra)


set (TEST_SOURCES
		${CMAKE_SOURCE_DIR}/tests/tests.c
		${CMAKE_SOURCE_DIR}/tests/tests.h
		)
include_directories ("${CMAKE_SOURCE_DIR}/tests")

if (BUILD_FULL)
	add_executable (testmod_iconv ${TEST_SOURCES} testmod_iconv.c)
	target_link_libraries (testmod_iconv elektra-full)
	set_target_properties (testmod_iconv PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H)
	add_test (testmod_iconv
			"${PROJECT_CURRENT_BINARY_DIR}/testmod_iconv"
			"${PROJECT_CURRENT_BINARY_DIR}"
			)
endif (BUILD_FULL)
