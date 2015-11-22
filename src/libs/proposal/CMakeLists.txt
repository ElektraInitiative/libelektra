file (GLOB SOURCES *.c)
add_headers(SOURCES)

if (BUILD_SHARED)
	add_library (elektra-proposal SHARED ${SOURCES})

	target_link_libraries (elektra-proposal elektra-core)
endif (BUILD_SHARED)

set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS"
	${SOURCES}
	)

set_property (GLOBAL APPEND PROPERTY "elektra-extension_LIBRARIES"
	elektra-proposal
	)
