include(LibAddMacros)

set (SRC_FILES kdbtools.c stream.c)

list (APPEND SRC_FILES "../libhelper/helper.c")

set (SOURCES ${SRC_FILES} ${HDR_FILES})

include_directories (${LIBXML2_INCLUDE_DIR})
add_definitions (${LIBXML2_DEFINITIONS})

add_headers(HDR_FILES)

if (BUILD_SHARED OR BUILD_FULL)
	add_library (elektratools SHARED ${SOURCES})
	set_target_properties (elektratools PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H
			)
	target_link_libraries (elektratools ${LIBXML2_LIBRARIES})
	install (TARGETS elektratools DESTINATION lib/elektra)
endif (BUILD_SHARED OR BUILD_FULL)

if (BUILD_STATIC)
	add_library (elektratools-static STATIC ${SOURCES})
	set_target_properties (elektratools-static PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H
			)
	target_link_libraries (elektratools-static elektra-static ${LIBXML2_LIBRARIES})
	install (TARGETS elektratools DESTINATION lib/elektra)
endif (BUILD_STATIC)
