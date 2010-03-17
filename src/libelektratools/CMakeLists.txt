include(LibAddMacros)
add_headers(HDR_FILES)

set (SRC_FILES kdbtools.c stream.c)

list (APPEND SRC_FILES "../libhelper/helper.c")

set (SOURCES ${SRC_FILES} ${HDR_FILES})

include_directories (${LIBXML2_INCLUDE_DIR})
add_definitions (${LIBXML2_DEFINITIONS})


add_library (elektratools SHARED ${SOURCES})
set_target_properties (elektratools PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H
		)
target_link_libraries (elektratools ${LIBXML2_LIBRARIES})
