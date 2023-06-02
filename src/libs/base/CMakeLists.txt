set (SOURCES "internal.c")

if (HAVE_LOGGER)
	list (APPEND SOURCES log.c)
else (HAVE_LOGGER)
	list (APPEND SOURCES nolog.c)
endif (HAVE_LOGGER)

add_library (elektra-base OBJECT ${SOURCES})
set_property (TARGET elektra-base PROPERTY POSITION_INDEPENDENT_CODE ON)

if (BUILD_FULL OR BUILD_STATIC)
	add_sources (elektra-full $<TARGET_OBJECTS:elektra-base>)
endif (BUILD_FULL OR BUILD_STATIC)
