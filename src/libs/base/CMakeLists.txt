set (SOURCES "internal.c")

if (HAVE_LOGGER)
	list (APPEND SOURCES log.c)
else (HAVE_LOGGER)
	list (APPEND SOURCES nolog.c)
endif (HAVE_LOGGER)

add_library (elektra-base OBJECT ${SOURCES})
set_property (TARGET elektra-base PROPERTY POSITION_INDEPENDENT_CODE ON)
