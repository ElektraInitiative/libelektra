include (FindXfconf)

if (NOT XFCONF_FOUND)
	exclude_binding (xfconf "xfconf >= 4.16 required for xfconf backend")
	return ()
endif ()

add_binding (xfconf)

set (
	XFCONF_BINDING_LIBRARY
	xfconf-elektra
	CACHE INTERNAL "XFCONF_BINDING_LIBRARY")

include_directories (${GLIB_INCLUDE_DIRS})
include_directories (${XFCONF_INCLUDE_DIRS})

file (GLOB XFCONF_BINDING_HEADERS *.h)
file (GLOB XFCONF_BINDING_SOURCES *.c)

set (SOURCES ${XFCONF_BINDING_HEADERS} ${XFCONF_BINDING_SOURCES})
add_library (${XFCONF_BINDING_LIBRARY} SHARED ${SOURCES})
target_compile_options (${XFCONF_BINDING_LIBRARY} PUBLIC)
target_link_libraries (${XFCONF_BINDING_LIBRARY} PUBLIC elektra-core elektra-kdb)

install (TARGETS ${XFCONF_BINDING_LIBRARY} LIBRARY DESTINATION lib${LIB_SUFFIX} COMPONENT xfconf-elektra)
