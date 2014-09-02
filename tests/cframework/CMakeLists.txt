include(LibAddMacros)

set(SOURCES tests.c)
add_headers(SOURCES)
add_testheaders(SOURCES)

add_library(cframework OBJECT ${SOURCES})

set_target_properties (cframework PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
