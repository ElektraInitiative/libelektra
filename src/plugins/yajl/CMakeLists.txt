if (DEPENDENCY_PHASE)
	find_package(Yajl)

	set (INCL
		${YAJL_INCLUDE_DIR}
		${CMAKE_CURRENT_BINARY_DIR}
	    )

	if (NOT YAJL_FOUND)
		remove_plugin (yajl "yajl not found")
	endif (NOT YAJL_FOUND)
endif ()

add_plugin(yajl
	SOURCES
		yajl.c iterator.c yajl_gen.c
		yajl_gen_open.c  yajl_gen_close.c
		yajl_parse.c name.c
		"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
	ADD_TEST
	INSTALL_TEST_DATA
	INCLUDE_DIRECTORIES
		"${INCL}"
	LINK_ELEKTRA
		elektra-ease
	LINK_LIBRARIES
		${YAJL_LIBRARIES}
	)

if (ADDTESTING_PHASE)
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/yajl.h.in"
		"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
		)
endif ()
