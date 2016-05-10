if (DEPENDENCY_PHASE)
	find_package(Yajl)

	if (YAJL_FOUND)
		configure_file (
			"${CMAKE_CURRENT_SOURCE_DIR}/yajl.h.in"
			"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
			)

		install(DIRECTORY yajl DESTINATION ${TARGET_TEST_DATA_FOLDER})

		include_directories(${CMAKE_CURRENT_BINARY_DIR})
		add_plugintest (yajl
			INCLUDE_DIRECTORIES
				${YAJL_INCLUDE_DIR}
			)
	else (YAJL_FOUND)
		remove_plugin (yajl "yajl not found")
	endif (YAJL_FOUND)
endif ()

add_plugin(yajl
	SOURCES
		yajl.c iterator.c yajl_gen.c
		yajl_gen_open.c  yajl_gen_close.c
		yajl_parse.c name.c
		"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
	INCLUDE_DIRECTORIES
		${YAJL_INCLUDE_DIR}
		${CMAKE_CURRENT_BINARY_DIR}
	LINK_ELEKTRA
		elektra-ease
	LINK_LIBRARIES
		${YAJL_LIBRARIES}
	)
