find_package(Yajl REQUIRED)

#converts a README.md (markdown texts with key/value pairs)
#to a README.c that contains keys to be added in the contract
#
# Supports all 
function (generate_readme p)
	# hack to rerun cmake when README.md is changed:
	# (but also allows cmake variable substitution as an upside
	configure_file(README.md ${CMAKE_CURRENT_BINARY_DIR}/README.out)
	FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/README.out contents)
	STRING(REGEX REPLACE "\\\\" "\\\\\\\\" contents "${contents}")
	STRING(REGEX REPLACE "\"" "\\\\\"" contents "${contents}")
	STRING(REGEX REPLACE "\n" "\\\\n\"\n\"" contents "${contents}")
	#STRING(REGEX REPLACE "\"info = (.*)\\n\"" "keyNew(\"system/elektra/modules/${p}/infos\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "infos = ([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/licence *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/licence\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/author *= *([.@<>a-zA-Z0-9 %_-]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/author\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/provides *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/provides\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/placements *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/placements\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/recommends *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/recommends\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/needs *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/needs\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"infos/description *= *(.*)\\\\n\"\n\"" "keyNew(\"system/elektra/modules/${p}/infos/description\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/README.c ${contents})
endfunction()


if (YAJL_FOUND)
	include (LibAddMacros)

	#set(ELEKTRA_YAJL_VERBOSE yes)

	generate_readme (yajl)

	add_includes (elektra-full ${YAJL_INCLUDE_DIR})
	include_directories (${YAJL_INCLUDE_DIR})

	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	set (SOURCES yajl.c array.c iterator.c yajl_gen.c
		yajl_gen_open.c  yajl_gen_close.c
		yajl_parse.c name.c)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/yajl.h.in"
		"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
		)

	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	#does not work with cmake 2.6
	file (COPY examples
		DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

	set (PLUGIN_NAME elektra-yajl)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	install (TARGETS ${PLUGIN_NAME} DESTINATION
		lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	target_link_libraries (${PLUGIN_NAME} elektra)
	target_link_libraries (${PLUGIN_NAME} ${YAJL_LIBRARIES})
	add_libraries (elektra-full ${YAJL_LIBRARIES})

	add_plugintest (yajl)
else (YAJL_FOUND)
	remove_plugin (yajl "yajl not found")
endif (YAJL_FOUND)
