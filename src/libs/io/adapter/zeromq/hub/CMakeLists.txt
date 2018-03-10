include (LibAddMacros)

set (SRC_FILES
		hub_zeromq.c
	)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})

## Build test
set (HUB hub_zeromq)

add_executable (${HUB} ${SOURCES})
add_dependencies (${HUB} kdberrors_generated)
target_link_libraries (${HUB} ${ZMQ_LIBRARIES})
target_link_elektra (${HUB} elektra-kdb)
target_include_directories (${HUB} SYSTEM PUBLIC ${ZMQ_INCLUDE_DIRS})
