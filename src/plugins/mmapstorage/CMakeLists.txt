#[[
include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	find_package (ZLIB QUIET)
	if (NOT ZLIB_FOUND)
		remove_plugin (mmapstorage_crc "zlib development files not found")
	endif ()
endif (DEPENDENCY_PHASE)

set (MMAPSTORAGE_SOURCES dynarray.h dynarray.c mmapstorage.h mmapstorage.c)

add_plugin (
	mmapstorage_crc
	SOURCES ${MMAPSTORAGE_SOURCES}
	INCLUDE_DIRECTORIES ${ZLIB_INCLUDE_DIRS}
	LINK_LIBRARIES ${ZLIB_LIBRARIES}
	LINK_ELEKTRA elektra-core
	ADD_TEST COMPONENT libelektra${SO_VERSION}-extra
	COMPILE_DEFINITIONS ELEKTRA_VARIANT=crc ELEKTRA_MMAP_CHECKSUM)

# Plugin variant: mmapstorage
add_plugin (
	mmapstorage
	SOURCES ${MMAPSTORAGE_SOURCES}
	LINK_ELEKTRA elektra-core
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION})
]]
