set (magic_number "")
math (EXPR size "${CMAKE_SIZEOF_VOID_P} - 1")
foreach (idx RANGE 0 "${size}")
	math (EXPR byte "${idx}" OUTPUT_FORMAT HEXADECIMAL)
	if (${idx} LESS 16)
		string (PREPEND magic_number "0${idx}")
	else ()
		string (PREPEND magic_number "${idx}")
	endif ()
endforeach (idx RANGE 0 ${CMAKE_SIZEOF_VOID_P})
string (PREPEND magic_number "0x")

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

if (NOT COLLECTION_PHASE)
	target_compile_definitions (elektra-mmapstorage-objects PRIVATE "ELEKTRA_MMAP_MAGIC_NUMBER=${magic_number}")
	target_compile_definitions (elektra-mmapstorage_crc-objects PRIVATE "ELEKTRA_MMAP_MAGIC_NUMBER=${magic_number}")
endif ()
