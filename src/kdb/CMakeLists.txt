include (LibAddMacros)

file (GLOB HDR_FILES *.h)
add_headers (HDR_FILES)
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

file (GLOB SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_executable (kdb ${SOURCES})

target_link_libraries (kdb elektra)
