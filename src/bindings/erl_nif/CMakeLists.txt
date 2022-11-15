cmake_minimum_required (VERSION 3.0)

project (libelektra-elixir)

list (APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

find_package (Erlang REQUIRED)
find_package (Elektra REQUIRED)

include_directories (${ELEKTRA_INCLUDE_DIR})

add_library (nif_kdb SHARED ${PROJECT_SOURCE_DIR}/lib/nif_kdb.c)

target_link_libraries (nif_kdb ${ELEKTRA_LIBRARIES})
target_link_libraries (nif_kdb erlang)
