cmake_minimum_required (VERSION 3.0)

project (libelektra-elixir)

list (APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

find_package (Erlang REQUIRED)
find_package (Elektra REQUIRED)

set (
	ELEKTRA_NIF_MODULE_NAME
	Elixir.Elektra.System
	CACHE STRING "The name of the module which exports the NIFs.")

if (ELEKTRA_FOUND)
	message (STATUS "Elektra ${ELEKTRA_VERSION} found")
	message (INFO ${ELEKTRA_INCLUDE_DIR})

	include_directories (${ELEKTRA_INCLUDE_DIR})

	add_library (nif_kdb SHARED ${PROJECT_SOURCE_DIR}/lib/nif_kdb.c)

	target_link_libraries (nif_kdb ${ELEKTRA_LIBRARIES})
	target_link_libraries (nif_kdb erlang)

	target_compile_definitions (nif_kdb PRIVATE _ELEKTRA_NIF_MODULE_NAME=${ELEKTRA_NIF_MODULE_NAME})
else (ELEKTRA_FOUND)
	message (FATAL_ERROR "Elektra not found")
endif (ELEKTRA_FOUND)
