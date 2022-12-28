find_package (Erlang)

if (NOT ERLANG_FOUND)
	exclude_binding (elixir "Erlang not found.")
elseif (NOT BUILD_SHARED)
	exclude_binding (elixir "it can only be built if `BUILD_SHARED` is enabled")
else ()
	add_binding (elixir)

	# Set the output directory of the Erlang NIF library.
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/elixir/priv")

	file (GLOB ERL_NIF_SRC_FILES lib/*.c)
	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${ERL_NIF_SRC_FILES} ${ELEKTRA_HEADERS})

	add_library (nif_kdb SHARED ${SOURCES})

	target_link_libraries (nif_kdb elektra-core elektra-kdb)
	target_link_libraries (nif_kdb erlang)
endif()
