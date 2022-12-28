find_program (
	ERL_BINARY
	NAMES erl
	DOC "Erlang `erl` executable command." QUIET)

if (ERL_BINARY)
	execute_process (COMMAND ${ERL_BINARY} -noshell -eval "io:put_chars(code:root_dir()), halt()." OUTPUT_VARIABLE ERLANG_ROOT_DIR)

	set (ERLANG_FOUND true)
	set (ERLANG_INCLUDE_DIR ${ERLANG_ROOT_DIR}/usr/include)

	add_library (erlang INTERFACE)
	target_include_directories (erlang INTERFACE ${ERLANG_INCLUDE_DIR})
else ()
	set (ERLANG_FOUND false)
endif ()
