list (FIND BINDINGS "io_uv" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (uv)
endif ()
