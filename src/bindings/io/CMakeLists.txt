list (FIND BINDINGS "io_uv" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (uv)
endif ()

list (FIND BINDINGS "io_doc" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (doc)
endif ()
