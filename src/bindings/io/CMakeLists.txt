check_binding_included ("io_uv" IS_INCLUDED SUBDIRECTORY "io/uv")
if (IS_INCLUDED)
	add_subdirectory (uv)
endif ()

#check_binding_included ("io_doc" IS_INCLUDED SUBDIRECTORY "io/doc")
#if (IS_INCLUDED)
#	add_subdirectory (doc)
#endif ()
