try_add_binding ("io_uv" IS_INCLUDED SUBDIRECTORY "uv")
if (IS_INCLUDED)
	add_subdirectory (uv)
endif ()

#try_add_binding ("io_doc" IS_INCLUDED SUBDIRECTORY "doc")
#if (IS_INCLUDED)
#	add_subdirectory (doc)
#endif ()
