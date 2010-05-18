add_subdirectory (include)

add_subdirectory (libhelper)
add_subdirectory (libloader)

add_subdirectory (backends)

add_subdirectory (libelektra)

add_subdirectory (bindings)

if (BUILD_TOOLS)
	find_package (LibXml2 REQUIRED)
	if (LIBXML2_FOUND)
		add_subdirectory (libelektratools)
	endif (LIBXML2_FOUND)
endif (BUILD_TOOLS)

add_subdirectory (kdb)
