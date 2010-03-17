add_subdirectory (include)
add_subdirectory (backends)
add_subdirectory (libelektra)

find_package (LibXml2 REQUIRED)
if (LIBXML2_FOUND)
	add_subdirectory (libelektratools)
endif (LIBXML2_FOUND)
