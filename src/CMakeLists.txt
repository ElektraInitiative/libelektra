add_subdirectory (include)

add_subdirectory (error)

add_subdirectory (libloader)

add_subdirectory (plugins)

add_subdirectory (libelektra)

add_subdirectory (bindings)

add_subdirectory (kdb)

# xml2 detection code will be needed
# later in plugin!
#	find_package (LibXml2 REQUIRED)
#	if (LIBXML2_FOUND)
#		add_subdirectory (libelektratools)
#	endif (LIBXML2_FOUND)
