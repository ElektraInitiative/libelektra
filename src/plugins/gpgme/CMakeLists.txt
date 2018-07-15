include (LibAddPlugin)

if (DEPENDENCY_PHASE)

	find_package (LibGpgme)
	if (NOT LIBGPGME_FOUND)
		remove_plugin ("gpgme" "GPGME development files not found")
	endif ()

endif ()

set (GPGME_SOURCE_FILES keylist.h keylist.c gpgme.h gpgme.c)

#
# Compile Variant: OpenSSL
#
add_plugin (gpgme
	    SOURCES ${GPGME_SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${LIBGPGME_INCLUDE_DIR}
	    LINK_LIBRARIES ${LIBGPGME_LIBRARIES}
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"gpgme\")

if (ADDTESTING_PHASE)

	add_plugintest (gpgme LINK_PLUGIN "gpgme")

endif ()
