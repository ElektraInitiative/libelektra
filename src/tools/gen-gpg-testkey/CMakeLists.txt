find_package (LibGpgme QUIET)
if (NOT LIBGPGME_FOUND)
	remove_tool (gen-gpg-testkey "libgpgme not found")
else ()
	# check GPGME version (requires at least 1.10)
	if (${LIBGPGME_VERSION} MATCHES "1[.][1-9][0-9]+.*")
		add_executable (gen-gpg-testkey gen-gpg-testkey.c)
		target_include_directories (gen-gpg-testkey PRIVATE ${LIBGPGME_INCLUDE_DIRS})
		target_link_libraries (gen-gpg-testkey ${LIBGPGME_LIBRARIES})
		install (
			TARGETS gen-gpg-testkey
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
			COMPONENT elektra-tests)
	else ()
		remove_tool (gen-gpg-testkey "libgpgme version ${LIBGPGME_VERSION} is too old - minimum version required is 1.10")
	endif ()
endif ()
