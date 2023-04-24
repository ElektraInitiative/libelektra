file (
	GLOB_RECURSE shellfiles
	RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
	LIST_DIRECTORIES false
	*.sh)

if (BUILD_SHARED)
	set (ElektraCodegen_ALL_LIBRARIES
	     "elektra-highlevel;elektra-opts;elektra-invoke;elektra-ease;elektra-kdb;elektra-core;elektra-types;elektra-utility")
elseif (BUILD_FULL)
	set (ElektraCodegen_ALL_LIBRARIES "elektra-full")
elseif (BUILD_STATIC)
	set (ElektraCodegen_ALL_LIBRARIES "elektra-static")
endif ()

if (CMAKE_C_FLAGS MATCHES "(^|.* )-m32( .*|$)")
	set (C_FLAG_32BIT "-m32")
else ()
	set (C_FLAG_32BIT "")
endif ()

foreach (file ${shellfiles})
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${file}" "${CMAKE_CURRENT_BINARY_DIR}/${file}" @ONLY)
endforeach ()

unset (C_FLAG_32BIT)

if (INSTALL_TESTING)
	# TODO: make clear via repo structure that this happens
	install (
		DIRECTORY "."
		DESTINATION "${TARGET_TEST_DATA_FOLDER}/gen"
		COMPONENT elektra-tests)
	# TODO: make clear via repo structure that this happens
	install (
		DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
		DESTINATION "${TARGET_TEST_DATA_FOLDER}/gen"
		COMPONENT elektra-tests
		FILES_MATCHING
		PATTERN "*.sh")
endif (INSTALL_TESTING)
