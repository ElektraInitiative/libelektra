file (GLOB_RECURSE shellfiles
      RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
	       LIST_DIRECTORIES
	       false
	       *.sh)

foreach (file ${shellfiles})
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${file}" "${CMAKE_CURRENT_BINARY_DIR}/${file}" @ONLY)
endforeach ()

if (INSTALL_TESTING)
	install (DIRECTORY "." DESTINATION "${TARGET_TEST_DATA_FOLDER}/gen")
	install (DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" DESTINATION "${TARGET_TEST_DATA_FOLDER}/gen" FILES_MATCHING PATTERN "*.sh")
endif (INSTALL_TESTING)
