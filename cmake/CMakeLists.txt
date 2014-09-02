#ElektraPackaging needs to be included first (sets PACKAGE_ variables)

#cmake settings for use by find_package
configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/FindElektra.cmake.in"
		"${CMAKE_CURRENT_BINARY_DIR}/FindElektra.cmake"
		@ONLY
	)

install ( FILES
		"${CMAKE_CURRENT_BINARY_DIR}/FindElektra.cmake"
		DESTINATION ${TARGET_CMAKE_FOLDER}
	)

#pkg-config files
configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/elektra.pc.in"
		"${CMAKE_CURRENT_BINARY_DIR}/elektra.pc"
		@ONLY
	)

install ( FILES
		"${CMAKE_CURRENT_BINARY_DIR}/elektra.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	)

ADD_CUSTOM_TARGET(uninstall
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/ElektraUninstall.cmake")
