#ElektraPackaging needs to be included first (sets PACKAGE_ variables)

include (CMakePackageConfigHelpers)

#cmake settings for use by find_package
configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/ElektraConfig.cmake.in"
		"${CMAKE_CURRENT_BINARY_DIR}/ElektraConfig.cmake"
		@ONLY
	)

write_basic_package_version_file (
		"${CMAKE_CURRENT_BINARY_DIR}/ElektraConfigVersion.cmake"
		VERSION ${KDB_VERSION}
		COMPATIBILITY AnyNewerVersion
	)

install ( FILES
		"${CMAKE_CURRENT_BINARY_DIR}/ElektraConfig.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/ElektraConfigVersion.cmake"
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
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_SOURCE_DIR}/ElektraUninstall.cmake")
