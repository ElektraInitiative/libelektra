get_filename_component(ELEKTRA_INSTALL_LIBRARY
		ELEKTRA_LIBRARY NAME
		)

set (ELEKTRA_LIBRARY ${CMAKE_INSTALL_PREFIX}/lib/${ELEKTRA_INSTALL_LIBRARY})
set (ELEKTRA_INCLUDE ${CMAKE_INSTALL_PREFIX}/include)

configure_file (
		"${PROJECT_SOURCE_DIR}/cmake/ElektraConfig.cmake.in"
		"${PROJECT_BINARY_DIR}/cmake/ElektraConfig.cmake"
		@ONLY
	       )

include (CMakeExportBuildSettings)

cmake_export_build_settings (
		${PROJECT_BINARY_DIR}/cmake/ElektraBuildSettings.cmake
		)

export_library_dependencies (
		${PROJECT_BINARY_DIR}/cmake/ElektraLibraryDepends.cmake
		)

# Install the packaging files for use by find_package
install_files (/lib FILES
		${PROJECT_SOURCE_DIR}/cmake/ElektraUse.cmake
		${PROJECT_BINARY_DIR}/cmake/ElektraConfig.cmake
		${PROJECT_BINARY_DIR}/cmake/ElektraBuildSettings.cmake
		${PROJECT_BINARY_DIR}/cmake/ElektraLibraryDepends.cmake
		)
