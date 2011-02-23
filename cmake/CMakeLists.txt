get_filename_component(ELEKTRA_INSTALL_LIBRARY
		ELEKTRA_LIBRARY NAME
		)

set (ELEKTRA_LIBRARY ${CMAKE_INSTALL_PREFIX}/lib/${ELEKTRA_INSTALL_LIBRARY})
set (ELEKTRA_INCLUDE ${CMAKE_INSTALL_PREFIX}/include)

#configure_file (
#		"${PROJECT_SOURCE_DIR}/cmake/ElektraConfig.cmake.in"
#		"${PROJECT_BINARY_DIR}/cmake/ElektraConfig.cmake"
#		@ONLY
#	       )
#
#include (CMakeExportBuildSettings)
#
#cmake_export_build_settings (
#		${PROJECT_BINARY_DIR}/cmake/ElektraBuildSettings.cmake
#		)
#
#export_library_dependencies (
#		${PROJECT_BINARY_DIR}/cmake/ElektraLibraryDepends.cmake
#		)


#because above configuring in fact does not work, provide a simple
#find package which should be enough for the moment
configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/FindElektra.cmake.in"
		"${CMAKE_CURRENT_BINARY_DIR}/FindElektra.cmake"
		@ONLY
	)

# Install the packaging files for use by find_package
install ( FILES
#		${PROJECT_SOURCE_DIR}/cmake/ElektraUse.cmake
#		${PROJECT_BINARY_DIR}/cmake/ElektraConfig.cmake
#		${PROJECT_BINARY_DIR}/cmake/ElektraBuildSettings.cmake
#		${PROJECT_BINARY_DIR}/cmake/ElektraLibraryDepends.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/FindElektra.cmake"
		DESTINATION ${CMAKE_DESTINATION}
	)

configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/elektra.pc.in"
		"${CMAKE_CURRENT_BINARY_DIR}/elektra.pc"
		@ONLY
	)

install ( FILES
		"${CMAKE_CURRENT_BINARY_DIR}/elektra.pc"
		DESTINATION ${PKGCONFIG_DESTINATION}
	)

