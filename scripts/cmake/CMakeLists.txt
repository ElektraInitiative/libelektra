# ElektraPackaging needs to be included first (sets PACKAGE_ variables)

include (CMakePackageConfigHelpers)

# cmake settings for use by find_package
if (BUILD_SHARED)
	set (Elektra_LIBRARIES "elektra-kdb elektra-core")
	set (ElektraHighlevel_LIBRARIES "elektra-highlevel")
	set (ElektraCodegen_LIBRARIES "elektra-highlevel elektra-opts elektra-invoke elektra-utility")
	set (Elektra_BUILD_TYPE "shared")
elseif (BUILD_FULL)
	set (Elektra_LIBRARIES "elektra-full")
	set (ElektraHighlevel_LIBRARIES "elektra-full")
	set (ElektraCodegen_LIBRARIES "elektra-full")
	set (Elektra_BUILD_TYPE "full")
elseif (BUILD_STATIC)
	set (Elektra_LIBRARIES "elektra-static")
	set (ElektraHighlevel_LIBRARIES "elektra-static")
	set (ElektraCodegen_LIBRARIES "elektra-static")
	set (Elektra_BUILD_TYPE "static")
endif ()
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/ElektraConfig.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/ElektraConfig.cmake" @ONLY)

write_basic_package_version_file (
	"${CMAKE_CURRENT_BINARY_DIR}/ElektraConfigVersion.cmake"
	VERSION ${KDB_VERSION}
	COMPATIBILITY AnyNewerVersion)

install (
	FILES "${CMAKE_CURRENT_BINARY_DIR}/ElektraConfig.cmake" "${CMAKE_CURRENT_BINARY_DIR}/ElektraConfigVersion.cmake"
	DESTINATION ${TARGET_CMAKE_FOLDER}
	COMPONENT libelektra-dev)

# pkg-config files
if (BUILD_SHARED)
	set (PACKAGE_LIBS "-lelektra-kdb -lelektra-core")
	set (PACKAGE_BUILD_TYPE "shared")
elseif (BUILD_FULL)
	set (PACKAGE_LIBS "-lelektra-full")
	set (PACKAGE_BUILD_TYPE "full")
elseif (BUILD_STATIC)
	set (PACKAGE_LIBS "-lelektra-static")
	set (PACKAGE_BUILD_TYPE "static")
endif ()
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra.pc" @ONLY)

install (
	FILES "${CMAKE_CURRENT_BINARY_DIR}/elektra.pc"
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	COMPONENT libelektra-dev)

add_custom_target (
	uninstall
	COMMAND
		"${CMAKE_COMMAND}" -D KDB_VERSION=${KDB_VERSION} -D CMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -D
		CMAKE_FIND_LIBRARY_PREFIXES=${CMAKE_FIND_LIBRARY_PREFIXES} -D CMAKE_FIND_LIBRARY_SUFFIXES=${CMAKE_FIND_LIBRARY_SUFFIXES} -P
		"${CMAKE_CURRENT_SOURCE_DIR}/ElektraUninstall.cmake"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
	COMMENT "Uninstalling Elektra from system or DESTDIR")
