set (PACKAGE_NAME "elektra")
set (CPACK_PACKAGE_NAME "${PACKAGE_NAME}")
set (PACKAGE_URL "https://www.libelektra.org/")
set (PACKAGE_BUGREPORT "https://bugs.libelektra.org/")

set (PROJECT_VERSION "${KDB_VERSION}")
set (CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set (CPACK_DEBIAN_PACKAGE_VERSION "${PROJECT_VERSION}")
set (CPACK_DEBIAN_PACKAGE_RELEASE "1")
set (CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)

set (
	CPACK_COMPONENTS_ALL
	libelektra4
	libelektra4-all
	libelektra4-full
	libelektra4-experimental
	libelektra4-extra
	libelektra4-augeas
	libelektra4-crypto
	libelektra4-curl
	libelektra4-dbus
	libelektra-dev
	libelektra4-journald
	libelektra4-lua
	libelektra4-python
	libelektra4-xerces
	libelektra4-xmltool
	libelektra4-yajl
	libelektra4-zeromq
	lua-elektra
	python3-elektra
	elektra-bin
	elektra-bin-extra
	elektra-doc
	elektra-qt-gui
	elektra-tests
	elektra-dbg)

set (ALL_PLUGINS ${CPACK_COMPONENTS_ALL})
list (FILTER ALL_PLUGINS INCLUDE REGEX "^libelektra4-.*")
string (REPLACE ";" ", " ALL_PLUGINS_STR "${ALL_PLUGINS}")

set (DBG_PACKAGE_NAMES "")
foreach(component ${CPACK_COMPONENTS_ALL})
	list(APPEND DBG_PACKAGE_NAMES "${component}-dbgsym")
endforeach()
list (FILTER DBG_PACKAGE_NAMES EXCLUDE REGEX "^elektra-doc.*") # elektra-doc doesn't contain dbgsym
list (FILTER DBG_PACKAGE_NAMES EXCLUDE REGEX "^elektra-dbg.*") 
list (FILTER DBG_PACKAGE_NAMES EXCLUDE REGEX "^libelektra-dev.*")
list (FILTER DBG_PACKAGE_NAMES EXCLUDE REGEX "^libelektra4-all.*")
list (FILTER DBG_PACKAGE_NAMES EXCLUDE REGEX "^elektra-bin-extra.*")
string (REPLACE ";" ", " DBG_PACKAGE_NAMES_STR "${DBG_PACKAGE_NAMES}")

set (
	PACKAGE_DESCRIPTION
	"Elektra provides a universal and secure framework to store configuration parameters in a global, hierarchical key database. The core is a small library implemented in C. The plugin-based framework fulfills many configuration-related tasks to avoid any unnecessary code duplication across applications while it still allows the core to stay without any external dependency.  Elektra abstracts from cross-platform-related issues with an consistent API, and allows applications to be aware of other applications' configurations, leveraging easy application integration.\n."
)
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "${PACKAGE_DESCRIPTION}")
set (CPACK_PACKAGE_CONTACT "${PACKAGE_URL}")
set (
	CPACK_SOURCE_IGNORE_FILES
	"/.cvsignore"
	"/.gitignore"
	"/songs/"
	"/build/"
	"/.svn/"
	"/.git/"
	"/osx-utils/"
	"/portage-overlay/")
set (CPACK_PACKAGE_EXECUTABLES kdb)
set (CPACK_SOURCE_GENERATOR "TBZ2")
set (CPACK_GENERATOR "TBZ2")

# needed because otherwise files would be written to system during creating the package
set (CPACK_SET_DESTDIR "ON")

# package is not relocatable:
unset (CPACK_RPM_PACKAGE_RELOCATABLE)
unset (CPACK_RPM_PACKAGE_RELOCATABLE CACHE)

if ("${CMAKE_BUILD_TYPE}" MATCHES "Release")
	set (CPACK_STRIP_FILES TRUE)
endif ("${CMAKE_BUILD_TYPE}" MATCHES "Release")

if (APPLE)
	set (CPACK_GENERATOR "PACKAGEMAKER;OSXX11")
endif (APPLE)
if (UNIX)

	# Try to find architecture
	execute_process (COMMAND uname -m OUTPUT_VARIABLE CPACK_PACKAGE_ARCHITECTURE)
	string (STRIP "${CPACK_PACKAGE_ARCHITECTURE}" CPACK_PACKAGE_ARCHITECTURE)

	# Try to find distro name and distro-specific arch
	execute_process (COMMAND lsb_release -is OUTPUT_VARIABLE LSB_ID)
	execute_process (COMMAND lsb_release -rs OUTPUT_VARIABLE LSB_RELEASE)
	string (STRIP "${LSB_ID}" LSB_ID)
	string (STRIP "${LSB_RELEASE}" LSB_RELEASE)
	set (LSB_DISTRIB "${LSB_ID}${LSB_RELEASE}")
	if (NOT LSB_DISTRIB)
		set (LSB_DISTRIB "unix")
	endif (NOT LSB_DISTRIB)

	# For Debian-based distros we want to create DEB packages.
	if ("${LSB_DISTRIB}" MATCHES "Ubuntu|Debian")
		set (CPACK_GENERATOR "DEB")
		set (CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
		set (CPACK_DEBIAN_PACKAGE_SOURCE "elektra")
		set (CPACK_DEBIAN_PACKAGE_RECOMMENDS "")
		set (CPACK_DEBIAN_PACKAGE_DEPENDS "") # no dependencies without any PLUGINS

		set (CPACK_DEB_COMPONENT_INSTALL "ON")
		set (CPACK_DEB_PACKAGE_COMPONENT "ON")
		set (CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS "ON")
		set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
		set (CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_NAME "libelektra4")
		set (CPACK_COMPONENT_LIBELEKTRA4_DISPLAY_NAME "libelektra4")
		set (CPACK_COMPONENT_LIBELEKTRA4_DESCRIPTION "This package contains the main elektra library, and most of the core plugins")
		set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_BREAKS
		     "elektra-bin (<< 0.8.14-5~), libelektra-core4 (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_REPLACES
		     "elektra-bin (<< 0.8.14-5~), libelelektra-core4 (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
		set (CPACK_DEBIAN_LIBELEKTRA4_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_NAME "libelektra4-full")
		set (CPACK_COMPONENT_LIBELEKTRA4-FULL_DISPLAY_NAME "libelektra4-full")
		set (CPACK_COMPONENT_LIBELEKTRA4-FULL_DESCRIPTION
		     "This package contains an variant of the Elektra library in which all plugins
			are linked together to a full library. The package is only needed for testing.")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_BREAKS
		     "elektra-bin (<< 0.8.14-5~), libelektra-core4 (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_REPLACES
		     "elektra-bin (<< 0.8.14-5~), libelelektra-core4 (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_CONFLICTS
		     "libelektra4 (<< ${CPACK_DEBIAN_PACKAGE_VERSION}), elektra-tests (<< ${CPACK_DEBIAN_PACKAGE_VERSION})")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
		set (CPACK_DEBIAN_LIBELEKTRA4-FULL_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_NAME "libelektra4-experimental")
		set (CPACK_COMPONENT_LIBELEKTRA4-EXPERIMENTAL_DISPLAY_NAME "libelektra4-experimental")
		set (CPACK_COMPONENT_LIBELEKTRA4-EXPERIMENTAL_DESCRIPTION "This package contains experimental plugins.")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_BREAKS "elektra-bin (<< 0.8.14-5~), libelektra-core4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_REPLACES "elektra-bin (<< 0.8.14-5~), libelelektra-core4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_NAME "libelektra4-extra")
		set (CPACK_COMPONENT_LIBELEKTRA4-EXTRA_DISPLAY_NAME "libelektra4-extra")
		set (CPACK_COMPONENT_LIBELEKTRA4-EXTRA_DESCRIPTION "This package contains extra plugins.")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_BREAKS "elektra-bin (<< 0.8.14-5~), libelektra-core4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_REPLACES "elektra-bin (<< 0.8.14-5~), libelelektra-core4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
		set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_NAME "elektra-bin")
		set (CPACK_COMPONENT_ELEKTRA-BIN_DISPLAY_NAME "elektra-bin")
		set (CPACK_COMPONENT_ELEKTRA-BIN_DESCRIPTION "This package contains command line utilities for Elektra.")
		set (CPACK_COMPONENT_ELEKTRA-BIN_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_SECTION "misc")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_BREAKS "elektra-bin (<< 0.8.14-5~),libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_REPLACES "elektra-bin (<< 0.8.14-5~), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_CONFLICTS "kernel-patch-kdb")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_CONTROL_STRICT_PERMISSION TRUE)
		file (GLOB CONTROL_FILES_ELEKTRA-BIN "${CMAKE_SOURCE_DIR}/scripts/packaging/debian-control/elektra-bin/*")
		file (COPY ${CONTROL_FILES_ELEKTRA-BIN} DESTINATION "${CMAKE_BINARY_DIR}/scripts/packaging/debian-control/elektra-bin/")
		file (GLOB CONTROL_FILES_ELEKTRA-BIN_BINARY "${CMAKE_BINARY_DIR}/scripts/packaging/debian-control/elektra-bin/*")
		set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_CONTROL_EXTRA "${CONTROL_FILES_ELEKTRA-BIN_BINARY}")
		set (CPACK_DEBIAN_ELEKTRA-BIN_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_NAME "libelektra4-augeas")
		set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DISPLAY_NAME "libelektra4-augeas")
		set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DESCRIPTION "This package contains the 'augeas' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_BREAKS "libelektra-augeas4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_REPLACES "libelektra-augeas4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_NAME "libelektra4-dbus")
		set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DISPLAY_NAME "libelektra4-dbus")
		set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DESCRIPTION "This package contains the 'dbus' plugins.")
		set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_BREAKS "libelektra-dbus4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_REPLACES "libelektra-dbus4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA-DEV_PACKAGE_NAME "libelektra-dev")
		set (CPACK_COMPONENT_LIBELEKTRA-DEV_DISPLAY_NAME "libelektra-dev")
		set (CPACK_COMPONENT_LIBELEKTRA-DEV_DESCRIPTION "This package contains the development files for the main Elektra library.")
		set (CPACK_COMPONENT_LIBELEKTRA4-DEV_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-DEV_PACKAGE_SECTION "libdevel")

		set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_PACKAGE_NAME "libelektra4-zeromq")
		set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DISPLAY_NAME "libelektra4-zeromq")
		set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DESCRIPTION "This package contains the 'zeromq' plugins.")
		set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_NAME "libelektra4-xmltool")
		set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DISPLAY_NAME "libelektra4-xmltool")
		set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DESCRIPTION "This package contains the 'xmltool' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_BREAKS "libelektra-xmltool4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_REPLACES "libelektra-xmltool4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_NAME "libelektra4-xerces")
		set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DISPLAY_NAME "libelektra4-xerces")
		set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DESCRIPTION "This package contains the 'xerces' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_BREAKS "libelektra-xerces4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_REPLACES "libelektra-xerces4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_NAME "libelektra4-yajl")
		set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DISPLAY_NAME "libelektra4-yajl")
		set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DESCRIPTION "This package contains the 'yajl' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_BREAKS "libelektra-yajl4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_REPLACES "libelektra-yajl4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_PACKAGE_NAME "libelektra4-crypto")
		set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DISPLAY_NAME "libelektra4-crypto")
		set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DESCRIPTION "This package contains the crypto plugins.")
		set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-CURL_PACKAGE_NAME "libelektra4-curl")
		set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DISPLAY_NAME "libelektra4-curl")
		set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DESCRIPTION "This package contains the 'curlget' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-CURL_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-CURL_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_PACKAGE_NAME "libelektra4-journald")
		set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DISPLAY_NAME "libelektra4-journald")
		set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DESCRIPTION "This package contains the 'journald' plugins.")
		set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-LUA_PACKAGE_NAME "libelektra4-lua")
		set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DISPLAY_NAME "libelektra4-lua")
		set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DESCRIPTION "This package contains the 'lua' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LIBELEKTRA4-LUA_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-LUA_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LUA-ELEKTRA_PACKAGE_NAME "lua-elektra")
		set (CPACK_COMPONENT_LUA-ELEKTRA_DISPLAY_NAME "lua-elektra")
		set (CPACK_COMPONENT_LUA-ELEKTRA_DESCRIPTION "This package contains the Lua bindings.")
		set (CPACK_COMPONENT_LUA-ELEKTRA_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_LUA-ELEKTRA_PACKAGE_SECTION "interpreters")
		set (CPACK_DEBIAN_LUA-ELEKTRA_DEBUGINFO_PACKAGE "ON")

		# maybe add python3:depends? (see control file)
		set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_NAME "python3-elektra")
		set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DISPLAY_NAME "python3-elektra")
		set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DESCRIPTION "This package contains the Python 3 bindings.")
		set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_SECTION "python")
		set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_CONTROL_STRICT_PERMISSION TRUE)
		file (GLOB CONTROL_FILES_PYTHON3-ELEKTRA "${CMAKE_SOURCE_DIR}/scripts/packaging/debian-control/python3-elektra/*")
		file (COPY ${CONTROL_FILES_PYTHON3-ELEKTRA}
		      DESTINATION "${CMAKE_BINARY_DIR}/scripts/packaging/debian-control/python3-elektra/")
		file (GLOB CONTROL_FILES_PYTHON3-ELEKTRA_BINARY "${CMAKE_BINARY_DIR}/scripts/packaging/debian-control/python3-elektra/*")
		set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_CONTROL_EXTRA "${CONTROL_FILES_PYTHON3-ELEKTRA_BINARY}")
		set (CPACK_DEBIAN_PYTHON3-ELEKTRA_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_NAME "libelektra4-python")
		set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DISPLAY_NAME "libelektra4-python")
		set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DESCRIPTION "This package contains the 'python' plugin.")
		set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DEPENDS "libelektra4" "python3-elektra")
		set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_BREAKS "libelelektra-python4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_REPLACES "libelelektra-python4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_NAME "elektra-bin-extra")
		set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DISPLAY_NAME "elektra-bin-extra")
		set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DESCRIPTION
		     "This package contains extra command line utilities for Elektra written in non-shell languages like python.")
		set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DEPENDS "libelektra4")
		set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_DEPENDS "python-all")
		set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_SECTION "misc")
		set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_CONFLICTS "elektra-bin (<< ${CPACK_DEBIAN_PACKAGE_VERSION})")
		set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_DEBUGINFO_PACKAGE "OFF")

		set (CPACK_DEBIAN_ELEKTRA-QT-GUI_PACKAGE_NAME "elektra-qt-gui")
		set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DISPLAY_NAME "elektra-qt-gui")
		set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DESCRIPTION "This package contains a Qt-based graphical interface for Elektra.")
		set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DEPENDS "libelektra4")
		if ("${LSB_DISTRIB}" MATCHES "Ubuntu20.04")
			# qml-module-qtquick-controls-styles-breeze not available on ubuntu focal
			set (
				CPACK_DEBIAN_ELEKTRA-QT-GUI_PACKAGE_DEPENDS
				"qml-module-qtquick2, qml-module-qtquick-window2, qml-module-qtquick-dialogs, qml-module-qt-labs-folderlistmodel, qml-module-qt-labs-settings"
			)
		else ()
			set (
				CPACK_DEBIAN_ELEKTRA-QT-GUI_PACKAGE_DEPENDS
				"qml-module-qtquick2, qml-module-qtquick-window2, qml-module-qtquick-dialogs, qml-module-qt-labs-folderlistmodel, qml-module-qt-labs-settings, qml-module-qtquick-controls-styles-breeze"
			)
		endif ()
		set (CPACK_DEBIAN_ELEKTRA-QT-GUI_PACKAGE_SECTION "misc")
		set (CPACK_DEBIAN_ELEKTRA-QT-GUI_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_NAME "elektra-tests")
		set (CPACK_COMPONENT_ELEKTRA-TESTS_DISPLAY_NAME "elektra-tests")
		set (CPACK_COMPONENT_ELEKTRA-TESTS_DESCRIPTION "This package contains the Elektra test suite.")
		set (CPACK_COMPONENT_ELEKTRA-TESTS_DEPENDS "libelektra4-full" "elektra-bin")
		set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_SECTION "misc")
		set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_PRIORITY "extra")
		set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_BREAKS "libelektra-test (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_REPLACES "libelektra-test (<< 0.8.19-1), libelektra-full4 (<< 0.8.19-1)")
		set (CPACK_DEBIAN_ELEKTRA-TESTS_DEBUGINFO_PACKAGE "ON")

		set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_NAME "elektra-doc")
		set (CPACK_COMPONENT_ELEKTRA-DOC_DISPLAY_NAME "elektra-doc")
		set (CPACK_COMPONENT_ELEKTRA-DOC_DESCRIPTION "This package contains the API documentation for the Elektra libraries.")
		set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_ARCHITECTURE "all")
		set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_SECTION "doc")
		set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_BREAKS "libelektra-doc (<< 0.8.19-1)")
		set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_REPLACES "libelektra-doc (<< 0.8.19-1)")
		install (
			FILES "${CMAKE_SOURCE_DIR}/scripts/packaging/doc-base/elektra-doc"
			COMPONENT elektra-doc
			DESTINATION ${TARGET_DOCUMENTATION_DOC-BASE_FOLDER})

		set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_NAME "libelektra4-all")
		set (CPACK_COMPONENT_LIBELEKTRA4-ALL_DISPLAY_NAME "libelektra4-all")
		set (CPACK_COMPONENT_LIBELEKTRA4-ALL_DESCRIPTION
		     "This package contains extra command line utilities for Elektra written in non-shell languages like python.")
		set (
			CPACK_COMPONENT_LIBELEKTRA4-ALL_DEPENDS
			"libelektra4"
			"libelektra4-experimental"
			"libelektra4-augeas"
			"libelektra4-dbus"
			"libelektra4-zeromq"
			"libelektra4-lua"
			"libelektra4-python"
			"libelektra4-xmltool"
			"libelektra4-xerces"
			"libelektra4-yajl"
			"lua-elektra"
			"elektra-bin"
			"elektra-qt-gui"
			"libelektra4-crypto"
			"libelektra4-curl"
			"libelektra4-journald"
			"libelektra4-extra")
		set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_SECTION "misc")
		set (
			CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_DEPENDS
			"libelektra4, libelektra4-experimental, libelektra4-augeas, libelektra4-dbus, libelektra4-zeromq, libelektra4-lua, libelektra4-python, libelektra4-xmltool, libelektra4-xerces, libelektra4-yajl, lua-elektra, elektra-bin, elektra-qt-gui, libelektra4-crypto, libelektra4-curl, libelektra4-journald, libelektra4-extra"
		)
		set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_RECOMMENDS "elektra-tests, elektra-doc, elektra-dbg, libelektra-dev") 

		set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_NAME "elektra-dbg")
		set (CPACK_COMPONENT_ELEKTRA-DBG_DISPLAY_NAME "elektra-dbg")
		set (CPACK_COMPONENT_ELEKTRA-DBG_DESCRIPTION "This package contains the dependencies to all dbgsym packages of Elektra.")
		set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_DEPENDS "${DBG_PACKAGE_NAMES_STR}")
		set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_SECTION "debug")

		# install copyright file
		configure_file ("${CMAKE_SOURCE_DIR}/doc/THIRD-PARTY-LICENSES" "${CMAKE_BINARY_DIR}/doc/copyright" COPYONLY)
		foreach(component ${CPACK_COMPONENTS_ALL})
			install (
				FILES "${CMAKE_BINARY_DIR}/doc/copyright"
				COMPONENT ${component}
				DESTINATION "share/doc/${component}/")
		endforeach()

		# We need to alter the architecture names as per distro rules
		if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
			set (CPACK_PACKAGE_ARCHITECTURE i386)
		endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
		if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")
			set (CPACK_PACKAGE_ARCHITECTURE amd64)
		endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")
	endif ("${LSB_DISTRIB}" MATCHES "Ubuntu|Debian")
	set (CPACK_SYSTEM_NAME "${LSB_DISTRIB}-${CPACK_PACKAGE_ARCHITECTURE}")
	message (STATUS "Detected ${CPACK_SYSTEM_NAME}. Use make package to build packages (${CPACK_GENERATOR}).")
endif (UNIX)

set (CPACK_RPM_SPEC_MORE_DEFINE "%define ignore \#")

set (
	CPACK_RPM_USER_FILELIST
	"%ignore /etc/profile.d"
	"%ignore /etc"
	"%ignore /etc/bash_completion.d"
	"%ignore /usr"
	"%ignore /usr/local"
	"%ignore /usr/local/bin"
	"%ignore /usr/local/include"
	"%ignore /usr/local/lib"
	"%ignore /usr/local/share"
	"%ignore /usr/local/share/man"
	"%ignore /etc"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /etc/bash_completion.d"
	"%ignore /usr"
	"%ignore /usr/bin"
	"%ignore /usr/include"
	"%ignore /usr/lib"
	"%ignore /usr/share"
	"%ignore /usr/share/doc"
	"%ignore /usr/share/man")

include (CPack)
