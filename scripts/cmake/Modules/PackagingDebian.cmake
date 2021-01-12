# ~~~
# In this file CPACK_DEBIAN_* vars necessary by CPack DEB
# to generate DEB packages are set. Also DEB package specifc
# files are installed.
# ~~~

set (CPACK_DEBIAN_PACKAGE_VERSION "${PROJECT_VERSION}")
set (CPACK_DEBIAN_PACKAGE_RELEASE "${CPACK_PACKAGE_RELEASE}")
set (DEBIAN_VERSION_RELEASE "${CPACK_DEBIAN_PACKAGE_VERSION}-${CPACK_DEBIAN_PACKAGE_RELEASE}")
set (CPACK_DEBIAN_PACKAGE_MAINTAINER "Robert Sowula <robert@sowula.at>")

set (CPACK_GENERATOR "DEB")
set (CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
set (CPACK_DEBIAN_PACKAGE_SOURCE "elektra")
set (CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)

set (CPACK_DEB_COMPONENT_INSTALL "ON")
set (CPACK_DEB_PACKAGE_COMPONENT "ON")
set (CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS "ON")
set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
set (CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS "ON")

set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_DEPENDS "netbase")
set (
	CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_BREAKS
	"elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelektra-core4 (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})"
)
set (
	CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_REPLACES
	"elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelelektra-core4 (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})"
)
set (CPACK_DEBIAN_LIBELEKTRA4_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
set (CPACK_DEBIAN_LIBELEKTRA4_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-FULL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_SECTION "libs")
set (
	CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_BREAKS
	"elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelektra-core4 (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})"
)
set (
	CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_REPLACES
	"elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelelektra-core4 (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})"
)
set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_CONFLICTS
     "libelektra4 (<< ${CPACK_DEBIAN_PACKAGE_VERSION}), elektra-tests (<< ${CPACK_DEBIAN_PACKAGE_VERSION})")
set (CPACK_DEBIAN_LIBELEKTRA4-FULL_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
set (CPACK_DEBIAN_LIBELEKTRA4-FULL_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-EXPERIMENTAL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_BREAKS
     "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelektra-core4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_REPLACES
     "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelelektra-core4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
set (CPACK_DEBIAN_LIBELEKTRA4-EXPERIMENTAL_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-EXTRA_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_BREAKS
     "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelektra-core4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_REPLACES
     "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelelektra-core4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_PACKAGE_SUGGESTS "elektra-doc, ${ALL_PLUGINS_STR}")
set (CPACK_DEBIAN_LIBELEKTRA4-EXTRA_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-BIN_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_SECTION "misc")
set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_BREAKS "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}),libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_REPLACES
     "elektra-bin (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_ELEKTRA-BIN_PACKAGE_CONFLICTS "kernel-patch-kdb")
set (CPACK_DEBIAN_ELEKTRA-BIN_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_BREAKS "libelektra-augeas4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_PACKAGE_REPLACES "libelektra-augeas4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-AUGEAS_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-DBUS_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_BREAKS "libelektra-dbus4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_PACKAGE_REPLACES "libelektra-dbus4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-DBUS_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA-DEV_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA-DEV_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-DEV_PACKAGE_SECTION "libdevel")

set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-ZEROMQ_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_BREAKS "libelektra-xmltool4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_PACKAGE_REPLACES "libelektra-xmltool4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-XMLTOOL_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-XERCES_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_BREAKS "libelektra-xerces4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_PACKAGE_REPLACES "libelektra-xerces4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-XERCES_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-YAJL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_BREAKS "libelektra-yajl4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_PACKAGE_REPLACES "libelektra-yajl4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-YAJL_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-CRYPTO_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-CURL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-CURL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-CURL_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-CURL_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-JOURNALD_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-YAMLCPP_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-YAMLCPP_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-YAMLCPP_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-YAMLCPP_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-LUA_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-LUA_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-LUA_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-LUA_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-JAVA_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-JAVA_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-JAVA_PACKAGE_DEPENDS "default-jdk")
set (CPACK_DEBIAN_LIBELEKTRA4-JAVA_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-JAVA_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_JAVA-ELEKTRA_PACKAGE_NAME "${CPACK_COMPONENT_JAVA-ELEKTRA_DISPLAY_NAME}")
set (CPACK_DEBIAN_JAVA-ELEKTRA_PACKAGE_DEPENDS "default-jdk")
set (CPACK_DEBIAN_JAVA-ELEKTRA_PACKAGE_SECTION "java")

set (CPACK_DEBIAN_LUA-ELEKTRA_PACKAGE_NAME "${CPACK_COMPONENT_LUA-ELEKTRA_DISPLAY_NAME}")
set (CPACK_DEBIAN_LUA-ELEKTRA_PACKAGE_SECTION "interpreters")
set (CPACK_DEBIAN_LUA-ELEKTRA_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_NAME "${CPACK_COMPONENT_PYTHON3-ELEKTRA_DISPLAY_NAME}")
set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_DEPENDS "python3")
set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_SECTION "python")
set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_CONTROL_STRICT_PERMISSION TRUE)
file (GLOB CONTROL_FILES_PYTHON3-ELEKTRA "${CMAKE_SOURCE_DIR}/scripts/packaging/debian/control/python3-elektra/*")
file (COPY ${CONTROL_FILES_PYTHON3-ELEKTRA} DESTINATION "${CMAKE_BINARY_DIR}/scripts/packaging/debian/control/python3-elektra/")
file (GLOB CONTROL_FILES_PYTHON3-ELEKTRA_BINARY "${CMAKE_BINARY_DIR}/scripts/packaging/debian/control/python3-elektra/*")
set (CPACK_DEBIAN_PYTHON3-ELEKTRA_PACKAGE_CONTROL_EXTRA "${CONTROL_FILES_PYTHON3-ELEKTRA_BINARY}")
set (CPACK_DEBIAN_PYTHON3-ELEKTRA_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_SECTION "libs")
set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_BREAKS "libelelektra-python4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_PACKAGE_REPLACES "libelelektra-python4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_LIBELEKTRA4-PYTHON_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_DEPENDS "python-all")
set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_SECTION "misc")
set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_PACKAGE_CONFLICTS "${CPACK_COMPONENT_ELEKTRA-BIN_DISPLAY_NAME} (<< ${CPACK_DEBIAN_PACKAGE_VERSION})")
set (CPACK_DEBIAN_ELEKTRA-BIN-EXTRA_DEBUGINFO_PACKAGE "OFF")

set (CPACK_DEBIAN_ELEKTRA-QT-GUI_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-QT-GUI_DISPLAY_NAME}")
if ("${OS_DISTRIB}" MATCHES "Ubuntu20.04")
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
set (CPACK_DEBIAN_ELEKTRA-QT-GUI_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-TESTS_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_SECTION "misc")
set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_PRIORITY "extra")
set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_BREAKS
     "libelektra-test (<<${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_REPLACES
     "libelektra-test (<< ${DEBIAN_VERSION_RELEASE}), libelektra-full4 (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_ELEKTRA-TESTS_DEBUGINFO_PACKAGE "${PACKAGE_DEBUGINFO}")

set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-DOC_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_ARCHITECTURE "all")
set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_SECTION "doc")
set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_BREAKS "libelektra-doc (<< ${DEBIAN_VERSION_RELEASE})")
set (CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_REPLACES "libelektra-doc (<< ${DEBIAN_VERSION_RELEASE})")
install (
	FILES "${CMAKE_SOURCE_DIR}/scripts/packaging/debian/doc-base/elektra-doc"
	COMPONENT elektra-doc
	DESTINATION ${TARGET_DOCUMENTATION_DOC-BASE_FOLDER})

set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-DBG_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_DEPENDS "${DEBIAN_DBG_PACKAGE_NAMES_STR}")
set (CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_SECTION "debug")

set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_NAME "${CPACK_COMPONENT_LIBELEKTRA4-ALL_DISPLAY_NAME}")
set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_SECTION "misc")
set (CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_DEPENDS ${LIBELEKTRA4-ALL_DEPENDS})
set (
	CPACK_DEBIAN_LIBELEKTRA4-ALL_PACKAGE_RECOMMENDS
	"${CPACK_DEBIAN_ELEKTRA-TESTS_PACKAGE_NAME}, ${CPACK_DEBIAN_ELEKTRA-DOC_PACKAGE_NAME}, ${CPACK_DEBIAN_ELEKTRA-DBG_PACKAGE_NAME}, ${CPACK_DEBIAN_LIBELEKTRA-DEV_PACKAGE_NAME}"
)

set (CPACK_DEBIAN_ELEKTRA-MISC_PACKAGE_NAME "${CPACK_COMPONENT_ELEKTRA-MISC_DISPLAY_NAME}")
set (CPACK_DEBIAN_ELEKTRA-MISC_PACKAGE_ARCHITECTURE "all")
set (CPACK_DEBIAN_ELEKTRA-MISC_PACKAGE_SECTION "misc")

# install copyright file
configure_file ("${CMAKE_SOURCE_DIR}/doc/THIRD-PARTY-LICENSES" "${CMAKE_BINARY_DIR}/doc/copyright" COPYONLY)
foreach (component ${PACKAGES})
	install (
		FILES "${CMAKE_BINARY_DIR}/doc/copyright"
		COMPONENT ${component}
		DESTINATION "share/doc/${component}/")
endforeach ()

# compress and install changelog
add_custom_command (
	OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian.gz"
	COMMAND gzip -cn9 "debian/changelog" > "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian.gz"
	WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/scripts/packaging"
	COMMENT "Compressing changelog")

add_custom_target (changelog ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian.gz")

foreach (component ${PACKAGES})
	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian.gz"
		COMPONENT ${component}
		DESTINATION "share/doc/${component}/")
endforeach ()
