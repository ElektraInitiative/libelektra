# ~~~
# check_component_dependencies
#
# check and excludes a component from packaging if
# the plugin/binding/tool the component depends on
# is missing
#
# dependency_name  name of the dependency the component depends on
# component_name   name of the component
# PLUGIN           whether the dependency is a plugin
# BINDING          whether the dependency is a binding
# TOOL             whether the dependency is a tool
# ADDITIONAL_DEPENDENCIES additional dependencies if the component
# 				          has multiple dependencies
# ~~~
macro (check_component_dependencies dependency_name component_name)
	cmake_parse_arguments (
		ARG
		"PLUGIN;BINDING;TOOL" # optional keywords
		"" # one value keywords
		"ADDITIONAL_DEPENDENCIES" # multi value keywords
		${ARGN})

	set (TMP_DEPENDENCY_NAMES ${dependency_name})
	if (ARG_ADDITIONAL_DEPENDENCIES)
		list (APPEND TMP_DEPENDENCY_NAMES "${ARG_ADDITIONAL_DEPENDENCIES}")
	endif ()

	set (TMP_UNFULFILLED_DEPENDENCIES "")

	foreach (name ${TMP_DEPENDENCY_NAMES})

		if (ARG_PLUGIN)
			if (NOT ${name} IN_LIST ADDED_PLUGINS)
				set (TMP_DEPENDENCY_TYPE "plugin")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()
		if (ARG_BINDING)
			if (NOT ${name} IN_LIST ADDED_BINDINGS)
				set (TMP_DEPENDENCY_TYPE "binding")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()
		if (ARG_TOOL)
			if (NOT ${name} IN_LIST TOOLS)
				message (STATUS "ADDED_TOOLS: ${ADDED_TOOLS}")
				set (TMP_DEPENDENCY_TYPE "tool")
				list (APPEND TMP_UNFULFILLED_DEPENDENCIES ${name})
			endif ()
		endif ()

	endforeach ()

	list (LENGTH TMP_UNFULFILLED_DEPENDENCIES UNFULFILLED_DEPENDENCIES_AMOUNT)
	list (LENGTH TMP_DEPENDENCY_NAMES EXPECTED_DEPENDENCIES_AMOUNT)
	if (${UNFULFILLED_DEPENDENCIES_AMOUNT} GREATER 1)
		set (TMP_DEPENDENCY_TYPE "${TMP_DEPENDENCY_TYPE}s")
	endif ()
	string (REPLACE ";" ", " TMP_UNFULFILLED_DEPENDENCIES_STR "${TMP_UNFULFILLED_DEPENDENCIES}")
	if (TMP_UNFULFILLED_DEPENDENCIES AND ${UNFULFILLED_DEPENDENCIES_AMOUNT} EQUAL ${EXPECTED_DEPENDENCIES_AMOUNT})
		# all dependencies of component are missing
		message (
			STATUS
				"Exclude component ${component_name} because ${TMP_UNFULFILLED_DEPENDENCIES_STR} ${TMP_DEPENDENCY_TYPE} excluded."
		)
		list (APPEND EXCLUDED_COMPONENTS ${component_name})
	elseif (TMP_UNFULFILLED_DEPENDENCIES AND ${UNFULFILLED_DEPENDENCIES_AMOUNT} LESS ${EXPECTED_DEPENDENCIES_AMOUNT})
		# at least one but not all dependencies of a component are missing
		message (
			STATUS
				"Component ${component_name} is missing ${TMP_UNFULFILLED_DEPENDENCIES_STR} ${TMP_DEPENDENCY_TYPE}. This package will still be generated."
		)
	endif ()

endmacro (check_component_dependencies)

set (PACKAGE_URL "https://www.libelektra.org/")
set (PACKAGE_BUGREPORT "https://bugs.libelektra.org/")
set (PROJECT_VERSION "${KDB_VERSION}")
set (CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")

set (CPACK_OUTPUT_FILE_PREFIX "packages")

set (
	PACKAGES
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
	libelektra4-java
	libelektra4-journald
	libelektra4-lua
	libelektra4-python
	libelektra4-xerces
	libelektra4-xmltool
	libelektra4-yajl
	libelektra4-yamlcpp
	libelektra4-zeromq
	java-elektra
	lua-elektra
	python3-elektra
	elektra-bin
	elektra-bin-extra
	elektra-doc
	elektra-qt-gui
	elektra-tests
	elektra-dbg
	${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME})

set (
	COMPONENTS_WITHOUT_DBGSYM
	elektra-doc
	elektra-dbg
	libelektra-dev
	libelektra4-all
	elektra-bin-extra
	java-elektra
	${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME})

set (ALL_PLUGINS "")
foreach (component ${PACKAGES})
	if (component MATCHES "^libelektra4-.*")
		list (APPEND ALL_PLUGINS "${component}")
	endif ()
endforeach ()
string (REPLACE ";" ", " ALL_PLUGINS_STR "${ALL_PLUGINS}")

set (EXCLUDED_COMPONENTS "")

if (CMAKE_BUILD_TYPE MATCHES "Release")
	set (PACKAGE_DEBUGINFO "OFF")
	list (REMOVE_ITEM PACKAGES elektra-dbg)
else ()
	set (PACKAGE_DEBUGINFO "ON")
endif ()

set (
	PACKAGE_DESCRIPTION
	"Elektra provides a universal and secure framework to store configuration parameters in a global, hierarchical key database. The core is a small library implemented in C. The plugin-based framework fulfills many configuration-related tasks to avoid any unnecessary code duplication across applications while it still allows the core to stay without any external dependency.  Elektra abstracts from cross-platform-related issues with an consistent API, and allows applications to be aware of other applications' configurations, leveraging easy application integration."
)
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "${PACKAGE_DESCRIPTION}\n.")
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
	execute_process (COMMAND bash "-c" "grep \"^NAME=\" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g'"
			 OUTPUT_VARIABLE OS_NAME)
	execute_process (COMMAND bash "-c" "grep \"^VERSION_ID=\" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g'"
			 OUTPUT_VARIABLE OS_VERSION_ID)
	string (STRIP "${OS_NAME}" OS_NAME)
	string (STRIP "${OS_VERSION_ID}" OS_VERSION_ID)
	set (OS_DISTRIB "${OS_NAME}${OS_VERSION_ID}")
	if (NOT OS_DISTRIB)
		set (OS_DISTRIB "unix")
	endif (NOT OS_DISTRIB)

	set (CPACK_COMPONENT_LIBELEKTRA4_DISPLAY_NAME "libelektra4")
	set (CPACK_COMPONENT_LIBELEKTRA4_DESCRIPTION "This package contains the main elektra library, and most of the core plugins")

	set (CPACK_COMPONENT_LIBELEKTRA4-FULL_DISPLAY_NAME "libelektra4-full")
	set (
		CPACK_COMPONENT_LIBELEKTRA4-FULL_DESCRIPTION
		"This package contains an variant of the Elektra library in which all plugins are linked together to a full library. The package is only needed for testing."
	)

	set (CPACK_COMPONENT_LIBELEKTRA4-EXPERIMENTAL_DISPLAY_NAME "libelektra4-experimental")
	set (CPACK_COMPONENT_LIBELEKTRA4-EXPERIMENTAL_DESCRIPTION "This package contains experimental plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA4-EXTRA_DISPLAY_NAME "libelektra4-extra")
	set (CPACK_COMPONENT_LIBELEKTRA4-EXTRA_DESCRIPTION "This package contains extra plugins.")

	set (CPACK_COMPONENT_ELEKTRA-BIN_DISPLAY_NAME "elektra-bin")
	set (CPACK_COMPONENT_ELEKTRA-BIN_DESCRIPTION "This package contains command line utilities for Elektra.")
	set (CPACK_COMPONENT_ELEKTRA-BIN_DEPENDS "libelektra4")

	set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DISPLAY_NAME "libelektra4-augeas")
	set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DESCRIPTION "This package contains the 'augeas' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-AUGEAS_DEPENDS "libelektra4")
	check_component_dependencies (augeas libelektra4-augeas PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DISPLAY_NAME "libelektra4-dbus")
	set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DESCRIPTION "This package contains the 'dbus' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA4-DBUS_DEPENDS "libelektra4")
	check_component_dependencies (dbus libelektra4-dbus PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA-DEV_DISPLAY_NAME "libelektra-dev")
	set (CPACK_COMPONENT_LIBELEKTRA-DEV_DESCRIPTION "This package contains the development files for the main Elektra library.")
	set (CPACK_COMPONENT_LIBELEKTRA4-DEV_DEPENDS "libelektra4")

	set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DISPLAY_NAME "libelektra4-zeromq")
	set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DESCRIPTION "This package contains the 'zeromq' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA4-ZEROMQ_DEPENDS "libelektra4")
	check_component_dependencies (zeromqrecv libelektra4-zeromq PLUGIN ADDITIONAL_DEPENDENCIES zeromqsend)

	set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DISPLAY_NAME "libelektra4-xmltool")
	set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DESCRIPTION "This package contains the 'xmltool' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-XMLTOOL_DEPENDS "libelektra4")
	check_component_dependencies (xmltool libelektra4-xmltool PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DISPLAY_NAME "libelektra4-xerces")
	set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DESCRIPTION "This package contains the 'xerces' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-XERCES_DEPENDS "libelektra4")
	check_component_dependencies (xerces libelektra4-xerces PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DISPLAY_NAME "libelektra4-yajl")
	set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DESCRIPTION "This package contains the 'yajl' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-YAJL_DEPENDS "libelektra4")
	check_component_dependencies (yajl libelektra4-yajl PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DISPLAY_NAME "libelektra4-crypto")
	set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DESCRIPTION "This package contains the crypto plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA4-CRYPTO_DEPENDS "libelektra4")
	check_component_dependencies (crypto libelektra4-crypto PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DISPLAY_NAME "libelektra4-curl")
	set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DESCRIPTION "This package contains the 'curlget' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-CURL_DEPENDS "libelektra4")
	check_component_dependencies (curlget libelektra4-curl PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DISPLAY_NAME "libelektra4-journald")
	set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DESCRIPTION "This package contains the 'journald' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA4-JOURNALD_DEPENDS "libelektra4")
	check_component_dependencies (journald libelektra4-journald PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-YAMLCPP_DISPLAY_NAME "libelektra4-yamlcpp")
	set (CPACK_COMPONENT_LIBELEKTRA4-YAMLCPP_DESCRIPTION "This package contains the 'yamlcpp' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-YAMLCPP_DEPENDS "libelektra4")
	check_component_dependencies (yamlcpp libelektra4-yamlcpp PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-JAVA_DISPLAY_NAME "libelektra4-java")
	set (CPACK_COMPONENT_LIBELEKTRA4-JAVA_DESCRIPTION "This package contains the 'jni' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-JAVA_DEPENDS "libelektra4" "java-elektra")
	check_component_dependencies (jni libelektra4-java PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DISPLAY_NAME "libelektra4-lua")
	set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DESCRIPTION "This package contains the 'lua' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-LUA_DEPENDS "libelektra4")
	check_component_dependencies (lua libelektra4-lua PLUGIN)

	set (CPACK_COMPONENT_JAVA-ELEKTRA_DISPLAY_NAME "java-elektra")
	set (CPACK_COMPONENT_JAVA-ELEKTRA_DESCRIPTION "This package contains the Java bindings.")
	set (CPACK_COMPONENT_JAVA-ELEKTRA_DEPENDS "libelektra4")
	check_component_dependencies (jna java-elektra BINDING)

	set (CPACK_COMPONENT_LUA-ELEKTRA_DISPLAY_NAME "lua-elektra")
	set (CPACK_COMPONENT_LUA-ELEKTRA_DESCRIPTION "This package contains the Lua bindings.")
	set (CPACK_COMPONENT_LUA-ELEKTRA_DEPENDS "libelektra4")
	check_component_dependencies (lua lua-elektra BINDING)

	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DISPLAY_NAME "python3-elektra")
	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DESCRIPTION "This package contains the Python 3 bindings.")
	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DEPENDS "libelektra4")
	check_component_dependencies (python python3-elektra BINDING)

	set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DISPLAY_NAME "libelektra4-python")
	set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DESCRIPTION "This package contains the 'python' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA4-PYTHON_DEPENDS "libelektra4" "python3-elektra")
	check_component_dependencies (python libelektra4-python PLUGIN)

	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DISPLAY_NAME "elektra-bin-extra")
	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DESCRIPTION
	     "This package contains extra command line utilities for Elektra written in non-shell languages like python.")
	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DEPENDS "libelektra4")

	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DISPLAY_NAME "elektra-qt-gui")
	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DESCRIPTION "This package contains a Qt-based graphical interface for Elektra.")
	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DEPENDS "libelektra4")
	check_component_dependencies (qt-gui elektra-qt-gui TOOL)

	set (CPACK_COMPONENT_ELEKTRA-TESTS_DISPLAY_NAME "elektra-tests")
	set (CPACK_COMPONENT_ELEKTRA-TESTS_DESCRIPTION "This package contains the Elektra test suite.")
	set (CPACK_COMPONENT_ELEKTRA-TESTS_DEPENDS "libelektra4-full" "elektra-bin")

	set (CPACK_COMPONENT_ELEKTRA-DOC_DISPLAY_NAME "elektra-doc")
	set (CPACK_COMPONENT_ELEKTRA-DOC_DESCRIPTION "This package contains the API documentation for the Elektra libraries.")

	set (CPACK_COMPONENT_LIBELEKTRA4-ALL_DISPLAY_NAME "libelektra4-all")
	set (CPACK_COMPONENT_LIBELEKTRA4-ALL_DESCRIPTION "This package provides the dependencies for all Elektra packages.")
	set (
		CPACK_COMPONENT_LIBELEKTRA4-ALL_DEPENDS
		"libelektra4"
		"libelektra4-experimental"
		"libelektra4-augeas"
		"libelektra4-dbus"
		"libelektra4-zeromq"
		"libelektra4-java"
		"libelektra4-lua"
		"libelektra4-python"
		"libelektra4-xmltool"
		"libelektra4-xerces"
		"libelektra4-yajl"
		"libelektra4-yamlcpp"
		"lua-elektra"
		"elektra-bin"
		"elektra-qt-gui"
		"libelektra4-crypto"
		"libelektra4-curl"
		"libelektra4-journald"
		"libelektra4-extra")
	string (REPLACE ";" ", " LIBELEKTRA4-ALL_DEPENDS "${CPACK_COMPONENT_LIBELEKTRA4-ALL_DEPENDS}")

	set (CPACK_COMPONENT_ELEKTRA-DBG_DISPLAY_NAME "elektra-dbg")
	set (CPACK_COMPONENT_ELEKTRA-DBG_DESCRIPTION "This package contains the dependencies to all dbgsym packages of Elektra.")

	set (CPACK_COMPONENT_ELEKTRA-MISC_DISPLAY_NAME "elektra-misc")
	set (CPACK_COMPONENT_ELEKTRA-MISC_DESCRIPTION "This package contains all files not part of any of the released Elektra packages.")

	# exclude components for package generation where dependencies are not fulfilled.
	foreach (component ${EXCLUDED_COMPONENTS})
		list (REMOVE_ITEM PACKAGES ${component})
	endforeach (component)
	# remove libelektra4-all package if a component is excluded
	if (EXCLUDED_COMPONENTS)
		list (REMOVE_ITEM PACKAGES libelektra4-all)
		string (REPLACE ";" ", " EXCLUDED_COMPONENTS_STR "${EXCLUDED_COMPONENTS}")
		message (STATUS "Excluding libelektra4-all because following components are excluded: ${EXCLUDED_COMPONENTS_STR}".)
		list (APPEND EXCLUDED_COMPONENTS libelektra4-all)
	endif ()
	# For Debian-based distros we want to create DEB packages.
	if ("${OS_NAME}" MATCHES "Ubuntu|Debian")
		set (DEBIAN_DBG_PACKAGE_NAMES "")
		foreach (component ${PACKAGES})
			if (NOT component IN_LIST COMPONENTS_WITHOUT_DBGSYM)
				list (APPEND DEBIAN_DBG_PACKAGE_NAMES "${component}-dbgsym")
			endif ()
		endforeach ()
		string (REPLACE ";" ", " DEBIAN_DBG_PACKAGE_NAMES_STR "${DEBIAN_DBG_PACKAGE_NAMES}")

		include (PackagingDebian)

	elseif ("${OS_NAME}" MATCHES "Fedora|CentOS")
		# no debuginfo for python3-elektra on fedora
		list (APPEND COMPONENTS_WITHOUT_DBGSYM "python3-elektra")
		set (FEDORA_DBG_PACKAGE_NAMES "")
		foreach (component ${PACKAGES})
			if (NOT component IN_LIST COMPONENTS_WITHOUT_DBGSYM)
				list (APPEND FEDORA_DBG_PACKAGE_NAMES "${component}-debuginfo")
			endif ()
		endforeach ()
		string (REPLACE ";" ", " FEDORA_DBG_PACKAGE_NAMES_STR "${FEDORA_DBG_PACKAGE_NAMES}")

		include (PackagingFedora)

	endif ()

	# We need to alter the architecture names as per distro rules
	if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
		set (CPACK_PACKAGE_ARCHITECTURE i386)
	endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
	if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")
		set (CPACK_PACKAGE_ARCHITECTURE amd64)
	endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")

	set (CPACK_SYSTEM_NAME "${OS_DISTRIB}-${CPACK_PACKAGE_ARCHITECTURE}")

	message (STATUS "Detected ${CPACK_SYSTEM_NAME}. Use make package to build packages (${CPACK_GENERATOR}).")
endif (UNIX)

set (CPACK_RPM_SPEC_MORE_DEFINE "%define ignore \#")

include (CPack)

foreach (component ${CPACK_COMPONENTS_ALL})
	if (NOT component IN_LIST PACKAGES)
		message (SEND_ERROR "Component ${component} is not
part of PACKAGES (ElektraPackaging.cmake). Please add it to this list, so the dependencies can be derived correctly.")
	endif ()
endforeach ()
