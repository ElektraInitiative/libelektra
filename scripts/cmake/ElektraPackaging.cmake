include (PackagingMacros)

set (PACKAGE_URL "https://www.libelektra.org/")
set (PACKAGE_BUGREPORT "https://bugs.libelektra.org/")
set (PROJECT_VERSION "${KDB_VERSION}")
set (CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")

if (NOT CPACK_PACKAGE_RELEASE)
	set (CPACK_PACKAGE_RELEASE "1")
endif ()

set (CPACK_OUTPUT_FILE_PREFIX "package")

set (
	PACKAGES
	libelektra${SO_VERSION}
	libelektra${SO_VERSION}-all
	libelektra${SO_VERSION}-full
	libelektra${SO_VERSION}-experimental
	libelektra${SO_VERSION}-extra
	libelektra${SO_VERSION}-augeas
	libelektra${SO_VERSION}-crypto
	libelektra${SO_VERSION}-curl
	libelektra${SO_VERSION}-dbus
	libelektra-dev
	libelektra${SO_VERSION}-gitresolver
	libelektra${SO_VERSION}-java
	libelektra${SO_VERSION}-journald
	libelektra${SO_VERSION}-lua
	libelektra${SO_VERSION}-python
	libelektra${SO_VERSION}-ruby
	libelektra${SO_VERSION}-xerces
	libelektra${SO_VERSION}-xmltool
	libelektra${SO_VERSION}-yajl
	libelektra${SO_VERSION}-yamlcpp
	libelektra${SO_VERSION}-zeromq
	libelektra${SO_VERSION}-fuse
	glib-elektra
	io-ev-elektra
	io-glib-elektra
	io-uv-elektra
	java-elektra
	lua-elektra
	ruby-elektra
	python3-elektra
	xfconf-elektra
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
	libelektra${SO_VERSION}-all
	elektra-bin-extra
	java-elektra
	ruby-elektra
	xfconf-elektra
	libelektra${SO_VERSION}-fuse
	${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME})

set (ALL_PLUGINS "")
foreach (component ${PACKAGES})
	if (component MATCHES "^libelektra${SO_VERSION}-.*")
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

# use all available threads
set (CPACK_THREADS 0)

# needed because otherwise files would be written to system during creating the package
set (CPACK_SET_DESTDIR "ON")

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

	if (NOT APPLE)
		# Try to find distro name and distro-specific arch
		execute_process (COMMAND bash "-c" "grep \"^NAME=\" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g'"
				 OUTPUT_VARIABLE OS_NAME)
		execute_process (COMMAND bash "-c" "grep \"^VERSION_ID=\" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g'"
				 OUTPUT_VARIABLE OS_VERSION_ID)
		execute_process (COMMAND bash "-c" "grep \"^PRETTY_NAME=\" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g'"
				 OUTPUT_VARIABLE OS_PRETTY_NAME)
		string (STRIP "${OS_NAME}" OS_NAME)
		string (STRIP "${OS_VERSION_ID}" OS_VERSION_ID)
		string (STRIP "${OS_PRETTY_NAME}" OS_PRETTY_NAME)
	endif (NOT APPLE)
	set (OS_DISTRIB "${OS_NAME}${OS_VERSION_ID}")
	string (REPLACE " " "-" OS_DISTRIB "${OS_DISTRIB}") # replace spaces with dashes, since rpm-build can't handle paths with spaces
	if (NOT OS_DISTRIB)
		set (OS_DISTRIB "unix")
	endif (NOT OS_DISTRIB)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}_DISPLAY_NAME "libelektra${SO_VERSION}")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}_DESCRIPTION
	     "This package contains the main elektra library, and most of the core plugins")

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-FULL_DISPLAY_NAME "libelektra${SO_VERSION}-full")
	set (
		CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-FULL_DESCRIPTION
		"This package contains an variant of the Elektra library in which all plugins are linked together to a full library. The package is only needed for testing."
	)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-EXPERIMENTAL_DISPLAY_NAME "libelektra${SO_VERSION}-experimental")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-EXPERIMENTAL_DESCRIPTION "This package contains experimental plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-EXTRA_DISPLAY_NAME "libelektra${SO_VERSION}-extra")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-EXTRA_DESCRIPTION "This package contains extra plugins.")

	set (CPACK_COMPONENT_ELEKTRA-BIN_DISPLAY_NAME "elektra-bin")
	set (CPACK_COMPONENT_ELEKTRA-BIN_DESCRIPTION "This package contains command line utilities for Elektra.")
	set (CPACK_COMPONENT_ELEKTRA-BIN_DEPENDS "libelektra${SO_VERSION}")

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-AUGEAS_DISPLAY_NAME "libelektra${SO_VERSION}-augeas")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-AUGEAS_DESCRIPTION "This package contains the 'augeas' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-AUGEAS_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (augeas libelektra${SO_VERSION}-augeas PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-DBUS_DISPLAY_NAME "libelektra${SO_VERSION}-dbus")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-DBUS_DESCRIPTION "This package contains the 'dbus' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-DBUS_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (dbus libelektra${SO_VERSION}-dbus PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA-DEV_DISPLAY_NAME "libelektra-dev")
	set (CPACK_COMPONENT_LIBELEKTRA-DEV_DESCRIPTION "This package contains the development files for the main Elektra library.")
	set (CPACK_COMPONENT_LIBELEKTRA-DEV_DEPENDS "libelektra${SO_VERSION}")

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ZEROMQ_DISPLAY_NAME "libelektra${SO_VERSION}-zeromq")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ZEROMQ_DESCRIPTION "This package contains the 'zeromq' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ZEROMQ_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (zeromqrecv libelektra${SO_VERSION}-zeromq PLUGIN ADDITIONAL_DEPENDENCIES zeromqsend)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-FUSE_DISPLAY_NAME "libelektra${SO_VERSION}-fuse")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-FUSE_DESCRIPTION "This package contains the 'fuse' tool.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-FUSE_DEPENDS "libelektra${SO_VERSION}" "python3-elektra")
	check_component_dependencies (python libelektra${SO_VERSION}-fuse BINDING)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XMLTOOL_DISPLAY_NAME "libelektra${SO_VERSION}-xmltool")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XMLTOOL_DESCRIPTION "This package contains the 'xmltool' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XMLTOOL_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (xmltool libelektra${SO_VERSION}-xmltool PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XERCES_DISPLAY_NAME "libelektra${SO_VERSION}-xerces")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XERCES_DESCRIPTION "This package contains the 'xerces' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-XERCES_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (xerces libelektra${SO_VERSION}-xerces PLUGIN)

	set (CPACK_COMPONENT_XFCONF-ELEKTRA_DISPLAY_NAME "xfconf-elektra")
	set (CPACK_COMPONENT_XFCONF-ELEKTRA_DESCRIPTION "This package contains the Xfconf bindings.")
	set (CPACK_COMPONENT_XFCONF-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (xfconf xfconf-elektra BINDING)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAJL_DISPLAY_NAME "libelektra${SO_VERSION}-yajl")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAJL_DESCRIPTION "This package contains the 'yajl' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAJL_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (yajl libelektra${SO_VERSION}-yajl PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CRYPTO_DISPLAY_NAME "libelektra${SO_VERSION}-crypto")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CRYPTO_DESCRIPTION "This package contains the crypto plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CRYPTO_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (crypto libelektra${SO_VERSION}-crypto PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CURL_DISPLAY_NAME "libelektra${SO_VERSION}-curl")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CURL_DESCRIPTION "This package contains the 'curlget' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-CURL_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (curlget libelektra${SO_VERSION}-curl PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JOURNALD_DISPLAY_NAME "libelektra${SO_VERSION}-journald")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JOURNALD_DESCRIPTION "This package contains the 'journald' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JOURNALD_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (journald libelektra${SO_VERSION}-journald PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-GITRESOLVER_DISPLAY_NAME "libelektra${SO_VERSION}-gitresolver")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-GITRESOLVER_DESCRIPTION "This package contains the 'gitresolver' plugins.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-GITRESOLVER_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (gitresolver libelektra${SO_VERSION}-gitresolver PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAMLCPP_DISPLAY_NAME "libelektra${SO_VERSION}-yamlcpp")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAMLCPP_DESCRIPTION "This package contains the 'yamlcpp' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-YAMLCPP_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (yamlcpp libelektra${SO_VERSION}-yamlcpp PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JAVA_DISPLAY_NAME "libelektra${SO_VERSION}-java")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JAVA_DESCRIPTION "This package contains the 'jni' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-JAVA_DEPENDS "libelektra${SO_VERSION}" "java-elektra")
	check_component_dependencies (jni libelektra${SO_VERSION}-java PLUGIN)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-LUA_DISPLAY_NAME "libelektra${SO_VERSION}-lua")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-LUA_DESCRIPTION "This package contains the 'lua' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-LUA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (lua libelektra${SO_VERSION}-lua PLUGIN)

	set (CPACK_COMPONENT_JAVA-ELEKTRA_DISPLAY_NAME "java-elektra")
	set (CPACK_COMPONENT_JAVA-ELEKTRA_DESCRIPTION "This package contains the Java bindings.")
	set (CPACK_COMPONENT_JAVA-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (jna java-elektra BINDING)

	set (CPACK_COMPONENT_LUA-ELEKTRA_DISPLAY_NAME "lua-elektra")
	set (CPACK_COMPONENT_LUA-ELEKTRA_DESCRIPTION "This package contains the Lua bindings.")
	set (CPACK_COMPONENT_LUA-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (lua lua-elektra BINDING)

	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DISPLAY_NAME "python3-elektra")
	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DESCRIPTION "This package contains the Python 3 bindings.")
	set (CPACK_COMPONENT_PYTHON3-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (python python3-elektra BINDING)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-PYTHON_DISPLAY_NAME "libelektra${SO_VERSION}-python")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-PYTHON_DESCRIPTION "This package contains the 'python' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-PYTHON_DEPENDS "libelektra${SO_VERSION}" "python3-elektra")
	check_component_dependencies (python libelektra${SO_VERSION}-python PLUGIN)

	set (CPACK_COMPONENT_RUBY-ELEKTRA_DISPLAY_NAME "ruby-elektra")
	set (CPACK_COMPONENT_RUBY-ELEKTRA_DESCRIPTION "This package contains the Ruby bindings.")
	set (CPACK_COMPONENT_RUBY-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (ruby ruby-elektra BINDING)

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-RUBY_DISPLAY_NAME "libelektra${SO_VERSION}-ruby")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-RUBY_DESCRIPTION "This package contains the 'ruby' plugin.")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-RUBY_DEPENDS "libelektra${SO_VERSION}" "ruby-elektra")
	check_component_dependencies (ruby libelektra${SO_VERSION}-ruby PLUGIN)

	set (CPACK_COMPONENT_GLIB-ELEKTRA_DISPLAY_NAME "glib-elektra")
	set (CPACK_COMPONENT_GLIB-ELEKTRA_DESCRIPTION "This package contains the 'glib' binding.")
	set (CPACK_COMPONENT_GLIB-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (glib glib-elektra BINDING)

	set (CPACK_COMPONENT_IO-EV-ELEKTRA_DISPLAY_NAME "io-ev-elektra")
	set (CPACK_COMPONENT_IO-EV-ELEKTRA_DESCRIPTION "This package contains the 'io_ev' binding.")
	set (CPACK_COMPONENT_IO-EV-ELEKTRA-IO-EV_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (io_ev io-ev-elektra BINDING)

	set (CPACK_COMPONENT_IO-GLIB-ELEKTRA_DISPLAY_NAME "io-glib-elektra")
	set (CPACK_COMPONENT_IO-GLIB-ELEKTRA_DESCRIPTION "This package contains the 'io_glib' binding.")
	set (CPACK_COMPONENT_IO-GLIB-ELEKTRA_DEPENDS "libelektra${SO_VERSION}" "glib-elektra")
	check_component_dependencies (io_glib io-glib-elektra BINDING)

	set (CPACK_COMPONENT_IO-UV-ELEKTRA_DISPLAY_NAME "io-uv-elektra")
	set (CPACK_COMPONENT_IO-UV-ELEKTRA_DESCRIPTION "This package contains the 'io_uv' binding.")
	set (CPACK_COMPONENT_IO-UV-ELEKTRA_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (io_uv io-uv-elektra BINDING)

	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DISPLAY_NAME "elektra-bin-extra")
	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DESCRIPTION
	     "This package contains extra command line utilities for Elektra written in non-shell languages like python.")
	set (CPACK_COMPONENT_ELEKTRA-BIN-EXTRA_DEPENDS "libelektra${SO_VERSION}")

	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DISPLAY_NAME "elektra-qt-gui")
	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DESCRIPTION "This package contains a Qt-based graphical interface for Elektra.")
	set (CPACK_COMPONENT_ELEKTRA-QT-GUI_DEPENDS "libelektra${SO_VERSION}")
	check_component_dependencies (qt-gui elektra-qt-gui TOOL)

	set (CPACK_COMPONENT_ELEKTRA-TESTS_DISPLAY_NAME "elektra-tests")
	set (CPACK_COMPONENT_ELEKTRA-TESTS_DESCRIPTION "This package contains the Elektra test suite.")
	set (CPACK_COMPONENT_ELEKTRA-TESTS_DEPENDS "libelektra${SO_VERSION}-full" "elektra-bin")

	set (CPACK_COMPONENT_ELEKTRA-DOC_DISPLAY_NAME "elektra-doc")
	set (CPACK_COMPONENT_ELEKTRA-DOC_DESCRIPTION "This package contains the API documentation for the Elektra libraries.")

	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DISPLAY_NAME "libelektra${SO_VERSION}-all")
	set (CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DESCRIPTION "This package provides the dependencies for all Elektra packages.")
	set (
		CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DEPENDS
		"libelektra${SO_VERSION}"
		"libelektra${SO_VERSION}-experimental"
		"libelektra${SO_VERSION}-augeas"
		"libelektra${SO_VERSION}-dbus"
		"libelektra${SO_VERSION}-gitresolver"
		"libelektra${SO_VERSION}-zeromq"
		"libelektra${SO_VERSION}-fuse"
		"libelektra${SO_VERSION}-java"
		"libelektra${SO_VERSION}-lua"
		"libelektra${SO_VERSION}-python"
		"libelektra${SO_VERSION}-ruby"
		"libelektra${SO_VERSION}-xmltool"
		"libelektra${SO_VERSION}-xerces"
		"libelektra${SO_VERSION}-yajl"
		"libelektra${SO_VERSION}-yamlcpp"
		"io-ev-elektra"
		"io-glib-elektra"
		"io-uv-elektra"
		"lua-elektra"
		"xfconf-elektra"
		"elektra-bin"
		"elektra-qt-gui"
		"libelektra${SO_VERSION}-crypto"
		"libelektra${SO_VERSION}-curl"
		"libelektra${SO_VERSION}-journald"
		"libelektra${SO_VERSION}-extra")

	set (CPACK_COMPONENT_ELEKTRA-DBG_DISPLAY_NAME "elektra-dbg")
	set (CPACK_COMPONENT_ELEKTRA-DBG_DESCRIPTION "This package contains the dependencies to all dbgsym packages of Elektra.")

	set (CPACK_COMPONENT_ELEKTRA-MISC_DISPLAY_NAME "elektra-misc")
	set (CPACK_COMPONENT_ELEKTRA-MISC_DESCRIPTION "This package contains all files not part of any of the released Elektra packages.")

	# exclude components for package generation where dependencies are not fulfilled.
	foreach (component ${EXCLUDED_COMPONENTS})
		list (REMOVE_ITEM PACKAGES ${component})
	endforeach (component)

	# remove excluded components from libelektra${SO_VERSION}-all dependent components
	foreach (component ${CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DEPENDS})
		if (component IN_LIST EXCLUDED_COMPONENTS)
			list (REMOVE_ITEM CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DEPENDS ${component})
		endif ()
	endforeach (component)
	string (REPLACE ";" ", " LIBELEKTRA${SO_VERSION}-ALL_DEPENDS "${CPACK_COMPONENT_LIBELEKTRA${SO_VERSION}-ALL_DEPENDS}")

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

	elseif ("${OS_NAME}" MATCHES "Fedora|CentOS|openSUSE")
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

include (CPack)

foreach (component ${CPACK_COMPONENTS_ALL})
	if (NOT component IN_LIST PACKAGES)
		message (SEND_ERROR "Component ${component} is not
part of PACKAGES (ElektraPackaging.cmake). Please add it to this list, so the dependencies can be derived correctly.")
	endif ()
endforeach ()
