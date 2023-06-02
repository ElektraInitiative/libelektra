find_package (Threads QUIET)

include (LibAddTest)

# This function checks information about the currently selected version of Xcode.
#
# The function exports the variables described below.
#
# - XCODE_INSTALLED: This variable specifies, if the current system uses Xcode or not.
# - XCODE_VERSION: This variable stores the version string of the selected Xcode version. If Xcode is not installed, then it stores the
#   value `0`.
#
function (check_xcode)
	set (
		XCODE_INSTALLED
		OFF
		PARENT_SCOPE)
	set (
		XCODE_VERSION
		0
		PARENT_SCOPE)

	if (NOT APPLE)
		return ()
	endif (NOT APPLE)

	execute_process (
		COMMAND xcode-select --print-path
		RESULT_VARIABLE XCODE_SELECT_UNSUCCESSFULL
		OUTPUT_VARIABLE XCODE_PATH
		OUTPUT_STRIP_TRAILING_WHITESPACE)

	if (XCODE_SELECT_UNSUCCESSFULL)
		return ()
	endif (XCODE_SELECT_UNSUCCESSFULL)

	execute_process (
		COMMAND "${XCODE_PATH}/usr/bin/xcodebuild" -version
		RESULT_VARIABLE XCODE_BUILD_UNSUCCESSFULL
		OUTPUT_VARIABLE XCODE_VERSION_STRING
		OUTPUT_STRIP_TRAILING_WHITESPACE)

	if (XCODE_BUILD_UNSUCCESSFULL)
		return ()
	endif (XCODE_BUILD_UNSUCCESSFULL)

	string (REGEX REPLACE ".*Xcode[ ]*([0-9]+(\\.[0-9]+)?(\\.[0-9]+)?).*" "\\1" XCODE_VERSION_STRING ${XCODE_VERSION_STRING})

	set (
		XCODE_INSTALLED
		ON
		PARENT_SCOPE)
	set (
		XCODE_VERSION
		"${XCODE_VERSION_STRING}"
		PARENT_SCOPE)
endfunction (check_xcode)

function (add_kdb_test suffix)
	set (multiValueArgs "REQUIRED_PLUGINS;LINK_ELEKTRA")
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	set (name testkdb_${suffix})
	add_gtest (${name} KDBTESTS LINK_TOOLS LINK_ELEKTRA ${ARG_LINK_ELEKTRA})
endfunction (add_kdb_test suffix)

if (CMAKE_SYSTEM_NAME MATCHES FreeBSD)
	add_definitions (-DASAN_NO_LEAK_SANITIZER_SUPPORT)
endif (CMAKE_SYSTEM_NAME MATCHES FreeBSD)

find_file (FEDORA fedora-release PATHS /etc)
if (FEDORA AND ENABLE_ASAN)
	# The plugin test fails most of the time on Fedora if we enable the address sanitizer
	#
	# See also:
	#
	# - https://build.libelektra.org/blue/organizations/jenkins/libelektra/detail/master/294/pipeline/646 -
	# - https://issues.libelektra.org/3572
	message (STATUS "Exclude KDB plugin test because it fails when ASAN is enabled")
else (FEDORA AND ENABLE_ASAN)
	add_kdb_test (allplugins)
endif (FEDORA AND ENABLE_ASAN)

add_kdb_test (conflict REQUIRED_PLUGINS error)
add_kdb_test (error REQUIRED_PLUGINS error list spec)
add_kdb_test (nested REQUIRED_PLUGINS error)
add_kdb_test (simple REQUIRED_PLUGINS error)
add_kdb_test (contracts REQUIRED_PLUGINS error list gopts)

check_xcode ()
if ("${XCODE_VERSION}" VERSION_EQUAL 10.1)
	# See: https://issues.libelektra.org/2686
	message (STATUS "Exclude KDB high-level test because it fails with a segmentation fault (Xcode ${XCODE_VERSION})")
else ("${XCODE_VERSION}" VERSION_EQUAL 10.1)
	set_source_files_properties (testkdb_highlevel PROPERTIES COMPILE_FLAGS -Wno-sign-promo)
	add_kdb_test (highlevel LINK_ELEKTRA elektra-highlevel REQUIRED_PLUGINS error)
endif ("${XCODE_VERSION}" VERSION_EQUAL 10.1)
