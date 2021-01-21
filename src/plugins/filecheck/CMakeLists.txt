include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (Iconv QUIET)

	if (NOT Iconv_FOUND)
		remove_plugin (filecheck "Cannot find iconv library")
	endif ()

	if (ENABLE_ASAN AND CMAKE_SYSTEM_NAME MATCHES FreeBSD)
		# See also: https://cirrus-ci.com/task/5751822404288512?command=tests#L237-L239
		remove_plugin (filecheck "the unit test of the plugin fails on FreeBSD if ASan is active")
	endif (ENABLE_ASAN AND CMAKE_SYSTEM_NAME MATCHES FreeBSD)
endif ()

add_plugin (
	filecheck
	SOURCES filecheck.h filecheck.c
	INCLUDE_DIRECTORIES ${Iconv_INCLUDE_DIRS}
	LINK_LIBRARIES ${Iconv_LIBRARIES}
	ADD_TEST INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION}-extra)
