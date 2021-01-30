if (DEPENDENCY_PHASE)
	find_package (Iconv QUIET)

	if (ENABLE_ASAN AND CMAKE_SYSTEM_NAME MATCHES FreeBSD)
		# See also: https://cirrus-ci.com/task/5751822404288512?command=tests#L253-L255
		remove_plugin (iconv "the unit test of the plugin fails on FreeBSD if ASan is active")
	endif (ENABLE_ASAN AND CMAKE_SYSTEM_NAME MATCHES FreeBSD)

	if (NOT Iconv_FOUND)
		remove_plugin (iconv "Cannot find iconv library")
	endif ()
endif ()

add_plugin (
	iconv
	SOURCES conv.h iconv.c
	INCLUDE_DIRECTORIES ${Iconv_INCLUDE_DIRS}
	LINK_LIBRARIES ${Iconv_LIBRARIES}
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-extra)
