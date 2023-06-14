include (CheckSymbolExists)
check_symbol_exists (__GNU_LIBRARY__ "features.h" HAVE_GLIBC)

if (NOT BUILD_SHARED)
	exclude_binding (intercept_fs "intercept_fs only works with BUILD_SHARED")
elseif (NOT HAVE_GLIBC)
	exclude_binding (intercept_fs "intercept_fs currently needs glibc")
else ()
	add_binding (intercept_fs)

	include (LibAddMacros)
	set_source_files_properties (intercept.c PROPERTIES COMPILE_FLAGS "-Wno-unused-result")
	add_library (elektraintercept-fs SHARED "intercept.c")
	target_link_libraries (elektraintercept-fs elektra-kdb elektra-meta)
	install (
		TARGETS elektraintercept-fs
		DESTINATION lib${LIB_SUFFIX}
		COMPONENT libelektra${SO_VERSION})

	mkdir (${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
	create_lib_symlink (libelektraintercept-fs.so libelektraintercept.so libelektra${SO_VERSION})
endif ()
