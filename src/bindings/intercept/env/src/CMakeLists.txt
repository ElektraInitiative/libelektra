include (LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.cpp)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektraintercept-env SHARED ${SOURCES})

target_link_libraries (elektraintercept-env ${CMAKE_DL_LIBS})
target_link_libraries (elektraintercept-env elektra-kdb elektra-meta) # must be shared!
target_include_directories (elektraintercept-env PRIVATE "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")

set_target_properties (
	elektraintercept-env
	PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H"
		   VERSION ${KDB_VERSION}
		   SOVERSION ${SO_VERSION_GETENV}
		   CLEAN_DIRECT_OUTPUT 1
		   LINKER_LANGUAGE CXX)

install (
	TARGETS elektraintercept-env
	DESTINATION lib${LIB_SUFFIX}
	COMPONENT libelektra${SO_VERSION})

mkdir (${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
create_lib_symlink (libelektraintercept-env.so libelektragetenv.so libelektra${SO_VERSION})
create_lib_symlink (libelektraintercept-env.so libelektragetenv.so.0 libelektra${SO_VERSION})
