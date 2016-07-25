include (LibAddMacros)

add_headers(HDR_FILES)
add_cppheaders(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.cpp)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektragetenv SHARED ${SOURCES})
add_dependencies (elektragetenv kdberrors_generated)

target_link_libraries (elektragetenv ${CMAKE_DL_LIBS})
target_link_libraries (elektragetenv elektra-kdb elektra-meta) # must be shared!

set_target_properties (elektragetenv PROPERTIES
	COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H"
	VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION_GETENV}
	CLEAN_DIRECT_OUTPUT 1
	LINKER_LANGUAGE CXX
)

install (TARGETS elektragetenv DESTINATION lib${LIB_SUFFIX})
