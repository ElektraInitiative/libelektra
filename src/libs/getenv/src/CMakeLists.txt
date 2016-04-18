include (LibAddMacros)

add_headers(HDR_FILES)
add_cppheaders(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.cpp)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektragetenv SHARED ${SOURCES})

target_link_libraries (elektragetenv ${CMAKE_DL_LIBS})
target_link_libraries (elektragetenv elektra-kdb elektra-meta) # must be shared!

install (TARGETS elektragetenv DESTINATION lib${LIB_SUFFIX})
