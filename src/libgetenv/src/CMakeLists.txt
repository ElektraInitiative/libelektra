include (LibAddMacros)

add_headers(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektragetenv SHARED ${SOURCES})

target_link_libraries (elektragetenv elektra ${CMAKE_DL_LIBS})

install (TARGETS elektragetenv DESTINATION lib${LIB_SUFFIX})
