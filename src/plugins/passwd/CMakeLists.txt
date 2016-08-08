include (LibAddMacros)
try_compile(HAS_FGETPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasfgetpwent.c")
if(HAS_FGETPWENT)
    add_definitions(-DHAS_FGETPWENT)
endif()
try_compile(HAS_PUTPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasputpwent.c")
if(HAS_PUTPWENT)
    add_definitions(-DHAS_PUTPWENT)
endif()
add_plugin (passwd
    SOURCES
	passwd.h
	passwd.c
    ADD_TEST
    INSTALL_TEST_DATA
    )
