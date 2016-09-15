include (LibAddMacros)

if (DEPENDENCY_PHASE)
	try_compile(TEST_FGETPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasfgetpwent.c")
	try_compile(TEST_PUTPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasputpwent.c")

endif ()

if(TEST_PUTPWENT)
	set(PUTPWENT 1)
else()
	set(PUTPWENT 0)
endif()
if(TEST_FGETPWENT)
	set(FGETPWENT 1)
else ()
	set(FGETPWENT 0)
endif()
add_plugin (passwd
	SOURCES
		passwd.h
		passwd.c
	COMPILE_DEFINITIONS
		HAS_PUTPWENT=${PUTPWENT};HAS_FGETPWENT=${FGETPWENT}
	ADD_TEST
	INSTALL_TEST_DATA
	)
