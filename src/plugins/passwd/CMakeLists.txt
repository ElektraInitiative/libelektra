include (LibAddMacros)

try_compile(HAS_FGETPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasfgetpwent.c")
try_compile(HAS_PUTPWENT "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/passwd/hasputpwent.c")

add_plugin (passwd
	SOURCES
		passwd.h
		passwd.c
	COMPILE_DEFINITIONS
		HAS_FGETPWENT;HAS_PUTPWENT
	ADD_TEST
	INSTALL_TEST_DATA
	)
