include (LibAddPlugin)

add_plugin (
	fcrypt
	SOURCES ../crypto/gpg.h ../crypto/gpg.c fcrypt.h fcrypt.c
	INCLUDE_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/plugins/crypto/
	ADD_TEST COMPONENT libelektra${SO_VERSION}-experimental)

if (ADDTESTING_PHASE)
	set_property (TEST "testmod_fcrypt" PROPERTY LABELS memleak)
endif (ADDTESTING_PHASE)
