include (LibAddPlugin)

add_plugin (
	fcrypt
	SOURCES ../crypto/gpg.h ../crypto/gpg.c ../crypto/gpg_shutdown.h ../crypto/gpg_shutdown.c fcrypt.h fcrypt.c
	INCLUDE_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/plugins/crypto/
	ADD_TEST)
