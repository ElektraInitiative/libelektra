include (LibAddMacros)

find_package(PkgConfig REQUIRED)

pkg_check_modules(XFCE REQUIRED
	libxfconf-0)

include_directories(${XFCE_INCLUDE_DIRS})

add_plugin (
	xfconf
	SOURCES xfconf.h xfconf.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
