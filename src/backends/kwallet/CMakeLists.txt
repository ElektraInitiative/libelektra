include(LibAddMacros)

find_package( KDE3 REQUIRED )

if (KDE3_FOUND)
# see http://techbase.kde.org/Development/Tutorials/CMake
	include_directories ( ${KDE3_INCLUDE_DIR} )
	include_directories ( ${QT_INCLUDE_DIR} )
	link_directories ( ${KDE3_LIB_DIR} )

	set (SOURCES kwallet.hpp kwallet.cpp)
	add_sources(elektra-full ${SOURCES})
	add_headers(SOURCES)

	add_library(elektra-kwallet MODULE ${SOURCES})
	install (TARGETS elektra-kwallet DESTINATION lib/elektra)
	target_link_libraries( elektra-kwallet ${QT_AND_KDECORE_LIBS} )
else (KDE3_FOUND)
	remove_backend (kwallet "kde3 not found")
endif (KDE3_FOUND)
