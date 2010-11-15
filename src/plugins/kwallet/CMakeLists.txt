include(LibAddMacros)

set (SOURCES kwallet.hpp kwallet.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-kwallet MODULE ${SOURCES})
target_link_libraries (elektra-kwallet /usr/lib/libkwalletclient.so.1)
install (TARGETS elektra-kwallet EXPORT kdbBackendFactory DESTINATION lib/elektra)


#find_package( KDE4 REQUIRED )

#if (KDE4_FOUND)
# see http://techbase.kde.org/Development/Tutorials/CMake

#else (KDE4_FOUND)
#	remove_plugin(kwallet "kde not found")
#endif (KDE4_FOUND)
