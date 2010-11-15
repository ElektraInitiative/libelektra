include(LibAddMacros)

set (INCLUDES /usr/include/qt3 /usr/include/kde)
set (LIBRARIES /usr/lib/libkwalletclient.so.1 /usr/lib/libqt-mt.so.3)

add_includes (elektra-full ${INCLUDES})
include_directories(${INCLUDES})

set (SOURCES kwallet.hpp kwallet.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-kwallet MODULE ${SOURCES})
install (TARGETS elektra-kwallet EXPORT kdbBackendFactory DESTINATION lib/elektra)

target_link_libraries (elektra-kwallet ${LIBRARIES})
add_libraries(elektra-full ${LIBRARIES})



#find_package( KDE4 REQUIRED )

#if (KDE4_FOUND)
# see http://techbase.kde.org/Development/Tutorials/CMake

#else (KDE4_FOUND)
#	remove_plugin(kwallet "kde not found")
#endif (KDE4_FOUND)
