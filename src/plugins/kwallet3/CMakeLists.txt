include(LibAddMacros)

if (IS_DIRECTORY /usr/include/qt3 AND IS_DIRECTORY /usr/include/kde)

	set (INCLUDES /usr/include/qt3 /usr/include/kde)
	set (LIBRARIES /usr/lib${LIB_SUFFIX}/libkwalletclient.so.1 /usr/lib/libqt-mt.so.3 pthread)

	add_includes (elektra-full ${INCLUDES})
	include_directories(${INCLUDES})

	set (SOURCES kwallet.hpp kwallet.cpp)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-kwallet MODULE ${SOURCES})
	install (TARGETS elektra-kwallet EXPORT kdbBackendFactory DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	target_link_libraries (elektra-kwallet ${LIBRARIES})
	add_libraries(elektra-full ${LIBRARIES})

else()
	remove_plugin (kwallet3 "kde3 not found")
endif ()


#find_package( KDE4 REQUIRED )

#if (KDE4_FOUND)
# see http://techbase.kde.org/Development/Tutorials/CMake

#else (KDE4_FOUND)
#	remove_plugin(kwallet "kde not found")
#endif (KDE4_FOUND)
