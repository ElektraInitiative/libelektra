include(LibAddMacros)

find_package( KDE4 REQUIRED )

if (KDE4_FOUND)
# see http://techbase.kde.org/Development/Tutorials/CMake

	set (SOURCES kwallet.hpp kwallet.cpp)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	include_directories(${KDE4_INCLUDES})

	add_library (elektra-kwallet MODULE ${SOURCES})
	set_target_properties (elektra-kwallet PROPERTIES ENABLE_EXPORTS 1)
	get_target_property (VAR elektra-kwallet ENABLE_EXPORTS)
	message (${VAR})
	target_link_libraries (elektra-kwallet elektra)
	target_link_libraries (elektra-kwallet ${KDE4_KDEUI_LIBS} ${KDE4_KPARTS_LIBS})
	install (TARGETS elektra-kwallet EXPORT kdbBackendFactory DESTINATION lib/elektra)
else (KDE4_FOUND)
	remove_backend (kwallet "kde not found")
endif (KDE4_FOUND)
