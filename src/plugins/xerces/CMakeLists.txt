
if (DEPENDENCY_PHASE)
	find_package(XercesC)

	if (NOT XercesC_FOUND)
		remove_plugin (xerces "XercesC library not found")
	endif (NOT XercesC_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (xerces
	CPP
	SOURCES
		xerces.hpp xerces.cpp
		deserializer.hpp deserializer.cpp
	INCLUDE_DIRECTORIES
		${XercesC_INCLUDE_DIRS}
	LINK_LIBRARIES
		${XercesC_LIBRARIES}
)

if (ADDTESTING_PHASE)
	include (LibAddTest)
	add_plugintest (xerces
		MEMLEAK
		INSTALL_TEST_DATA
	)
endif (ADDTESTING_PHASE)