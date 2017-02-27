
if (DEPENDENCY_PHASE)
	find_package(XercesC)

	if (NOT XercesC_FOUND)
		remove_plugin (xerces "XercesC library not found")
	else (NOT XercesC_FOUND)
		# this has to be done here and not in ADDTESTING_PHASE
		# type plugin does it the same way
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
	INSTALL_TEST_DATA
)

if (ADDTESTING_PHASE)
	include (LibAddTest)
	add_plugintest (xerces
		MEMLEAK
		INSTALL_TEST_DATA
	)
endif (ADDTESTING_PHASE)