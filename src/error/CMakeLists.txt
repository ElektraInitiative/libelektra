add_executable(elektra-export-errors exporterrors.cpp parser.hpp parser.cpp)

if (INSTALL_BUILD_TOOLS)
	install (TARGETS elektra-export-errors DESTINATION bin)
endif (INSTALL_BUILD_TOOLS)

set_source_files_properties(elektra-export-errors OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)

add_executable(exporttranslations exporttranslations.cpp parser.hpp parser.cpp)

set_source_files_properties(exporttranslations OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)
