add_executable(elektra-export-errors exporterrors.cpp parser.hpp parser.cpp)

if (INSTALL_BUILD_TOOLS)
	install (TARGETS elektra-export-errors DESTINATION bin)
endif (INSTALL_BUILD_TOOLS)

set_source_files_properties(elektra-export-errors OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)

find_util (elektra-export-errors EXE_ERR_LOC EXE_ERR_ARG)
set (BINARY_INCLUDE_DIR "${PROJECT_BINARY_DIR}/src/include")
add_custom_command (
		OUTPUT ${BINARY_INCLUDE_DIR}/kdberrors.h
		DEPENDS elektra-export-errors
		COMMAND ${EXE_ERR_LOC}
		ARGS ${EXE_ERR_ARG} ${CMAKE_SOURCE_DIR}/src/error/specification ${BINARY_INCLUDE_DIR}/kdberrors.h
		)
add_custom_target (
    kdberrors_generated ALL
    DEPENDS ${BINARY_INCLUDE_DIR}/kdberrors.h
    )

add_executable(exporttranslations exporttranslations.cpp parser.hpp parser.cpp)

set_source_files_properties(exporttranslations OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)
