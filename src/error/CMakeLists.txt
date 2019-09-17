#add_executable (elektra-export-errors parser.hpp parser.cpp)
#
#if (INSTALL_BUILD_TOOLS)
#	install (TARGETS elektra-export-errors DESTINATION bin)
#endif (INSTALL_BUILD_TOOLS)
#
#set_source_files_properties (elektra-export-errors OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)
#
#find_util (elektra-export-errors EXE_ERR_LOC EXE_ERR_ARG)
#set (BINARY_INCLUDE_DIR "${PROJECT_BINARY_DIR}/src/include")
#
#add_custom_target (kdberrors_generated ALL DEPENDS ${BINARY_INCLUDE_DIR}/kdberrors.h)
#
#find_util (elektra-export-errors EXE_ERR_LOC EXE_ERR_ARG)
#file (MAKE_DIRECTORY ${BINARY_INCLUDE_DIR}/elektra)
#add_custom_command (OUTPUT ${BINARY_INCLUDE_DIR}/elektra/errors.h ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c
#		    DEPENDS elektra-export-errors ${CMAKE_CURRENT_SOURCE_DIR}/specification
#		    COMMAND ${EXE_ERR_LOC}
#			    ARGS
#			    ${EXE_ERR_ARG}
#			    highlevel
#			    ${CMAKE_CURRENT_SOURCE_DIR}/specification
#			    ${BINARY_INCLUDE_DIR}/elektra/errors.h
#			    ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c
#			    elektra/errors.h)
#add_custom_target (elektra_error_codes_generated ALL
#		   DEPENDS ${BINARY_INCLUDE_DIR}/elektra/errors.h ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c)
#
#add_executable (exporttranslations exporttranslations.cpp parser.hpp parser.cpp)
#
#set_source_files_properties (exporttranslations OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)
