add_executable (elektra-kdb-gen-collect-templates collect.cpp)

if (INSTALL_BUILD_TOOLS)
	install (TARGETS elektra-kdb-gen-collect-templates DESTINATION bin)
endif (INSTALL_BUILD_TOOLS)

file (GLOB_RECURSE TEMPLATE_FILES
		   LIST_DIRECTORIES
		   false
      RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	       *.mustache)

set_source_files_properties (elektra-export-errors OBJECT_DEPENDS "${TEMPLATE_FILES}")

find_util (elektra-kdb-gen-collect-templates EXE_ERR_LOC EXE_ERR_ARG)
set (BINARY_INCLUDE_DIR "${PROJECT_BINARY_DIR}/src/include")
mkdir ("${BINARY_INCLUDE_DIR}/gen")
add_custom_command (OUTPUT ${BINARY_INCLUDE_DIR}/gen/templates.hpp
		    DEPENDS elektra-kdb-gen-collect-templates
			    ${TEMPLATE_FILES}
		    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		    COMMAND ${EXE_ERR_LOC}
			    ARGS ${EXE_ERR_ARG}
				 ${BINARY_INCLUDE_DIR}/gen/templates.hpp
				 ${TEMPLATE_FILES})
add_custom_target (kdb_gen_templates_generated DEPENDS ${BINARY_INCLUDE_DIR}/gen/templates.hpp)
