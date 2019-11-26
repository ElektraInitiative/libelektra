file (
	GLOB_RECURSE TEMPLATE_FILES
	LIST_DIRECTORIES false
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	*.mustache)

set (BINARY_INCLUDE_DIR "${PROJECT_BINARY_DIR}/src/include")
mkdir ("${BINARY_INCLUDE_DIR}/gen")
add_custom_command (
	OUTPUT ${BINARY_INCLUDE_DIR}/gen/templates.hpp
	DEPENDS collect.sh ${TEMPLATE_FILES}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/collect.sh" ARGS "${BINARY_INCLUDE_DIR}/gen/templates.hpp" ${TEMPLATE_FILES})
add_custom_target (kdb_gen_templates_generated DEPENDS ${BINARY_INCLUDE_DIR}/gen/templates.hpp)
