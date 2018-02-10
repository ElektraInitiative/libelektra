# This script is a modified version of the code available here:
#	https://github.com/antlr/antlr4/blob/master/runtime/Cpp/cmake/ExternalAntlr4Cpp.cmake

include (ExternalProject)

find_package (Git REQUIRED)
find_package (Java COMPONENTS Runtime REQUIRED)

set (ANTLR4CPP_EXTERNAL_ROOT ${CMAKE_BINARY_DIR}/externals/antlr4cpp)
set (ANTLR4CPP_EXTERNAL_REPO "https://github.com/antlr/antlr4.git")
set (ANTLR4CPP_GENERATED_SRC_DIR ${CMAKE_BINARY_DIR}/antlr4cpp_generated_src)

if (NOT EXISTS "${ANTLR4CPP_JAR_LOCATION}")
	message(FATAL_ERROR "Unable to find antlr tool. ANTLR4CPP_JAR_LOCATION: “${ANTLR4CPP_JAR_LOCATION}”")
endif (NOT EXISTS "${ANTLR4CPP_JAR_LOCATION}")

ExternalProject_Add(
	antlr4cpp
	PREFIX            ${ANTLR4CPP_EXTERNAL_ROOT}
	GIT_REPOSITORY    ${ANTLR4CPP_EXTERNAL_REPO}
	TIMEOUT           10
	UPDATE_COMMAND    ${GIT_EXECUTABLE} pull -q
	CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -BUILD_TESTS=OFF
					   -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR> <SOURCE_DIR>/runtime/Cpp
					   -DCMAKE_INSTALL_MESSAGE=NEVER # Do not show install messages
)

ExternalProject_Get_Property (antlr4cpp INSTALL_DIR)

list (APPEND ANTLR4CPP_INCLUDE_DIRS ${INSTALL_DIR}/include/antlr4-runtime)
foreach (src_path misc atn dfa tree support)
	list(APPEND ANTLR4CPP_INCLUDE_DIRS ${INSTALL_DIR}/include/antlr4-runtime/${src_path})
endforeach (src_path)

set (ANTLR4CPP_LIBS "${INSTALL_DIR}/lib")

# Macro to add dependencies to target
#
# Parameter 1 project name
# Parameter 2 namespace (postfix for dependencies)
# Parameter 3 grammar file (full path)
#
# Output
#
# antlr4cpp_src_files_{namespace} - src files for add_executable
# antlr4cpp_include_dirs_{namespace} - include dir for generated headers
# antlr4cpp_generation_{namespace} - for add_dependencies tracking
macro (antlr4cpp_process_grammar
       antlr4cpp_project
       antlr4cpp_project_namespace
       antlr4cpp_grammar)

	if (NOT EXISTS "${ANTLR4CPP_JAR_LOCATION}")
		message(FATAL_ERROR "Unable to find antlr tool. ANTLR4CPP_JAR_LOCATION: “${ANTLR4CPP_JAR_LOCATION}”")
	endif (EXISTS "${ANTLR4CPP_JAR_LOCATION}")

	add_custom_target ("antlr4cpp_generation_${antlr4cpp_project_namespace}"
		COMMAND ${CMAKE_COMMAND} -E make_directory ${ANTLR4CPP_GENERATED_SRC_DIR}
		COMMAND "${Java_JAVA_EXECUTABLE}" -jar "${ANTLR4CPP_JAR_LOCATION}" -Werror -Dlanguage=Cpp -listener -visitor
			-o "${ANTLR4CPP_GENERATED_SRC_DIR}/${antlr4cpp_project_namespace}" -package ${antlr4cpp_project_namespace}
			"${antlr4cpp_grammar}"
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
		DEPENDS "${antlr4cpp_grammar}"
	)

	file (GLOB generated_files ${ANTLR4CPP_GENERATED_SRC_DIR}/${antlr4cpp_project_namespace}/*.cpp)

	foreach (generated_file ${generated_files})
		list (APPEND antlr4cpp_src_files_${antlr4cpp_project_namespace} ${generated_file})
	endforeach(generated_file)
	message(STATUS "Antlr4Cpp  ${antlr4cpp_project_namespace} Generated: ${generated_files}")

	set(antlr4cpp_include_dirs_${antlr4cpp_project_namespace} ${ANTLR4CPP_GENERATED_SRC_DIR}/${antlr4cpp_project_namespace})
	message(STATUS "Antlr4Cpp ${antlr4cpp_project_namespace} include: ${ANTLR4CPP_GENERATED_SRC_DIR}/${antlr4cpp_project_namespace}")

endmacro (antlr4cpp_process_grammar)
