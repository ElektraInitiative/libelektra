include (LibAddMacros)

file (GLOB HDR_FILES
	   *.hpp
	   gen/*.hpp)
add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

file (GLOB SRC_FILES
	   *.cpp
	   gen/*.cpp)
set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_subdirectory (gen/templates)

if (BUILD_SHARED OR BUILD_FULL)
	add_library (kdb-objects OBJECT ${SRC_FILES})
	add_dependencies (kdb-objects elektra_error_codes_generated kdb_gen_templates_generated)
endif (BUILD_SHARED OR BUILD_FULL)

if (BUILD_SHARED)
	add_executable (kdb $<TARGET_OBJECTS:kdb-objects>)
	add_dependencies (kdb kdberrors_generated elektra_error_codes_generated kdb_gen_templates_generated)

	target_link_libraries (kdb elektra-core elektra-kdb elektratools)

	install (TARGETS kdb DESTINATION bin)
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_executable (kdb-full $<TARGET_OBJECTS:kdb-objects>)
	add_dependencies (kdb-full kdberrors_generated elektra_error_codes_generated kdb_gen_templates_generated)

	target_link_libraries (kdb-full elektra-full elektratools-full)

	install (TARGETS kdb-full DESTINATION bin)
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_executable (kdb-static ${SOURCES})
	add_dependencies (kdb-static kdberrors_generated elektra_error_codes_generated kdb_gen_templates_generated)

	set_target_properties (kdb-static
			       PROPERTIES LINKER_LANGUAGE
					  CXX)
	set_target_properties (kdb-static
			       PROPERTIES COMPILE_DEFINITIONS
					  "HAVE_KDBCONFIG_H;ELEKTRA_STATIC")

	target_link_libraries (kdb-static elektra-static elektratools-static)

	# TODO: add helper libraries of plugins, too

	if (CMAKE_STATIC_FLAGS)
		set_target_properties (kdb-static
				       PROPERTIES LINK_FLAGS
						  ${CMAKE_STATIC_FLAGS})
	endif ()

	install (TARGETS kdb-static DESTINATION bin)
endif (BUILD_STATIC)
