# This functions checks if the dependencies for the YAy PEG plugin are available.
#
# If they are, the function sets the variable `FOUND_DEPENDENCIES` to `TRUE`. The function then also sets:
#
# - `PEGTL_VERSION` to the version number of the detected version of PEGTL, and
# - `PEGTL_INCLUDE_DIRS` to the header directory of PEGTL
#
# . If the function was unsuccessful it sets `FOUND_DEPENDENCIES` to `FALSE` and stores the reason for the failure in the variable
# `FAILURE_MESSAGE`.
function (check_dependencies)
	set (FOUND_DEPENDENCIES FALSE PARENT_SCOPE)

	find_package (PEGTL 2 QUIET)

	if (NOT PEGTL_FOUND)
		set (FAILURE_MESSAGE "PEGTL (tao-pegtl 2) not found" PARENT_SCOPE)
		return ()
	endif (NOT PEGTL_FOUND)

	if (PEGTL_VERSION VERSION_LESS 2.7.1)
		set (FAILURE_MESSAGE "PEGTL (tao-pegtl >= 2.7.1) not found" PARENT_SCOPE)
		return ()
	endif (PEGTL_VERSION VERSION_LESS 2.7.1)

	if (NOT (PEGTL_VERSION VERSION_LESS 2.9))
		set (FAILURE_MESSAGE "PEGTL (tao-pegtl < 2.9) not found" PARENT_SCOPE)
		return ()
	endif (NOT (PEGTL_VERSION VERSION_LESS 2.9))

	set (PEGTL_INCLUDE_DIRS "${PEGTL_INCLUDE_DIRS}" PARENT_SCOPE)
	set (PEGTL_VERSION "${PEGTL_VERSION}" PARENT_SCOPE)
	set (FOUND_DEPENDENCIES TRUE PARENT_SCOPE)
endfunction (check_dependencies)

if (DEPENDENCY_PHASE)
	check_dependencies ()
	if (NOT FOUND_DEPENDENCIES)
		remove_plugin (yaypeg "${FAILURE_MESSAGE}")
	else (NOT FOUND_DEPENDENCIES)
		if (PEGTL_VERSION VERSION_LESS 2.8)
			set (STORE_CONTENT "apply_store_content::to")
			set (REMOVE_CONTENT "apply_remove_content::to")
		else (PEGTL_VERSION VERSION_LESS 2.8)
			set (STORE_CONTENT "store_content::on")
			set (REMOVE_CONTENT "remove_content::on")
		endif (PEGTL_VERSION VERSION_LESS 2.8)

		configure_file (parser.in.hpp parser.hpp @ONLY)

		set (SOURCE_FILES
		     parser.hpp
		     state.hpp
		     state.cpp
		     listener.hpp
		     listener.cpp
		     walk.hpp
		     walk.cpp
		     convert.hpp
		     convert.cpp
		     yaypeg.hpp
		     yaypeg.cpp)
	endif (NOT FOUND_DEPENDENCIES)
endif (DEPENDENCY_PHASE)

add_plugin (yaypeg
	    CPP
	    ADD_TEST
	    CPP_TEST
	    INSTALL_TEST_DATA
	    TEST_README
	    TEST_REQUIRED_PLUGINS directoryvalue sync yamlsmith
	    INCLUDE_SYSTEM_DIRECTORIES ${PEGTL_INCLUDE_DIRS}
	    INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}
	    SOURCES ${SOURCE_FILES})
