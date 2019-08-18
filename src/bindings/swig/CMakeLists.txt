if (CMAKE_VERSION VERSION_GREATER 3.12)
	cmake_policy (PUSH)
	cmake_policy (SET CMP0078 OLD)
endif (CMAKE_VERSION VERSION_GREATER 3.12)

set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")

find_swig ()
if (NOT SWIG_FOUND)
	exclude_binding (swig "neither swig2/3 found. Please install swig3.0 and set -DSWIG_EXECUTABLE=")
	return ()
endif ()

if (ENABLE_ASAN)
	exclude_binding (swig "SWIG is not compatible with ENABLE_ASAN")
	return ()
endif (ENABLE_ASAN)

check_binding_included ("swig_python2"
			IS_INCLUDED
			SUBDIRECTORY
			"swig/python2")
if (IS_INCLUDED)
	add_subdirectory (python2)
endif ()

check_binding_included ("swig_python"
			IS_INCLUDED
			SUBDIRECTORY
			"swig/python")
if (IS_INCLUDED)
	add_subdirectory (python)
else ()

	# TODO rename swig_python3 to swig_python
	check_binding_included ("swig_python3"
				IS_INCLUDED
				SUBDIRECTORY
				"swig/python"
				SILENT)

	# remove sometime in the future
	if (IS_INCLUDED)
		exclude_binding ("swig_python3" "swig_python3 has been renamed to swig_python")
		add_subdirectory (python)
	endif ()
endif ()

check_binding_included ("swig_lua"
			IS_INCLUDED
			SUBDIRECTORY
			"swig/lua")
if (IS_INCLUDED)
	add_subdirectory (lua)
endif ()

check_binding_included ("swig_ruby"
			IS_INCLUDED
			SUBDIRECTORY
			"swig/ruby")
if (IS_INCLUDED)
	add_subdirectory (ruby)
endif ()

if (CMAKE_VERSION VERSION_GREATER 3.12)
	cmake_policy (POP)
endif (CMAKE_VERSION VERSION_GREATER 3.12)
