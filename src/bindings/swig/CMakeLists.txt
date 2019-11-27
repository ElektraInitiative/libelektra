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

check_binding_included ("swig_python2" IS_INCLUDED SUBDIRECTORY "swig/python2")
if (IS_INCLUDED)
	add_subdirectory (python2)
endif ()

check_binding_included ("swig_python" IS_INCLUDED SUBDIRECTORY "swig/python")
if (IS_INCLUDED)
	add_subdirectory (python)
endif ()

check_binding_included ("swig_lua" IS_INCLUDED SUBDIRECTORY "swig/lua")
if (IS_INCLUDED)
	add_subdirectory (lua)
endif ()

check_binding_included ("swig_ruby" IS_INCLUDED SUBDIRECTORY "swig/ruby")
if (IS_INCLUDED)
	add_subdirectory (ruby)
endif ()
