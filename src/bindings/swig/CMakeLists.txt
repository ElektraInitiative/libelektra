set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")

list (FIND BINDINGS "swig_python2" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (python2)
endif ()

list (FIND BINDINGS "swig_python" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (python)
endif ()

list (FIND BINDINGS "swig_lua" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (lua)
endif ()
