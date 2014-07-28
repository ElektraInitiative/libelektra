add_subdirectory (cpp)

if (BUILD_SWIG)
	find_package(SWIG 2 REQUIRED)
endif (BUILD_SWIG)

if (BUILD_SWIG_LUA)
	set (TARGET_LUA_CMOD_FOLDER "lib${LIB_SUFFIX}/lua/5.2"
		CACHE PATH
		"Directory to install Lua binary modules (configure lua via LUA_CPATH)"
	)
endif (BUILD_SWIG_LUA)

if (SWIG_FOUND)
	add_subdirectory (swig)
endif (SWIG_FOUND)
