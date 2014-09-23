add_plugin(type
	CPP
	SOURCES
		type.hpp type.cpp
		types.hpp types.cpp
		type_checker.hpp
	)

add_cpp_plugintest(type MEMLEAK)
