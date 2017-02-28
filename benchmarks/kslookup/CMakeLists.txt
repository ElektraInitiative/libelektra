include (LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")

# gendata
set (SOURCES ${HDR_FILES} benchmark.h benchmark.c gendata.h gendata.c)
add_executable (benchmark_kslookup_gendata ${SOURCES})
target_link_elektra(benchmark_kslookup_gendata)
set_target_properties (benchmark_kslookup_gendata PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

# hsearch and kslookup
macro (benchmark_kslookup_c source)
set (SOURCES ${HDR_FILES} benchmark.h benchmark.c ${source}.c)
add_executable (benchmark_kslookup_${source} ${SOURCES})
target_link_elektra(benchmark_kslookup_${source})
set_target_properties (benchmark_kslookup_${source} PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (benchmark_kslookup_c)

benchmark_kslookup_c (hsearch)
benchmark_kslookup_c (kslookup)

# unorderedmap

set (SOURCES ${HDR_FILES} benchmark.h benchmark.c unorderedmap_Interface.hpp
		unorderedmap_Search.hpp unorderedmap_Build.hpp unorderedmap_Search.cpp
		unorderedmap_Build.cpp unorderedmap.cpp)
add_executable (benchmark_kslookup_unorderedmap ${SOURCES})
target_link_elektra(benchmark_kslookup_unorderedmap)
target_link_elektratools(benchmark_kslookup_unorderedmap)
set_target_properties (benchmark_kslookup_unorderedmap PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)


add_custom_target (benchmark_kslookup)
add_dependencies (benchmark_kslookup benchmark_kslookup_gendata benchmark_kslookup_hsearch benchmark_kslookup_kslookup benchmark_kslookup_unorderedmap)
