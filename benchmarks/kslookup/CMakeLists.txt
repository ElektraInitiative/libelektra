#TODO KURT build all with one target
include (LibAddMacros)

add_headers (HDR_FILES)

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
