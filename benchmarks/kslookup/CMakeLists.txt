add_headers (HDR_FILES)


include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")

# gendata
set (SOURCES ${HDR_FILES} benchmark.h benchmark.c gendata.h gendata.c)
add_executable (benchmark_kslookup_gendata ${SOURCES})
target_link_elektra(benchmark_kslookup_gendata)
set_target_properties (benchmark_kslookup_gendata PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

# hsearch
set (SOURCES ${HDR_FILES} benchmark.h benchmark.c hsearch.c)
add_executable (benchmark_kslookup_hsearch ${SOURCES})
target_link_elektra(benchmark_kslookup_hsearch)
set_target_properties (benchmark_kslookup_hsearch PROPERTIES
		COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
