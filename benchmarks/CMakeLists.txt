include (LibAddMacros)

add_headers (HDR_FILES)

macro (do_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${ADDITIONAL_SOURCES} benchmarks.c benchmarks.h ${source}.c)
	add_executable (benchmark_${source} ${SOURCES})

	target_link_elektra (benchmark_${source} elektra-kdb elektra-meta)

	set_target_properties (benchmark_${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

	# ~~~
	# Useful for debugging: COMMAND strace -o strace.out "${command}"
	# ~~~
	add_custom_target (
		benchmark_${source}_callgrind
		COMMAND rm -f *.out
		COMMAND valgrind --tool=callgrind --callgrind-out-file=callgrind.out "$<TARGET_FILE:benchmark_${source}>" > output.out
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endmacro (do_benchmark)

do_benchmark (large)
do_benchmark (cmp)
do_benchmark (createkeys)
do_benchmark (memoryleak)
do_benchmark (deepdup)
do_benchmark (diff)
do_benchmark (diffappend)
do_benchmark (changetracking)
do_benchmark (mountpoint)

# exclude storage and KDB benchmark from mingw
if (NOT WIN32)
	include_directories ("${CMAKE_SOURCE_DIR}/tests/cframework")
	set (ADDITIONAL_SOURCES $<TARGET_OBJECTS:cframework>)
	do_benchmark (storage)
	do_benchmark (kdb)
	do_benchmark (kdbget)
	do_benchmark (kdbmodify)
endif (NOT WIN32)

# exclude the OPMPHM benchmarks from mingw
if (ENABLE_OPTIMIZATIONS AND NOT WIN32)

	# set USE_OPENMP here and define it in opmphm.c
	set (USE_OPENMP 0)
	if (USE_OPENMP)
		find_package (OpenMP)
		if (OPENMP_FOUND)
			set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
			set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
		endif (OPENMP_FOUND)
	endif (USE_OPENMP)
	do_benchmark (opmphm)
endif (ENABLE_OPTIMIZATIONS AND NOT WIN32)

add_executable (benchmark_plugingetset plugingetset.c)
target_link_elektra (benchmark_plugingetset elektra-core elektra-kdb)
