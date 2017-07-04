include (LibAddMacros)

add_headers (HDR_FILES)

macro (do_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} benchmarks.c benchmarks.h ${source}.c)
	add_executable (benchmark_${source} ${SOURCES})
	add_dependencies (benchmark_${source} kdberrors_generated)

	target_link_elektra(benchmark_${source} elektra-kdb elektra-meta)

	set_target_properties (benchmark_${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

	get_target_property (command benchmark_${source} LOCATION)

	add_custom_target (benchmark_${source}_callgrind
		rm -f *.out
		COMMAND valgrind --tool=callgrind --callgrind-out-file=callgrind.out "${command}" > output.out
		# COMMAND strace -o strace.out "${command}"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
		)
endmacro (do_benchmark)

do_benchmark (large)
do_benchmark (cmp)
do_benchmark (createkeys)

