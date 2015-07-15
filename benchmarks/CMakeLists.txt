include (LibAddMacros)

add_headers (HDR_FILES)

macro (do_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} benchmarks.c benchmarks.h ${source}.c)
	add_executable (${source} ${SOURCES})

	target_link_elektra(${source})

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

	get_target_property (command ${source} LOCATION)

	add_custom_target (benchmark_${source}
		rm -f *.out
		COMMAND valgrind --tool=callgrind --callgrind-out-file=callgrind.out "${command}" > output.out
		# COMMAND strace -o strace.out "${command}"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
		)
endmacro (do_benchmark)

do_benchmark (large)
do_benchmark (cmp)
do_benchmark (createkeys)


#OPMPHM benchmarks start

macro (do_opmph_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} opmph_benchmarks.c opmph_benchmarks.h opmph_${source}.c)
	add_executable (opmph_${source} ${SOURCES})

	target_link_elektra(opmph_${source})

	set_target_properties (opmph_${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)

	get_target_property (command opmph_${source} LOCATION)

	add_custom_target (benchmark_opmph_${source}
		COMMAND "${command}"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
		)
endmacro (do_opmph_benchmark)

do_opmph_benchmark (hsearch)

#OPMPHM benchmarks end
