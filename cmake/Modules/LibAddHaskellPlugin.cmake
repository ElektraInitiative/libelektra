include (LibAddMacros)

# Allows one to add plugins written in haskell, setting up the include paths and
# libraries automatically.
#
# Expects that plugins make use of cabal as their build system.
# Expects that
#
# MODULES:
#  the name of the haskell modules to be compiled
#  by default it assumes there is a single module called Elektra.<pluginName>
macro (add_haskell_plugin target)
	cmake_parse_arguments (ARG
		"" # optional keywords
		"MODULE" # one value keywords
		"MODULES" # multi value keywords
		${ARGN}
	)

	set (PLUGIN_NAME ${target})
	set (PLUGIN_NAME_CAPITALIZED ${target})
	string (SUBSTRING ${PLUGIN_NAME} 0 1 FIRST_LETTER)
	string (TOUPPER ${FIRST_LETTER} FIRST_LETTER)
	string (REGEX REPLACE "^.(.*)" "${FIRST_LETTER}\\1" PLUGIN_NAME_CAPITALIZED "${PLUGIN_NAME}")
	string (TOUPPER ${PLUGIN_NAME} PLUGIN_NAME_UPPERCASE)

	if (DEPENDENCY_PHASE)
		find_program (GHC_EXECUTABLE ghc)
		find_program (GHC-PKG_EXECUTABLE ghc-pkg)
		find_program (CABAL_EXECUTABLE cabal)

		 # set by find_program
		if (CABAL_EXECUTABLE)
		if (GHC_EXECUTABLE)
		if (GHC-PKG_EXECUTABLE)
		list (FIND BINDINGS "haskell" FINDEX)
		if (FINDEX GREATER -1)

			# needed for HsFFI.h
			execute_process (
				COMMAND ${GHC_EXECUTABLE} --numeric-version
				OUTPUT_VARIABLE GHC_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE
			)
			execute_process (
				COMMAND ${GHC_EXECUTABLE} --print-libdir
				OUTPUT_VARIABLE GHC_LIB_DIR OUTPUT_STRIP_TRAILING_WHITESPACE
			)

			set (GHC_INCLUDE_DIRS
				${CMAKE_CURRENT_BINARY_DIR}/dist/build/Elektra # for the haskell function stubs
				${GHC_LIB_DIR}/include # for HsFFI.h
			)

			# since we want to continue to use our cmake add_plugin macro
			# we compile via the c compiler instead of ghc
			# so we must feed it with the ghc library paths manually
			# inspired by https://github.com/jarrett/cpphs/blob/master/Makefile
			find_library (GHC_FFI_LIB Cffi PATHS ${GHC_LIB_DIR}/rts)
			# use HSrts_thr for the threaded version of the rts
			find_library (GHC_RTS_LIB HSrts PATHS ${GHC_LIB_DIR}/rts)
			execute_process (
				COMMAND ${GHC-PKG_EXECUTABLE} latest base
				OUTPUT_VARIABLE GHC_BASE_NAME OUTPUT_STRIP_TRAILING_WHITESPACE
			)
			find_library (GHC_BASE_LIB "HS${GHC_BASE_NAME}" ${GHC_LIB_DIR}/${GHC_BASE_NAME})
			execute_process (
				COMMAND ${GHC-PKG_EXECUTABLE} latest integer-gmp
				OUTPUT_VARIABLE GHC_GMP_NAME OUTPUT_STRIP_TRAILING_WHITESPACE
			)
			find_library (GHC_GMP_LIB "HS${GHC_GMP_NAME}" ${GHC_LIB_DIR}/${GHC_GMP_NAME})
			execute_process (
				COMMAND ${GHC-PKG_EXECUTABLE} latest ghc-prim
				OUTPUT_VARIABLE GHC_PRIM_NAME OUTPUT_STRIP_TRAILING_WHITESPACE
			)
			find_library (GHC_PRIM_LIB "HS${GHC_PRIM_NAME}" ${GHC_LIB_DIR}/${GHC_PRIM_NAME})

			if (GHC_FFI_LIB)
			if (GHC_RTS_LIB)
			if (GHC_BASE_LIB)
			if (GHC_GMP_LIB)
			if (GHC_PRIM_LIB)

			set (GHC_LIB_DIRS
				"${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHS${target}.a"
				"${CMAKE_BINARY_DIR}/src/bindings/haskell/dist/build/libHSlibelektra-haskell-${KDB_VERSION}.a"
				${GHC_FFI_LIB}
				${GHC_RTS_LIB}
				${GHC_BASE_LIB}
				${GHC_GMP_LIB}
				${GHC_PRIM_LIB}
				iconv
				gmp
			)

			# configure include paths
			configure_file (
				"${CMAKE_CURRENT_SOURCE_DIR}/${target}.cabal.in"
				"${CMAKE_CURRENT_BINARY_DIR}/${target}.cabal"
				@ONLY
			)
			# configure the haskell plugin base file for the current plugin
			configure_file (
				"${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.c.in"
				"${CMAKE_CURRENT_BINARY_DIR}/haskell.c"
				@ONLY
			)
			# same for the header
			configure_file (
				"${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.h.in"
				"${CMAKE_CURRENT_BINARY_DIR}/haskell.h"
				@ONLY
			)
			# copy the readme so the macro in haskell.c finds it
			file (
				COPY "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
				DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
			)

			# register the bindings for the compilation
			add_custom_command (
				OUTPUT "${CMAKE_BINARY_DIR}/src/bindings/haskell/${target}-register"
				COMMAND ${CABAL_EXECUTABLE} register --inplace
				COMMAND ${CMAKE_COMMAND} ARGS -E touch "${target}-register"
				WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/bindings/haskell
				DEPENDS c2hs_haskell
			)
			add_custom_command (
				OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHS${target}.a
				# this way it will generate predictable output filenames
				# and compile the haskell part of this plugin with cabal
				COMMAND ${CABAL_EXECUTABLE} --ipid=${target} configure
				COMMAND ${CABAL_EXECUTABLE} build
				WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
				DEPENDS "${CMAKE_BINARY_DIR}/src/bindings/haskell/${target}-register"
			)
			add_custom_target (${target} DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHS${target}.a")

			else (GHC_PRIM_LIB)
				remove_plugin (${target} "GHC_PRIM_LIB not found")
			endif (GHC_PRIM_LIB)
			else (GHC_GMP_LIB)
				remove_plugin (${target} "GHC_GMP_LIB not found")
			endif (GHC_GMP_LIB)
			else (GHC_BASE_LIB)
				remove_plugin (${target} "GHC_BASE_LIB not found")
			endif (GHC_BASE_LIB)
			else (GHC_RTS_LIB)
				remove_plugin (${target} "GHC_RTS_LIB not found")
			endif (GHC_RTS_LIB)
			else (GHC_FFI_LIB)
				remove_plugin (${target} "GHC_FFI_LIB not found")
			endif (GHC_FFI_LIB)

		else (FINDEX GREATER -1)
			remove_plugin (${target} "haskell bindings are not included in the cmake configuration")
		endif (FINDEX GREATER -1)
		else (GHC-PKG_EXECUTABLE)
			remove_plugin (${target} "ghc-pkg not found")
		endif (GHC-PKG_EXECUTABLE)
		else (GHC_EXECUTABLE)
			remove_plugin (${target} "GHC not found")
		endif (GHC_EXECUTABLE)
		else (CABAL_EXECUTABLE)
			remove_plugin (${target} "cabal not found")
		endif (CABAL_EXECUTABLE)
	endif ()

	# compile our c wrapper which takes care of invoking the haskell runtime
	# the actual haskell plugin gets linked in dynamically as a library
	add_plugin (${target}
		SOURCES
			${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.h
			${CMAKE_CURRENT_BINARY_DIR}/haskell.c
		INCLUDE_DIRECTORIES
			${GHC_INCLUDE_DIRS}
		LINK_LIBRARIES
			${GHC_LIB_DIRS}
		DEPENDS
			${target} c2hs_haskell
		ADD_TEST
	)

	mark_as_advanced (
		GHC_EXECUTABLE
		GHC-PKG_EXECUTABLE
		CABAL_EXECUTABLE
		GHC_FFI_LIB
		GHC_RTS_LIB
		GHC_BASE_LIB
		GHC_GMP_LIB
		GHC_PRIM_LIB
	)
endmacro (add_haskell_plugin)
