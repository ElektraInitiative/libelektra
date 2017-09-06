include (LibAddMacros)

if (DEPENDENCY_PHASE)
	find_program (GHC_EXECUTABLE ghc)
	find_program (GHC-PKG_EXECUTABLE ghc-pkg)
	find_program (CABAL_EXECUTABLE cabal)

	 # set by find_program
	if (CABAL_EXECUTABLE)
	if (GHC_EXECUTABLE)

		# needed for HsFFI.h
		exec_program (${GHC_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS --numeric-version
			OUTPUT_VARIABLE GHC_VERSION
		)
		exec_program (${GHC_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS --print-libdir
			OUTPUT_VARIABLE GHC_LIB_DIR
		)

		set (GHC_INCLUDE_DIRS
			${CMAKE_CURRENT_BINARY_DIR}/dist/build/Elektra # for Haskelltemplate_stub.h
			${GHC_LIB_DIR}/include # for HsFFI.h
		)

		# since we want to continue to use our cmake add_plugin macro
		# we compile via the c compiler instead of ghc
		# so we must feed it with the ghc library paths manually
		# inspired by https://github.com/jarrett/cpphs/blob/master/Makefile
		find_library (GHC_FFI_LIB Cffi PATHS ${GHC_LIB_DIR}/rts)
		# use HSrts_thr for the threaded version of the rts
		find_library (GHC_RTS_LIB HSrts PATHS ${GHC_LIB_DIR}/rts)
		exec_program (${GHC-PKG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS latest base
			OUTPUT_VARIABLE GHC_BASE_NAME
		)
		find_library (GHC_BASE_LIB "HS${GHC_BASE_NAME}" ${GHC_LIB_DIR}/${GHC_BASE_NAME})
		exec_program (${GHC-PKG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS latest integer-gmp
			OUTPUT_VARIABLE GHC_GMP_NAME
		)
		find_library (GHC_GMP_LIB "HS${GHC_GMP_NAME}" ${GHC_LIB_DIR}/${GHC_GMP_NAME})
		exec_program (${GHC-PKG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS latest ghc-prim
			OUTPUT_VARIABLE GHC_PRIM_NAME
		)
		find_library (GHC_PRIM_LIB "HS${GHC_PRIM_NAME}" ${GHC_LIB_DIR}/${GHC_PRIM_NAME})

		exec_program (${GHC-PKG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS --simple-output field libelektra-haskell id
			OUTPUT_VARIABLE GHC_LIBELEKTRA_HASKELL_NAME
		)
		exec_program (${GHC-PKG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
			ARGS --simple-output field libelektra-haskell library-dirs
			OUTPUT_VARIABLE GHC_LIBELEKTRA_HASKELL_DIR
		)
		# TODO right now this depends on the library being installed locally, and only one version of it...
		find_library (GHC_LIBELEKTRA_HASKELL_LIB "HS${GHC_LIBELEKTRA_HASKELL_NAME}" ${GHC_LIBELEKTRA_HASKELL_DIR})

		set (HASKELLTEMPLATE_LIBS
			"${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHShaskelltemplate.a"
			${GHC_LIBELEKTRA_HASKELL_LIB}
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
			"${CMAKE_CURRENT_SOURCE_DIR}/haskelltemplate.cabal.in"
			"${CMAKE_CURRENT_BINARY_DIR}/haskelltemplate.cabal"
			@ONLY
		)

		# this way it will generate predictable output filenames
		# and compile the haskell part of this plugin with cabal
		add_custom_target (
			haskelltemplate
			COMMAND ${CABAL_EXECUTABLE} --enable-shared --ipid=haskelltemplate configure
			COMMAND ${CABAL_EXECUTABLE} build
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		)

	else (GHC_EXECUTABLE)
		remove_plugin (haskell "GHC not found")
	endif (GHC_EXECUTABLE)
	else (CABAL_EXECUTABLE)
		remove_plugin (haskell "cabal not found")
	endif (CABAL_EXECUTABLE)
endif ()

# compile our c wrapper which takes care of invoking the haskell runtime
add_plugin (haskelltemplate
	SOURCES
		haskelltemplate.h
		haskelltemplate.c
	INCLUDE_DIRECTORIES
		${GHC_INCLUDE_DIRS}
	LINK_LIBRARIES
		${HASKELLTEMPLATE_LIBS}
	DEPENDS
		haskelltemplate
	ADD_TEST
)