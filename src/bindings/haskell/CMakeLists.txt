find_program (GHC_EXECUTABLE ghc)
find_program (CABAL_EXECUTABLE cabal)
find_program (C2HS_EXECUTABLE c2hs)

if (CABAL_EXECUTABLE) # set by find_program
if (C2HS_EXECUTABLE)
if (GHC_EXECUTABLE)

	set (CABAL_INCLUDE_DIRS "\"${CMAKE_SOURCE_DIR}/src/include\", \"${CMAKE_BINARY_DIR}/src/include\"")

	execute_process (
		COMMAND ${GHC_EXECUTABLE} --numeric-version
		OUTPUT_VARIABLE GHC_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE
	)

	set (BINDING_HASKELL_NAME "${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHSlibelektra-haskell-${KDB_VERSION}")
	set (CABAL_OPTS "--prefix=${CMAKE_INSTALL_PREFIX}")
	if (BUILD_SHARED OR BUILD_FULL)
		set (GHC_DYNAMIC_SUFFIX "-ghc${GHC_VERSION}")
		if (APPLE)
			set (GHC_DYNAMIC_ENDING ".dylib")
		else (APPLE)
			set (GHC_DYNAMIC_ENDING ".so")
		endif (APPLE)
		set (BINDING_HASKELL_NAME "${BINDING_HASKELL_NAME}${GHC_DYNAMIC_SUFFIX}${GHC_DYNAMIC_ENDING}")
		set (CABAL_OPTS "${CABAL_OPTS};--enable-shared")
		if (BUILD_SHARED)
			set (ELEKTRA_DEPENDENCY "elektra;elektra-kdb;")
		elseif (BUILD_FULL)
			set (ELEKTRA_DEPENDENCY "elektra-full;")
		endif ()
	elseif (BUILD_STATIC)
		set (BINDING_HASKELL_NAME "${BINDING_HASKELL_NAME}.a")
		set (ELEKTRA_DEPENDENCY "elektra-static;")
	endif ()

	string (REPLACE ";" " " CABAL_ELEKTRA_DEPENDENCY "${ELEKTRA_DEPENDENCY}")
	
	# configure include paths
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		@ONLY
	)

	add_custom_command (
		OUTPUT ${BINDING_HASKELL_NAME}
		COMMAND ${CABAL_EXECUTABLE} --ipid=libelektra-haskell-${KDB_VERSION} ${CABAL_OPTS} configure
		COMMAND ${CABAL_EXECUTABLE} build
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Key.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KeySet.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Plugin.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KDB.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/test/Elektra.hs"
		"${CMAKE_CURRENT_SOURCE_DIR}/test/ElektraRealWorld.hs"
	)
	add_custom_target (c2hs_haskell ALL DEPENDS "${BINDING_HASKELL_NAME}")
	if (BUILD_SHARED)
		add_custom_command(TARGET c2hs_haskell POST_BUILD
	    	COMMAND ${CMAKE_COMMAND} -E copy
	    		"${BINDING_HASKELL_NAME}"
	    		"${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/"
		)
	endif (BUILD_SHARED)
	# build and install it to the cabal db
	install (CODE "execute_process (COMMAND ${CABAL_EXECUTABLE} install WORKING_DIRECTORY \"${CMAKE_CURRENT_BINARY_DIR}\")")

	if (BUILD_TESTING)
		# recompile with tests enabled, to get the dependency graph for the static versions correct
		# the tests need the elektra library already built - while for the haskell plugins it doesn't matter
		# as a static build depends on the plugins but not on the bindings, this is the way we can resolve the
		# circular dependency by treating the tests separately
		set (HASKELL_TESTS 
			"${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic/testhaskell_basic"
			"${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic_optimized/testhaskell_basic_optimized"
			"${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld/testhaskell_realworld"
			"${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld_optimized/testhaskell_realworld_optimized"
		)
		add_custom_command (
			OUTPUT ${HASKELL_TESTS}
			COMMAND ${CABAL_EXECUTABLE} --ipid=libelektra-haskell-${KDB_VERSION} ${CABAL_OPTS} --enable-tests configure
			COMMAND ${CABAL_EXECUTABLE} build
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
			"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Key.chs"
			"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KeySet.chs"
			"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Plugin.chs"
			"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KDB.chs"
			"${CMAKE_CURRENT_SOURCE_DIR}/test/Elektra.hs"
			"${CMAKE_CURRENT_SOURCE_DIR}/test/ElektraRealWorld.hs"
			${ELEKTRA_DEPENDENCY}
			c2hs_haskell
		)
		add_custom_target (c2hs_haskell_tests ALL DEPENDS ${HASKELL_TESTS})

		# test it using the executables, so cmake takes care about the rpaths
		add_test (
			NAME testhaskell_basic
			COMMAND "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic/testhaskell_basic"
		)
		set_property (TEST testhaskell_basic PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		add_test (
			NAME testhaskell_basic_optimized
			COMMAND "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic_optimized/testhaskell_basic_optimized"
		)
		set_property (TEST testhaskell_basic_optimized PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		if (ENABLE_KDB_TESTING)
			add_test (
				NAME testhaskell_realworld
				COMMAND "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld/testhaskell_realworld"
			)
			set_property (TEST testhaskell_realworld PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

			add_test (
				NAME testhaskell_realworld_optimized
				COMMAND "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld_optimized/testhaskell_realworld_optimized"
			)
			set_property (TEST testhaskell_realworld_optimized PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld_optimized APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
		endif (ENABLE_KDB_TESTING)
	endif (BUILD_TESTING)
else (GHC_EXECUTABLE)
	remove_binding (c2hs_haskell "GHC not found")
endif (GHC_EXECUTABLE)
else (C2HS_EXECUTABLE)
	remove_binding (c2hs_haskell "c2hs not found")
endif (C2HS_EXECUTABLE)
else (CABAL_EXECUTABLE)
	remove_binding (c2hs_haskell "cabal not found")
endif (CABAL_EXECUTABLE)

mark_as_advanced (
	GHC_EXECUTABLE
	C2HS_EXECUTABLE
	CABAL_EXECUTABLE
)
