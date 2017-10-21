find_program (GHC_EXECUTABLE ghc)
find_program (CABAL_EXECUTABLE cabal)
find_program (C2HS_EXECUTABLE c2hs)

if (CABAL_EXECUTABLE) # set by find_program
if (C2HS_EXECUTABLE)
if (GHC_EXECUTABLE)

	set (CABAL_INCLUDE_DIRS "\"${CMAKE_SOURCE_DIR}/src/include\", \"${CMAKE_BINARY_DIR}/src/include\"")

	# configure include paths
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		@ONLY
	)

	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHSlibelektra-haskell-${KDB_VERSION}.a"
		COMMAND ${CABAL_EXECUTABLE} --ipid=libelektra-haskell-${KDB_VERSION} configure
		COMMAND ${CABAL_EXECUTABLE} build
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
	)
	add_custom_target (c2hs_haskell ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/dist/build/libHSlibelektra-haskell-${KDB_VERSION}.a")

	# build and install it to the cabal db
	install(CODE "execute_process (COMMAND ${CABAL_EXECUTABLE} install WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

	if (BUILD_TESTING)
		# also test it with cabal
		add_test (
			NAME testhaskell_cabal
			COMMAND ${CABAL_EXECUTABLE} test libelektra-haskell-test libelektra-haskell-test-optimized
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		)
		set_property (TEST testhaskell_cabal PROPERTY LABELS bindings)

		if (ENABLE_KDB_TESTING)
			add_test (
				NAME testhaskell_realworld_cabal
				COMMAND ${CABAL_EXECUTABLE} test libelektra-haskell-test-realworld libelektra-haskell-test-realworld-optimized
				WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			)
			set_property (TEST testhaskell_realworld_cabal PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld_cabal APPEND PROPERTY LABELS kdbtests)
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
