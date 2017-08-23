find_program (GHC_EXECUTABLE ghc)
find_program (CABAL_EXECUTABLE cabal)
find_program (C2HS_EXECUTABLE c2hs)


if (CABAL_EXECUTABLE AND C2HS_EXECUTABLE AND GHC_EXECUTABLE) # set by find_program
	
	SET (CABAL_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/src/include, ${CMAKE_BINARY_DIR}/src/include")
	SET (CABAL_EXTRA_LIB_DIRS "")

	# configure include paths
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		@ONLY
	)
	# compile it with cabal as our build tool
	add_custom_target (
		haskell ALL
		COMMAND ${CABAL_EXECUTABLE} build
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)

	# TODO where to install this? install the cabal package globally?

	# also test it with cabal
	add_test (
		NAME testhaskell_cabal
		COMMAND ${CABAL_EXECUTABLE} test
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)

	set_property (
		TEST testhaskell_cabal
		PROPERTY LABELS bindings
	)
else ()
	remove_binding (haskell "Either Cabal, C2HS or GHC not found, but required")
endif ()
