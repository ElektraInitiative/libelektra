find_program (GHC_EXECUTABLE ghc)
find_program (CABAL_EXECUTABLE cabal)
find_program (C2HS_EXECUTABLE c2hs)


if (CABAL_EXECUTABLE) # set by find_program
if (C2HS_EXECUTABLE)
if (GHC_EXECUTABLE)
	
	SET (CABAL_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/src/include, ${CMAKE_BINARY_DIR}/src/include")
	SET (CABAL_EXTRA_LIB_DIRS "")

	# configure include paths
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		@ONLY
	)

	# build and install it to the cabal db
	if (INSTALL_SYSTEM_FILES)
		add_custom_target (
			c2hs_haskell ALL
			COMMAND ${CABAL_EXECUTABLE} --enable-shared configure
			COMMAND ${CABAL_EXECUTABLE} install
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		)
	else (INSTALL_SYSTEM_FILES) # else just build it
		add_custom_target (
			c2hs_haskell ALL
			COMMAND ${CABAL_EXECUTABLE} --enable-shared configure
			COMMAND ${CABAL_EXECUTABLE} build
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		)
	endif (INSTALL_SYSTEM_FILES)

	# also test it with cabal
	add_test (
		NAME testhaskell_cabal
		COMMAND ${CABAL_EXECUTABLE} test
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)

	set_property (TEST testhaskell_cabal PROPERTY LABELS bindings)
	set_property (TEST testhaskell_cabal PROPERTY LABELS kdbtests)
else (GHC_EXECUTABLE)
	remove_binding (c2hs_haskell "GHC not found")
endif (GHC_EXECUTABLE)
else (C2HS_EXECUTABLE)
	remove_binding (c2hs_haskell "c2hs not found")
endif (C2HS_EXECUTABLE)
else (CABAL_EXECUTABLE)
	remove_binding (c2hs_haskell "cabal not found")
endif (CABAL_EXECUTABLE)
