include (LibAddHaskellPlugin)

find_package (Haskell)

if (HASKELL_FOUND)
	list (FIND BINDINGS "haskell" FINDEX)
	if (FINDEX GREATER -1)

		set (TYPECHECKER_GHC_PLUGIN_PATH "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/elektra/typechecker-ghc-plugin")
		set (TYPECHECKER_GHC_PLUGIN "${TYPECHECKER_GHC_PLUGIN_PATH}/${GHC_TARGET_PLATFORM2}-ghc-${GHC_VERSION}-packages.conf.d/")

		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/Elektra/Typechecker.hs.in"
				"${CMAKE_CURRENT_BINARY_DIR}/Elektra/Typechecker.hs"
				@ONLY)

		add_haskell_plugin (typechecker
				    SANDBOX_ADD_SOURCES
				    "src/bindings/haskell"
				    "src/libs/typesystem/libfa"
				    "src/libs/typesystem/SpecElektra"
				    "src/libs/typesystem/SpecTranslator")

		if (DEPENDENCY_PHASE)
			file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/sandbox/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/sandbox/")

			add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/sandbox/cabal.sandbox.config"
					    COMMAND ${CABAL_EXECUTABLE} sandbox delete -v0
					    COMMAND ${CABAL_EXECUTABLE} sandbox init -v0
					    COMMAND ${CABAL_EXECUTABLE} sandbox add-source -v0
						    "${CMAKE_BINARY_DIR}/src/libs/typesystem/libfa"
					    COMMAND ${CABAL_EXECUTABLE} sandbox add-source -v0
						    "${CMAKE_BINARY_DIR}/src/libs/typesystem/SpecElektra"
					    COMMAND ${CABAL_EXECUTABLE} install --only-dependencies -v0
					    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/sandbox/")
			add_custom_target ("typechecker-ghc-plugin" ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/sandbox/cabal.sandbox.config")
		endif (DEPENDENCY_PHASE)

		# copy our small typechecker sandbox to the library dir so it can be easily deployed and use the dependencies
		# for hint from there
		install (DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/sandbox/.cabal-sandbox/" DESTINATION ${TYPECHECKER_GHC_PLUGIN_PATH})

		mark_as_advanced (TYPECHECKER_GHC_PLUGIN)

	endif (FINDEX GREATER -1)
endif (HASKELL_FOUND)
