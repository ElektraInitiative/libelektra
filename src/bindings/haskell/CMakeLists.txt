find_package (Haskell)

if (HASKELL_FOUND) 
if (NOT BUILD_STATIC)

	set (CABAL_INCLUDE_DIRS "\"${CMAKE_SOURCE_DIR}/src/include\", \"${CMAKE_BINARY_DIR}/src/include\"")

	set (BINDING_HASKELL_NAME "${CMAKE_CURRENT_BINARY_DIR}/libHSlibelektra-haskell")
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
			set (ELEKTRA_DEPENDENCY "elektra;elektra-kdb;elektra-ease;")
		elseif (BUILD_FULL)
			set (ELEKTRA_DEPENDENCY "elektra-full;")
		endif ()
	elseif (BUILD_STATIC)
		set (BINDING_HASKELL_NAME "${BINDING_HASKELL_NAME}.a")
		set (CABAL_OPTS "${CABAL_OPTS};--disable-shared")
		set (ELEKTRA_DEPENDENCY "elektra-static;")
	endif ()
	string (REPLACE ";" " " CABAL_ELEKTRA_DEPENDENCY "${ELEKTRA_DEPENDENCY}")
	
	# configure include paths
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
		@ONLY
	)
	# Use the post-build logic to glue the bindings together with cmake as its done for plugins		
	file (
		COPY "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs"
		DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
	)
	set (BINDING_HASKELL_DEPENDENCIES
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Key.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KeySet.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Plugin.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KDB.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Ease.chs"
		"${CMAKE_CURRENT_SOURCE_DIR}/test/Elektra.hs"
		"${CMAKE_CURRENT_SOURCE_DIR}/test/ElektraRealWorld.hs"
		"${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs"
	)

	execute_process (COMMAND ${CABAL_EXECUTABLE} sandbox init --sandbox ${CMAKE_BINARY_DIR}/.cabal-sandbox 
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} OUTPUT_QUIET)
	add_custom_command (
		OUTPUT ${BINDING_HASKELL_NAME}
		COMMAND ${CABAL_EXECUTABLE} install --only-dependencies
		COMMAND ${CABAL_EXECUTABLE} ${CABAL_OPTS} configure
		 # this only installs to the sandbox so plugins can depend on it
		 # without breaking the global db through shared sandboxes
		COMMAND ${CABAL_EXECUTABLE} build
		# COMMAND ${CABAL_EXECUTABLE} install
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		DEPENDS ${BINDING_HASKELL_DEPENDENCIES} ${ELEKTRA_DEPENDENCY}
	)
	add_custom_target (c2hs_haskell ALL DEPENDS "${BINDING_HASKELL_NAME}")
	if (BUILD_SHARED OR BUILD_FULL)
		# install (FILES "${BINDING_HASKELL_NAME}" DESTINATION lib${LIB_SUFFIX})
		# build and install it to the global db
		# install (CODE "execute_process (COMMAND ${CABAL_EXECUTABLE} --ignore-sandbox install WORKING_DIRECTORY \"${CMAKE_CURRENT_BINARY_DIR}\")")
	endif (BUILD_SHARED OR BUILD_FULL)

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
			# they are getting installed to the sandbox, and they get cached to avoid reinstalling every time
			# disable everything that slows down compilation times of the dependencies to have reasonable travis build times
			COMMAND ${CABAL_EXECUTABLE} install --enable-tests --only-dependencies
			COMMAND ${CABAL_EXECUTABLE} ${CABAL_OPTS} --enable-tests configure
			COMMAND ${CABAL_EXECUTABLE} build
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			DEPENDS ${BINDING_HASKELL_DEPENDENCIES} ${ELEKTRA_DEPENDENCY} c2hs_haskell
		)
		add_custom_target (c2hs_haskell_tests ALL DEPENDS ${HASKELL_TESTS})
		if (INSTALL_TESTING)
			# as those are not controlled by cmake really, adjust the rpath manually
			# install rpath is enough, we don't depend on any cabal libraries anymore
			# tests are statically compiled
			foreach (HASKELL_TEST ${HASKELL_TESTS})
				install (CODE "execute_process (COMMAND ${CMAKE_INSTALL_NAME_TOOL} -add_rpath \"${CMAKE_INSTALL_RPATH}\" \"${HASKELL_TEST}\" OUTPUT_QUIET ERROR_QUIET)")
			endforeach (HASKELL_TEST ${HASKELL_TESTS})
			install (FILES ${HASKELL_TESTS}
				DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
				PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
		endif (INSTALL_TESTING)

		add_test (testhaskell_basic "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic/testhaskell_basic")
		set_property (TEST testhaskell_basic PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		add_test (testhaskell_basic_optimized "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic_optimized/testhaskell_basic_optimized")
		set_property (TEST testhaskell_basic_optimized PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		if (ENABLE_KDB_TESTING)
			add_test (testhaskell_realworld "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld/testhaskell_realworld")
			set_property (TEST testhaskell_realworld PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

			add_test (testhaskell_realworld_optimized "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld_optimized/testhaskell_realworld_optimized")
			set_property (TEST testhaskell_realworld_optimized PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld_optimized APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
		endif (ENABLE_KDB_TESTING)
	endif (BUILD_TESTING)
else (NOT BUILD_STATIC)
	remove_binding (haskell "BUILD_STATIC is currently not compatible with the haskell bindings")
endif (NOT BUILD_STATIC)
else (HASKELL_FOUND)
	remove_binding (haskell ${HASKELL_NOTFOUND_INFO})
endif (HASKELL_FOUND)
