macro (compile_haskell_binding
       CABAL_EXECUTABLE
       GHC_VERSION
       HASKELL_SHARED_SANDBOX)
	add_binding (haskell ONLY_SHARED)

	set (CABAL_INCLUDE_DIRS "\"${CMAKE_SOURCE_DIR}/src/include\", \"${CMAKE_BINARY_DIR}/src/include\"")

	set (BINDING_HASKELL_NAME "${CMAKE_CURRENT_BINARY_DIR}/libHSlibelektra-haskell")
	if (BUILD_SHARED)
		set (GHC_DYNAMIC_SUFFIX "-ghc${GHC_VERSION}")
		set (BINDING_HASKELL_NAME "${BINDING_HASKELL_NAME}${GHC_DYNAMIC_SUFFIX}${CMAKE_SHARED_LIBRARY_SUFFIX}")
		set (CABAL_OPTS "--enable-shared")
		set (ELEKTRA_DEPENDENCY "elektra;elektra-kdb;elektra-ease;elektra-invoke;elektra-pluginprocess")
	elseif (BUILD_STATIC)
		set (BINDING_HASKELL_NAME "${BINDING_HASKELL_NAME}.a")
		set (CABAL_OPTS "--disable-shared")
		set (ELEKTRA_DEPENDENCY "elektra-static;")
	endif ()
	string (REPLACE ";"
			" "
			CABAL_ELEKTRA_DEPENDENCY
			"${ELEKTRA_DEPENDENCY}")

	# configure include paths
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in" "${CMAKE_CURRENT_BINARY_DIR}/libelektra-haskell.cabal"
			@ONLY)

	# Use the post-build logic to glue the bindings together with cmake  as its done for plugins
	configure_file ("${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in" "${CMAKE_CURRENT_BINARY_DIR}/Setup.hs" @ONLY)
	file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/src/c" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/src/")

	set (BINDING_HASKELL_DEPENDENCIES
	     "${CMAKE_CURRENT_SOURCE_DIR}/libelektra-haskell.cabal.in"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Key.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KeySet.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Plugin.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/KDB.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Ease.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/Elektra/Invoke.chs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/test/Elektra.hs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/test/ElektraRealWorld.hs"
	     "${CMAKE_CURRENT_SOURCE_DIR}/src/c/hskdberrors.c"
	     "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in")

	# as we require the sandbox to exist we can do this during the configuration phase it doesn't compile or install anything
	execute_process (COMMAND ${CABAL_EXECUTABLE}
				 sandbox
				 init
				 --sandbox
				 "${HASKELL_SHARED_SANDBOX}"
				 -v0
			 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
	execute_process (COMMAND ${CABAL_EXECUTABLE}
				 sandbox
				 add-source
				 ${CMAKE_CURRENT_BINARY_DIR}
				 -v0
			 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	# Sometimes building fails reporting an outdated package file. In this case we need to configure the package again.
	add_custom_command (OUTPUT ${BINDING_HASKELL_NAME}
			    COMMAND ${CABAL_EXECUTABLE}
				    configure
				    ${CABAL_OPTS}
				    -v0
			    COMMAND ${CABAL_EXECUTABLE} build -v0
			    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			    DEPENDS ${BINDING_HASKELL_DEPENDENCIES} ${ELEKTRA_DEPENDENCY})
	add_custom_target (c2hs_haskell ALL DEPENDS "${BINDING_HASKELL_NAME}")

	if (BUILD_TESTING)

		# recompile with tests enabled, to get the dependency graph for the static versions correct the tests need the elektra
		# library already built - while for the haskell plugins it doesn't matter as a static build depends on the plugins but not
		# on the bindings, this is the way we can resolve the circular dependency by treating the tests separately
		set (HASKELL_TESTS
		     "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic/testhaskell_basic"
		     "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic_optimized/testhaskell_basic_optimized"
		     "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld/testhaskell_realworld"
		     "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld_optimized/testhaskell_realworld_optimized")

		# Sometimes building fails reporting an outdated package file. In this case we need to configure the package again.
		add_custom_command (OUTPUT ${HASKELL_TESTS}
				    COMMAND ${CABAL_EXECUTABLE}
					    configure
					    ${CABAL_OPTS}
					    --enable-tests
					    -v0
				    COMMAND ${CABAL_EXECUTABLE} build -v0
				    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
				    DEPENDS ${BINDING_HASKELL_DEPENDENCIES} ${ELEKTRA_DEPENDENCY} c2hs_haskell)
		add_custom_target (c2hs_haskell_tests ALL DEPENDS ${HASKELL_TESTS})
		if (INSTALL_TESTING)

			# as those are not controlled by cmake really, adjust the rpath manually install rpath is enough, we don't depend on
			# any cabal libraries anymore tests are statically compiled
			foreach (HASKELL_TEST ${HASKELL_TESTS})
				install (
					CODE
						"execute_process (COMMAND ${CMAKE_INSTALL_NAME_TOOL} -add_rpath \"${CMAKE_INSTALL_RPATH}\" \"${HASKELL_TEST}\" OUTPUT_QUIET ERROR_QUIET)"
					)
			endforeach (HASKELL_TEST ${HASKELL_TESTS})
			install (FILES ${HASKELL_TESTS}
				 DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
				 PERMISSIONS OWNER_READ
					     OWNER_WRITE
					     OWNER_EXECUTE
					     GROUP_READ
					     GROUP_EXECUTE
					     WORLD_READ
					     WORLD_EXECUTE)
		endif (INSTALL_TESTING)

		add_test (testhaskell_basic "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic/testhaskell_basic")
		set_property (TEST testhaskell_basic PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic APPEND PROPERTY LABELS memleak)
		set_property (TEST testhaskell_basic PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		add_test (testhaskell_basic_optimized
			  "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_basic_optimized/testhaskell_basic_optimized")
		set_property (TEST testhaskell_basic_optimized PROPERTY LABELS bindings)
		set_property (TEST testhaskell_basic_optimized APPEND PROPERTY LABELS memleak)
		set_property (TEST testhaskell_basic_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		if (ENABLE_KDB_TESTING)
			add_test (testhaskell_realworld
				  "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld/testhaskell_realworld")
			set_property (TEST testhaskell_realworld PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld PROPERTY RUN_SERIAL TRUE)
			set_property (TEST testhaskell_realworld APPEND PROPERTY LABELS memleak)
			set_property (TEST testhaskell_realworld PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

			add_test (testhaskell_realworld_optimized
				  "${CMAKE_CURRENT_BINARY_DIR}/dist/build/testhaskell_realworld_optimized/testhaskell_realworld_optimized")
			set_property (TEST testhaskell_realworld_optimized PROPERTY LABELS bindings)
			set_property (TEST testhaskell_realworld_optimized APPEND PROPERTY LABELS kdbtests)
			set_property (TEST testhaskell_realworld_optimized PROPERTY RUN_SERIAL TRUE)
			set_property (TEST testhaskell_realworld_optimized APPEND PROPERTY LABELS memleak)
			set_property (TEST testhaskell_realworld_optimized PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
		endif (ENABLE_KDB_TESTING)
	endif (BUILD_TESTING)
endmacro (compile_haskell_binding)

find_package (Haskell)

set (GHC_MIN_VERSION "8.0.1")
if (BUILD_SHARED)
	if (NOT ENABLE_ASAN)
		if (HASKELL_FOUND)
			if (NOT GHC_VERSION VERSION_LESS ${GHC_MIN_VERSION})
				compile_haskell_binding (${CABAL_EXECUTABLE} ${GHC_VERSION} ${HASKELL_SHARED_SANDBOX})
			else (NOT GHC_VERSION VERSION_LESS ${GHC_MIN_VERSION})
				exclude_binding (haskell "ghc: ${GHC_VERSION} found, but ${GHC_MIN_VERSION} required")
			endif (NOT GHC_VERSION VERSION_LESS ${GHC_MIN_VERSION})
		else (HASKELL_FOUND)
			exclude_binding (haskell ${HASKELL_NOTFOUND_INFO})
		endif (HASKELL_FOUND)
	else (NOT ENABLE_ASAN)
		exclude_binding (haskell "not compatible with ENABLE_ASAN")
	endif (NOT ENABLE_ASAN)
else (BUILD_SHARED)
	exclude_binding (haskell "it can only be built if BUILD_SHARED is enabled, both BUILD_FULL or BUILD_STATIC may be enabled as well")
endif (BUILD_SHARED)
