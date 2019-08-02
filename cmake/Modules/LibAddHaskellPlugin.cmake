include (LibAddMacros)

# ~~~
# Allows one to add plugins written in Haskell, setting up the include paths and
# libraries automatically.
#
# Expects that plugins make use of Cabal as their build system.
#
# MODULES:
#  the name of the Haskell modules to be compiled
#  by default it assumes there is a single module called Elektra.<pluginName>
# NO_SHARED_SANDBOX:
#  By default all Haskell plugins and the bindings are compiled in a shared sandbox to
#  speed up compilation times by only compiling commonly-used libraries once. Set this
#  flag to use an independent sandbox instead in case there are e.g. library version conflicts
# SANDBOX_ADD_SOURCES:
#  additional source paths which should be added to the Cabal sandbox
#  required if the build should depend on Haskell libraries not available on hackage
# ADDITIONAL_SOURCES:
#  in case your plugin depends on other files than *.hs and *.lhs Haskell files and the default
#  Cabal file and c test file and setup file, you can specify them here
# ~~~
macro (add_haskell_plugin target)
	set (MULTI_VALUE_KEYWORDS
	     MODULES
	     SANDBOX_ADD_SOURCES
	     ADDITIONAL_SOURCES
	     TEST_ENVIRONMENT
	     TEST_REQUIRED_PLUGINS)
	cmake_parse_arguments (ARG
			       "NO_SHARED_SANDBOX;TEST_README;INSTALL_TEST_DATA" # optional keywords
			       "MODULE" # one value keywords
			       "${MULTI_VALUE_KEYWORDS}" # multi value keywords
			       ${ARGN})

	set (PLUGIN_NAME ${target})
	set (PLUGIN_NAME_CAPITALIZED ${target})
	if (ARG_MODULE)
		set (PLUGIN_NAME_CAPITALIZED ${ARG_MODULE})
	else (ARG_MODULE)
		string (SUBSTRING ${PLUGIN_NAME}
				  0
				  1
				  FIRST_LETTER)
		string (TOUPPER ${FIRST_LETTER} FIRST_LETTER)
		string (REGEX
			REPLACE "^.(.*)"
				"${FIRST_LETTER}\\1"
				PLUGIN_NAME_CAPITALIZED
				"${PLUGIN_NAME}")
	endif (ARG_MODULE)

	if (DEPENDENCY_PHASE)
		find_package (Haskell)
		find_package (Pluginprocess)

		if (NOT ENABLE_ASAN)
			if (PLUGINPROCESS_FOUND)
				if (HASKELL_FOUND)
					check_binding_included ("haskell" HAVE_HASKELL_BINDING)
					if (HAVE_HASKELL_BINDING)
						if (BUILD_SHARED)
							prepare_and_compile_haskell_plugin (${target}
											    ${PLUGIN_NAME}
											    ${PLUGIN_NAME_CAPITALIZED}
											    ${ARG_SANDBOX_ADD_SOURCES}
											    ${ARG_ADDITIONAL_SOURCES})
						endif (BUILD_SHARED)
					else (HAVE_HASKELL_BINDING)
						remove_plugin (${target} "haskell bindings are not included in the cmake configuration")
					endif (HAVE_HASKELL_BINDING)
				else (HASKELL_FOUND)
					remove_plugin (${target} ${HASKELL_NOTFOUND_INFO})
				endif (HASKELL_FOUND)
			else (PLUGINPROCESS_FOUND)
				remove_plugin (${target} "${PLUGINPROCESS_NOTFOUND_INFO}, but required for haskell plugins")
			endif (PLUGINPROCESS_FOUND)
		else (NOT ENABLE_ASAN)
			remove_plugin (${target} "haskell plugins are not compatible with ENABLE_ASAN")
		endif (NOT ENABLE_ASAN)
	endif (DEPENDENCY_PHASE)

	set (SANDBOX_PACKAGEDB "${HASKELL_SHARED_SANDBOX}/${GHC_TARGET_PLATFORM}-ghc-${GHC_VERSION}-packages.conf.d/")
	set (PLUGIN_LD_LIBRARY_PATH "${CMAKE_BINARY_DIR}/lib:${CMAKE_CURRENT_BINARY_DIR}/haskell")
	set (PLUGIN_ARGS "")
	if (ARG_TEST_README)
		list (APPEND PLUGIN_ARGS "TEST_README")
		list (APPEND PLUGIN_ARGS "TEST_ENVIRONMENT")
		list (APPEND PLUGIN_ARGS "SANDBOX_PACKAGEDB=${SANDBOX_PACKAGEDB}")
		list (APPEND PLUGIN_ARGS "LD_LIBRARY_PATH=${PLUGIN_LD_LIBRARY_PATH}")
		if (ARG_TEST_ENVIRONMENT)
			list (APPEND PLUGIN_ARGS "${ARG_TEST_ENVIRONMENT}")
		endif (ARG_TEST_ENVIRONMENT)

		if (ARG_TEST_REQUIRED_PLUGINS)
			list (APPEND PLUGIN_ARGS "TEST_REQUIRED_PLUGINS")
			list (APPEND PLUGIN_ARGS "${ARG_TEST_REQUIRED_PLUGINS}")
		endif (ARG_TEST_REQUIRED_PLUGINS)

	endif (ARG_TEST_README)

	# compile our c wrapper which takes care of invoking the haskell runtime the actual haskell plugin gets linked in dynamically as a
	# library
	add_plugin (${target} ONLY_SHARED
		    SOURCES ${CMAKE_CURRENT_BINARY_DIR}/haskell.h ${CMAKE_CURRENT_BINARY_DIR}/haskell.c
		    INCLUDE_DIRECTORIES ${GHC_INCLUDE_DIRS}
		    LINK_LIBRARIES ${GHC_LIBS}
		    LINK_ELEKTRA elektra-pluginprocess "${PLUGIN_ARGS}"
		    DEPENDS ${target} c2hs_haskell)

	if (DEPENDENCY_PHASE AND TARGET elektra-${target})
		set_target_properties (elektra-${target}
				       PROPERTIES INSTALL_RPATH
						  "${HASKELL_RPATH}"
						  SANDBOX_PACKAGEDB
						  "${SANDBOX_PACKAGEDB}")
	endif (DEPENDENCY_PHASE AND TARGET elektra-${target})

	if (ADDTESTING_PHASE AND TARGET elektra-${target})
		set (PLUGINTEST_ARGS "")
		if (ARG_INSTALL_TEST_DATA)
			list (APPEND PLUGINTEST_ARGS "INSTALL_TEST_DATA")
		endif (ARG_INSTALL_TEST_DATA)

		add_plugintest (${target} MEMLEAK "${PLUGINTEST_ARGS}")
		get_target_property (HASKELL_RPATH elektra-${target} INSTALL_RPATH)
		get_target_property (SANDBOX_PACKAGEDB elektra-${target} SANDBOX_PACKAGEDB)

		# this is required so that it finds the type checker plugin and all the libraries
		set_target_properties (testmod_${target} PROPERTIES INSTALL_RPATH "${HASKELL_RPATH}")
		set_property (TEST testmod_${target}
			      PROPERTY ENVIRONMENT "SANDBOX_PACKAGEDB=${SANDBOX_PACKAGEDB};LD_LIBRARY_PATH=${PLUGIN_LD_LIBRARY_PATH}")
	endif (ADDTESTING_PHASE AND TARGET elektra-${target})

	mark_as_advanced (GHC_FFI_LIB
			  GHC_RTS_LIB
			  GHC_BASE_LIB
			  GHC_GMP_LIB
			  GHC_PRIM_LIB)
endmacro (add_haskell_plugin)

macro (prepare_and_compile_haskell_plugin
       target
       PLUGIN_NAME
       PLUGIN_NAME_CAPITALIZED)
	cmake_parse_arguments (ARG
			       ""
			       ""
			       "SANDBOX_ADD_SOURCES ADDITIONAL_SOURCES"
			       ${ARGN})

	# needed for HsFFI.h
	execute_process (COMMAND ${GHC_EXECUTABLE} --print-libdir OUTPUT_VARIABLE GHC_LIB_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)

	# for the haskell function stubs and HsFFI.h
	set (GHC_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/dist/build/Elektra ${GHC_LIB_DIR}/include)

	# shared variants of ghc libraries have the ghc version as a suffix
	set (GHC_DYNAMIC_SUFFIX "-ghc${GHC_VERSION}")
	if (APPLE)
		set (GHC_DYNAMIC_ENDING ".dylib")
	else (APPLE)
		set (GHC_DYNAMIC_ENDING ".so")
	endif (APPLE)

	# ~~~
	# since we want to continue to use our cmake add_plugin macro we compile via the c compiler instead of ghc so we must feed it with
	# the ghc library paths manually
	# inspired by https://github.com/jarrett/cpphs/blob/master/Makefile
	# ~~~
	set (GHC_RTS_PATH "${GHC_LIB_DIR}/rts")
	find_library (GHC_RTS_LIB "HSrts${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_RTS_PATH})

	execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest base OUTPUT_VARIABLE GHC_BASE_NAME OUTPUT_STRIP_TRAILING_WHITESPACE)
	set (GHC_BASE_PATH "${GHC_LIB_DIR}/${GHC_BASE_NAME}")
	find_library (GHC_BASE_LIB "HS${GHC_BASE_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_BASE_PATH})

	execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest integer-gmp OUTPUT_VARIABLE GHC_GMP_NAME OUTPUT_STRIP_TRAILING_WHITESPACE)
	set (GHC_GMP_PATH "${GHC_LIB_DIR}/${GHC_GMP_NAME}")
	find_library (GHC_GMP_LIB "HS${GHC_GMP_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_GMP_PATH})

	execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest ghc-prim OUTPUT_VARIABLE GHC_PRIM_NAME OUTPUT_STRIP_TRAILING_WHITESPACE)
	set (GHC_PRIM_PATH "${GHC_LIB_DIR}/${GHC_PRIM_NAME}")
	find_library (GHC_PRIM_LIB "HS${GHC_PRIM_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_PRIM_PATH})

	set (PLUGIN_HASKELL_NAME "${CMAKE_CURRENT_BINARY_DIR}/libHS${target}${GHC_DYNAMIC_SUFFIX}${GHC_DYNAMIC_ENDING}")
	set (GHC_LIBS
	     ${GHC_RTS_LIB}
	     ${GHC_BASE_LIB}
	     ${GHC_GMP_LIB}
	     gmp
	     ${GHC_PRIM_LIB}
	     ${PLUGIN_HASKELL_NAME})

	# GHC's structure differs between OSX and Linux On OSX we need to link iconv and Cffi additionally
	if (APPLE)
		find_library (GHC_FFI_LIB Cffi PATHS "${GHC_LIB_DIR}/rts")
		set (GHC_LIBS ${GHC_LIBS} ${GHC_FFI_LIB} iconv)
	endif (APPLE)

	set (HASKELL_RPATH
	     "${GHC_RTS_PATH}"
	     "${GHC_BASE_PATH}"
	     "${GHC_PRIM_PATH}"
	     "${GHC_GMP_PATH}"
	     "${CMAKE_INSTALL_RPATH}"
	     "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/elektra/haskell")

	if (GHC_RTS_LIB)
		if (GHC_BASE_LIB)
			if (GHC_GMP_LIB)
				if (GHC_PRIM_LIB)
					if (GHC_FFI_LIB OR NOT APPLE)
						compile_haskell_plugin (${target}
									${PLUGIN_HASKELL_NAME}
									SANDBOX_ADD_SOURCES
									${ARG_SANDBOX_ADD_SOURCES}
									ADDITIONAL_SOURCES
									${ARG_ADDITIONAL_SOURCES})
					else (GHC_FFI_LIB OR NOT APPLE)
						remove_plugin (${target} "GHC_FFI_LIB not found")
					endif (GHC_FFI_LIB OR NOT APPLE)
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
endmacro (prepare_and_compile_haskell_plugin)

macro (compile_haskell_plugin target PLUGIN_HASKELL_NAME)
	cmake_parse_arguments (ARG
			       ""
			       ""
			       "SANDBOX_ADD_SOURCES ADDITIONAL_SOURCES"
			       ${ARGN})

	# configure include paths
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${target}.cabal.in" "${CMAKE_CURRENT_BINARY_DIR}/${target}.cabal" @ONLY)

	# configure the haskell plugin base file for the current plugin
	configure_file ("${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.c.in" "${CMAKE_CURRENT_BINARY_DIR}/haskell.c" @ONLY)

	# same for the header
	configure_file ("${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.h.in" "${CMAKE_CURRENT_BINARY_DIR}/haskell.h" @ONLY)

	# copy the readme so the macro in haskell.c finds it
	file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/README.md" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

	# same for the setup logic, depending on wheter a custom one exists use the default suitable for almost everything
	set (CABAL_CUSTOM_SETUP_FILE "${CMAKE_CURRENT_SOURCE_DIR}/Setup.hs.in")
	if (NOT EXISTS ${CABAL_CUSTOM_SETUP_FILE})
		set (CABAL_CUSTOM_SETUP_FILE "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in")
	endif (NOT EXISTS ${CABAL_CUSTOM_SETUP_FILE})
	configure_file (${CABAL_CUSTOM_SETUP_FILE} "${CMAKE_CURRENT_BINARY_DIR}/Setup.hs" @ONLY)

	# as we require the sandbox to exist we can do this during the configuration phase it doesn't compile or install anything
	execute_process (COMMAND ${CABAL_EXECUTABLE}
				 sandbox
				 init
				 --sandbox
				 "${HASKELL_SHARED_SANDBOX}"
				 -v0
			 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
	foreach (SANDBOX_ADD_SOURCE ${ARG_SANDBOX_ADD_SOURCES})

		# our custom libs are all to be processed by cmake before we can add them, so enforce that, the build is more stable this
		# way
		if (NOT IS_DIRECTORY "${CMAKE_BINARY_DIR}/${SANDBOX_ADD_SOURCE}")
			add_subdirectory ("${CMAKE_SOURCE_DIR}/${SANDBOX_ADD_SOURCE}" "${CMAKE_BINARY_DIR}/${SANDBOX_ADD_SOURCE}")
		endif (NOT IS_DIRECTORY "${CMAKE_BINARY_DIR}/${SANDBOX_ADD_SOURCE}")
		execute_process (COMMAND ${CABAL_EXECUTABLE}
					 sandbox
					 add-source
					 "${CMAKE_BINARY_DIR}/${SANDBOX_ADD_SOURCE}"
					 -v0
				 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
	endforeach ()

	# Grab potential haskell source files
	file (GLOB_RECURSE
	      PLUGIN_SOURCE_FILES_UNFILTERED
	      "${CMAKE_CURRENT_SOURCE_DIR}/*.hs"
	      "${CMAKE_CURRENT_SOURCE_DIR}/*.lhs"
	      "${CMAKE_CURRENT_BINARY_DIR}/*.hs"
	      "${CMAKE_CURRENT_BINARY_DIR}/*.lhs")

	# exclude the dist directory
	set (PLUGIN_SOURCE_FILES)
	foreach (file ${PLUGIN_SOURCE_FILES_UNFILTERED})
		if (NOT file MATCHES "${CMAKE_CURRENT_BINARY_DIR}/dist/")
			list (APPEND PLUGIN_SOURCE_FILES ${file})
		endif ()
	endforeach ()

	set (PLUGIN_SOURCE_FILES
	     "${PLUGIN_SOURCE_FILES}"
	     "${CMAKE_CURRENT_SOURCE_DIR}/${target}.cabal.in"
	     "${CMAKE_CURRENT_SOURCE_DIR}/testmod_${target}.c"
	     "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in"
	     "${ARG_ADDITIONAL_SOURCES}")

	set (CABAL_OPTS "--prefix=${CMAKE_INSTALL_PREFIX};--enable-shared")
	get_property (HASKELL_LAST_PLUGIN GLOBAL PROPERTY HASKELL_LAST_PLUGIN)
	if (NOT HASKELL_LAST_PLUGIN)
		set (HASKELL_LAST_PLUGIN ${target})
		add_custom_command (OUTPUT ${PLUGIN_HASKELL_NAME}
				    COMMAND ${CABAL_EXECUTABLE}
					    install
					    ${CABAL_OPTS}
					    --only-dependencies
					    --offline
					    -v0
					    ||
					    true
				    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
				    DEPENDS c2hs_haskell)
	else (NOT HASKELL_LAST_PLUGIN)
		add_custom_command (OUTPUT ${PLUGIN_HASKELL_NAME}
				    COMMAND ${CABAL_EXECUTABLE}
					    install
					    ${CABAL_OPTS}
					    --only-dependencies
					    --offline
					    -v0
					    ||
					    true
				    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
				    DEPENDS ${HASKELL_LAST_PLUGIN})
	endif (NOT HASKELL_LAST_PLUGIN)
	set_property (GLOBAL PROPERTY HASKELL_LAST_PLUGIN "${target}")

	# reconfiguration due to Cabal library version issues on stretch
	add_custom_command (OUTPUT ${PLUGIN_HASKELL_NAME}
			    COMMAND ${CABAL_EXECUTABLE}
				    configure
				    ${CABAL_OPTS}
				    -v0
			    COMMAND ${CABAL_EXECUTABLE}
				    build
				    -v0
				    ||
				    (${CABAL_EXECUTABLE} configure ${CABAL_OPTS} -v0 && ${CABAL_EXECUTABLE} build -v0)
			    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			    DEPENDS c2hs_haskell ${PLUGIN_SOURCE_FILES} ${HASKELL_ADD_SOURCES_TARGET}
			    APPEND)
	add_custom_target (${target} ALL DEPENDS ${PLUGIN_HASKELL_NAME})

	install (DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/haskell" DESTINATION "lib${LIB_SUFFIX}/elektra/")
endmacro (compile_haskell_plugin)
