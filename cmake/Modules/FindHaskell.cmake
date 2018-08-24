# ~~~
# Find the haskell environment required for the haskell bindings and plugins.
# Only searches for the GHC compiler and not other haskell compilers as it is
# the most widespread and advanced haskell compiler.
#
# If stack is used, it will check if stack has been initialized correctly
# (e.g. the script install-stack-haskell-dependencies has been executed or
# otherwise everything was manually installed and stack.yaml exists in build)
# and pick the programs from stack instead.
#
#  CABAL_EXECUTABLE       - Path to the cabal executable
#  C2HS_EXECUTABLE        - Path to the c2hs executable
#  ALEX_EXECUTABLE        - Path to the alex executable
#  HAPPY_EXECUTABLE       - Path to the happy executable
#  GHC_EXECUTABLE         - Path to the ghc executable
#  GHC-PKG_EXECUTABLE     - Path to the ghc-pkg executable
#  GHC_HSPEC_FOUND        - True if the hspec library is available
#  GHC_QUICKCHECK_FOUND   - True if the QuickCheck library is available
#  GHC_VERSION            - The numeric version of the ghc executable
#  GHC_TARGET_PLATFORM    - The target platform string of ghc
#                           sanitized (darwin -> osx, unknown-linux -> linux)
#  CABAL_DYNLIB_PATH      - The default path where cabal installs dynamic libraries
#  CABAL_CUSTOM_TARGET    - The default dependencies of the custom Setup.hs for plugins
#  STACK_EXECUTABLE       - Path to the stack executable
#  HASKELL_SHARED_SANDBOX - The sandbox containing all required dependencies for haskell things
#  HASKELL_FOUND          - True if the whole required haskell environment exists
#    This variable is set to true if CABAL_EXECUTABLE, C2HS_EXECUTABLE, GHC_EXECUTABLE
#    and GHC-PKG_EXECUTABLE are all available. If BUILD_TESTING is enabled, it also
#    requires GHC_HSPEC_FOUND and GHC_QUICKCHECK_FOUND to be true.
#  HASKELL_NOTFOUND_INFO  - A string describing which haskell dependency is missing
# ~~~

mark_as_advanced (GHC_EXECUTABLE
		  GHC-PKG_EXECUTABLE
		  ALEX_EXECUTABLE
		  HAPPY_EXECUTABLE
		  C2HS_EXECUTABLE
		  CABAL_EXECUTABLE
		  STACK_EXECUTABLE
		  CABAL_DYNLIB_PATH
		  CABAL_CUSTOM_TARGET
		  GHC_VERSION
		  GHC_HSPEC_FOUND
		  GHC_QUICKCHECK_FOUND
		  GHC_TARGET_PLATFORM)

find_program (STACK_EXECUTABLE stack)
if (HASKELL_USE_CABAL)
	find_program (GHC_EXECUTABLE ghc)
	find_program (CABAL_EXECUTABLE cabal)
	find_program (ALEX_EXECUTABLE alex)
	find_program (HAPPY_EXECUTABLE happy)
	find_program (C2HS_EXECUTABLE c2hs)
	find_program (GHC-PKG_EXECUTABLE ghc-pkg)
else (HASKELL_USE_CABAL)
	if (STACK_EXECUTABLE)
		execute_process (COMMAND ${STACK_EXECUTABLE} path --bin-path
				 OUTPUT_VARIABLE STACK_BIN_PATH
				 OUTPUT_STRIP_TRAILING_WHITESPACE
				 WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
		string (REPLACE ":"
				";"
				STACK_BIN_PATH
				${STACK_BIN_PATH})
		# find the programs in stack's path instead
		find_program (GHC_EXECUTABLE ghc PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
		find_program (CABAL_EXECUTABLE cabal PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
		find_program (ALEX_EXECUTABLE alex PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
		find_program (HAPPY_EXECUTABLE happy PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
		find_program (C2HS_EXECUTABLE c2hs PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
		find_program (GHC-PKG_EXECUTABLE ghc-pkg PATHS ${STACK_BIN_PATH} NO_DEFAULT_PATH)
	endif (STACK_EXECUTABLE)
endif (HASKELL_USE_CABAL)

set (HASKELL_FOUND 0)
if (NOT HASKELL_USE_CABAL AND NOT STACK_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "stack not found")
	return ()
endif (NOT HASKELL_USE_CABAL AND NOT STACK_EXECUTABLE)
if (NOT CABAL_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "cabal not found")
	return ()
endif (NOT CABAL_EXECUTABLE)
if (NOT C2HS_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "c2hs not found")
	return ()
endif (NOT C2HS_EXECUTABLE)
if (NOT ALEX_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "alex not found")
	return ()
endif (NOT ALEX_EXECUTABLE)
if (NOT HAPPY_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "happy not found")
	return ()
endif (NOT HAPPY_EXECUTABLE)
if (NOT GHC_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "GHC not found")
	return ()
endif (NOT GHC_EXECUTABLE)
if (NOT GHC-PKG_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "ghc-pkg not found")
	return ()
endif (NOT GHC-PKG_EXECUTABLE)

# check for hspec and QuickCheck ghc-pkg return code is 0 on success, 1 otherwise
execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest hspec
		 RESULT_VARIABLE GHC_HSPEC_FOUND
		 OUTPUT_QUIET
		 ERROR_QUIET)

execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest QuickCheck
		 RESULT_VARIABLE GHC_QUICKCHECK_FOUND
		 OUTPUT_QUIET
		 ERROR_QUIET)

execute_process (COMMAND ${GHC_EXECUTABLE} --print-target-platform
		 OUTPUT_VARIABLE GHC_TARGET_PLATFORM
		 OUTPUT_STRIP_TRAILING_WHITESPACE)

# correct the mapping..
string (REPLACE "apple-darwin"
		"osx"
		GHC_TARGET_PLATFORM
		${GHC_TARGET_PLATFORM})
string (REPLACE "unknown-linux"
		"linux"
		GHC_TARGET_PLATFORM
		${GHC_TARGET_PLATFORM})

# normalize the result variables, 0 means success which corresponds to 1 in cmake booleans
if (GHC_HSPEC_FOUND EQUAL 0)
	set (GHC_HSPEC_FOUND 1)
else (GHC_HSPEC_FOUND EQUAL 0)
	set (GHC_HSPEC_FOUND 0)
endif (GHC_HSPEC_FOUND EQUAL 0)

if (GHC_QUICKCHECK_FOUND EQUAL 0)
	set (GHC_QUICKCHECK_FOUND 1)
else (GHC_QUICKCHECK_FOUND EQUAL 0)
	set (GHC_QUICKCHECK_FOUND 0)
endif (GHC_QUICKCHECK_FOUND EQUAL 0)

execute_process (COMMAND ${GHC_EXECUTABLE} --numeric-version
		 OUTPUT_VARIABLE GHC_VERSION
		 OUTPUT_STRIP_TRAILING_WHITESPACE)

# find the default cabal installation path sort of hacky but i haven't found a uniform way of doing this first find the global cabal
# directory
execute_process (COMMAND ${CABAL_EXECUTABLE} --ignore-sandbox help
		 COMMAND tail -n1
		 COMMAND xargs dirname
		 OUTPUT_VARIABLE CABAL_LOCATION
		 OUTPUT_STRIP_TRAILING_WHITESPACE)
# filter the library path matching our ghc version, ignoring architectures for now
execute_process (COMMAND ls -F "${CABAL_LOCATION}/lib"
		 COMMAND grep ghc-${GHC_VERSION}
		 OUTPUT_VARIABLE GHC_DYNAMIC_LIBRARY_DIR
		 OUTPUT_STRIP_TRAILING_WHITESPACE)
set (CABAL_DYNLIB_PATH "${CABAL_LOCATION}/lib/${GHC_DYNAMIC_LIBRARY_DIR}")

# dependencies for the default cmake Setup.hs For cabal the system version should be used
set (CABAL_CUSTOM_SETUP "custom-setup
  setup-depends:
    Cabal      >= 1.24.0 && < 2.4  ,
    base       >= 4.9    && < 4.12 ,
    containers >= 0.5    && < 0.6  ,
    directory  >= 1.2    && < 1.4  ,
    process    >= 1.4    && < 1.7  ,
    binary     >= 0.8    && < 0.9")

set (HASKELL_SHARED_SANDBOX "$ENV{HASKELL_SHARED_SANDBOX}/.cabal-sandbox")
get_filename_component (HASKELL_SHARED_SANDBOX "${HASKELL_SHARED_SANDBOX}" REALPATH)
if (HASKELL_USE_CABAL)
	if (HASKELL_SHARED_SANDBOX AND IS_DIRECTORY "${HASKELL_SHARED_SANDBOX}")
		set (HASKELL_FOUND 1)
	else (HASKELL_SHARED_SANDBOX AND IS_DIRECTORY "${HASKELL_SHARED_SANDBOX}")
		if (IS_DIRECTORY "${CMAKE_BINARY_DIR}/.cabal-sandbox")
			set (HASKELL_SHARED_SANDBOX "${CMAKE_BINARY_DIR}/.cabal-sandbox")
			set (HASKELL_FOUND 1)
		else (IS_DIRECTORY "${CMAKE_BINARY_DIR}/.cabal-sandbox")
			set (HASKELL_NOTFOUND_INFO "cabal build enabled, but the sandbox is missing")
			set (HASKELL_FOUND 0)
		endif (IS_DIRECTORY "${CMAKE_BINARY_DIR}/.cabal-sandbox")
	endif (HASKELL_SHARED_SANDBOX AND IS_DIRECTORY "${HASKELL_SHARED_SANDBOX}")
else (HASKELL_USE_CABAL)
	# for stack we don't use a sandbox stack handles that itself and in case the dependencies haven't been installed properly it will
	# fail to find e.g. c2hs so this is good to go
	set (HASKELL_FOUND 1)
endif (HASKELL_USE_CABAL)

# By using cabal sandboxes we can install hspec and QuickCheck to the sandbox without any concerns as they are independent from the global
# environment. So they are not required. All set, have fun with haskell!

set (HASKELL_NOTFOUND_INFO "${HASKELL_NOTFOUND_INFO}, please refer to the readme in src/bindings/haskell/README.md")
