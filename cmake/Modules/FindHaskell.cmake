# Find the haskell environment required for the haskell bindings and plugins.
# Only searches for the GHC compiler and not other haskell compilers as it is
# the most widespread and advanced haskell compiler.
#
#  CABAL_EXECUTABLE      - Path to the cabal executable
#  C2HS_EXECUTABLE	     - Path to the c2hs executable
#  GHC_EXECUTABLE        - Path to the ghc executable
#  GHC-PKG_EXECUTABLE    - Path to the ghc-pkg executable
#  GHC_HSPEC_FOUND       - True if the hspec library is available
#  GHC_QUICKCHECK_FOUND  - True if the QuickCheck library is available
#  HASKELL_FOUND         - True if the whole required haskell environment exists
#    This variable is set to true if CABAL_EXECUTABLE, C2HS_EXECUTABLE, GHC_EXECUTABLE
#    and GHC-PKG_EXECUTABLE are all available. If BUILD_TESTING is enabled, it also
#    requires GHC_HSPEC_FOUND and GHC_QUICKCHECK_FOUND to be true.
#  HASKELL_NOTFOUND_INFO - A string describing which haskell dependency is missing
#

find_program (GHC_EXECUTABLE ghc)
find_program (CABAL_EXECUTABLE cabal)
find_program (C2HS_EXECUTABLE c2hs)
find_program (GHC-PKG_EXECUTABLE ghc-pkg)

set (HASKELL_FOUND 0)
if (CABAL_EXECUTABLE)
if (C2HS_EXECUTABLE)
if (GHC_EXECUTABLE)
if (GHC-PKG_EXECUTABLE)
	
	# check for hspec and QuickCheck
	# ghc-pkg return code is 0 on success, 1 otherwise
	execute_process (
		COMMAND ${GHC-PKG_EXECUTABLE} latest hspec
		RESULT_VARIABLE GHC_HSPEC_FOUND
		OUTPUT_QUIET ERROR_QUIET
	)

	execute_process (
		COMMAND ${GHC-PKG_EXECUTABLE} latest QuickCheck
		RESULT_VARIABLE GHC_QUICKCHECK_FOUND
		OUTPUT_QUIET ERROR_QUIET
	)

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

	# By using cabal sandboxes we can install hspec and QuickCheck to the sandbox without
	# any concerns as they are independent from the global environment. So they are not required.
	# All set, have fun with haskell!
	set (HASKELL_FOUND 1)

else (GHC-PKG_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "ghc-pkg not found")
endif (GHC-PKG_EXECUTABLE)
else (GHC_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "GHC not found")
endif (GHC_EXECUTABLE)
else (C2HS_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "c2hs not found")
endif (C2HS_EXECUTABLE)
else (CABAL_EXECUTABLE)
	set (HASKELL_NOTFOUND_INFO "cabal not found")
endif (CABAL_EXECUTABLE)

set (HASKELL_NOTFOUND_INFO "${HASKELL_NOTFOUND_INFO}, please refer to the readme in src/bindings/haskell/README.md")

mark_as_advanced (
	GHC_EXECUTABLE
	GHC-PKG_EXECUTABLE
	C2HS_EXECUTABLE
	CABAL_EXECUTABLE
	GHC_HSPEC_FOUND
	GHC_QUICKCHECK_FOUND
)
