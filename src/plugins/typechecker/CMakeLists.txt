include (LibAddHaskellPlugin)

add_haskell_plugin (typechecker
	SANDBOX_ADD_SOURCES
	"${CMAKE_SOURCE_DIR}/src/libs/typesystem/SpecElektra"
	"${CMAKE_SOURCE_DIR}/src/libs/typesystem/SpecTranslator"
)
