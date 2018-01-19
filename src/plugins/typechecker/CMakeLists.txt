include (LibAddHaskellPlugin)

add_haskell_plugin (typechecker
	SANDBOX_ADD_SOURCES
	"${CMAKE_SOURCE_DIR}/src/libs/typesystem/SpecElektra"
	"${CMAKE_SOURCE_DIR}/src/libs/typesystem/SpecTranslator"
)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/Elektra/Typechecker.hs.in"
	"${CMAKE_CURRENT_SOURCE_DIR}/Elektra/Typechecker.hs"
	@ONLY
)

install (FILES ${CMAKE_SOURCE_DIR}/src/libs/typesystem/SpecElektra/Elektra/SpecElektra.hs DESTINATION ${KDB_DB_SPEC})
install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/simpleTestSpecification.ini DESTINATION ${KDB_DB_SPEC})
