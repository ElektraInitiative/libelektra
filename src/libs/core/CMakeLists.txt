include (LibAddLib)

set (
	SOURCES
	cow.c
	errors.c
	key.c
	keyhelpers.c
	keymeta.c
	keyname.c
	keyset.c
	keytest.c
	keyvalue.c
	proposal.c
	rand.c)

if (ENABLE_OPTIMIZATIONS)
	list (APPEND SOURCES opmphm.c opmphmpredictor.c)
endif (ENABLE_OPTIMIZATIONS)

add_lib (core SOURCES ${SOURCES} COMPONENT libelektra${SO_VERSION})
if (BUILD_SHARED)
	target_link_libraries (elektra-core $<TARGET_OBJECTS:elektra-base>)
endif (BUILD_SHARED)
