file (GLOB HDR_FILES *.hpp)

install (
	FILES ${HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT libelektra-dev)

file (GLOB ERROR_HDR_FILES errors/*.hpp)

install (
	FILES ${ERROR_HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}/errors
	COMPONENT libelektra-dev)

file (GLOB HELPER_HDR_FILES helper/*.hpp)

install (
	FILES ${HELPER_HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}/helper
	COMPONENT libelektra-dev)

file (GLOB MERGING_HDR_FILES merging/*.hpp)

install (
	FILES ${MERGING_HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}/merging
	COMPONENT libelektra-dev)
