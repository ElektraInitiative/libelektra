#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA DOCU CONSISTENCY CHECK
echo

DOC_DIR="@CMAKE_SOURCE_DIR@/doc"

for DEV_DOCU in $(ls "$DOC_DIR/dev" | grep -v README.md); do
	grep -Eq "(${DEV_DOCU})" "$DOC_DIR/dev/README.md"
	succeed_if "Failed to find link to ${DEV_DOCU} in the file $DOC_DIR/dev/README.md"
done

for DECISION_DOCU in $(ls "$DOC_DIR/decisions" | grep -v README.md); do
	grep -Eq "(${DECISION_DOCU})" "$DOC_DIR/decisions/README.md"
	succeed_if "Failed to find link to ${DECISION_DOCU} in the file $DOC_DIR/decisions/README.md"
done

end_script
