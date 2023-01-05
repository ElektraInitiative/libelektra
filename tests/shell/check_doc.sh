#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA DOCU CONSISTENCY CHECK
echo

DOC_DIR="@CMAKE_SOURCE_DIR@/doc"

if test -f "$DOC_DIR/dev/README.md"; then
	for DEV_DOCU in $(ls "$DOC_DIR/dev" | grep -v README.md); do
		grep -Eq "(\(${DEV_DOCU}\))" "$DOC_DIR/dev/README.md"
		succeed_if "Failed to find link to ${DEV_DOCU} in the file $DOC_DIR/dev/README.md"
	done
else
	exit_if_fail "$DOC_DIR/dev/README.md not found!"
fi

if test -f "$DOC_DIR/tutorials/README.md"; then
	for TUTORIAL_DOCU in $(ls "$DOC_DIR/tutorials" | grep -v README.md); do
		grep -Eq "(\(${TUTORIAL_DOCU}\))" "$DOC_DIR/tutorials/README.md"
		succeed_if "Failed to find link to ${TUTORIAL_DOCU} in the file $DOC_DIR/tutorials/README.md"
	done
else
	exit_if_fail "$DOC_DIR/tutorials/README.md not found!"
fi

if test -f "$DOC_DIR/contrib/README.md"; then
	for CONTRIB_DOCU in $(ls "$DOC_DIR/contrib" | grep -v README.md); do
		grep -Eq "(${CONTRIB_DOCU})" "$DOC_DIR/contrib/README.md"
		succeed_if "Failed to find link to ${CONTRIB_DOCU} in the file $DOC_DIR/contrib/README.md"
	done
else
	exit_if_fail "$DOC_DIR/contrib/README.md not found!"
fi

end_script
