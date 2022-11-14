#!/bin/sh

VALIDATOR=/usr/bin/xmllint

[ "z$srcdir" = 'z' ] && srcdir=$(dirname "$0")

SCHEMA=$srcdir/xmlschema/elektra.xsd

if [ ! -f "$SCHEMA" ]; then
	echo "Schema $SCHEMA not found, will do nothing"
	exit 0
fi

if [ ! -x "$VALIDATOR" ]; then
	echo "Tool $VALIDATOR not found, will do nothing"
	exit 0
fi

echo
echo ELEKTRA VALIDATE TESTS
echo ========================================
echo

$VALIDATOR --schema "$SCHEMA" "$srcdir"/xmltool/key.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" "$srcdir"/xmltool/keyset.xml > /dev/null || exit 5

$VALIDATOR --schema "$SCHEMA" "$srcdir"/xmltool/filesys.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" "$srcdir"/xmltool/fstab.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" "$srcdir"/xmltool/passwd.xml > /dev/null || exit 5

exit 0
