#!/bin/sh

VALIDATOR=/usr/bin/xmllint

[ "z$srcdir" = 'z' ] && srcdir=.

SCHEMA=$srcdir/../xmlschema/elektra.xsd

[ -x "$VALIDATOR" ] || exit 0

echo
echo ELEKTRA VALIDATE TESTS
echo ========================================
echo

$VALIDATOR --schema "$SCHEMA" $srcdir/key.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" $srcdir/keyset.xml > /dev/null || exit 5

$VALIDATOR --schema "$SCHEMA" $srcdir/filesys.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" $srcdir/fstab.xml > /dev/null || exit 5
$VALIDATOR --schema "$SCHEMA" $srcdir/passwd.xml > /dev/null || exit 5

exit 0

