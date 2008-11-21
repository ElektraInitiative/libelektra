#!/bin/sh

set -e

VERSION=1.10

AUTOMAKE=`which automake-$VERSION`
ACLOCAL=`which aclocal-$VERSION`

die()
{
	echo $*
	exit 1
}

#automake must be version $VERSION
#make some tests for correct automake
[ -f $AUTOMAKE ] || die could not find automake-$VERSION
[ -f $ACLOCAL ] || die could not find aclocal-$VERSION
$AUTOMAKE --version | grep $VERSION || die wrong automake
$ACLOCAL --version | grep $VERSION || die wrong aclocal

#this script bootstraps elektra
echo "BOOTSTRAPPING ELEKTRA"

#Testing if in correct directory
test -f elektra.xml || exit 1

# Removing temporary directories
echo "CLEANING UP DIRECTORIES"
rm -rf autom4te.cache
rm -rf libltdl

# Removing temporary files
echo "CLEANING UP FILES"
rm -f aclocal.m4
rm -f compile
rm -f config.guess
rm -f config.log
rm -f config.status
rm -f config.sub
rm -f configure
rm -f depcomp
rm -f elektra.pc
rm -f elektra.spec
rm -f elektratools.pc
rm -f install-sh
rm -f libtool
rm -f ltmain.sh
rm -f Makefile
rm -f Makefile.in
rm -f missing
rm -f mkinstalldirs

# Bootstrapping Elektra
echo "LIBTOOLIZE"
libtoolize --ltdl --copy

echo "ACLOCAL"
$ACLOCAL -I m4

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
$AUTOMAKE --add-missing --copy

echo "AUTOCONF"
autoconf

# Bootstrapping libltdl
echo "BOOTSTRAPPING LIBLTDL"

cd libltdl

echo "ACLOCAL"
$ACLOCAL

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
$AUTOMAKE --add-missing --copy

echo "AUTOCONF"
autoconf

cd ..

echo "Finished, now use ./configure or read INSTALL"
