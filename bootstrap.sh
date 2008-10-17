#!/bin/sh

AUTOMAKE=`which automake-1.9`
ACLOCAL=`which aclocal-1.9`

die()
{
	echo $*
	exit 1
}

#automake must be version 1.9
#make some tests for correct automake
[ -f $AUTOMAKE ] || die could not find automake-1.9
[ -f $ACLOCAL ] || die could not find aclocal-1.9
$AUTOMAKE --version | grep 1.9 || die wrong automake
$ACLOCAL --version | grep 1.9 || die wrong aclocal

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
$AUTOMAKE --add-missing

echo "AUTOCONF"
autoconf

cd ..
