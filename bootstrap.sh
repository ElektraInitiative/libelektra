#!/bin/sh
#this script bootstraps elektra
echo "BOOTSTRAPING ELEKTRA"

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

# Bootstraping Elektra
echo "LIBTOOLIZE"
libtoolize --ltdl --copy

echo "ACLOCAL"
aclocal -I m4

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
automake --add-missing -c

echo "AUTOCONF"
autoconf

# Bootstraping libltdl
echo "BOOTSTRAPING LIBLTDL"

cd libltdl

echo "ACLOCAL"
aclocal

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
automake --add-missing -c

echo "AUTOCONF"
autoconf

cd ..
