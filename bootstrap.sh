#!/bin/sh
#this script bootstraps elektra

set -e

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
echo "BOOTSTRAPPING ELEKTRA"
echo "libtoolize"
libtoolize --ltdl --copy

echo "aclocal"
aclocal -I m4

echo "autoheader"
autoheader

echo "automake"
automake --add-missing --copy

echo "autoconf"
autoconf

# bootstrapping libltdl
echo "bootstrapping libltdl"

cd libltdl

echo "aclocal"
aclocal

echo "autoheader"
autoheader

echo "automake"
automake --add-missing --copy

echo "autoconf"
autoconf

cd ..

echo "Finished, now use ./configure or read INSTALL"
