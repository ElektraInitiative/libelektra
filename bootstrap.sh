#!/bin/sh

# Bootstraping Elektra
echo "BOOTSTRAPING ELEKTRA"
echo "LIBTOOLIZE"
libtoolize

echo "ACLOCAL"
aclocal

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
automake --add-missing

echo "AUTOCONF"
autoconf

# Bootstraping libltdl
echo "BOOTSTRAPING LIBLTDL"
cd libltdl
echo "LIBTOOLIZE"
libtoolize

echo "ACLOCAL"
aclocal

echo "AUTOHEADER"
autoheader

echo "AUTOMAKE"
automake --add-missing

echo "AUTOCONF"
autoconf

cd ..
