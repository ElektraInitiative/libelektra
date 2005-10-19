#!/bin/sh

# Bootstraping Elektra
echo "BOOTSTRAPING ELEKTRA"
echo "LIBTOOLIZE"
libtoolize --force

echo "ACLOCAL"
aclocal --force

echo "AUTOHEADER"
autoheader --force

echo "AUTOMAKE"
automake --add-missing --force

echo "AUTOCONF"
autoconf --force

# Bootstraping libltdl
echo "BOOTSTRAPING LIBLTDL"
cd libltdl
echo "LIBTOOLIZE"
libtoolize --force

echo "ACLOCAL"
aclocal --force

echo "AUTOHEADER"
autoheader --force

echo "AUTOMAKE"
automake --add-missing --force

echo "AUTOCONF"
autoconf --force

cd ..
