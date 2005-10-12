#!/bin/sh

# Bootstraping
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
