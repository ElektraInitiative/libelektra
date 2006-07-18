# $Id: Makefile.am 741 2006-04-16 10:54:57Z aviram $

testdir = $(develdocdir)/src/bindings/cpp/

lib_LTLIBRARIES = libelektra-cpp.la
libelektra_cpp_la_SOURCES = kdb.cpp key.cpp keyset.cpp include/kdb include/key include/keyset ../../include/kdb.h
libelektra_cpp_la_LDFLAGS = -avoid-version -module
libelektra_cpp_la_LIBADD = ../../libelektra/libelektra.la
libelektra_cpp_la_CFLAGS = -DDEBUG
INCLUDES = -I../../include -Iinclude

EXTRA_DIST = test.cpp

test_PROGRAMS = test

test_SOURCES = test.cpp
test_LDADD = libelektra-cpp.la ../../libelektra/libelektra.la

