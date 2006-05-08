# $Id: Makefile.am 741 2006-04-16 10:54:57Z aviram $

testdir = $(develdocdir)/src/bindings/cpp/

lib_LTLIBRARIES = libelektra-cpp.la
libelektra_cpp_la_SOURCES = ckdb.cpp ../../include/kdb.h
libelektra_cpp_la_LDFLAGS = -avoid-version -module
libelektra_cpp_la_LIBADD = -lelektra
libelektra_cpp_la_CFLAGS = -DDEBUG
INCLUDES = -I../../include

EXTRA_DIST = test.cpp

test_PROGRAMS = test

test_SOURCES = test.cpp
test_LDADD = -lelektra-cpp -lelektra

