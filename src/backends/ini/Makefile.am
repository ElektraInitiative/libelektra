# $Id$

testdir = $(develdocdir)/src/backends/ini/

lib_LIBRARIES = libelektra-ini.a
libelektra_ini_a_SOURCES = ini.c helpers.c parser.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_ini_a_CFLAGS = -D__STATIC

lib_LTLIBRARIES = libelektra-ini.la
libelektra_ini_la_SOURCES = ini.c helpers.c parser.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_ini_la_LDFLAGS = -version-info 1:0:0 -module
INCLUDES = -I../../include

EXTRA_DIST = test.c

test_PROGRAMS = test

test_SOURCES = test.c
test_LDFLAGS = -L. -L../../libelektra
test_LDADD = -lelektra -lelektra-ini

