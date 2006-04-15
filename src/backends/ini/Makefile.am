# $Id$

testdir = $(develdocdir)/src/backends/ini/

lib_LIBRARIES = libelektra-ini.a
libelektra_ini_a_SOURCES = ini.c helpers.c parser.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_ini_a_CFLAGS = -DDEBUG -D_POSIX_SOURCE -D_BSD_SOURCE -D__STATIC -ansi -pedantic -Wall

lib_LTLIBRARIES = libelektra-ini.la
libelektra_ini_la_SOURCES = ini.c helpers.c parser.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_ini_la_LDFLAGS = -version-info 2:0:0 -module
libelektra_ini_la_CFLAGS = -DDEBUG -D_POSIX_SOURCE -D_BSD_SOURCE -ansi -pedantic -Wall
INCLUDES = -I../../include

EXTRA_DIST = test.c

test_PROGRAMS = test

test_SOURCES = test.c
test_LDADD = -lelektra -lelektra-ini

