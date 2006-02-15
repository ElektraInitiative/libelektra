
# $Id$

lib_LIBRARIES = libelektra-filesys.a
libelektra_filesys_a_SOURCES = filesys.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_a_CFLAGS = -D__STATIC -Wall


lib_LTLIBRARIES = libelektra-filesys.la
libelektra_filesys_la_SOURCES = filesys.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_la_LDFLAGS = -version-info 1:0:0 -module
libelektra_filesys_la_CFLAGS = -Wall
INCLUDES = -I../../include
