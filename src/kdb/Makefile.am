
# $Id$

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = kdb.c ../include/kdb.h
kdb_LDFLAGS = -L../libelektra
kdb_LDADD = ../libloader/kdbLibLoader.lo -lelektra $(LIBLTDL)
kdb_CFLAGS = -Wall
INCLUDES = -I../include $(LTDLINCL)

kdb_static_SOURCES = kdb.c ../include/kdb.h
kdb_static_CFLAGS = -D__STATIC -Wall
kdb_static_LDADD = ../libelektra/libelektra.a 
