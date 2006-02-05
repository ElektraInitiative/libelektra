
# $Id$

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = kdb.c ../include/kdb.h
kdb_LDFLAGS = -L../libelektra
kdb_LDADD = -lelektra $(LIBLTDL)
INCLUDES = -I../include $(LTDLINCL)

kdb_static_SOURCES = kdb.c ../include/kdb.h
kdb_static_LDADD = ../libelektra/libelektra.a
