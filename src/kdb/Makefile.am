
# $Id$

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = kdb.c help.c ../include/kdb.h
kdb_LDFLAGS = -L../libelektra
kdb_LDADD = -lelektra $(LIBLTDL)
kdb_CFLAGS = -Wall
AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

kdb_static_SOURCES = kdb.c help.c ../include/kdb.h
kdb_static_CFLAGS = -D__STATIC -Wall
kdb_static_LDADD = ../libelektra/libelektra.a
