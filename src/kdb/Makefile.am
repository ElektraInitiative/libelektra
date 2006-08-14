
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = kdb.c help.c ../include/kdb.h
kdb_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)

kdb_static_SOURCES = kdb.c help.c ../include/kdb.h
kdb_static_CFLAGS = -DELEKTRA_STATIC
kdb_static_LDADD = $(privatelibs) ../libelektra/libelektra.a $(LIBICONV)
