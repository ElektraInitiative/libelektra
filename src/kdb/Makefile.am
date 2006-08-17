
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

kdb_source = kdb.c help.c BSDgetopt.c BSDgetopt.h ../include/kdb.h

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = $(kdb_source)
kdb_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)

kdb_static_SOURCES =  $(kdb_source)
kdb_static_CFLAGS = -DELEKTRA_STATIC
kdb_static_LDADD = $(privatelibs) ../libelektra/libelektra.a $(LIBICONV)

../libelektra/libelektra.a:
	cd ../libelektra && $(MAKEà libelektra.a
