
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

kdb_source = kdb-tool.c kdb-tool.h help.c BSDgetopt.c BSDgetopt.h ../include/kdb.h ../include/kdbloader.h ../include/kdbtools.h

bin_PROGRAMS = kdb kdb_static
kdb_SOURCES = $(kdb_source)
kdb_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)
kdb_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

kdb_static_SOURCES =  $(kdb_source)
kdb_static_LDADD = ../libelektra/libelektra.a $(privatelibs) $(LIBICONV)
kdb_static_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.a:
	cd ../libelektra && $(MAKE) libelektra.a

clean-local:
	rm -f *.gcno *.gcda *.gcno
