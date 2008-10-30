
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

kdb_source = kdb-tool.c kdb-tool.h help.c BSDgetopt.c BSDgetopt.h ../include/kdb.h ../include/kdbloader.h ../include/kdbtools.h

if ENABLE_STATIC
KDB_STATIC=kdb_static
else
KDB_STATIC=
endif

bin_PROGRAMS = kdb $(KDB_STATIC)
kdb_SOURCES = $(kdb_source)
kdb_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)
kdb_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

if ENABLE_STATIC
kdb_static_SOURCES =  $(kdb_source)
kdb_static_LDADD = $(privatelibs) ../libelektra/libelektra.a $(LIBICONV)
kdb_static_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.a:
	cd ../libelektra && $(MAKE) libelektra.a
endif

clean-local:
	rm -f *.gcno *.gcda *.gcno
