AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

kdb_source = kdb-tool.c kdb-tool.h help.c BSDgetopt.c BSDgetopt.h ../include/kdb.h ../include/kdbloader.h ../include/kdbtools.h

bin_PROGRAMS = kdb
kdb_SOURCES = $(kdb_source)
kdb_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)
kdb_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

if ENABLE_STATIC
# cant build statically when not having libxml
if HAVE_XML
bin_PROGRAMS = kdb_static
kdb_static_SOURCES =  $(kdb_source)
kdb_static_LDADD = $(privatelibs) ../libelektra/libelektra.la $(LIBICONV)
kdb_static_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.la:
	cd ../libelektra && $(MAKE) libelektra.la
endif
endif

clean-local:
	rm -f *.gcno *.gcda *.gcno

MAINTAINERCLEANFILES = \
	Makefile.in
