if HAVE_XML

AM_CPPFLAGS = -DDATADIR=\"$(datadir)\" \
 -I$(top_srcdir)/src/include $(libxml2_CFLAGS)

elektratools_sources = kdbtools.c stream.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h

hlvlbackend_LIBRARIES = libelektratools.a
libelektratools_a_SOURCES = $(elektratools_sources)
libelektratools_a_LIBADD = $(libxml2_LIBS)
libelektratools_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

hlvlbackend_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = $(elektratools_sources)
libelektratools_la_LDFLAGS = -version-info $(ELEKTRATOOLS_VERSION_API)
libelektratools_la_LIBADD = ../libelektra/libelektra.la $(libxml2_LIBS)
libelektratools_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.la:
	cd ../libelektra && $(MAKE) libelektra.la

clean-local:
	rm -f *.gcno *.gcda *.gcno

endif

MAINTAINERCLEANFILES = \
	Makefile.in
