# NOTE: on INCLUDES i added $(XML_CFLAGS) too because
# it seems previous version of libxml macro export
# path in XML_CFLAGS rather than XML_CPPFLAGS
# -- Yannick

if HAVE_XML

AM_CPPFLAGS = -DDATADIR=\"$(datadir)\" \
 -I$(top_srcdir)/src/include $(XML_CPPFLAGS) $(XML_CFLAGS)

elektratools_sources = kdbtools.c stream.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h

hlvlbackend_LIBRARIES = libelektratools.a
libelektratools_a_SOURCES = $(elektratools_sources)
libelektratools_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

hlvlbackend_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = $(elektratools_sources)
libelektratools_la_LDFLAGS = -version-info $(ELEKTRATOOLS_VERSION_API)
libelektratools_la_LIBADD = ../libelektra/libelektra.la $(XML_LIBS)
libelektratools_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

endif

