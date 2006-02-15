# $Id$
#
# NOTE: on INCLUDES i added $(XML_CFLAGS) too because
# it seems previous version of libxml macro export
# path in XML_CFLAGS rather than XML_CPPFLAGS
# -- Yannick

if HAVE_XML

ulib_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = kdbtools.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h
libelektratools_la_CPPFLAGS = -DDATADIR=\"@datadir@\" -Wall
libelektratools_la_LDFLAGS = -version-info 1:0:0
libelektratools_la_LIBADD = $(XML_LIBS)
INCLUDES = -I../include $(XML_CPPFLAGS) $(XML_CFLAGS)

endif
