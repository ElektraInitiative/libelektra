# $Id$

if HAVE_XML
ulibdir = $(exec_prefix)/usr/lib
ulib_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = kdbtools.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h
libelektratools_la_LDFLAGS = -version-info 1:0:0 
libelektratools_la_LIBADD = $(XML_LIBS)
INCLUDES = -I../include $(XML_CPPFLAGS)

endif
