# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)\
  -DBACKEND_LIBDIR="\"$(backenddir):$(hlvlbackenddir)\""

noinst_LIBRARIES = libloader-static.a
libloader_static_a_SOURCES = kdbLibLoader.c ../include/kdbLibLoader.h
libloader_static_a_CFLAGS = -DELEKTRA_STATIC

noinst_LTLIBRARIES = libloader-dynamic.la
libloader_dynamic_la_SOURCES = kdbLibLoader.c ../include/kdbLibLoader.h
