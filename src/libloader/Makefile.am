# $Id$

AM_CPPFLAGS = -DBACKEND_LIBDIR="\"$(backenddir):$(hlvlbackenddir):$(ulibdir):$(libdir)\""

INCLUDES = -I$(top_srcdir)/src/include

if HAVE_LIBLTDL
INCLUDES += $(INCLTDL)
endif

noinst_LIBRARIES = libloader-static.a
libloader_static_a_SOURCES = kdbLibLoader.c ../include/kdbloader.h ../include/ltdl.h
libloader_static_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

noinst_LTLIBRARIES = libloader-dynamic.la
libloader_dynamic_la_SOURCES = kdbLibLoader.c ../include/kdbloader.h ../include/ltdl.h
libloader_dynamic_la_LIBADD = $(LIBLTDL)
libloader_dynamic_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

