AM_CPPFLAGS = -I$(top_srcdir)/src/include

sources = internal.c helper.c helper.h ../include/kdb.h ../include/kdbbackend.h

noinst_LIBRARIES = libhelper-static.a
libhelper_static_a_SOURCES = $(sources)
libhelper_static_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)


noinst_LTLIBRARIES = libhelper-dynamic.la
libhelper_dynamic_la_SOURCES = $(sources)
libhelper_dynamic_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno
