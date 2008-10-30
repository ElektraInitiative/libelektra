AM_CPPFLAGS =  -DDATADIR=\"$(datadir)\" \
	-I$(top_srcdir)/src/include $(LTDLINCL)

preload_source = preload.c ../include/kdb.h

if ENABLE_STATIC
PRELOAD_STATIC=preload_static
else
PRELOAD_STATIC=
endif

bin_PROGRAMS = preload $(PRELOAD_STATIC)
preload_SOURCES = $(preload_source)
preload_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)
preload_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

if ENABLE_STATIC
preload_static_SOURCES =  $(preload_source)
preload_static_LDADD = $(privatelibs) ../libelektra/libelektra.a $(LIBICONV)
preload_static_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.a:
	cd ../libelektra && $(MAKE) libelektra.a
endif

clean-local:
	rm -f *.gcno *.gcda *.gcno
