AM_CPPFLAGS =  -DDATADIR=\"$(datadir)\" \
	-I$(top_srcdir)/src/include $(LTDLINCL)

preload_source = preload.c ../include/kdb.h

bin_PROGRAMS = preload preload_static
preload_SOURCES = $(preload_source)
preload_LDADD = ../libelektra/libelektra.la $(LIBLTDL) $(LIBICONV)
preload_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

preload_static_SOURCES =  $(preload_source)
preload_static_LDADD = ../libelektra/libelektra.a $(privatelibs) $(LIBICONV)
preload_static_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

../libelektra/libelektra.a:
	cd ../libelektra && $(MAKE) libelektra.a

clean-local:
	rm -f *.gcno *.gcda *.gcno
