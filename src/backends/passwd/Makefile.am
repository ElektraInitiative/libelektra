#elekpasswddir = $(develdocdir)/backend-passwd/

AM_CPPFLAGS = -I$(top_srcdir)/src/include

sources = passwd.h passwd.c

noinst_LIBRARIES = libelektra-passwd.a
libelektra_passwd_a_SOURCES = $(sources) ../../include/kdb.h ../../include/kdbbackend.h
libelektra_passwd_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)


backend_LTLIBRARIES = libelektra-passwd.la
libelektra_passwd_la_SOURCES = $(sources) ../../include/kdb.h ../../include/kdbbackend.h
libelektra_passwd_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_passwd_la_LIBADD = ../../libelektra/libelektra.la
libelektra_passwd_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

MAINTAINERCLEANFILES = \
	Makefile.in
