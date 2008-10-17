#elekhostsdir = $(develdocdir)/backend-hosts/

#EXTRA_DIST = hosts.c README

AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-hosts.a
libelektra_hosts_a_SOURCES = hosts.c hosts.h ../../include/kdb.h ../../include/kdbbackend.h
libelektra_hosts_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)


backend_LTLIBRARIES = libelektra-hosts.la
libelektra_hosts_la_SOURCES = hosts.c hosts.h ../../include/kdb.h ../../include/kdbbackend.h
libelektra_hosts_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_hosts_la_LIBADD = ../../libelektra/libelektra.la
libelektra_hosts_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la
