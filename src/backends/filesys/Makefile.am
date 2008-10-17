AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-filesys.a
libelektra_filesys_a_SOURCES = filesys.c filesys.h ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)


backend_LTLIBRARIES = libelektra-filesys.la
libelektra_filesys_la_SOURCES = filesys.c filesys.h ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_filesys_la_LIBADD = ../../libelektra/libelektra.la
libelektra_filesys_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la
