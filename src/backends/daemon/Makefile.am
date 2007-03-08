
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include -DLOCALSTATEDIR="\"$(localstatedir)\"" -DKDBD_NAME='"'`echo kdbd | sed 's|^.*/||;$(transform)'`'"'

noinst_LIBRARIES = libelektra-daemon.a 
noinst_LTLIBRARIES = libproto.la

libproto_la_SOURCES = message.h message.c protocol.h protocol.c ipc.h ipc.c sig.h sig.c serial.h serial.c serial_int.h serial_int.c serial_string.h serial_string.c serial_key.h serial_key.c serial_keyset.h serial_keyset.c datatype.h

libelektra_daemon_a_SOURCES = daemon.c ../../include/kdb.h ../../include/kdbbackend.h $(libproto_la_SOURCES)
libelektra_daemon_a_CFLAGS = -DELEKTRA_STATIC


backend_LTLIBRARIES = libelektra-daemon.la
libelektra_daemon_la_SOURCES = daemon.c
libelektra_daemon_la_LDFLAGS = -avoid-version -module
libelektra_daemon_la_LIBADD = ../../libelektra/libelektra.la libproto.la

sbin_PROGRAMS = kdbd
# kdbd_static
kdbd_SOURCES = main.c kdbd.h kdbd.c kdb_wrapper.h kdb_wrapper.c thread.h thread.c
kdbd_CFLAGS = -DDEFAULT_DBACKEND=\"ddefault\"
kdbd_LDADD = libproto.la ../../libelektra/libelektra.la -lpthread $(LIBICONV)

# kdbd_static_SOURCES = main.c kdbd.g kdbd.c kdb_wrapper.h kdb_wrapper.c thread.h thread.c
# kdbd_static_CFLAGS = -DELEKTRA_STATIC -DDEFAULT_DBACKEND=\"$(default_dbackend)\"
# kdbd_static_LDADD = @privatelibs@ libproto.la ../../libelektra/libelektra.a -lpthread

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

# ../../libelektra/libelektra.a:
#	cd ../../libelektra/ && $(MAKE) libelektra.a



