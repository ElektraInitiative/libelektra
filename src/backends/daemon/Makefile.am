
# $Id: Makefile.am 796 2006-06-01 21:44:44Z aviram $

noinst_LTLIBRARIES = libproto.la
libproto_la_SOURCES = message.c protocol.c ipc.c sig.c serial.c serial_int.c serial_string.c serial_key.c serial_keyset.c


backend_LTLIBRARIES = libelektra-daemon.la
libelektra_daemon_la_SOURCES = daemon.c
libelektra_daemon_la_LDFLAGS = -avoid-version -module
libelektra_daemon_la_LIBADD = libproto.la

bin_PROGRAMS = kdbd
kdbd_SOURCES = main.c kdbd.c kdb_wrapper.c thread.c
kdbd_LDFLAGS = -L../../libelektra/
kdbd_LDADD = libproto.la -lelektra -lpthread

INCLUDES = -I../../include

