
# $Id$

noinst_LTLIBRARIES = libproto.la
libproto_la_SOURCES = argument.c message.c serial_bin.c protocol.c
libproto_la_LDFLAGS = -version-info 1:0:0


backend_LTLIBRARIES = libelektra-daemon.la
libelektra_daemon_la_SOURCES = daemon.c
libelektra_daemon_la_LDFLAGS = -avoid-version -module
libelektra_daemon_la_LIBADD = libproto.la

bin_PROGRAMS = kdbd
kdbd_SOURCES = kdbd.c
kdbd_LDFLAGS = -L../../libelektra/
kdbd_LDADD = libproto.la -lelektra

INCLUDES = -I../../include

