
# $Id$

noinst_LTLIBRARIES = libproto.la
libproto_la_SOURCES = argument.c message.c serial_bin.c protocol.c
libproto_la_LDFLAGS = -version-info 1:0:0


lib_LTLIBRARIES = libelektra-daemon.la
libelektra_daemon2_la_SOURCES = daemon.c 
libelektra_daemon2_la_LDFLAGS = -version-info 1:0:0 -module
libelektra_daemon2_la_LIBADD = libproto.la

bin_PROGRAMS = kdbd
elektra_daemon_SOURCES = daemon-test.c
elektra_daemon_LDFLAGS = -L../../libelektra/
elektra_daemon_LDADD = libproto.la -lelektra

INCLUDES = -I../../include

