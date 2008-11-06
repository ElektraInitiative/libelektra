# Svn stuff
# $Id$
# $LastChangedBy: rayman $

#not sure what the hell I need CPP flags for since there is no cpp needed?
#AM_CPPFLAGS = @CPPFLAGS@ -D_REENTRANT
debug_parms = @debug_parms@

AM_CFLAGS = @CFLAGS@ -W -Wall -Wstrict-prototypes -Wpointer-arith -Wundef $(debug_parms)


TOOLS = passwdconvert
EXTRA_DIST = addtestuser.sh

tools_PROGRAMS = $(TOOLS)
toolsdir = @sbindir@
passwdconvert_SOURCES = passwdconvert.c
passwdconvert_LDFLAGS = -lelektra

MAINTAINERCLEANFILES = \
	Makefile.in
