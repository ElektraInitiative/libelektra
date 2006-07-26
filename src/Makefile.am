
# $Id$

# bin_PROGRAMS = localeinfo
# localeinfo_SOURCES = localeinfo.c

#AM_CFLAGS=-g -O2

all-local:
	(cd backends/$(default_backend) && $(MAKE) $(AM_MAKEFLAGS) libelektra_$(default_backend)_a-$(default_backend).$(OBJEXT))
if EXPERIMENTAL
	(cd backends/ini && $(MAKE) $(AM_MAKEFLAGS) \
		libelektra_ini_a-ini.$(OBJEXT) \
		libelektra_ini_a-helpers.$(OBJEXT) \
		libelektra_ini_a-parser.$(OBJEXT) \
	)
endif

SUBDIRS = . libloader libelektra backends libelektratools libregistry kdb include bindings
EXTRA_DIST = Doxyfile

