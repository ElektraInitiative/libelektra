
# $Id$

SUBDIRS = filesys berkeleydb 

if EXPERIMENTAL
SUBDIRS += gconf ini template
endif

if HAVE_GLIBC_MNTENT
SUBDIRS += fstab
endif

if HAVE_UCRED
SUBDIRS += daemon
endif

install-exec-hook:
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-ddefault.so || \
	$(LN_S) libelektra-@default_dbackend@.so libelektra-ddefault.so

