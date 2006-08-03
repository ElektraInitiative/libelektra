
# $Id$

SUBDIRS = filesys fstab berkeleydb daemon

if EXPERIMENTAL
SUBDIRS += gconf ini template
endif

install-exec-hook:
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-ddefault.so || \
	$(LN_S) libelektra-@default_dbackend@.so libelektra-ddefault.so

