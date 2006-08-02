
# $Id$

SUBDIRS = filesys fstab berkeleydb daemon

if EXPERIMENTAL
SUBDIRS += gconf ini template
endif

objects: all
	$(SHELL) exportobjects.sh $(SUBDIRS)

symbols: all
	$(SHELL) exportsymbols.sh $(SUBDIRS)

clean-local:
	-rm -f objects
	-rm -f backend_symbols.c

install-exec-hook:
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-ddefault.so || \
	$(LN_S) libelektra-@default_dbackend@.so libelektra-ddefault.so

