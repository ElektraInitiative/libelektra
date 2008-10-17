DIST_SUBDIRS = filesys berkeleydb daemon fstab passwd hosts gconf ini template
SUBDIRS = @BACKENDS@

EXTRA_DIST = winregistry doc/backend.c

install-exec-hook:
	cd $(DESTDIR)$(backenddir) && \
	test -L libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so
	cd $(DESTDIR)$(backenddir) && \
	test -L libelektra-ddefault.so || \
	$(LN_S) libelektra-@default_dbackend@.so libelektra-ddefault.so

uninstall-hook:
	-cd $(DESTDIR)$(backenddir) && \
	rm -f libelektra-default.so
	-cd $(DESTDIR)$(backenddir) && \
	rm -f libelektra-ddefault.so

