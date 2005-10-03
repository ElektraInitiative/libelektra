SUBDIRS = berkeleydb filesys fstab ini gconf template

install-exec-hook:
	cd $(libdir) && \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so
