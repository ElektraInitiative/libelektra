
# $Id$

SUBDIRS = berkeleydb filesys fstab ini gconf template

install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
