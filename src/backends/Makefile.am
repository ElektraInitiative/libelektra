
# $Id$

SUBDIRS = filesys fstab

if EXPERIMENTAL
SUBDIRS += daemon gconf template ini  berkeleydb
endif

install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
