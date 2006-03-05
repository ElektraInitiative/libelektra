
# $Id$

SUBDIRS = filesys fstab berkeleydb

if EXPERIMENTAL
SUBDIRS += daemon gconf template ini
endif

install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
