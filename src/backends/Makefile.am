
# $Id$

SUBDIRS = filesys fstab berkeleydb 

if EXPERIMENTAL
SUBDIRS += daemon gconf ini
endif

install-exec-hook:
	cd $(DESTDIR)$(backenddir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
