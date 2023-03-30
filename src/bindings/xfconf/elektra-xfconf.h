#ifndef ELEKTRA_XFCONF_H
#define ELEKTRA_XFCONF_H

#include <kdb.h>
#include <xfconf/xfconf.h>

#define I_(string) (g_intern_static_string ((string)))
#define XFCONF_NAMESPACE "system:"
#define XFCONF_ROOT "/sw/xfce4"
#define XFCONF_GTYPE_META_NAME "gtype"

#define XFCONF_NUM_BUF_SIZE 64

#define XFCONF_PERSIST_DEFAULT 1

extern GList * channel_list;
extern KDB * elektraKdb;
extern pthread_rwlock_t channel_lock;


#define require_read_lock()                                                                                                                \
	g_debug ("acquiring read lock in %s on line %d...", __func__, __LINE__);                                                           \
	if (pthread_rwlock_rdlock (&channel_lock) != 0)                                                                                    \
	{                                                                                                                                  \
		g_error ("unable to acquire read lock in %s on line %d", __func__, __LINE__);                                              \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("acquired read lock in %s on line %d", __func__, __LINE__);                                                       \
	}

#define require_write_lock()                                                                                                               \
	g_debug ("acquiring write lock in %s on line %d...", __func__, __LINE__);                                                          \
	if (pthread_rwlock_wrlock (&channel_lock) != 0)                                                                                    \
	{                                                                                                                                  \
		g_warning ("unable to acquire write lock in %s on line %d", __func__, __LINE__);                                           \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("acquired write lock in %s on line %d", __func__, __LINE__);                                                      \
	}

#define release_lock()                                                                                                                     \
	if (pthread_rwlock_unlock (&channel_lock) != 0)                                                                                    \
	{                                                                                                                                  \
		g_warning ("unable to release lock in %s on line %d, subsequent locks may fail", __func__, __LINE__);                      \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("released lock in %s on line %d", __func__, __LINE__);                                                            \
	}

typedef struct
{
	XfconfChannel * channel;
	KeySet * keySet;
} ChannelKeySetPair;

#endif
