#ifndef ELEKTRA_XFCONF_H
#define ELEKTRA_XFCONF_H

#include <elektra.h>
#include <kdb.h>
#include <xfconf/xfconf.h>

#define I_(string) (g_intern_static_string ((string)))
#define XFCONF_NAMESPACE "system:"
#define XFCONF_ROOT "/sw/org/xfce/xfconf"
#define XFCONF_GTYPE_META_NAME "gtype"

#define XFCONF_NUM_BUF_SIZE 64

#define XFCONF_PERSIST_DEFAULT 1
#define XFCONF_DEBUG_LOG_FOUND_KEYS 0
#define XFCONF_INDIVIDUAL_KEY_SETS 0

extern GList * channel_list;
extern KDB * elektraKdb;
extern KeySet * globalKeySet;
extern pthread_rwlock_t channel_lock;


#define require_read_lock(lock, name)                                                                                                      \
	g_debug ("acquiring read lock in %s on line %d...", __func__, __LINE__);                                                           \
	if (pthread_rwlock_rdlock (lock) != 0)                                                                                             \
	{                                                                                                                                  \
		g_error ("unable to acquire read lock %s in %s on line %d", name, __func__, __LINE__);                                     \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("acquired read lock %s in %s on line %d", name, __func__, __LINE__);                                              \
	}

#define require_write_lock(lock, name)                                                                                                     \
	g_debug ("acquiring write lock %s in %s on line %d...", name, __func__, __LINE__);                                                 \
	if (pthread_rwlock_wrlock (lock) != 0)                                                                                             \
	{                                                                                                                                  \
		g_warning ("unable to acquire write lock %s in %s on line %d", name __func__, __LINE__);                                   \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("acquired write lock %s in %s on line %d", name, __func__, __LINE__);                                             \
	}

#define release_lock(lock, name)                                                                                                           \
	if (pthread_rwlock_unlock (lock) != 0)                                                                                             \
	{                                                                                                                                  \
		g_warning ("unable to release lock %s in %s on line %d, subsequent locks may fail", name, __func__, __LINE__);             \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		g_debug ("released lock %s in %s on line %d", name, __func__, __LINE__);                                                   \
	}

#define require_channel_read_lock() require_read_lock (&channel_lock, "CHANNEL")
#define require_channel_write_lock() require_read_lock (&channel_lock, "CHANNEL")
#define release_channel_lock() require_read_lock (&channel_lock, "CHANNEL")

typedef struct
{
	XfconfChannel * channel;
	KeySet * keySet;
} ChannelKeySetPair;

#endif
