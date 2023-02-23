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

typedef struct
{
	XfconfChannel * channel;
	KeySet * keySet;
} ChannelKeySetPair;

#endif
