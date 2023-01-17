#ifndef ELEKTRA_XFCONF_H
#define ELEKTRA_XFCONF_H

#include <elektra/kdb.h>
#include <xfconf/xfconf.h>

#define I_(string) (g_intern_static_string ((string)))
#define XFCONF_NAMESPACE "system:"
#define XFCONF_ROOT "/sw/xfce4"
#define XFCONF_GTYPE_META_NAME "gtype"

extern GList * channel_list;
extern KDB * elektraKdb;

typedef struct
{
	XfconfChannel * channel;
	KeySet * keySet;
} ChannelKeySetPair;

#endif
