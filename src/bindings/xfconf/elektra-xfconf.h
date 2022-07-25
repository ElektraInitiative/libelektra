#ifndef ELEKTRA_XFCONF_H
#define ELEKTRA_XFCONF_H

#include <gelektra-kdb.h>
#include <xfconf/xfconf.h>

#define I_(string) (g_intern_static_string ((string)))
#define XFCONF_NAMESPACE "system:"
#define XFCONF_ROOT "/sw/xfce4"
#define XFCONF_GTYPE_META_NAME "gtype"

extern GList * channel_list;
extern GElektraKdb * gElektraKdb;

typedef struct
{
	XfconfChannel * channel;
	GElektraKeySet * keySet;
} ChannelKeySetPair;

#endif
