#ifndef ELEKTRA_XFCONF_CHANNEL_H
#define ELEKTRA_XFCONF_CHANNEL_H

#include <xfconf/xfconf.h>

struct _XfconfChannel
{
	GObject parent;
	gchar * channel_name;
};

gboolean xfconf_channel_get_formatted (XfconfChannel * channel, const gchar * property, GValue * g_value);

#endif
