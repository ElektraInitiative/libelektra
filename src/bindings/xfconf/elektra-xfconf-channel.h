#ifndef ELEKTRA_XFCONF_CHANNEL_H
#define ELEKTRA_XFCONF_CHANNEL_H

#include <elektra.h>
#include <xfconf/xfconf.h>

struct ELEKTRA_UNUSED _XfconfChannel
{
	GObject parent;
	gchar * channel_name;
};

gboolean xfconf_channel_get_formatted (XfconfChannel * channel, const gchar * property, GValue * g_value);

#endif
