/**
 * @file
 *
 * @brief Header for chosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CHOSTS_H
#define ELEKTRA_PLUGIN_CHOSTS_H

#include <kdbplugin.h>
#include <stdint.h>

#define PF_INET 2
#define PF_INET6 10
#define AF_INET PF_INET
#define AF_INET6 PF_INET6

struct hostent
{
	char * h_name;       /* Official name of host.  */
	char ** h_aliases;   /* Alias list.  */
	int h_addrtype;      /* Host address type.  */
	int h_length;	/* Length of address.  */
	char ** h_addr_list; /* List of addresses from name server.  */
};

typedef uint32_t in_addr_t;
struct in_addr
{
	in_addr_t s_addr;
};

struct in6_addr
{
	union
	{
		uint8_t __s6_addr[16];
		uint16_t __s6_addr16[8];
		uint32_t __s6_addr32[4];
	} __in6_union;
};

typedef enum
{
	NAME,
	ADDRESS,
} SortBy;

int elektraChostsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraChostsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (chosts);

#endif
