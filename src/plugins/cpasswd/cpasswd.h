/**
 * @file
 *
 * @brief Header for cpasswd plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CPASSWD_H
#define ELEKTRA_PLUGIN_CPASSWD_H

#include <kdbplugin.h>

#define PASSWD_FIELDS 7
#define ID_MAX_CHARACTERS 11

struct passwd
{
	char * pw_name;   /* Username.  */
	char * pw_passwd; /* Password.  */
	char * pw_uid;    /* User ID.  */
	char * pw_gid;    /* Group ID.  */
	char * pw_gecos;  /* Real name.  */
	char * pw_dir;    /* Home directory.  */
	char * pw_shell;  /* Shell program.  */
};

typedef enum
{
	NAME,
	UID,
} SortBy;

int elektraCpasswdGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCpasswdSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (cpasswd);

#endif
