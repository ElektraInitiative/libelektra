/**
 * @file
 *
 * @brief Header for cshadow plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SHADOW_H
#define ELEKTRA_PLUGIN_SHADOW_H

#include <kdbplugin.h>


#define NUM(n) ((n) == -1 ? 0 : -1), ((n) == -1 ? 0 : (n))
#define STR(s) ((s) ? (s) : "")

#define SHADOW_FIELDS 9


struct spwd
{
	char * sp_namp;		   /* Login name.  */
	char * sp_pwdp;		   /* Encrypted password.  */
	long int sp_lstchg;	/* Date of last change.  */
	long int sp_min;	   /* Minimum number of days between changes.  */
	long int sp_max;	   /* Maximum number of days between changes.  */
	long int sp_warn;	  /* Number of days to warn user to change
				       the password.  */
	long int sp_inact;	 /* Number of days the account may be
					    inactive.  */
	long int sp_expire;	/* Number of days since 1970-01-01 until
						 account expires.  */
	unsigned long int sp_flag; /* Reserved.  */
};


int elektraShadowGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraShadowSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (shadow);

#endif
