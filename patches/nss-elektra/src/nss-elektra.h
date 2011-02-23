/* Svn stuff
$Id: nss-registry.h 36 2004-06-12 12:19:09Z rayman $
$LastChangedBy: rayman $
*/


#ifndef _HAVE_NSS_REGISTRY_H
#define _HAVE_NSS_REGISTRY_H
#include <kdb.h>
#include <sys/types.h>

#ifdef HAVE_KDBCONFIG_H
	#include "kdbconfig.h"
#endif

/* Taken from nss-mysql */
#ifdef HAVE_NSSWITCH_H
#include <nss_common.h>

typedef nss_status_t NSS_STATUS;

#define NSS_STATUS_SUCCESS      NSS_SUCCESS
#define NSS_STATUS_NOTFOUND     NSS_NOTFOUND
#define NSS_STATUS_UNAVAIL      NSS_UNAVAIL
#define NSS_STATUS_TRYAGAIN     NSS_TRYAGAIN

#else
#include <nss.h>

typedef enum nss_status NSS_STATUS;

#endif


#endif /* _HAVE_NSS_REGISTRY_H */
