/* Nss-elektra
*  Copyright (C) 2004 Jens Andersen <rayman@skumler.net>
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License
*  as published by the Free Software Foundation; either version 2
*  of the License, or (at your option) any later version.
*  
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*  
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

/*
$Id: group.c 40 2004-11-26 23:25:09Z rayman $
$Author: rayman $
*/

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <grp.h>
#include <stdio.h>

#include "nss-elektra.h"

#include "group.h"
#include "lib.h"

/* Taken from nss-mysql */
#define FALLBACK -1		/* if the last change coloum can't be read,
				   fall back to -1.(This is what the nss-files does so) */

/* Global keyset & key for setspent, getspent and endspent */
KeySet *groupks = NULL;
Key *groupkey = NULL;

NSS_STATUS _nss_elektra_initgroups (const char *user, gid_t group,
				     long *start, long int *size,
				     gid_t * groups, long int limit,
				     int *errnop);
NSS_STATUS _nss_elektra_setgrent (void);
NSS_STATUS _nss_elektra_endgrent (void);
NSS_STATUS _nss_elektra_getgrent_r (struct group *gr,
				     char *buffer, size_t buflen,
				     int *errnop);
NSS_STATUS _nss_elektra_getgrnam_r (const char *name, struct group *gr,
				     char *buffer, size_t buflen,
				     int *errnop);
NSS_STATUS _nss_elektra_getgrgid_r (const gid_t gid, struct group *gr,
				     char *buffer, size_t buflen,
				     int *errnop);

/* I really ought to implement this */
NSS_STATUS
_nss_elektra_initgroups (const char *user, gid_t group, long *start,
			  long int *size, gid_t * groups, long int limit,
			  int *errnop)
{

  return NSS_STATUS_UNAVAIL;
}

/* getgrnam
 * looks for a group by its name
 * Arguments:
 * name: user's name
 * result: struct we'll fill
 * buffer:
 * buflen: sizeof(buffer)
 * errnop: ptr on the application errno
 */

NSS_STATUS
_nss_elektra_getgrnam_r (const char *name, struct group * gr,
			  char *buffer, size_t buflen, int *errnop)
{
  int i, ret;
  char *tmpbuf = NULL, *end_of_buf;
  char **addrptr = NULL;
  Key *key = NULL;
  KeySet *ks = NULL;
  char *grname = NULL;
  int namesize;

  *errnop = ENOENT;

/* Open elektra(kdb) connection */
  kdbOpenDefault ();
  if (_nss_elektra_findgroupbyname (name) == NSS_STATUS_NOTFOUND)
    return NSS_STATUS_NOTFOUND;
/* Yay! the group exists, lets continue */
  gr->gr_name =
    (char *) _nss_elektra_copy_to_buffer (&buffer, &buflen, name);
  if (!gr->gr_name)
    goto out_nomem;

  tmpbuf = _nss_elektra_get_string (ELEKTRAGROUP, gr->gr_name, "gid",&i);
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing GID for Group %s."
                         "Error (%d): %s.",
                         gr->gr_name, i, strerror(i));

  }
  gr->gr_gid = _nss_elektra_strtol (tmpbuf, FALLBACK, &i);
  if (tmpbuf != NULL)
    free (tmpbuf);
  tmpbuf = _nss_elektra_get_string (ELEKTRAGROUP, gr->gr_name, "passwd",&i);
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing UID for User %s."
                         "Error (%d): %s.",
                         gr->gr_name,i, strerror(i));

  }

  if (_nss_elektra_isempty (tmpbuf))
  {
    /* Password isn't set so set it to "x" */
    gr->gr_passwd = _nss_elektra_copy_to_buffer (&buffer, &buflen, "x");
    /* Since isempty also checks if it just contains spaces I better 
     * free this */
    if (tmpbuf != NULL)
      free (tmpbuf);
  }
  else
  {
    gr->gr_passwd = _nss_elektra_copy_to_buffer (&buffer, &buflen, tmpbuf);
    free (tmpbuf);
  }
  if (!gr->gr_passwd)
    goto out_nomem;

/* Member list...How the hell do I do that? */
/* kdbGetChildkeys (system/groups/<groupname>/members) 
 * with options KDB_O_STATONLY since all we need is the names of the keys 
 * i.e. keyGetBaseName for each key in keyset */
/* Mainly taken from nss-mysql */

  addrptr = (char **) buffer;
  gr->gr_mem = addrptr;
  end_of_buf = buffer + buflen - 1;
/*_nss_elektra_log(LOG_ERR, "nss_elektra_getgrnam: "
			   "addr %p, data %p", addrptr,
				buffer);*/

  ks = ksNew(); /* Need error checking here */
  /* Not sure if this big a buffer is actually used? Do anyone have 1000 character or above usernames */
  tmpbuf = (char *) malloc (1024);
  snprintf (tmpbuf, 1023, "system/groups/%s/members", gr->gr_name);
  ret = kdbGetChildKeys (tmpbuf, ks, KDB_O_STATONLY);
  free (tmpbuf);
  if (ret == 0 && ksGetSize(ks) > 0)
  {
    for (key = ksHead(ks); key; key = ksNext(ks))
	  {
	    char *p, *tmp;
	    namesize = keyGetBaseNameSize (key);
	    grname = (char *) malloc (namesize);
	    keyGetBaseName (key, grname, namesize);
	    end_of_buf -= namesize;
	    if ((void *) addrptr >= (void *) end_of_buf)
	      goto out_nomem;

	    tmp = end_of_buf;
	    p = _nss_elektra_copy_to_buffer (&tmp, NULL, grname);
	    if (!p)
  	    goto out_nomem;
	    *addrptr = p;
	    ++addrptr;
	    free (grname);
	  }
  }
  ksDel (ks);
  ks = NULL;

  if ((void *) addrptr >= (void *) end_of_buf)
    goto out_nomem;
  /* end */
  *addrptr = NULL;

/* Woo! this means it was successfull. Go on! tell everyone :) */

  *errnop = 0;
  kdbClose ();
  return NSS_STATUS_SUCCESS;


/* Taken from nss-mysql */
out_nomem:
  /* if we're here, that means that the buffer is too small, so
   * we return ERANGE
   */
  if (!grname)
    free (grname);
  if (!ks)
    ksDel(ks);
  *errnop = ERANGE;
  kdbClose ();
  return NSS_STATUS_TRYAGAIN;

}

NSS_STATUS
_nss_elektra_getgrgid_r (gid_t gid, struct group * gr,
			  char *buffer, size_t buflen, int *errnop)
{
  char *groupname;
  NSS_STATUS tmpstatus;
  kdbOpenDefault ();
  if ((_nss_elektra_findgroupbygid (gid, &groupname)) == NSS_STATUS_NOTFOUND)
    return NSS_STATUS_NOTFOUND;
/* Due to the way the kdb is made it's far more efficient to work with
 * usernames only, hence once we have the username for a uid we might as well 
 * just pass it on to getspnam
 *
 * Possibly some kind of uid/username caching for easy/quick lookup?
*/
  kdbClose ();
  tmpstatus =
    _nss_elektra_getgrnam_r (groupname, gr, buffer, buflen, errnop);
  free (groupname);
  return tmpstatus;
}


NSS_STATUS
_nss_elektra_setgrent (void)
{
  int ret;
/* We need to first open kdb, then get a KeySet of all keys in system/users
 * and store it globally, ready for returning the first key
 */
  kdbOpenDefault ();
  groupks = ksNew();
  ret = kdbGetChildKeys ("system/groups", groupks, KDB_O_DIR);
  if (!ret)
  {
    if (ksGetSize(groupks) <= 0)
	  {
	    _nss_elektra_log (LOG_ERR, "No groups in tree."
                         "Fix your kdb entries.");
	    ksDel (groupks);
	    groupks = NULL;
	    kdbClose ();
	    return NSS_STATUS_NOTFOUND;
	  }
    /* No error, return success! */
    groupkey = ksHead(groupks);
    kdbClose ();
    return NSS_STATUS_SUCCESS;
  }

/* If we get here it usually means that system/users doesn't exist,
 * which means this function is unavailable :) as well as the other 
 * related ones */
  kdbClose ();
  return NSS_STATUS_UNAVAIL;
}

NSS_STATUS
_nss_elektra_endgrent (void)
{
  if (groupks != NULL)
  {
    ksDel (groupks); /* ksDel free's all associated keys as well */
    groupks = NULL;
    groupkey = NULL;
  } else if(groupkey != NULL) /* If this happens the keyset has been closed but the key hasn't...That should _never_ happen */
  {
    /* It's probably not safe to do this, but do it anyways */
    keyDel(groupkey);
    groupkey = NULL;
  }
  return NSS_STATUS_SUCCESS;
}


NSS_STATUS
_nss_elektra_getgrent_r (struct group * gr, char *buffer,
			  size_t buflen, int *errnop)
{
  int groupnamesize;
  char *groupname = NULL;
  NSS_STATUS tmpstatus;
/* Hmm..I wonder if I should start it implicitly when this function is
 * called without setent */

  if (groupks == NULL)
    return NSS_STATUS_UNAVAIL;
  if (groupkey == NULL)
  {
    /* End of list */
    return NSS_STATUS_NOTFOUND;
  }
  groupnamesize = keyGetBaseNameSize (groupkey);
  groupname = (char *) malloc (groupnamesize);
  keyGetBaseName (groupkey, groupname, groupnamesize);
  tmpstatus =
    _nss_elektra_getgrnam_r (groupname, gr, buffer, buflen, errnop);
  free (groupname);
  groupkey = ksNext(groupks);
  return tmpstatus;
}

