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
$Id: passwd.c 40 2004-11-26 23:25:09Z rayman $
$Author: rayman $
*/


#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>

#include "nss-elektra.h"

#include "passwd.h"
#include "lib.h"

/* Taken from nss-mysql */
#define FALLBACK_GID 65534	/* if the gid column can't be read,
				   fall back to this GID. should be nogroup */
#define FALLBACK_UID 65534

#define FALLBACK_TMP "/tmp"
#define FALLBACK_SHELL "/bin/sh"

/* Global keyset & key for setpwent, getpwent and endpwent */
KeySet *ks = NULL;
Key *key = NULL;

NSS_STATUS _nss_elektra_getpwuid_r (uid_t, struct passwd *, char *, size_t,
				     int *);
NSS_STATUS _nss_elektra_setpwent (void);
NSS_STATUS _nss_elektra_getpwent_r (struct passwd *pw, char *buffer,
				     size_t buflen, int *errnop);
NSS_STATUS _nss_elektra_endpwent (void);
NSS_STATUS _nss_elektra_getpwnam_r (const char *, struct passwd *, char *,
				     size_t, int *);


/* getpwnam
 * looks for an user by its name
 * Arguments:
 * name: user's name
 * result: struct we'll fill
 * buffer:
 * buflen: sizeof(buffer)
 * errnop: ptr on the application errno
 */

NSS_STATUS
_nss_elektra_getpwnam_r (const char *name, struct passwd *pw,
			  char *buffer, size_t buflen, int *errnop)
{
  int i;
  char *tmpbuf = NULL;
  *errnop = ENOENT;

/* Open elektra connection */
  kdbOpen ();
  if (_nss_elektra_finduserbyname (name) == NSS_STATUS_NOTFOUND)
    return NSS_STATUS_NOTFOUND;
/* Yay! the users exists, lets continue */
  pw->pw_name =
    (char *) _nss_elektra_copy_to_buffer (&buffer, &buflen, name);
  if (!pw->pw_name)
    goto out_nomem;
  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "password",&i);
  if(tmpbuf == NULL && i > 0)
  {
	/* Set errnumber to returned error number, permission denied etc */
        _nss_elektra_log (LOG_ERR, "Problem accessing password for User %s. "
                         " Reverted to \"x\"."
			 "Error (%d): %s",
                         pw->pw_name, i, strerror(i));
  }
  if (!_nss_elektra_isempty (tmpbuf))
    {
      pw->pw_passwd =
	(char *) _nss_elektra_copy_to_buffer (&buffer, &buflen, tmpbuf);
      free (tmpbuf);
    }
  else
    {
/* We assume shadow if tmpbuf is empty...but check if it's null and free if not */
      pw->pw_passwd =
	(char *) _nss_elektra_copy_to_buffer (&buffer, &buflen, "x");
      if (tmpbuf != NULL)
	free (tmpbuf);
    }
  if (!pw->pw_passwd)
    goto out_nomem;

  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "uid",&i);
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing UID for User %s."
			 "Error (%d): %s.", 
                         pw->pw_name,i, strerror(i));

  }
  pw->pw_uid = _nss_elektra_strtol (tmpbuf, FALLBACK_UID, &i);
  if (i)
    {
      _nss_elektra_log (LOG_ERR, "User %s has invalid uid(%s). "
			 " Reverted to %d. Fix you elektra entries.",
			 pw->pw_name, tmpbuf, pw->pw_uid);
    }
  if (tmpbuf != NULL)
    free (tmpbuf);

  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "gid",&i);
  /* This will cause two error messages if tmpbuf == NULL sadly, but imho best way */
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing GID for User %s."
                         "Error (%d): %s.",
                         pw->pw_name,i, strerror(i));

  }
  pw->pw_gid = _nss_elektra_strtol (tmpbuf, FALLBACK_GID, &i);
  if (i)
    {
      _nss_elektra_log (LOG_ERR, "User %s has invalid gid(%s). "
			 " Reverted to %d. Fix you elektra entries.",
			 pw->pw_name, tmpbuf, pw->pw_gid);
    }
  if (tmpbuf != NULL)
    free (tmpbuf);

  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "gecos",&i);
/* if tmpbuf is null just set it to an empty string*/
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing gecos for User %s."
                         "Error (%d): %s.",
                         pw->pw_name,i, strerror(i));

  }
  pw->pw_gecos = _nss_elektra_copy_to_buffer (&buffer, &buflen,
					       tmpbuf ? tmpbuf : "");
  if (tmpbuf != NULL)
    free (tmpbuf);

  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "home",&i);
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing home entry for User %s."
                         "Error (%d): %s.",
                         pw->pw_name,i, strerror(i));

  }
  if (_nss_elektra_isempty (tmpbuf))
    {
      _nss_elektra_log (LOG_ERR, "Empty or NULL home entry for "
			 "user %s(%d). Falling back to " FALLBACK_TMP
			 ". Fix your elektra entries.",
			 pw->pw_name, pw->pw_uid);
      pw->pw_dir = _nss_elektra_copy_to_buffer (&buffer, &buflen,
						 FALLBACK_TMP);
      if (tmpbuf != NULL)
	free (tmpbuf);
    }
  else
    {
      pw->pw_dir = _nss_elektra_copy_to_buffer (&buffer, &buflen, tmpbuf);
      free (tmpbuf);
    }
  if (!pw->pw_dir)
    goto out_nomem;

  tmpbuf = _nss_elektra_get_string (ELEKTRAUSER, pw->pw_name, "shell",&i);
  if(tmpbuf == NULL && i > 0)
  {
        _nss_elektra_log (LOG_ERR, "Problem accessing shell entry for User %s."
                         "Error (%d): %s.",
                         pw->pw_name,i, strerror(i));

  }
  if (_nss_elektra_isempty (tmpbuf))
    {
      _nss_elektra_log (LOG_ERR, "Empty or NULL shell column for "
			 "user %s(%d). Falling back to " FALLBACK_SHELL
			 ". Fix your elektra entries.",
			 pw->pw_name, pw->pw_uid);
      pw->pw_shell = _nss_elektra_copy_to_buffer (&buffer, &buflen,
						   FALLBACK_SHELL);
      if (tmpbuf != NULL)
	free (tmpbuf);
    }
  else
    {
      pw->pw_shell = _nss_elektra_copy_to_buffer (&buffer, &buflen, tmpbuf);
      free (tmpbuf);
    }
  if (!pw->pw_shell)
    goto out_nomem;

/* Woo! this means it was successfull. Go on! tell everyone :) */

  *errnop = 0;
  kdbClose ();
  return NSS_STATUS_SUCCESS;


/* Taken from nss-mysql */
out_nomem:
  /* if we're here, that means that the buffer is too small, so
   * we return ERANGE
   */
  *errnop = ERANGE;
  kdbClose ();
  return NSS_STATUS_TRYAGAIN;

}

NSS_STATUS
_nss_elektra_getpwuid_r (uid_t uid, struct passwd * pw,
			  char *buffer, size_t buflen, int *errnop)
{
/* I'm not sure how long a username can actually be, so...)*/
  char *username;
  NSS_STATUS tmpstatus;
  kdbOpen ();
  if ((_nss_elektra_finduserbyuid (uid, &username)) == NSS_STATUS_NOTFOUND)
    return NSS_STATUS_NOTFOUND;
/* Due to the way elektra is made it's far more efficient to work with
 * usernames only, hence once we have the username for a uid we might as well 
 * just pass it on to getpwnam
 * 
 * Again, some kind of caching would be quite useful since the uid lookup is
 * quite expensive/slow.
*/
  kdbClose ();
  tmpstatus = _nss_elektra_getpwnam_r (username, pw, buffer, buflen, errnop);
  free (username);
  return tmpstatus;
}


NSS_STATUS
_nss_elektra_setpwent (void)
{
  int ret;
/* We need to first open elektra, then get a KeySet of all keys in system/users
 * and store it globally, ready for returning the first key
 */
  kdbOpen ();
  ks = (KeySet *) malloc (sizeof (KeySet));
  memset(ks, 0, sizeof(KeySet));
  ksInit (ks);
  ret = kdbGetChildKeys ("system/users", ks, KDB_O_DIR);
  if (!ret)
    {
      if (ks->size <= 0)
	{
	  _nss_elektra_log (LOG_ERR, "No users in elektra database!\n");
	  ksClose (ks);
	  free (ks);
	  ks = NULL;
	  kdbClose ();
	  return NSS_STATUS_NOTFOUND;
	}
      /* No error, return success! */
      key = ks->start;
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
_nss_elektra_endpwent (void)
{
  if (ks != NULL)
    {
      ksClose (ks);
      free (ks);
      ks = NULL;
      key = NULL;
    } else if(key != NULL)
    {
	keyClose(key);
	free(key);
    }
  return NSS_STATUS_SUCCESS;
}


NSS_STATUS
_nss_elektra_getpwent_r (struct passwd * pw, char *buffer,
			  size_t buflen, int *errnop)
{
  Key *tempkey = NULL;
  int usernamesize = 0;
  char *username = NULL;
  NSS_STATUS tmpstatus;
/* Hmm..I wonder if I should start it implicitly when this function is
 * called without setent */

  if (ks == NULL)
  {
   _nss_elektra_setpwent();
   /* return NSS_STATUS_UNAVAIL;*/
  }
  if (key == NULL)
    {
      /* End of list */
      return NSS_STATUS_NOTFOUND;
    }
  usernamesize = keyGetBaseNameSize (key);
  username = (char *) malloc (usernamesize);
  keyGetBaseName (key, username, usernamesize);
  tmpstatus = _nss_elektra_getpwnam_r (username, pw, buffer, buflen, errnop);
  free (username);
  tempkey = key;
  key = tempkey->next;
  return tmpstatus;
}
