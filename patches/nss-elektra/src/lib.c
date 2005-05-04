/* Nss-elektra
*  Copyright (C) 2004 Jens Andersen <rayman@skumler.net>
*  Portions taken from nss-mysql is copyrighted as below :
*  Copyright (C) 2000 Steve Brown
*  Copyright (C) 2000,2001,2002 Guillaume Morin, Alc▒ve
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
$Id: lib.c 40 2004-11-26 23:25:09Z rayman $
$LastChangedBy: rayman $ 
*/

/* Not quite sure if I need this yet, but I'll try and remove it 
* #define _GNU_SOURCE 1
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>

#include "lib.h"

/*taken from nss-mysql */
void
_nss_elektra_log (int err, const char *format, ...)
{
  static int openlog_ac = 0;
  va_list args;

  va_start (args, format);
  if (!openlog_ac)
    {
      ++openlog_ac;
      openlog ("nss-elektra", LOG_PID, LOG_AUTH);
    }
  vsyslog (err, format, args);
  va_end (args);
}

/* _nss_elektra_get_string
 * Parameters:
 * int type : Type of value to retrieve, Can be GROUP or USER. 
 * char *username username/groupname to retrieve data for.
 * char *keyname name of value to retrieve
 * returns pointer to string containing the contents of the key.
*/
char *
_nss_elektra_get_string (int type, char *username, char *keyname, int *errnop)
{
  int ret, size;
  Key *key = NULL;
  char *value = NULL;
  char keypath[256];
  /* Reset errno to 0 to make sure no old value lingers around */
  *errnop = 0;

  if (type == ELEKTRAGROUP)
    {
      snprintf (keypath, 1023, "system/groups/%s/%s", username, keyname);
    }
  else
    {
      snprintf (keypath, 1023, "system/users/%s/%s", username, keyname);
    }

  kdbOpen ();
  key = (Key *)malloc(sizeof(Key));
  memset(key, 0, sizeof(Key));
  keyInit (key);
  keySetName (key, keypath);
  ret = kdbGetKey (key);
/* Key doesn't exist...Or other error? */
  if (ret)
  {
    *errnop = errno;
    value = NULL;
    goto out_exit;
  }
  size = keyGetDataSize (key);
/* If size is zero or less then return NULL) */
  if (size <= 0)
{
    *errnop = -1;
    value = NULL;
    goto out_exit;
}
/* We only want strings! Abort otherwise */
  if (keyGetType (key) != KEY_TYPE_STRING)
  {
	*errnop = -2;
	value = NULL;
	goto out_exit;
  }
  value = (char *) malloc (size);
  keyGetString (key, value, size);

/* Where to goto to get out! */

  out_exit:
  keyClose (key);
  free(key);
  kdbClose ();
  return value;
}

/* Function to check if user exists.
 * Name version
*/
NSS_STATUS
_nss_elektra_finduserbyname (const char *name)
{
  char keypath[256];
  Key key;
  int ret;
  sprintf (keypath, "system/users/%s", name);
  keyInit (&key);
  keySetName (&key, keypath);
  ret = kdbGetKey (&key);
  keyClose (&key);
  if (ret == 0)
    return NSS_STATUS_SUCCESS;
  else
    return NSS_STATUS_NOTFOUND;
}

/* Function to check if user exists.
 * uid version
*/
NSS_STATUS
_nss_elektra_finduserbyuid (uid_t uid, char **name)
{
  Key key;
  char keyname[1024];
  int ret;
/* Where to store where the link points to */
  int linksize;
  char *link;
#ifdef DEBUG
  char fullpath[1024 + 1];
#endif
  char *p;
  NSS_STATUS status = NSS_STATUS_NOTFOUND;

  snprintf (keyname, 1023, "system/users/.ByID/%li", uid);
  keyInit (&key);
  keySetName (&key, keyname);
#ifdef DEBUG
  kdbGetFilename(&key,fullpath, sizeof(fullpath));
  _D(LOG_INFO, "Full path to key %s is %s\n", keyname, fullpath);
#endif
  ret = kdbStatKey (&key);
  if (ret != 0)
  {
     _D(LOG_ERR, "Error accessing UID %d\nError(%d): %s\n", uid, errno, strerror(errno));
     return NSS_STATUS_NOTFOUND;
  }
  if (!keyIsLink(&key))
    {
      _nss_elektra_log (LOG_ERR,
			 "finduserbyuid: Error: key %s is not a link!\n",
			 keyname);
      keyClose (&key);
      return NSS_STATUS_NOTFOUND;
    }
/* Woo! it's a link and stuff...return basename of link */
  linksize = keyGetDataSize (&key);
  link = (char *) malloc (linksize);
  keyGetLink (&key, link, linksize);
  _D(LOG_ERR, "Got link for UID %li pointing to %s\n", uid, link);
  p = rindex (link, '/');
  if (p != NULL)
    {
      p++;
      *name = strdup (p);
      status = NSS_STATUS_SUCCESS;
    } else _D(LOG_ERR, "Error in link, no /. Unable to find username\n");
  keyClose (&key);
  p = NULL;
  free (link);

  return status;
}

/* Function to check if group exists.
 * Name version
*/
NSS_STATUS
_nss_elektra_findgroupbyname (const char *name)
{
  char keypath[256];
  Key key;
  int ret;
  sprintf (keypath, "system/groups/%s", name);
  keyInit (&key);
  keySetName (&key, keypath);
  ret = kdbGetKey (&key);
  keyClose (&key);
  if (ret == 0)
    return NSS_STATUS_SUCCESS;
  else
    return NSS_STATUS_NOTFOUND;
}


/* Function to check if group exists.
 * returns NSS_STATUS_SUCCESS when user found and sets name to point at a string
 * containing the name of the group 
 * gid version
*/
NSS_STATUS
_nss_elektra_findgroupbygid (gid_t gid, char **name)
{
  Key key;
  char keyname[1024];
  int ret;
/* Where to store where the link points to */
  int linksize;
  char *link;
  char *p;
  NSS_STATUS status = NSS_STATUS_NOTFOUND;;

  snprintf (keyname, 1023, "system/groups/.ByID/%li", gid);
  keyInit (&key);
  keySetName (&key, keyname);
  ret = kdbStatKey (&key);
  if (ret != 0)
  {
    _D(LOG_ERR, "findgroupbygid: Error stat'ing key %s\nError(%d) : %s\n", keyname, errno, strerror(errno));
    return NSS_STATUS_NOTFOUND;
  }
  if (!keyIsLink(&key))
    {
      _nss_elektra_log (LOG_ERR,
			 "findgroupbyuid: Error: key %s is not a link!\n",
			 keyname);
      keyClose (&key);
      return NSS_STATUS_NOTFOUND;
    }
/* Woo! it's a link and stuff...return basename of link */
  linksize = keyGetDataSize (&key);
  link = (char *) malloc (linksize);
  keyGetLink (&key, link, linksize);
  p = rindex (link, '/');
  if (p != NULL)
    {
      p++;
      *name = strdup (p);
      status = NSS_STATUS_SUCCESS;
    }
  keyClose (&key);
  p = NULL;
  free (link);

  return status;
}



/* Taken from nss-mysql */
/* _nss_elektra_copy_to_buffer
 * copy a string to the buffer given as arguments
 * returns a pointer to the address in the buffer
 */

char *
_nss_elektra_copy_to_buffer (char **buffer, size_t * buflen,
			      const char *string)
{
  size_t len = strlen (string) + 1;
  char *ptr;


  if (buflen && len > *buflen)
    {
      return NULL;
    }
  memcpy (*buffer, string, len);
  if (buflen)
    *buflen -= len;
  ptr = *buffer;
  (*buffer) += len;
  return ptr;
}

/* Taken from nss-mysql
 * However, there isn't a very big chance of this going wrong so it's
 * just there for backup
*/

/* _nss_elektra_strtol
 * nss-elektra strtol version
 * Converts ascii into long
 * str: string to convert
 * fallback: fallback to this value if strtol is not happy
 * error: if (*error), an error has occured, we have fallback.
 */

long
_nss_elektra_strtol (char *str, long fallback, int *error)
{
  char *endptr;
  long toreturn;


  /* sanity checks */
  if (!str)
    {
      _nss_elektra_log (LOG_ERR, "_nss_elektra_strtol: string pointer "
			 "is NULL.");
      *error = 1;
      return fallback;
    }
  if (*str == '\0')
    {
      _nss_elektra_log (LOG_ERR, "_nss_elektra_strtol: string is empty.");
      *error = 1;
      return fallback;
    }

  toreturn = strtol (str, &endptr, 10);

  if (endptr == str)
    {
      _nss_elektra_log (LOG_ERR, "_nss_elektra_strtol: can't convert %s",
			 str);
      *error = 1;
      return fallback;
    }

  if (*endptr != '\0')
    {
      _nss_elektra_log (LOG_ERR, "_nss_elektra_strtol_: incomplete "
			 "conversion of %s to %ld. Falling back "
			 "to %ld.", str, toreturn, fallback);
      *error = 1;
      return fallback;
    }

  if (errno != ERANGE)
    {
      *error = 0;
      return toreturn;
    }

  _nss_elektra_log (LOG_ERR,
		     "_nss_elektra_strol: overflow when converting %s. "
		     "Fix your elektra entries.", str);
  *error = 1;
  return toreturn;
}

/* Taken from nss-mysql */
/* _nss_elektra_isempty
 * checks if a string only contains spaces
 * Returns:
 * 0, string is not empty
 * 1, string is empty
 */

int
_nss_elektra_isempty (char *str)
{
  if (!str)
    return 1;
  while (*str != '\0')
    if (!isspace ((unsigned char) *(str++)))
      return 0;
  return 1;
}
