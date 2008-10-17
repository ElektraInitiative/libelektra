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

/* Svn stuff
$Id: passwdconvert.c 40 2004-11-26 23:25:09Z rayman $
$LastChangedBy: rayman $
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include <shadow.h>
#include <kdb.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

void addusers(int options);
void adduser(struct passwd *pw, struct spwd *spw);
void addgroups(int options);
void addgroup(struct group *grp);
void showhelp(void);
void SetValue(char *key, char *value, int mode);
void debugprint(const char *format, ...);
int userexists(int type,const  char *name);
void SetIDLink(int mode, void *pw);

#define USERFLAG 0x1
#define SHADOWFLAG 0x2
#define GROUPFLAG 0x4
#define UPDATEFLAG 0x8

#define ELEKTRAUSER 0
#define ELEKTRAGROUP 1
/* Global variable containing the "root" key string. (i.e. default is system)*/
char *root=NULL;

int main(int argc, char *argv[])
{
int options=0;
int opt;
options = (SHADOWFLAG | GROUPFLAG | USERFLAG | UPDATEFLAG);
/* Options 
Blah. We assume add all & update if existing */

while((opt = getopt(argc, argv, "ogsur:")) != -1)
{
	switch(opt)
	{
		/* o = -userflag */
		case 'o':	options &= ~USERFLAG;
				debugprint("Minus Userflag\n");
			break;
		/* -groupflag */
		case 'g':	options &= ~GROUPFLAG;
				debugprint("Minus Groupflag\n");
			break;
		/* -shadowflag */
		case 's':	options &= ~SHADOWFLAG;
				debugprint("Minus Shadowflag\n");
			break;
		/* -updateflag */
		case 'u':	options &= ~UPDATEFLAG;
				debugprint("Minus Updateflag\n");
			break;
		/* Set root to something else...for testing purposes 
		 * Tells the program to add all entries under <root>/users 
		 * and <root>/groups
		 */
		case 'r':	if(optarg != NULL) 
					root = strdup(optarg);
				/* Check if arg is valid here */
				debugprint("Setting root to %s\n", root);
				break;
		case 'h':	showhelp();
				return 0;
	}
}
if(options == 0 || options == UPDATEFLAG)
	options = (SHADOWFLAG | GROUPFLAG | USERFLAG | UPDATEFLAG);

if(root == NULL)
{
if(geteuid() != 0)
{
	showhelp();
	return 0;
}
	debugprint("Defaulting root to system");
	root = "system";
} else if(geteuid() != 0 && (options & SHADOWFLAG))
{
        showhelp();
        return 0;
} 

printf("Starting...\n");
debugprint("Options = %d\n",options);
kdbOpenDefault();
addusers(options);
addgroups(options);
kdbClose();
return 0;
}

void addusers(int options)
{
struct passwd *pw = NULL;
struct spwd *spw = NULL;
if(options & USERFLAG)
{
printf("Adding User entries...\n");
	setpwent();
	while((pw = getpwent()) != NULL)
	{
		/* Hrm..this might not be completely correct */
                if (!(options & UPDATEFLAG))
                {
                        if(userexists(ELEKTRAUSER, pw->pw_name))
			{
				debugprint("Update flag not set and user %s already exists\n", pw->pw_name);
                                continue;
			}
                }
		if (options & SHADOWFLAG)
		{
			printf("Retrieving Shadow entry for %s\n",pw->pw_name);
			spw = getspnam(pw->pw_name);
		}
		adduser(pw, spw);
		SetIDLink(ELEKTRAUSER, pw);
		pw = NULL;
		spw = NULL;
	}
	endpwent();
} else if(options & SHADOWFLAG)
{
	printf("Adding only shadow entries...\n");
        setpwent();
        while((spw = getspent()) != NULL)
        {
		if (!(options & UPDATEFLAG))
                {
                        if(userexists(ELEKTRAUSER, spw->sp_namp))
			{
				debugprint("Update flag is not set and user %s already exists. Skipping\n", spw->sp_namp);
				continue;
			}
                }

                adduser(NULL, spw);
        }
	if(errno)
		printf("Error: %s\n",strerror(errno));
        endspent();

}

}

void adduser(struct passwd *pw, struct spwd *spw)
{
char key[1024];
char value[1024];
/* Add Passwd entries */
if(pw != NULL)
{
printf("Adding user: %s\n",pw->pw_name);
snprintf(key,1023,"%s/users/%s/password", root, pw->pw_name);
SetValue(key, pw->pw_passwd, -1);

snprintf(key,1023,"%s/users/%s/uid", root, pw->pw_name);
snprintf(value,1023,"%li",pw->pw_uid);
SetValue(key, value,-1);

snprintf(key,1023,"%s/users/%s/gid", root, pw->pw_name);
snprintf(value,1023,"%li",pw->pw_gid);
SetValue(key, value, -1);

snprintf(key,1023,"%s/users/%s/home", root, pw->pw_name);
SetValue(key, pw->pw_dir,-1);

snprintf(key,1023,"%s/users/%s/gecos", root, pw->pw_name);
SetValue(key, pw->pw_gecos,-1);

snprintf(key,1023,"%s/users/%s/shell", root, pw->pw_name);
SetValue(key, pw->pw_shell,-1);
}

/* Add Shadow Entries 
   We add those with mode 0600 */
if(spw != NULL)
{
	printf("Adding shadow entry for %s\n",spw->sp_namp);

	snprintf(key,1023,"%s/users/%s/shadowPassword", root, spw->sp_namp);
	SetValue(key, spw->sp_pwdp, 0600);
	
        snprintf(key,1023,"%s/users/%s/passwdChangeBefore", root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_min);
        SetValue(key, value, 0600);

        snprintf(key,1023,"%s/users/%s/passwdChangeAfter", root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_max);
        SetValue(key, value, 0600);

        snprintf(key,1023,"%s/users/%s/passwdWarnBefore", root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_warn);
        SetValue(key, value, 0600);

        snprintf(key,1023,"%s/users/%s/passwdDisableAfter",root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_inact);
        SetValue(key, value, 0600);

        snprintf(key,1023,"%s/users/%s/passwdDisabledSince", root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_expire);
        SetValue(key, value, 0600);

        snprintf(key,1023,"%s/users/%s/passwdReserved", root, spw->sp_namp);
        snprintf(value,1023, "%li", spw->sp_flag);
        SetValue(key, value, 0600);
}
}

void SetValue(char *keyname, char *value, int mode)
{
Key *key;
int ret=0;
/* mode -1 = standard access permissions */
debugprint("SetValue('%s', '%s', '%d')\n", keyname, value, mode);
if(mode == -1)
{
	ret = kdbSetValue(keyname,value);
	debugprint("kdbSetValue key=%s , value=%s\nReturned %d\n",keyname, value, ret);
} else
{
/* Special Access permissions */
	key = keyNew(keyname, 
		KEY_SWITCH_VALUE, value,
		KEY_SWITCH_ACCESS, mode,
		KEY_SWITCH_END);
	kdbSetKey(key);
	keyDel(key);
}
debugprint("SetValue done with %s\n", keyname);
}

void addgroups(int options)
{
struct group *gr;

if(options & GROUPFLAG)
{
	printf("Adding Groups...\n");
	setgrent();
	while((gr = getgrent()) != NULL)
	{
               if (!(options & UPDATEFLAG))
                {
                        if(userexists(ELEKTRAGROUP, gr->gr_name))
			{
				debugprint("Update flag is not set and group %s already exist.\n",gr->gr_name);
                                continue;
			}
                }
		addgroup(gr);
		SetIDLink(ELEKTRAGROUP, gr);
	}
}
}

void addgroup(struct group *grp)
{
char key[1024];
char value[1024];
char **members;

if(grp!=NULL)
{
	printf("Adding group %s\n",grp->gr_name);

	snprintf(key,1023,"%s/groups/%s/passwd", root, grp->gr_name);
	SetValue(key,grp->gr_passwd, -1);

	snprintf(key,1023, "%s/groups/%s/gid", root, grp->gr_name);
	snprintf(value,1023, "%li",grp->gr_gid);
	SetValue(key, value, -1);

	/* Group has at least one member */
	if(*(grp->gr_mem) != NULL)
	{
	members = grp->gr_mem;
	while((*members) != NULL)
	{
		/* Not sure of ideal way to do this...
		 * Either commaseperated in one file like /etc/group
		 * or seperated entries for each member. */
		printf("Adding member %s of group %s\n",*members,grp->gr_name);
		snprintf(key,1023, "%s/groups/%s/members/%s", root, grp->gr_name,(*members));
		SetValue(key,*members, -1);
		members++;
	}
	}
}
}


void showhelp(void)
{
puts("Usage: passwdconvert [options]\n");
puts("	-o Don't update user entries\n");
puts("	-g Don't update group entries\n");
puts("	-s Don't update shadow entries\n");
puts("	-u Don't update data for already existing users\n");
puts("	-r <root>  Use <root> as root for keys,\n	 i.e. data will be in <root>/users and <root>/groups\n");
puts(" Note: You have to be root to use this program");
}

/* Checks if a user exists...Returns 0 if the user does exist and 
 * 1 if it doesn't */
int userexists(int type, const char *name)
{
char keypath[1024];
Key *key;
int ret;
/* If it's not elektragroup then assume it's user */
if(type == ELEKTRAGROUP)
	snprintf(keypath,1023,"%s/groups/%s", root, name);
else snprintf(keypath,1023,"%s/users/%s", root, name);

/* If stat succeeds the key exists and we have our answer */
key = keyNew(keypath, KEY_SWITCH_END);
ret = kdbStatKey(key);
keyDel(key);

return (ret == 0);
}

void debugprint(const char *format, ...)
{
#if DEBUG
va_list args;

va_start(args, format);
vprintf(format, args);
va_end(args);
#endif
}

/* SetIDLink:
 * Sets link from .ByID/<uid> to name. */
void SetIDLink(int mode, void *data)
{
char keyname[1024];
char linkname[1024];
int ret;
char *errorstring=NULL;
if(mode == ELEKTRAGROUP)
{
struct group *gr;
gr = (struct group *)data;
snprintf(keyname, 1023, "%s/groups/%s",root,  gr->gr_name);
snprintf(linkname, 1023, "%s/groups/.ByID/%li",root, gr->gr_gid);
ret = kdbLink(keyname, linkname);
debugprint("Adding grouplink from %s to %s\nReturned %d\n",linkname, keyname, ret);
if (ret != 0)
{
        errorstring = strerror(errno);
	debugprint("Error: (%d) = %s\n",errno, errorstring);
}

} else if(mode == ELEKTRAUSER)
{
struct passwd *pw;
pw = (struct passwd *)data;
snprintf(keyname, 1023, "%s/users/%s", root, pw->pw_name);
snprintf(linkname, 1023, "%s/users/.ByID/%li",root, pw->pw_uid);
ret = kdbLink(keyname, linkname);
debugprint("Adding userlink from %s to %s\nReturned %d\n",linkname, keyname,  ret);
if (ret != 0)
{
        errorstring = strerror(errno);
        debugprint("Error: (%d) = %s\n",errno, errorstring);
}

}
/* Do nothing if it's not user or group...in case of new additions later */
}
