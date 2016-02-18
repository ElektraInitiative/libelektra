/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <mntent.h>
#include <stdio.h>

int main ()
{
	struct mntent * m;
	FILE * f;
	char *fsname, *dir, *type, *opts;
	int freq, passno;
	f = setmntent ("/etc/fstab", "r");
	m = getmntent (f);
	fsname = m->mnt_fsname;
	dir = m->mnt_dir;
	type = m->mnt_type;
	opts = m->mnt_opts;
	freq = m->mnt_freq;
	passno = m->mnt_passno;

	return 0;
}
