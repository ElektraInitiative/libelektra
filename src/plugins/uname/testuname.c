/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <sys/utsname.h>

int main ()
{
	struct utsname buf;

	int ret = uname (&buf);
	(void)buf.sysname;
	(void)buf.nodename;
	(void)buf.release;
	(void)buf.version;
	(void)buf.machine;

	return 0;
}
