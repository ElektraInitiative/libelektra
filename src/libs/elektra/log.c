/**
 * @file
 *
 * @brief Logger Implementation
 *
 * If you often change the file, you might want to set CMAKE_LINK_DEPENDS_NO_SHARED
 * to avoid relinking everything.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

// XXX (marks places for configuration)
// logger configuration to be changed by you
// please do not include changes here to PRs
// unless you want to change the defaults.
#define USE_STDERR_SINK
#define USE_SYSLOG_SINK
#define USE_FILE_SINK
#define NO_FILTER


#define _GNU_SOURCE /* For asprintf */
#include <stdio.h>

#include <kdblogger.h>

#ifdef USE_SYSLOG_SINK
#include <syslog.h>
#endif

#ifdef USE_STDERR_SINK
#include <stdio.h>
#endif

#include <stdarg.h>
#include <string.h>

#ifdef USE_STDERR_SINK
static int elektraLogStdErr (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED,
		      const char * file ELEKTRA_UNUSED, const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED,
		      const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	return vfprintf (stderr, msg, args);
}
#endif

#ifdef USE_SYSLOG_SINK
static int elektraLogSyslog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED,
		      const char * file ELEKTRA_UNUSED, const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED,
		      const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	vsyslog (level, msg, args);
	return 0;
}
#endif

static FILE * elektraLoggerFileHandle;

#ifdef USE_FILE_SINK
static int elektraLogFile (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED,
		      const char * file ELEKTRA_UNUSED, const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED,
		      const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	if (!elektraLoggerFileHandle)
	{
		elektraLoggerFileHandle = fopen ("elektra.log", "a");
	}
	return vfprintf (elektraLoggerFileHandle, msg, args);
}
#endif

int elektraLog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * absFile ELEKTRA_UNUSED,
		const int line ELEKTRA_UNUSED, const char * mmsg ELEKTRA_UNUSED, ...)
{
#ifndef NO_FILTER
	// XXX Filter here for every sink
	if (level <= ELEKTRA_LOG_LEVEL) return 0;
#endif

	char * msg = strdupa (mmsg);
	size_t lenOfMsg = strlen (msg);
	do
	{
		if (msg [lenOfMsg] == '\n') msg [lenOfMsg] = '_';
	}
	while (lenOfMsg--);

	size_t lenOfLogFileName = sizeof("src/libs/elektra/log.c")-1;
	size_t lenOfLogPathName = strlen(__FILE__)-lenOfLogFileName;
	const char * file = &absFile[lenOfLogPathName];

	char * str;
	// XXX Change here default format for messages
	asprintf (&str, "%s:%d:%s: %s\n", file, line, function, msg);

	int ret = -1;
#ifdef USE_STDERR_SINK
	{
		va_list args;
		va_start (args, mmsg);
		ret |= elektraLogStdErr (level, function, file, line, str, msg, args);
		va_end (args);
	}
#endif
#ifdef USE_SYSLOG_SINK
	{
		va_list args;
		va_start (args, mmsg);
		ret |= elektraLogSyslog (level, function, file, line, str, msg, args);
		va_end (args);
	}
#endif
#ifdef USE_FILE_SINK
	{
		va_list args;
		va_start (args, mmsg);
		ret |= elektraLogFile (level, function, file, line, str, msg, args);
		va_end (args);
	}
#endif
	return ret;
}
