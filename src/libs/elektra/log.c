/**
 * @file
 *
 * @brief Non-C99 Logger Implementation
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
// #define USE_FILE_SINK
// #define NO_FILTER


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
#include <stdlib.h>
#include <string.h>

#ifdef USE_STDERR_SINK
static int elektraLogStdErr (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			     const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED, const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	int ret = vfprintf (stderr, msg, args);
	fflush (stderr);
	return ret;
}
#endif

#ifdef USE_SYSLOG_SINK
static int elektraLogSyslog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			     const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED, const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	int vlevel = LOG_CRIT; // if incorrect level given
	switch (level)
	{
	case ELEKTRA_LOG_LEVEL_ERROR:
		vlevel = LOG_ERR;
	case ELEKTRA_LOG_LEVEL_WARNING:
		vlevel = LOG_WARNING;
	case ELEKTRA_LOG_LEVEL_NOTICE:
		vlevel = LOG_NOTICE;
	case ELEKTRA_LOG_LEVEL_INFO:
		vlevel = LOG_INFO;
	case ELEKTRA_LOG_LEVEL_DEBUG:
		vlevel = LOG_DEBUG;
	}
	vsyslog (vlevel, msg, args);
	return 0;
}
#endif

#ifdef USE_FILE_SINK
static FILE * elektraLoggerFileHandle;

static int elektraLogFile (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			   const int line ELEKTRA_UNUSED, const char * msg ELEKTRA_UNUSED, const char * fmt ELEKTRA_UNUSED, va_list args)
{
#ifndef NO_FILTER
// XXX Filter here for specific sink
#endif
	if (!elektraLoggerFileHandle)
	{
		elektraLoggerFileHandle = fopen ("elektra.log", "a");
	}
	int ret = vfprintf (elektraLoggerFileHandle, msg, args);
	fflush (elektraLoggerFileHandle);
	return ret;
}
#endif

int elektraVLog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * absFile ELEKTRA_UNUSED,
		 const int line ELEKTRA_UNUSED, const char * mmsg ELEKTRA_UNUSED, va_list args)
{
	char * msg = strdupa (mmsg);
	size_t lenOfMsg = strlen (msg);
	do
	{
		if (msg[lenOfMsg] == '\n') msg[lenOfMsg] = '_';
	} while (lenOfMsg--);

	size_t lenOfLogFileName = sizeof ("src/libs/elektra/log.c") - 1;
	size_t lenOfLogPathName = strlen (__FILE__) - lenOfLogFileName;
	const char * file = &absFile[lenOfLogPathName];

	char * str;
	// XXX Change here default format for messages
	asprintf (&str, "%s:%d:%s: %s\n", file, line, function, msg);

#ifndef NO_FILTER
	// XXX Filter level here globally (for every sink)
	// by default: discard everything except warnings+assertions
	if (level <= ELEKTRA_LOG_LEVEL) goto end;

	// and discard log statements from the log statement itself:
	if (!strcmp (file, "src/libs/elektra/log.c")) goto end;
	// or e.g. discard everything, but log statements from simpleini.c:
	// if (strcmp (file, "src/plugins/simpleini/simpleini.c")) goto end;
#endif

	int ret = -1;
#ifdef USE_STDERR_SINK
	{
		va_list copy;
		va_copy (copy, args);
		ret |= elektraLogStdErr (level, function, file, line, str, msg, copy);
	}
#endif
#ifdef USE_SYSLOG_SINK
	{
		va_list copy;
		va_copy (copy, args);
		ret |= elektraLogSyslog (level, function, file, line, str, msg, copy);
	}
#endif
#ifdef USE_FILE_SINK
	{
		va_list copy;
		va_copy (copy, args);
		ret |= elektraLogFile (level, function, file, line, str, msg, copy);
	}
#endif

end:
	free (str);
	return ret;
}

int elektraLog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * absFile ELEKTRA_UNUSED,
		const int line ELEKTRA_UNUSED, const char * mmsg ELEKTRA_UNUSED, ...)
{
	va_list args;
	va_start (args, mmsg);
	int ret = elektraVLog (level, function, absFile, line, mmsg, args);
	va_end (args);
	return ret;
}

void elektraAbort (const char * expression, const char * function, const char * file, const int line, const char * mmsg, ...)
{
	{
		va_list args;
		va_start (args, mmsg);
		char * msg;
		asprintf (&msg, "Assertion `%s' failed: %s", expression, mmsg);
		elektraVLog (ELEKTRA_LOG_LEVEL_ERROR, function, file, line, msg, args);
		va_end (args);
	}
	abort ();
}
