/**
 * @file
 *
 * @brief Non-C99 Logger Implementation
 *
 * If you often change the file, you might want to set CMAKE_LINK_DEPENDS_NO_SHARED
 * to avoid relinking everything.
 *
 * Do not commit changes do this file, except if you want to change the default.
 * In that case, make sure to update also doc/tutorials/logger.md
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#include <internal/utility/logger.h>

#ifdef USE_SYSLOG_SINK
#include <syslog.h>
#endif

#ifdef USE_STDERR_SINK
#include <stdio.h>
#endif

#include <elektra/utility/format.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_STDERR_SINK

static int elektraLogStdErr (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			     const int line ELEKTRA_UNUSED, const char * msg)
{
#ifndef NO_FILTER
	// XXX Filter here for specific sink
	if (level < ELEKTRA_LOG_LEVEL_STDERR) return -1;
#endif
	int ret = fprintf (stderr, "%s", msg);
	fflush (stderr);
	return ret;
}
#endif

#ifdef USE_SYSLOG_SINK
static int elektraLogSyslog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			     const int line ELEKTRA_UNUSED, const char * msg)
{
#ifndef NO_FILTER
	// XXX Filter here for specific sink
	if (level < ELEKTRA_LOG_LEVEL_SYSLOG) return -1;
#endif
	int vlevel;
	switch (level)
	{
	case ELEKTRA_LOG_LEVEL_ERROR:
		vlevel = LOG_ERR;
		break;
	case ELEKTRA_LOG_LEVEL_WARNING:
		vlevel = LOG_WARNING;
		break;
	case ELEKTRA_LOG_LEVEL_NOTICE:
		vlevel = LOG_NOTICE;
		break;
	case ELEKTRA_LOG_LEVEL_INFO:
		vlevel = LOG_INFO;
		break;
	case ELEKTRA_LOG_LEVEL_DEBUG:
		vlevel = LOG_DEBUG;
		break;
	default:
		vlevel = LOG_CRIT;
	}

	openlog ("Elektra", LOG_PID, LOG_USER);
	syslog (vlevel, "%s", msg);
	return 0;
}
#endif

#ifdef USE_FILE_SINK
static FILE * elektraLoggerFileHandle;

static int elektraLogFile (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * file ELEKTRA_UNUSED,
			   const int line ELEKTRA_UNUSED, const char * msg)
{
#ifndef NO_FILTER
	// XXX Filter here for specific sink
	if (level < ELEKTRA_LOG_LEVEL_FILE) return -1;
#endif
	if (!elektraLoggerFileHandle)
	{
		elektraLoggerFileHandle = fopen ("elektra.log", "a");
	}
	int ret = fprintf (elektraLoggerFileHandle, "%s", msg);
	fflush (elektraLoggerFileHandle);
	return ret;
}
#endif

static void replaceChars (char * str)
{
	size_t lenOfMsg = strlen (str);
	size_t last = lenOfMsg - 1;
	do
	{
		if (str[lenOfMsg] == '\n')
			str[lenOfMsg] = '@';
		else if (str[lenOfMsg] == '\r')
			str[lenOfMsg] = '@';
		else if (str[lenOfMsg] == '\f')
			str[lenOfMsg] = '@';
		lenOfMsg--;
	} while (lenOfMsg);
	str[last] = '\n';
}

int elektraVLog (int level, const char * function, const char * absFile, int line, const char * mmsg, va_list args)
{
	const char * file;
	if (absFile[0] == '/' || absFile[0] == '.')
	{
		size_t lenOfLogFileName = sizeof ("src/libs/elektra/log.c") - 1;
		size_t lenOfLogPathName = strlen (__FILE__) - lenOfLogFileName;
		file = &absFile[lenOfLogPathName];
	}
	else
	{
		file = absFile;
	}

	int ret = -1;
#ifndef NO_FILTER
	// XXX Filter level here globally (for every sink)
	if (level < ELEKTRA_LOG_LEVEL_GLOBAL) return -1;

	// or e.g. discard everything, but log statements from simpleini.c:
	// if (strcmp (file, "src/plugins/simpleini/simpleini.c")) return -1;
	// and discard log statements from the log statement itself:
	if (!strcmp (file, "src/libs/elektra/log.c")) return -1;
#endif

	char * str;
	// XXX Change here default format for messages.
	//
	// For example, to use a style similar to the default one used by compilers such as Clang and GCC, replace the following statement
	// with:
	//
	//     str = elektraFormat ("%s:%d:%s: %s\n", file, line, function, mmsg);
	//
	// .
	str = elektraFormat ("%s (in %s at %s:%d)\n", mmsg, function, file, line);
	char * msg = elektraVFormat (str, args);
	replaceChars (msg);

#ifdef USE_STDERR_SINK
	ret |= elektraLogStdErr (level, function, file, line, msg);
#endif
#ifdef USE_SYSLOG_SINK
	ret |= elektraLogSyslog (level, function, file, line, msg);
#endif
#ifdef USE_FILE_SINK
	ret |= elektraLogFile (level, function, file, line, msg);
#endif

	elektraFree (str);
	elektraFree (msg);
	return ret;
}

int elektraLog (int level, const char * function, const char * absFile, const int line, const char * mmsg, ...)
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
		int exitCode = asprintf (&msg, "Assertion `%s' failed: %s", expression, mmsg);
		if (exitCode == -1 && msg == NULL)
		{
			fprintf (stderr, "Function `asprintf` was unable to allocate enough memory");
		}
		else if (exitCode < 0)
		{
			fprintf (stderr, "Call of function `asprintf` failed with error code %d", exitCode);
		}
		elektraVLog (ELEKTRA_LOG_LEVEL_ERROR, function, file, line, msg, args);
		va_end (args);
	}
	abort ();
}
