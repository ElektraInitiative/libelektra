/**
 * @file
 *
 * @brief Logger Interface
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBLOGGER_H
#define KDBLOGGER_H

#include "kdbconfig.h"

enum ElektraLogLevel
{
	/**
	 * @brief assertion failed, will abort
	 *
	 * Should only be used by ELEKTRA_ASSERT, not by other code!
	 */
	ELEKTRA_LOG_LEVEL_ERROR = 16,

	/**
	 * @brief something happened an end user would be interested in
	 *
	 * The action requested by the user failed in an unusual way, e.g.
	 * no memory.
	 *
	 * This level is especially for API misuse, something we want to
	 * report the user but otherwise could not (e.g. because we are not in
	 * kdb* context and cannot yield warnings/errors).
	 *
	 */
	ELEKTRA_LOG_LEVEL_WARNING = 8,

	/**
	 * @brief something important happened which usually should not
	 *
	 * Despite its name this level can be used for errors, failed
	 * activities and so on, if other plugins or similar might recover
	 * the error, the action will be retried, and/or the user will
	 * not notice or notice by warnings/errors/return value anyway.
	 */
	ELEKTRA_LOG_LEVEL_NOTICE = 4,

	/**
	 * @brief The standard log level.
	 *
	 * Use it to report mentionable occurrences which do not flood
	 * the system too badly. They should be indicative for what the
	 * problem is in the most cases.
	 */
	ELEKTRA_LOG_LEVEL_INFO = 2,

	/**
	 * @brief Alias for the standard log level.
	 */
	ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_INFO,

	/**
	 * @brief The debug log level.
	 *
	 * Can be used to log anything which was useful to debug
	 * the code. Is usually only turned on by the developer
	 * itself and you do not need consider the number of output
	 * it produces (expected to be used together with line-based
	 * suppressions in log.c).
	 */
	ELEKTRA_LOG_LEVEL_DEBUG = 1,
};

#ifdef HAVE_LOGGER

int elektraLog (int level, const char * function, const char * file, const int line, const char * msg, ...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 5, 6)))
#endif
	;


#define ELEKTRA_LOG_WARNING(...) elektraLog (ELEKTRA_LOG_LEVEL_WARNING, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG_NOTICE(...) elektraLog (ELEKTRA_LOG_LEVEL_NOTICE, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG(...) elektraLog (ELEKTRA_LOG_LEVEL, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG_DEBUG(...) elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, __func__, __FILE__, __LINE__, ##__VA_ARGS__)

#else

#define ELEKTRA_LOG_WARNING(...)
#define ELEKTRA_LOG_NOTICE(...)
#define ELEKTRA_LOG(...)
#define ELEKTRA_LOG_DEBUG(...)

#endif

#endif
