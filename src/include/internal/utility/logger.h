/**
 * @file
 *
 * @brief Logger Interface
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBLOGGER_H
#define KDBLOGGER_H

#include "kdbconfig.h"
#include "kdbmacros.h"

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

#if DEBUG
/**
 * @brief Sets the global minimum log level
 */
static const int ELEKTRA_LOG_LEVEL_GLOBAL = ELEKTRA_LOG_LEVEL_DEBUG;
#else
/**
 * @brief Sets the global minimum log level
 */
static const int ELEKTRA_LOG_LEVEL_GLOBAL = ELEKTRA_LOG_LEVEL_INFO;
#endif

/**
 * @brief Sets the minimum log level for the syslog sink
 */
static const int ELEKTRA_LOG_LEVEL_SYSLOG = ELEKTRA_LOG_LEVEL_DEBUG;

/**
 * @brief Sets the minimum log level for the stderr sink
 */
static const int ELEKTRA_LOG_LEVEL_STDERR = ELEKTRA_LOG_LEVEL_WARNING;

/**
 * @brief Sets the minimum log level for the file sink
 */
static const int ELEKTRA_LOG_LEVEL_FILE = ELEKTRA_LOG_LEVEL_DEBUG;


#ifdef __cplusplus
extern "C" {
#endif

int elektraLog (int level, const char * function, const char * file, int line, const char * msg, ...)
	ELEKTRA_ATTRIBUTE_FORMAT (printf, 5, 6);

#ifdef __cplusplus
}
#endif


#ifdef HAVE_LOGGER

#define ELEKTRA_LOG_WARNING(...) elektraLog (ELEKTRA_LOG_LEVEL_WARNING, __func__, __FILE__, __LINE__, __VA_ARGS__)
#define ELEKTRA_LOG_NOTICE(...) elektraLog (ELEKTRA_LOG_LEVEL_NOTICE, __func__, __FILE__, __LINE__, __VA_ARGS__)
#define ELEKTRA_LOG(...) elektraLog (ELEKTRA_LOG_LEVEL_INFO, __func__, __FILE__, __LINE__, __VA_ARGS__)
#define ELEKTRA_LOG_DEBUG(...) elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, __func__, __FILE__, __LINE__, __VA_ARGS__)

#else

#define ELEKTRA_LOG_WARNING(...)
#define ELEKTRA_LOG_NOTICE(...)
#define ELEKTRA_LOG(...)
#define ELEKTRA_LOG_DEBUG(...)

#endif

#endif
